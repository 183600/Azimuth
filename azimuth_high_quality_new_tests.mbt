// Azimuth High-Quality New Test Suite
// This file contains comprehensive test cases focusing on advanced telemetry features

// Test 1: Advanced Telemetry Data Processing Pipeline
test "advanced telemetry data processing pipeline" {
  // Create a data processing pipeline
  let pipeline = {
    stages: [],
    error_handlers: [],
    metrics: {
      processed: 0,
      errors: 0,
      dropped: 0
    }
  }
  
  // Define pipeline stages
  let validation_stage = fn(data) {
    if data.trace_id != "" and data.span_id != "" {
      { valid: true, data }
    } else {
      { valid: false, error: "Invalid trace or span ID" }
    }
  }
  
  let enrichment_stage = fn(data) {
    let enriched_data = { data |
      timestamp: if data.timestamp == 0 { 1640995200 } else { data.timestamp },
      service_name: if data.service_name == "" { "unknown-service" } else { data.service_name },
      environment: if data.environment == "" { "production" } else { data.environment }
    }
    { valid: true, data: enriched_data }
  }
  
  let filtering_stage = fn(data) {
    let should_include = data.attributes.any(fn(attr) {
      match attr {
        ("sampling.priority", IntValue(priority)) => priority >= 5
        _ => false
      }
    }) or data.attributes.length() > 3
    
    if should_include {
      { valid: true, data }
    } else {
      { valid: false, error: "Filtered out by sampling policy" }
    }
  }
  
  // Process telemetry data through pipeline
  let process_through_pipeline = fn(pipeline, data) {
    let mut current_data = data
    let mut current_pipeline = pipeline
    
    // Validation stage
    match validation_stage(current_data) {
      { valid: true, data: valid_data } => {
        current_data = valid_data
      }
      { valid: false, error } => {
        return { pipeline | metrics: { pipeline.metrics | errors: pipeline.metrics.errors + 1 } }
      }
    }
    
    // Enrichment stage
    match enrichment_stage(current_data) {
      { valid: true, data: enriched_data } => {
        current_data = enriched_data
      }
      { valid: false, error } => {
        return { pipeline | metrics: { pipeline.metrics | errors: pipeline.metrics.errors + 1 } }
      }
    }
    
    // Filtering stage
    match filtering_stage(current_data) {
      { valid: true, data: filtered_data } => {
        current_data = filtered_data
      }
      { valid: false, error } => {
        return { pipeline | metrics: { pipeline.metrics | dropped: pipeline.metrics.dropped + 1 } }
      }
    }
    
    { current_pipeline | metrics: { current_pipeline.metrics | processed: current_pipeline.metrics.processed + 1 } }
  }
  
  // Test with valid data
  let valid_telemetry_data = {
    trace_id: "trace-pipeline-123",
    span_id: "span-456",
    timestamp: 0,  // Will be enriched
    service_name: "",  // Will be enriched
    environment: "",  // Will be enriched
    attributes: [
      ("sampling.priority", IntValue(7)),
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("/api/users")),
      ("http.status_code", IntValue(200))
    ]
  }
  
  let pipeline_after_valid = process_through_pipeline(pipeline, valid_telemetry_data)
  assert_eq(pipeline_after_valid.metrics.processed, 1)
  assert_eq(pipeline_after_valid.metrics.errors, 0)
  assert_eq(pipeline_after_valid.metrics.dropped, 0)
  
  // Test with invalid data
  let invalid_telemetry_data = {
    trace_id: "",  // Invalid
    span_id: "span-789",
    timestamp: 1640995300,
    service_name: "test-service",
    environment: "staging",
    attributes: [
      ("sampling.priority", IntValue(3)),
      ("operation.name", StringValue("test_operation"))
    ]
  }
  
  let pipeline_after_invalid = process_through_pipeline(pipeline_after_valid, invalid_telemetry_data)
  assert_eq(pipeline_after_invalid.metrics.processed, 1)  // Unchanged
  assert_eq(pipeline_after_invalid.metrics.errors, 1)
  assert_eq(pipeline_after_invalid.metrics.dropped, 0)
  
  // Test with filtered data
  let filtered_telemetry_data = {
    trace_id: "trace-pipeline-789",
    span_id: "span-012",
    timestamp: 1640995400,
    service_name: "low-priority-service",
    environment: "development",
    attributes: [
      ("sampling.priority", IntValue(2)),  // Low priority
      ("operation.name", StringValue("debug_operation"))
    ]
  }
  
  let pipeline_after_filtered = process_through_pipeline(pipeline_after_invalid, filtered_telemetry_data)
  assert_eq(pipeline_after_filtered.metrics.processed, 1)  // Unchanged
  assert_eq(pipeline_after_filtered.metrics.errors, 1)     // Unchanged
  assert_eq(pipeline_after_filtered.metrics.dropped, 1)
}

// Test 2: Distributed Tracing Context Propagation
test "distributed tracing context propagation with baggage" {
  // Create trace context with baggage
  let initial_context = {
    trace_id: "trace-distributed-123",
    span_id: "span-root",
    parent_span_id: None,
    baggage: [
      ("user.id", "user-456"),
      ("request.id", "req-789"),
      ("tenant.id", "tenant-001"),
      ("correlation.id", "corr-123")
    ],
    trace_flags: ["sampled", "debug"],
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // Simulate context extraction and injection
  let extract_context = fn(headers) {
    let trace_parent = headers.find(fn(h) { h.0 == "traceparent" })
    let trace_state = headers.find(fn(h) { h.0 == "tracestate" })
    let baggage_header = headers.find(fn(h) { h.0 == "baggage" })
    
    match trace_parent {
      Some((_, trace_parent_value)) => {
        let parts = trace_parent_value.split("-")
        if parts.length() >= 3 {
          let trace_id = parts[1]
          let span_id = parts[2]
          
          let baggage = match baggage_header {
            Some((_, baggage_value)) => {
              baggage_value.split(",").map(fn(item) {
                let kv = item.split("=")
                if kv.length() == 2 {
                  (kv[0].trim(), kv[1].trim())
                } else {
                  ("", "")
                }
              }).filter(fn(kv) { kv.0 != "" })
            }
            None => []
          }
          
          Some({
            trace_id,
            span_id,
            parent_span_id: None,
            baggage,
            trace_flags: ["sampled"],
            trace_state: match trace_state { Some((_, value)) => value, None => "" }
          })
        } else {
          None
        }
      }
      None => None
    }
  }
  
  let inject_context = fn(context) {
    let trace_parent = "00-" + context.trace_id + "-" + context.span_id + "-01"
    let baggage_value = context.baggage.map(fn(item) { item.0 + "=" + item.1 }).join(",")
    
    [
      ("traceparent", trace_parent),
      ("tracestate", context.trace_state),
      ("baggage", baggage_value)
    ]
  }
  
  // Test context injection
  let injected_headers = inject_context(initial_context)
  assert_eq(injected_headers.length(), 3)
  
  let trace_parent_header = injected_headers.find(fn(h) { h.0 == "traceparent" })
  match trace_parent_header {
    Some((_, value)) => {
      assert_true(value.contains("trace-distributed-123"))
      assert_true(value.contains("span-root"))
    }
    None => assert_true(false)
  }
  
  let baggage_header = injected_headers.find(fn(h) { h.0 == "baggage" })
  match baggage_header {
    Some((_, value)) => {
      assert_true(value.contains("user.id=user-456"))
      assert_true(value.contains("request.id=req-789"))
    }
    None => assert_true(false)
  }
  
  // Test context extraction
  let extracted_context = extract_context(injected_headers)
  assert_true(extracted_context.is_some())
  
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, "trace-distributed-123")
      assert_eq(context.span_id, "span-root")
      assert_eq(context.baggage.length(), 4)
      assert_true(context.baggage.contains(("user.id", "user-456")))
      assert_true(context.baggage.contains(("tenant.id", "tenant-001")))
    }
    None => assert_true(false)
  }
  
  // Test context propagation across service boundaries
  let create_child_context = fn(parent_context, span_name, additional_baggage) {
    let child_span_id = span_name + "-" + (Random::next_u64(Random::system()) % 10000).to_string()
    let merged_baggage = parent_context.baggage + additional_baggage
    
    {
      trace_id: parent_context.trace_id,
      span_id: child_span_id,
      parent_span_id: Some(parent_context.span_id),
      baggage: merged_baggage,
      trace_flags: parent_context.trace_flags,
      trace_state: parent_context.trace_state
    }
  }
  
  let service_a_context = create_child_context(
    initial_context,
    "service-a",
    [("service.name", "authentication-service")]
  )
  
  let service_b_context = create_child_context(
    service_a_context,
    "service-b",
    [("service.name", "user-service"), ("operation.type", "database_query")]
  )
  
  let service_c_context = create_child_context(
    service_b_context,
    "service-c",
    [("service.name", "notification-service")]
  )
  
  // Verify trace consistency
  assert_eq(service_a_context.trace_id, initial_context.trace_id)
  assert_eq(service_b_context.trace_id, initial_context.trace_id)
  assert_eq(service_c_context.trace_id, initial_context.trace_id)
  
  // Verify parent-child relationships
  assert_eq(service_a_context.parent_span_id, Some(initial_context.span_id))
  assert_eq(service_b_context.parent_span_id, Some(service_a_context.span_id))
  assert_eq(service_c_context.parent_span_id, Some(service_b_context.span_id))
  
  // Verify baggage propagation
  assert_eq(service_c_context.baggage.length(), 7)
  assert_true(service_c_context.baggage.contains(("user.id", "user-456")))
  assert_true(service_c_context.baggage.contains(("service.name", "notification-service")))
  assert_true(service_c_context.baggage.contains(("operation.type", "database_query")))
  
  // Test trace reconstruction
  let reconstruct_trace = fn(contexts) {
    let trace_id = contexts[0].trace_id
    let root_context = contexts.find(fn(ctx) { ctx.parent_span_id.is_none() })
    
    let sorted_contexts = contexts.sort(fn(a, b) {
      match a.parent_span_id {
        Some(parent_id) => parent_id == b.span_id
        None => false
      }
    })
    
    {
      trace_id,
      root_span: match root_context {
        Some(ctx) => ctx.span_id
        None => ""
      },
      span_count: contexts.length(),
      baggage_items: contexts.reduce(fn(acc, ctx) { acc + ctx.baggage }, []).unique().length()
    }
  }
  
  let trace_info = reconstruct_trace([
    initial_context,
    service_a_context,
    service_b_context,
    service_c_context
  ])
  
  assert_eq(trace_info.trace_id, "trace-distributed-123")
  assert_eq(trace_info.root_span, "span-root")
  assert_eq(trace_info.span_count, 4)
  assert_eq(trace_info.baggage_items, 7)
}

// Test 3: Telemetry Metrics Aggregation and Analysis
test "telemetry metrics aggregation and statistical analysis" {
  // Create metrics aggregator
  let metrics_aggregator = {
    counters: [],
    gauges: [],
    histograms: [],
    summaries: [],
    time_series: []
  }
  
  // Define metric types
  let record_counter = fn(aggregator, name, value, tags, timestamp) {
    let updated_counters = aggregator.counters.push({
      name,
      value,
      tags,
      timestamp
    })
    { aggregator | counters: updated_counters }
  }
  
  let record_gauge = fn(aggregator, name, value, tags, timestamp) {
    let updated_gauges = aggregator.gauges.push({
      name,
      value,
      tags,
      timestamp
    })
    { aggregator | gauges: updated_gauges }
  }
  
  let record_histogram = fn(aggregator, name, value, buckets, tags, timestamp) {
    let bucket_counts = buckets.map(fn(bucket) {
      let count = if value <= bucket { 1 } else { 0 }
      (bucket, count)
    })
    
    let updated_histograms = aggregator.histograms.push({
      name,
      value,
      buckets,
      bucket_counts,
      tags,
      timestamp
    })
    { aggregator | histograms: updated_histograms }
  }
  
  // Record various metrics
  let base_time = 1640995200
  let aggregator_with_metrics = (0..=99).reduce(fn(acc, i) {
    let timestamp = base_time + i * 10
    let service_tag = if i % 2 == 0 { "api" } else { "worker" }
    let status_tag = if i % 10 == 0 { "error" } else { "success" }
    
    acc
    |> record_counter("requests.total", 1, [("service", service_tag), ("status", status_tag)], timestamp)
    |> record_gauge("memory.usage", 512.0 + (i % 100).to_float(), [("service", service_tag)], timestamp)
    |> record_histogram("response.time", 50.0 + (i % 200).to_float(), [10.0, 50.0, 100.0, 200.0, 500.0], [("service", service_tag)], timestamp)
  }, metrics_aggregator)
  
  // Calculate aggregations
  let calculate_counter_sum = fn(aggregator, name, tag_filters) {
    aggregator.counters
      .filter(fn(c) { c.name == name })
      .filter(fn(c) { 
        tag_filters.all(fn(filter) {
          c.tags.any(fn(tag) { tag.0 == filter.0 and tag.1 == filter.1 })
        })
      })
      .reduce(fn(acc, c) { acc + c.value }, 0)
  }
  
  let calculate_gauge_stats = fn(aggregator, name, tag_filters) {
    let filtered_gauges = aggregator.gauges
      .filter(fn(g) { g.name == name })
      .filter(fn(g) { 
        tag_filters.all(fn(filter) {
          g.tags.any(fn(tag) { tag.0 == filter.0 and tag.1 == filter.1 })
        })
      })
    
    if filtered_gauges.length() > 0 {
      let values = filtered_gauges.map(fn(g) { g.value })
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let count = values.length().to_float()
      let avg = sum / count
      let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc }, values[0])
      let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc }, values[0])
      
      { avg, min, max, count: count.to_int() }
    } else {
      { avg: 0.0, min: 0.0, max: 0.0, count: 0 }
    }
  }
  
  let calculate_histogram_percentiles = fn(aggregator, name, tag_filters) {
    let filtered_histograms = aggregator.histograms
      .filter(fn(h) { h.name == name })
      .filter(fn(h) { 
        tag_filters.all(fn(filter) {
          h.tags.any(fn(tag) { tag.0 == filter.0 and tag.1 == filter.1 })
        })
      })
    
    if filtered_histograms.length() > 0 {
      let values = filtered_histograms.map(fn(h) { h.value }).sort(fn(a, b) { a < b })
      let count = values.length()
      
      let percentile = fn(p) {
        let index = ((p / 100.0) * count.to_float()).to_int()
        if index >= count { values[count - 1] } else { values[index] }
      }
      
      {
        p50: percentile(50),
        p90: percentile(90),
        p95: percentile(95),
        p99: percentile(99),
        count
      }
    } else {
      { p50: 0.0, p90: 0.0, p95: 0.0, p99: 0.0, count: 0 }
    }
  }
  
  // Test counter aggregations
  let total_requests = calculate_counter_sum(aggregator_with_metrics, "requests.total", [])
  assert_eq(total_requests, 100)
  
  let api_requests = calculate_counter_sum(aggregator_with_metrics, "requests.total", [("service", "api")])
  assert_eq(api_requests, 50)
  
  let error_requests = calculate_counter_sum(aggregator_with_metrics, "requests.total", [("status", "error")])
  assert_eq(error_requests, 10)
  
  let api_error_requests = calculate_counter_sum(aggregator_with_metrics, "requests.total", [("service", "api"), ("status", "error")])
  assert_eq(api_error_requests, 5)
  
  // Test gauge statistics
  let memory_stats = calculate_gauge_stats(aggregator_with_metrics, "memory.usage", [])
  assert_eq(memory_stats.count, 100)
  assert_true(memory_stats.avg > 512.0)
  assert_true(memory_stats.min >= 512.0)
  assert_true(memory_stats.max < 612.0)
  
  let api_memory_stats = calculate_gauge_stats(aggregator_with_metrics, "memory.usage", [("service", "api")])
  assert_eq(api_memory_stats.count, 50)
  
  // Test histogram percentiles
  let response_percentiles = calculate_histogram_percentiles(aggregator_with_metrics, "response.time", [])
  assert_eq(response_percentiles.count, 100)
  assert_true(response_percentiles.p50 >= 50.0)
  assert_true(response_percentiles.p90 <= 200.0)
  assert_true(response_percentiles.p95 <= 230.0)
  assert_true(response_percentiles.p99 <= 250.0)
  
  // Test rate calculations
  let calculate_rate = fn(aggregator, metric_name, window_seconds) {
    let metrics = aggregator.counters.filter(fn(c) { c.name == metric_name })
    if metrics.length() > 1 {
      let sorted_metrics = metrics.sort(fn(a, b) { a.timestamp < b.timestamp })
      let earliest = sorted_metrics[0].timestamp
      let latest = sorted_metrics[sorted_metrics.length() - 1].timestamp
      let time_diff = latest - earliest
      
      if time_diff > 0 {
        let total_value = sorted_metrics.reduce(fn(acc, m) { acc + m.value }, 0)
        (total_value.to_float() / time_diff.to_float()) * window_seconds.to_float()
      } else {
        0.0
      }
    } else {
      0.0
    }
  }
  
  let requests_per_second = calculate_rate(aggregator_with_metrics, "requests.total", 1)
  assert_eq(requests_per_second, 10.0)  // 100 requests over 10 seconds
  
  let requests_per_minute = calculate_rate(aggregator_with_metrics, "requests.total", 60)
  assert_eq(requests_per_minute, 600.0)  // 100 requests over 10 seconds * 6
  
  // Test time series aggregation
  let aggregate_by_time_window = fn(aggregator, metric_name, window_size) {
    let metrics = aggregator.counters.filter(fn(c) { c.name == metric_name })
    if metrics.length() > 0 {
      let sorted_metrics = metrics.sort(fn(a, b) { a.timestamp < b.timestamp })
      let start_time = sorted_metrics[0].timestamp
      
      let windows = sorted_metrics.reduce(fn(acc, metric) {
        let window_index = (metric.timestamp - start_time) / window_size
        let current_windows = acc
        
        let updated_windows = if current_windows.contains(window_index) {
          current_windows.map(fn(entry) {
            if entry.0 == window_index {
              (window_index, entry.1 + metric.value)
            } else {
              entry
            }
          })
        } else {
          current_windows.push((window_index, metric.value))
        }
        
        updated_windows
      }, []).sort(fn(a, b) { a.0 < b.0 })
      
      windows
    } else {
      []
    }
  }
  
  let time_windows = aggregate_by_time_window(aggregator_with_metrics, "requests.total", 100)  // 100-second windows
  assert_eq(time_windows.length(), 10)  // 1000 seconds total / 100-second windows
  
  let first_window_count = time_windows[0].1
  let last_window_count = time_windows[time_windows.length() - 1].1
  assert_eq(first_window_count, 10)  // 10 requests per window
  assert_eq(last_window_count, 10)
}

// Test 4: Telemetry Data Compression and Optimization
test "telemetry data compression and optimization techniques" {
  // Define telemetry data structure
  let telemetry_batch = {
    trace_id: "trace-compression-123",
    common_attributes: [
      ("service.name", "api-service"),
      ("service.version", "1.2.3"),
      ("environment", "production"),
      ("host.name", "server-001")
    ],
    spans: []
  }
  
  // Create spans with varying attribute overlap
  let create_span = fn(span_id, operation_name, duration, specific_attributes) {
    {
      span_id,
      operation_name,
      start_time: 1640995200,
      end_time: 1640995200 + duration,
      status: "ok",
      attributes: specific_attributes
    }
  }
  
  // Add spans to batch
  let spans = [
    create_span("span-001", "database_query", 50, [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users"),
      ("db.rows", IntValue(42))
    ]),
    create_span("span-002", "cache_lookup", 5, [
      ("cache.type", "redis"),
      ("cache.key", "user:123"),
      ("cache.hit", BoolValue(true))
    ]),
    create_span("span-003", "database_query", 75, [
      ("db.type", "postgresql"),
      ("db.statement", "UPDATE users SET active = true"),
      ("db.rows", IntValue(1))
    ]),
    create_span("span-004", "http_request", 120, [
      ("http.method", "POST"),
      ("http.url", "/api/users"),
      ("http.status_code", IntValue(201))
    ]),
    create_span("span-005", "cache_lookup", 3, [
      ("cache.type", "redis"),
      ("cache.key", "user:456"),
      ("cache.hit", BoolValue(false))
    ])
  ]
  
  let batch_with_spans = { telemetry_batch | spans }
  
  // Implement compression by extracting common attributes
  let extract_common_attributes = fn(batch) {
    let all_attributes = batch.spans.reduce(fn(acc, span) { acc + span.attributes }, [])
    
    // Find attributes that appear in multiple spans
    let attribute_frequency = all_attributes.reduce(fn(acc, attr) {
      let current_count = match acc.get(attr.0) {
        Some(count) => count
        None => 0
      }
      acc.set(attr.0, current_count + 1)
    }, {})
    
    let common_attrs = attribute_frequency.filter(fn(kv) { kv.1 > 1 }).keys()
    
    // Extract common attributes to batch level
    let new_common_attributes = batch.common_attributes + common_attrs.map(fn(attr_name) {
      let first_span_with_attr = batch.spans.find(fn(span) {
        span.attributes.any(fn(a) { a.0 == attr_name })
      })
      match first_span_with_attr {
        Some(span) => {
          let attr_value = span.attributes.find(fn(a) { a.0 == attr_name })
          match attr_value {
            Some((_, value)) => (attr_name, value)
            None => ("", StringValue(""))
          }
        }
        None => ("", StringValue(""))
      }
    }).filter(fn(attr) { attr.0 != "" })
    
    // Remove common attributes from individual spans
    let optimized_spans = batch.spans.map(fn(span) {
      let filtered_attributes = span.attributes.filter(fn(attr) {
        not(common_attrs.contains(attr.0))
      })
      { span | attributes: filtered_attributes }
    })
    
    {
      batch | 
      common_attributes: new_common_attributes,
      spans: optimized_spans
    }
  }
  
  // Test compression
  let compressed_batch = extract_common_attributes(batch_with_spans)
  
  // Should have extracted common database type and cache type
  assert_true(compressed_batch.common_attributes.length() > batch_with_spans.common_attributes.length())
  assert_true(compressed_batch.common_attributes.any(fn(attr) { 
    match attr {
      ("db.type", _) => true
      _ => false
    }
  }))
  assert_true(compressed_batch.common_attributes.any(fn(attr) { 
    match attr {
      ("cache.type", _) => true
      _ => false
    }
  }))
  
  // Individual spans should have fewer attributes
  for span in compressed_batch.spans {
    let original_span = batch_with_spans.spans.find(fn(s) { s.span_id == span.span_id })
    match original_span {
      Some(orig) => assert_true(span.attributes.length() <= orig.attributes.length())
      None => assert_true(false)
    }
  }
  
  // Calculate compression ratio
  let calculate_size = fn(batch) {
    let common_size = batch.common_attributes.length()
    let spans_size = batch.spans.reduce(fn(acc, span) { acc + span.attributes.length() }, 0)
    common_size + spans_size
  }
  
  let original_size = calculate_size(batch_with_spans)
  let compressed_size = calculate_size(compressed_batch)
  let compression_ratio = (original_size - compressed_size).to_float() / original_size.to_float()
  
  assert_true(compression_ratio > 0.0)  // Should have some compression
  assert_true(compression_ratio < 1.0)  // But not completely compressed
  
  // Test delta encoding for timestamps
  let apply_delta_encoding = fn(batch) {
    let sorted_spans = batch.spans.sort(fn(a, b) { a.start_time < b.start_time })
    
    if sorted_spans.length() > 0 {
      let base_timestamp = sorted_spans[0].start_time
      let delta_encoded_spans = sorted_spans.map_with_index(fn(i, span) {
        if i == 0 {
          { span | start_time: span.start_time - base_timestamp }
        } else {
          let prev_timestamp = sorted_spans[i - 1].start_time
          { span | start_time: span.start_time - prev_timestamp }
        }
      })
      
      { batch | spans: delta_encoded_spans, base_timestamp: Some(base_timestamp) }
    } else {
      { batch | base_timestamp: None }
    }
  }
  
  let delta_encoded_batch = apply_delta_encoding(compressed_batch)
  
  assert_true(delta_encoded_batch.base_timestamp.is_some())
  assert_eq(delta_encoded_batch.spans[0].start_time, 0)  // First span should have delta of 0
  
  // Test batch optimization by grouping similar operations
  let optimize_by_operation = fn(batch) {
    let grouped_spans = batch.spans.reduce(fn(acc, span) {
      let current_group = match acc.get(span.operation_name) {
        Some(group) => group
        None => []
      }
      acc.set(span.operation_name, current_group.push(span))
    }, {})
    
    let optimized_spans = grouped_spans.reduce(fn(acc, kv) {
      let operation_name = kv.0
      let spans_in_group = kv.1
      
      if spans_in_group.length() > 1 {
        // Create a template span for the group
        let template_span = spans_in_group[0]
        let common_group_attributes = spans_in_group.reduce(fn(acc, span) {
          span.attributes.filter(fn(attr) {
            acc.all(fn(a) { a.0 != attr.0 }) or spans_in_group.all(fn(s) {
              s.attributes.any(fn(sa) { sa.0 == attr.0 and sa.1 == attr.1 })
            })
          })
        }, template_span.attributes)
        
        let optimized_group_spans = spans_in_group.map(fn(span) {
          let diff_attributes = span.attributes.filter(fn(attr) {
            not(common_group_attributes.any_fn(common_attr => common_attr.0 == attr.0 and common_attr.1 == attr.1))
          })
          { span | attributes: diff_attributes }
        })
        
        acc + optimized_group_spans
      } else {
        acc + spans_in_group
      }
    }, [])
    
    { batch | spans: optimized_spans }
  }
  
  let operation_optimized_batch = optimize_by_operation(delta_encoded_batch)
  
  // Should have fewer attributes in database_query spans
  let db_spans = operation_optimized_batch.spans.filter(fn(s) { s.operation_name == "database_query" })
  assert_eq(db_spans.length(), 2)
  
  for span in db_spans {
    assert_true(span.attributes.length() <= 2)  // Should only have operation-specific attributes
  }
  
  // Test final compression metrics
  let final_size = calculate_size(operation_optimized_batch)
  let final_compression_ratio = (original_size - final_size).to_float() / original_size.to_float()
  
  assert_true(final_compression_ratio >= compression_ratio)  // Should have better compression
  assert_true(final_compression_ratio > 0.1)  // At least 10% compression
}

// Test 5: Telemetry Error Handling and Recovery Strategies
test "telemetry error handling and recovery strategies" {
  // Define error types
  enum TelemetryError {
    NetworkTimeout(String, Int)  // message, timeout_ms
    SerializationError(String)   // message
    BufferOverflow(Int)          // buffer_size
    RateLimitExceeded(Int)       // retry_after_seconds
    AuthenticationError(String)  // message
    ConfigurationError(String)   // message
  }
  
  // Define recovery strategies
  enum RecoveryStrategy {
    Retry(Int)                    // max_attempts
    ExponentialBackoff(Int, Int)  // max_attempts, base_delay_ms
    CircuitBreaker(Int, Int)      // failure_threshold, recovery_timeout_ms
    Fallback(String)              // fallback_value
    DropData                      // Drop the telemetry data
    QueueForLater                 // Queue for later processing
  }
  
  // Create error handler configuration
  let error_handler_config = {
    default_strategies: {
      TelemetryError::NetworkTimeout(_, _) => ExponentialBackoff(3, 1000),
      TelemetryError::SerializationError(_) => Fallback("default_serialized_data"),
      TelemetryError::BufferOverflow(_) => CircuitBreaker(5, 30000),
      TelemetryError::RateLimitExceeded(_) => Retry(1),
      TelemetryError::AuthenticationError(_) => DropData,
      TelemetryError::ConfigurationError(_) => Fallback("default_config")
    },
    circuit_breaker_states: {},
    retry_attempts: {},
    last_errors: {}
  }
  
  // Implement error handling logic
  let handle_error = fn(config, error, operation_id) {
    let strategy = config.default_strategies.get(error)
    
    match strategy {
      Some(ExponentialBackoff(max_attempts, base_delay)) => {
        let current_attempts = config.retry_attempts.get(operation_id).unwrap_or(0)
        if current_attempts < max_attempts {
          let delay = base_delay * (2 ^ current_attempts)
          {
            action: "retry",
            delay_ms: delay,
            updated_config: { config | 
              retry_attempts: config.retry_attempts.set(operation_id, current_attempts + 1)
            }
          }
        } else {
          {
            action: "max_retries_exceeded",
            delay_ms: 0,
            updated_config: config
          }
        }
      }
      Some(CircuitBreaker(threshold, timeout)) => {
        let failure_count = config.circuit_breaker_states.get("global_failure_count").unwrap_or(0)
        if failure_count < threshold {
          {
            action: "proceed",
            delay_ms: 0,
            updated_config: { config |
              circuit_breaker_states: config.circuit_breaker_states.set("global_failure_count", failure_count + 1)
            }
          }
        } else {
          {
            action: "circuit_breaker_open",
            delay_ms: timeout,
            updated_config: { config |
              circuit_breaker_states: config.circuit_breaker_states.set("circuit_open", true)
            }
          }
        }
      }
      Some(Fallback(fallback_value)) => {
        {
          action: "use_fallback",
          delay_ms: 0,
          fallback_value: Some(fallback_value),
          updated_config: config
        }
      }
      Some(Retry(max_attempts)) => {
        let current_attempts = config.retry_attempts.get(operation_id).unwrap_or(0)
        if current_attempts < max_attempts {
          {
            action: "retry",
            delay_ms: 1000,
            updated_config: { config | 
              retry_attempts: config.retry_attempts.set(operation_id, current_attempts + 1)
            }
          }
        } else {
          {
            action: "max_retries_exceeded",
            delay_ms: 0,
            updated_config: config
          }
        }
      }
      Some(DropData) => {
        {
          action: "drop_data",
          delay_ms: 0,
          updated_config: config
        }
      }
      Some(QueueForLater) => {
        {
          action: "queue_for_later",
          delay_ms: 0,
          updated_config: config
        }
      }
      None => {
        {
          action: "unknown_error",
          delay_ms: 0,
          updated_config: config
        }
      }
    }
  }
  
  // Test network timeout with exponential backoff
  let network_error = TelemetryError::NetworkTimeout("Connection timeout", 5000)
  let operation_id = "send_telemetry_data"
  
  let first_retry_result = handle_error(error_handler_config, network_error, operation_id)
  assert_eq(first_retry_result.action, "retry")
  assert_eq(first_retry_result.delay_ms, 1000)
  assert_eq(first_retry_result.updated_config.retry_attempts.get(operation_id), Some(1))
  
  let second_retry_result = handle_error(first_retry_result.updated_config, network_error, operation_id)
  assert_eq(second_retry_result.action, "retry")
  assert_eq(second_retry_result.delay_ms, 2000)  // 1000 * 2^1
  assert_eq(second_retry_result.updated_config.retry_attempts.get(operation_id), Some(2))
  
  let third_retry_result = handle_error(second_retry_result.updated_config, network_error, operation_id)
  assert_eq(third_retry_result.action, "retry")
  assert_eq(third_retry_result.delay_ms, 4000)  // 1000 * 2^2
  assert_eq(third_retry_result.updated_config.retry_attempts.get(operation_id), Some(3))
  
  let max_retries_result = handle_error(third_retry_result.updated_config, network_error, operation_id)
  assert_eq(max_retries_result.action, "max_retries_exceeded")
  assert_eq(max_retries_result.delay_ms, 0)
  
  // Test circuit breaker
  let buffer_error = TelemetryError::BufferOverflow(1024)
  let cb_operation_id = "process_batch"
  
  let cb_result1 = handle_error(error_handler_config, buffer_error, cb_operation_id)
  assert_eq(cb_result1.action, "proceed")
  assert_eq(cb_result1.updated_config.circuit_breaker_states.get("global_failure_count"), Some(1))
  
  let cb_result2 = handle_error(cb_result1.updated_config, buffer_error, cb_operation_id)
  assert_eq(cb_result2.action, "proceed")
  assert_eq(cb_result2.updated_config.circuit_breaker_states.get("global_failure_count"), Some(2))
  
  // Simulate reaching threshold
  let cb_config_with_failures = (0..=3).reduce(fn(acc, _) {
    let result = handle_error(acc, buffer_error, cb_operation_id)
    result.updated_config
  }, cb_result2.updated_config)
  
  assert_eq(cb_config_with_failures.circuit_breaker_states.get("global_failure_count"), Some(5))
  
  // Next error should trigger circuit breaker
  let cb_trigger_result = handle_error(cb_config_with_failures, buffer_error, cb_operation_id)
  assert_eq(cb_trigger_result.action, "circuit_breaker_open")
  assert_eq(cb_trigger_result.delay_ms, 30000)
  assert_true(cb_trigger_result.updated_config.circuit_breaker_states.get("circuit_open").unwrap_or(false))
  
  // Test fallback strategy
  let serialization_error = TelemetryError::SerializationError("Invalid JSON format")
  let fallback_result = handle_error(error_handler_config, serialization_error, "serialize_data")
  
  assert_eq(fallback_result.action, "use_fallback")
  assert_eq(fallback_result.fallback_value, Some("default_serialized_data"))
  
  // Test rate limiting with retry
  let rate_limit_error = TelemetryError::RateLimitExceeded(60)
  let rate_limit_result = handle_error(error_handler_config, rate_limit_error, "send_metrics")
  
  assert_eq(rate_limit_result.action, "retry")
  assert_eq(rate_limit_result.delay_ms, 1000)
  
  // Test authentication error with drop data
  let auth_error = TelemetryError::AuthenticationError("Invalid API key")
  let auth_result = handle_error(error_handler_config, auth_error, "authenticate")
  
  assert_eq(auth_result.action, "drop_data")
  
  // Test error recovery simulation
  let simulate_error_recovery = fn(config, errors, operation_id) {
    errors.reduce(fn(acc_config, error) {
      let result = handle_error(acc_config, error, operation_id)
      
      match result.action {
        "retry" => {
          // Simulate successful retry after delay
          if result.delay_ms > 2000 {  // After exponential backoff
            // Reset retry attempts on success
            { result.updated_config | 
              retry_attempts: result.updated_config.retry_attempts.remove(operation_id)
            }
          } else {
            result.updated_config
          }
        }
        "circuit_breaker_open" => {
          // Simulate circuit breaker recovery after timeout
          { result.updated_config |
            circuit_breaker_states: result.updated_config.circuit_breaker_states
              .remove("circuit_open")
              .set("global_failure_count", 0)
          }
        }
        "use_fallback" => result.updated_config
        "drop_data" => result.updated_config
        _ => result.updated_config
      }
    }, config)
  }
  
  let recovery_errors = [
    TelemetryError::NetworkTimeout("Timeout", 1000),
    TelemetryError::NetworkTimeout("Timeout", 1000),
    TelemetryError::NetworkTimeout("Timeout", 1000)
  ]
  
  let recovered_config = simulate_error_recovery(error_handler_config, recovery_errors, "recovery_test")
  
  // Should have reset retry attempts after successful recovery
  assert_eq(recovered_config.retry_attempts.get("recovery_test"), None)
}

// Test 6: Telemetry Sampling Strategies
test "telemetry sampling strategies and algorithms" {
  // Define sampling strategies
  enum SamplingStrategy {
    Constant(Float)           // Fixed sampling rate (0.0 to 1.0)
    Probabilistic(Float)      // Probability-based sampling
    RateLimiting(Int)         // Max samples per second
    Adaptive(String)          // Adaptive based on system conditions
    TraceIdBased(Float)       // Based on trace ID hash
  }
  
  // Create sampling configuration
  let sampling_config = {
    default_strategy: SamplingStrategy::Probabilistic(0.1),  // 10% sampling
    operation_strategies: {
      "database_query" => SamplingStrategy::RateLimiting(100),
      "http_request" => SamplingStrategy::Adaptive("error_rate"),
      "cache_lookup" => SamplingStrategy::Constant(0.05),  // 5% sampling
      "critical_operation" => SamplingStrategy::TraceIdBased(1.0)  // 100% for critical traces
    },
    adaptive_parameters: {
      "error_rate" => {
        high_threshold: 0.05,      // 5% error rate
        low_threshold: 0.01,       // 1% error rate
        high_sample_rate: 0.5,     // 50% sampling when errors are high
        low_sample_rate: 0.05      // 5% sampling when errors are low
      }
    },
    rate_limit_counters: {},
    trace_id_salt: "azimuth-sampling-salt"
  }
  
  // Implement sampling decision logic
  let should_sample = fn(config, operation_name, trace_id, attributes) {
    let strategy = config.operation_strategies.get(operation_name).unwrap_or(config.default_strategy)
    
    match strategy {
      SamplingStrategy::Constant(rate) => {
        Random::next_float(Random::system()) < rate
      }
      SamplingStrategy::Probabilistic(rate) => {
        Random::next_float(Random::system()) < rate
      }
      SamplingStrategy::RateLimiting(max_per_second) => {
        let current_time = 1640995200
        let time_window = current_time / 1  // 1-second windows
        let current_count = config.rate_limit_counters.get(time_window.to_string()).unwrap_or(0)
        current_count < max_per_second
      }
      SamplingStrategy::Adaptive(param_name) => {
        let params = config.adaptive_parameters.get(param_name)
        match params {
          Some(param) => {
            // Simulate current error rate (would come from metrics in real implementation)
            let current_error_rate = 0.03  // 3% error rate
            
            if current_error_rate > param.high_threshold {
              Random::next_float(Random::system()) < param.high_sample_rate
            } else if current_error_rate < param.low_threshold {
              Random::next_float(Random::system()) < param.low_sample_rate
            } else {
              // Linear interpolation between low and high rates
              let rate_range = param.high_sample_rate - param.low_sample_rate
              let error_range = param.high_threshold - param.low_threshold
              let error_position = (current_error_rate - param.low_threshold) / error_range
              let interpolated_rate = param.low_sample_rate + (rate_range * error_position)
              
              Random::next_float(Random::system()) < interpolated_rate
            }
          }
          None => false
        }
      }
      SamplingStrategy::TraceIdBased(rate) => {
        // Simple hash-based deterministic sampling
        let hash_input = config.trace_id_salt + trace_id
        let hash_value = hash_input.length() % 1000  // Simple hash simulation
        let normalized_hash = hash_value.to_float() / 1000.0
        normalized_hash < rate
      }
    }
  }
  
  // Update rate limit counters
  let update_rate_limit_counter = fn(config, operation_name) {
    let current_time = 1640995200
    let time_window = current_time / 1
    let window_key = time_window.to_string()
    let current_count = config.rate_limit_counters.get(window_key).unwrap_or(0)
    
    { config |
      rate_limit_counters: config.rate_limit_counters.set(window_key, current_count + 1)
    }
  }
  
  // Test constant sampling
  let constant_operation = "cache_lookup"
  let constant_sampled = (0..=99).filter(fn(_) {
    should_sample(sampling_config, constant_operation, "trace-123", [])
  }).length()
  
  assert_true(constant_sampled >= 0)  // Should be around 5% (5 samples out of 100)
  assert_true(constant_sampled <= 15)  // Allow some variance
  
  // Test trace ID-based deterministic sampling
  let trace_id_operation = "critical_operation"
  let trace_id_samples = ["trace-001", "trace-002", "trace-003", "trace-004", "trace-005"]
  
  let trace_id_results = trace_id_samples.map(fn(trace_id) {
    let sampled = should_sample(sampling_config, trace_id_operation, trace_id, [])
    (trace_id, sampled)
  })
  
  // All critical operations should be sampled (100% rate)
  for result in trace_id_results {
    assert_true(result.1)  // Should all be true
  }
  
  // Test rate limiting sampling
  let rate_limit_operation = "database_query"
  let mut rate_limit_config = sampling_config
  
  let rate_limit_results = (0..=149).map(fn(i) {
    let should_sample_result = should_sample(rate_limit_config, rate_limit_operation, "trace-" + i.to_string(), [])
    if should_sample_result {
      rate_limit_config = update_rate_limit_counter(rate_limit_config, rate_limit_operation)
    }
    should_sample_result
  })
  
  let rate_limit_sampled_count = rate_limit_results.filter(fn(sampled) { sampled }).length()
  assert_eq(rate_limit_sampled_count, 100)  // Should be exactly 100 (rate limit)
  
  // Test adaptive sampling
  let adaptive_operation = "http_request"
  let adaptive_sampled = (0..=99).filter(fn(_) {
    should_sample(sampling_config, adaptive_operation, "trace-123", [])
  }).length()
  
  // With 3% error rate (between 1% and 5%), should interpolate between 5% and 50%
  assert_true(adaptive_sampled >= 5)   // At least 5% (low rate)
  assert_true(adaptive_sampled <= 50)  // At most 50% (high rate)
  
  // Test sampling with priority attribute
  let should_sample_with_priority = fn(config, operation_name, trace_id, attributes) {
    let priority_attr = attributes.find(fn(attr) {
      match attr {
        ("sampling.priority", _) => true
        _ => false
      }
    })
    
    match priority_attr {
      Some((_, IntValue(priority))) => {
        if priority >= 8 {
          true  // Always sample high priority
        } else if priority >= 5 {
          // Higher sampling rate for medium priority
          Random::next_float(Random::system()) < 0.5
        } else {
          should_sample(config, operation_name, trace_id, attributes)
        }
      }
      _ => should_sample(config, operation_name, trace_id, attributes)
    }
  }
  
  // Test priority-based sampling
  let high_priority_attrs = [("sampling.priority", IntValue(9))]
  let medium_priority_attrs = [("sampling.priority", IntValue(6))]
  let low_priority_attrs = [("sampling.priority", IntValue(3))]
  
  let high_priority_sampled = should_sample_with_priority(sampling_config, "cache_lookup", "trace-123", high_priority_attrs)
  let medium_priority_sampled = should_sample_with_priority(sampling_config, "cache_lookup", "trace-124", medium_priority_attrs)
  let low_priority_sampled = should_sample_with_priority(sampling_config, "cache_lookup", "trace-125", low_priority_attrs)
  
  assert_true(high_priority_sampled)  // Always sample high priority
  // Medium and low priority follow normal sampling rules
  
  // Test sampling decision consistency
  let test_sampling_consistency = fn(config, operation_name, trace_id, attributes, num_checks) {
    let results = (0..=num_checks - 1).map(fn(_) {
      should_sample(config, operation_name, trace_id, attributes)
    })
    
    let sampled_count = results.filter(fn(r) { r }).length()
    let sampling_rate = sampled_count.to_float() / num_checks.to_float()
    
    sampling_rate
  }
  
  // Test probabilistic sampling consistency
  let probabilistic_rate = test_sampling_consistency(sampling_config, "cache_lookup", "trace-consistency", [], 1000)
  assert_true(probabilistic_rate >= 0.02)  // Within variance of 5%
  assert_true(probabilistic_rate <= 0.08)
  
  // Test trace ID-based sampling consistency (should be deterministic)
  let trace_id_rate1 = test_sampling_consistency(sampling_config, "critical_operation", "trace-deterministic", [], 10)
  let trace_id_rate2 = test_sampling_consistency(sampling_config, "critical_operation", "trace-deterministic", [], 10)
  assert_eq(trace_id_rate1, trace_id_rate2)  // Should be identical
  assert_eq(trace_id_rate1, 1.0)  // Should always be 1.0 (100% sampling)
}

// Test 7: Telemetry Data Validation and Quality Assurance
test "telemetry data validation and quality assurance" {
  // Define validation rules
  let validation_rules = {
    trace_id: {
      required: true,
      pattern: "^[a-f0-9]{32}$|^[a-zA-Z0-9-]{1,64}$",
      min_length: 1,
      max_length: 64
    },
    span_id: {
      required: true,
      pattern: "^[a-f0-9]{16}$|^[a-zA-Z0-9-]{1,16}$",
      min_length: 1,
      max_length: 16
    },
    operation_name: {
      required: true,
      min_length: 1,
      max_length: 128,
      forbidden_chars: ["<", ">", "&", "\"", "'"]
    },
    timestamp: {
      required: true,
      min_value: 0,
      max_value: 4102444800  // Year 2100
    },
    duration: {
      required: false,
      min_value: 0,
      max_value: 3600000  // 1 hour in milliseconds
    },
    status: {
      required: true,
      allowed_values: ["ok", "error", "timeout", "cancelled"]
    }
  }
  
  // Define attribute validation rules
  let attribute_validation_rules = {
    "http.method": {
      type: "string",
      allowed_values: ["GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS"]
    },
    "http.status_code": {
      type: "integer",
      min_value: 100,
      max_value: 599
    },
    "http.url": {
      type: "string",
      max_length: 2048,
      pattern: "^https?://.*"
    },
    "db.type": {
      type: "string",
      allowed_values: ["postgresql", "mysql", "redis", "mongodb", "elasticsearch"]
    },
    "db.rows": {
      type: "integer",
      min_value: 0
    },
    "sampling.priority": {
      type: "integer",
      min_value: 0,
      max_value: 10
    }
  }
  
  // Create validator
  let validate_string_field = fn(value, rule, field_name) {
    let mut errors = []
    
    if rule.required and (value == "" or value == None) {
      errors = errors.push(field_name + " is required")
    }
    
    match value {
      Some(str_value) => {
        if str_value.length() < rule.min_length {
          errors = errors.push(field_name + " is too short (min: " + rule.min_length.to_string() + ")")
        }
        
        if str_value.length() > rule.max_length {
          errors = errors.push(field_name + " is too long (max: " + rule.max_length.to_string() + ")")
        }
        
        if rule.forbidden_chars.length() > 0 {
          let contains_forbidden = rule.forbidden_chars.any_fn(char => str_value.contains(char))
          if contains_forbidden {
            errors = errors.push(field_name + " contains forbidden characters")
          }
        }
        
        if rule.pattern != "" {
          // Simple pattern matching simulation
          if field_name == "trace_id" {
            if not(str_value.length() >= 1 and str_value.length() <= 64) {
              errors = errors.push(field_name + " does not match required pattern")
            }
          } else if field_name == "span_id" {
            if not(str_value.length() >= 1 and str_value.length() <= 16) {
              errors = errors.push(field_name + " does not match required pattern")
            }
          }
        }
      }
      None => {}  // Already handled by required check
    }
    
    errors
  }
  
  let validate_integer_field = fn(value, rule, field_name) {
    let mut errors = []
    
    if rule.required and value == None {
      errors = errors.push(field_name + " is required")
    }
    
    match value {
      Some(int_value) => {
        if int_value < rule.min_value {
          errors = errors.push(field_name + " is too small (min: " + rule.min_value.to_string() + ")")
        }
        
        if rule.max_value != None and int_value > rule.max_value.unwrap() {
          errors = errors.push(field_name + " is too large (max: " + rule.max_value.unwrap().to_string() + ")")
        }
      }
      None => {}  // Already handled by required check
    }
    
    errors
  }
  
  let validate_attribute = fn(key, value, rule) {
    let mut errors = []
    
    match rule.type {
      "string" => {
        match value {
          StringValue(str_val) => {
            if rule.allowed_values.length() > 0 and not(rule.allowed_values.contains(str_val)) {
              errors = errors.push(key + " has invalid value: " + str_val)
            }
            
            if rule.max_length != None and str_val.length() > rule.max_length.unwrap() {
              errors = errors.push(key + " is too long")
            }
            
            if rule.pattern != "" and key == "http.url" {
              if not(str_val.starts_with("http://") or str_val.starts_with("https://")) {
                errors = errors.push(key + " must start with http:// or https://")
              }
            }
          }
          _ => errors = errors.push(key + " must be a string")
        }
      }
      "integer" => {
        match value {
          IntValue(int_val) => {
            if int_val < rule.min_value {
              errors = errors.push(key + " is too small")
            }
            
            if rule.max_value != None and int_val > rule.max_value.unwrap() {
              errors = errors.push(key + " is too large")
            }
          }
          _ => errors = errors.push(key + " must be an integer")
        }
      }
      _ => errors = errors.push(key + " has unknown type")
    }
    
    errors
  }
  
  let validate_telemetry_data = fn(data) {
    let mut all_errors = []
    
    // Validate trace_id
    let trace_id_errors = validate_string_field(Some(data.trace_id), validation_rules.trace_id, "trace_id")
    all_errors = all_errors + trace_id_errors
    
    // Validate span_id
    let span_id_errors = validate_string_field(Some(data.span_id), validation_rules.span_id, "span_id")
    all_errors = all_errors + span_id_errors
    
    // Validate operation_name
    let operation_errors = validate_string_field(Some(data.operation_name), validation_rules.operation_name, "operation_name")
    all_errors = all_errors + operation_errors
    
    // Validate timestamp
    let timestamp_errors = validate_integer_field(Some(data.timestamp), validation_rules.timestamp, "timestamp")
    all_errors = all_errors + timestamp_errors
    
    // Validate duration if present
    if data.duration != None {
      let duration_errors = validate_integer_field(data.duration, validation_rules.duration, "duration")
      all_errors = all_errors + duration_errors
    }
    
    // Validate status
    let status_errors = validate_string_field(Some(data.status), validation_rules.status, "status")
    all_errors = all_errors + status_errors
    
    // Validate attributes
    for (key, value) in data.attributes {
      let attribute_rule = attribute_validation_rules.get(key)
      match attribute_rule {
        Some(rule) => {
          let attr_errors = validate_attribute(key, value, rule)
          all_errors = all_errors + attr_errors
        }
        None => {}  // No validation rule for this attribute
      }
    }
    
    {
      valid: all_errors.length() == 0,
      errors: all_errors
    }
  }
  
  // Test valid telemetry data
  let valid_data = {
    trace_id: "trace-validation-123456789012345678901234567890",
    span_id: "span-1234",
    operation_name: "database_query",
    timestamp: 1640995200,
    duration: Some(150),
    status: "ok",
    attributes: [
      ("http.method", StringValue("GET")),
      ("http.status_code", IntValue(200)),
      ("http.url", StringValue("https://api.example.com/users")),
      ("db.type", StringValue("postgresql")),
      ("db.rows", IntValue(42)),
      ("sampling.priority", IntValue(5))
    ]
  }
  
  let valid_result = validate_telemetry_data(valid_data)
  assert_true(valid_result.valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid trace_id
  let invalid_trace_data = { valid_data | trace_id: "" }
  let invalid_trace_result = validate_telemetry_data(invalid_trace_data)
  assert_false(invalid_trace_result.valid)
  assert_true(invalid_trace_result.errors.any_fn(err => err.contains("trace_id is required")))
  
  // Test invalid operation_name with forbidden characters
  let invalid_operation_data = { valid_data | operation_name: "bad<script>" }
  let invalid_operation_result = validate_telemetry_data(invalid_operation_data)
  assert_false(invalid_operation_result.valid)
  assert_true(invalid_operation_result.errors.any_fn(err => err.contains("contains forbidden characters")))
  
  // Test invalid timestamp
  let invalid_timestamp_data = { valid_data | timestamp: -1 }
  let invalid_timestamp_result = validate_telemetry_data(invalid_timestamp_data)
  assert_false(invalid_timestamp_result.valid)
  assert_true(invalid_timestamp_result.errors.any_fn(err => err.contains("is too small")))
  
  // Test invalid duration
  let invalid_duration_data = { valid_data | duration: Some(4000000) }  // Over 1 hour
  let invalid_duration_result = validate_telemetry_data(invalid_duration_data)
  assert_false(invalid_duration_result.valid)
  assert_true(invalid_duration_result.errors.any_fn(err => err.contains("is too large")))
  
  // Test invalid status
  let invalid_status_data = { valid_data | status: "unknown" }
  let invalid_status_result = validate_telemetry_data(invalid_status_data)
  assert_false(invalid_status_result.valid)
  assert_true(invalid_status_result.errors.any_fn(err => err.contains("status has invalid value")))
  
  // Test invalid attribute type
  let invalid_attr_type_data = { valid_data | 
    attributes: [
      ("http.status_code", StringValue("200"))  // Should be integer
    ]
  }
  let invalid_attr_type_result = validate_telemetry_data(invalid_attr_type_data)
  assert_false(invalid_attr_type_result.valid)
  assert_true(invalid_attr_type_result.errors.any_fn(err => err.contains("must be an integer")))
  
  // Test invalid attribute value
  let invalid_attr_value_data = { valid_data | 
    attributes: [
      ("http.method", StringValue("INVALID"))  // Not in allowed values
    ]
  }
  let invalid_attr_value_result = validate_telemetry_data(invalid_attr_value_data)
  assert_false(invalid_attr_value_result.valid)
  assert_true(invalid_attr_value_result.errors.any_fn(err => err.contains("has invalid value")))
  
  // Test invalid URL pattern
  let invalid_url_data = { valid_data | 
    attributes: [
      ("http.url", StringValue("ftp://example.com"))  // Invalid protocol
    ]
  }
  let invalid_url_result = validate_telemetry_data(invalid_url_data)
  assert_false(invalid_url_result.valid)
  assert_true(invalid_url_result.errors.any_fn(err => err.contains("must start with http:// or https://")))
  
  // Test data sanitization
  let sanitize_string = fn(input) {
    let mut sanitized = input
    
    // Remove potentially dangerous characters
    let dangerous_chars = ["<", ">", "&", "\"", "'", "\x00", "\n", "\r", "\t"]
    for char in dangerous_chars {
      sanitized = sanitized.replace(char, "")
    }
    
    // Trim whitespace
    sanitized = sanitized.trim()
    
    // Ensure reasonable length
    if sanitized.length() > 1024 {
      sanitized = sanitized.substring(0, 1024)
    }
    
    sanitized
  }
  
  let test_sanitization = fn() {
    let malicious_input = "<script>alert('xss')</script>\n\r\t"
    let sanitized = sanitize_string(malicious_input)
    
    assert_false(sanitized.contains("<script>"))
    assert_false(sanitized.contains("alert"))
    assert_false(sanitized.contains("'"))
    assert_false(sanitized.contains("\n"))
    assert_false(sanitized.contains("\r"))
    assert_false(sanitized.contains("\t"))
  }
  
  test_sanitization()
  
  // Test data quality metrics
  let calculate_quality_metrics = fn(validation_results) {
    let total = validation_results.length()
    let valid_count = validation_results.filter(fn(r) { r.valid }).length()
    let invalid_count = total - valid_count
    
    let error_types = validation_results
      .filter(fn(r) { not(r.valid) })
      .reduce(fn(acc, result) {
        result.errors.reduce(fn(inner_acc, error) {
          let current_count = inner_acc.get(error).unwrap_or(0)
          inner_acc.set(error, current_count + 1)
        }, acc)
      }, {})
    
    let most_common_error = error_types
      .reduce(fn(acc, kv) {
        if kv.1 > acc.1 { kv } else { acc }
      }, ("", 0))
    
    {
      total_validated: total,
      valid_count,
      invalid_count,
      quality_score: if total > 0 { valid_count.to_float() / total.to_float() } else { 0.0 },
      error_types: error_types.length(),
      most_common_error: most_common_error.0
    }
  }
  
  let test_data_set = [
    valid_data,
    invalid_trace_data,
    invalid_operation_data,
    invalid_timestamp_data,
    invalid_status_data
  ]
  
  let validation_results = test_data_set.map(validate_telemetry_data)
  let quality_metrics = calculate_quality_metrics(validation_results)
  
  assert_eq(quality_metrics.total_validated, 5)
  assert_eq(quality_metrics.valid_count, 1)
  assert_eq(quality_metrics.invalid_count, 4)
  assert_eq(quality_metrics.quality_score, 0.2)  // 1/5 = 0.2
  assert_true(quality_metrics.error_types > 0)
}

// Test 8: Telemetry Performance Optimization
test "telemetry performance optimization techniques" {
  // Create performance optimization configuration
  let optimization_config = {
    batch_size: 100,
    flush_interval_ms: 5000,
    compression_enabled: true,
    async_processing: true,
    memory_limit_mb: 100,
    cpu_threshold_percent: 80.0,
    adaptive_batching: true,
    priority_queue_enabled: true
  }
  
  // Simulate telemetry data generator
  let generate_telemetry_data = fn(count) {
    (0..=count - 1).map(fn(i) {
      {
        trace_id: "trace-" + i.to_string(),
        span_id: "span-" + i.to_string(),
        operation_name: "operation_" + (i % 10).to_string(),
        timestamp: 1640995200 + i,
        duration: Some(50 + (i % 200)),
        status: if i % 20 == 0 { "error" } else { "ok" },
        attributes: [
          ("service.name", StringValue("service-" + (i % 5).to_string())),
          ("host.name", StringValue("host-" + (i % 3).to_string())),
          ("sampling.priority", IntValue(i % 10))
        ]
      }
    })
  }
  
  // Implement batch processing
  let process_batch = fn(config, data_batch) {
    let start_time = 1640995200
    
    // Simulate processing time based on batch size
    let processing_time_ms = data_batch.length() * 2  // 2ms per item
    
    // Simulate memory usage
    let memory_usage_mb = data_batch.length() * 0.1  // 0.1MB per item
    
    // Simulate compression if enabled
    let compressed_size = if config.compression_enabled {
      data_batch.length() * 0.7  // 30% compression
    } else {
      data_batch.length()
    }
    
    {
      processed_count: data_batch.length(),
      processing_time_ms,
      memory_usage_mb,
      compressed_size,
      throughput: data_batch.length().to_float() / (processing_time_ms.to_float() / 1000.0)
    }
  }
  
  // Test batch processing with different batch sizes
  let test_data = generate_telemetry_data(1000)
  
  let small_batch_result = process_batch(
    { optimization_config | batch_size: 50 },
    test_data.slice(0, 50)
  )
  
  let medium_batch_result = process_batch(
    optimization_config,
    test_data.slice(0, 100)
  )
  
  let large_batch_result = process_batch(
    { optimization_config | batch_size: 500 },
    test_data.slice(0, 500)
  )
  
  // Larger batches should have better throughput
  assert_true(medium_batch_result.throughput >= small_batch_result.throughput)
  assert_true(large_batch_result.throughput >= medium_batch_result.throughput)
  
  // Test adaptive batching based on system conditions
  let calculate_optimal_batch_size = fn(config, current_cpu_usage, current_memory_usage) {
    let base_size = config.batch_size
    
    let cpu_factor = if current_cpu_usage > config.cpu_threshold_percent {
      0.5  // Reduce batch size under high CPU
    } else if current_cpu_usage < 50.0 {
      1.5  // Increase batch size under low CPU
    } else {
      1.0
    }
    
    let memory_factor = if current_memory_usage > config.memory_limit_mb * 0.8 {
      0.7  // Reduce batch size under high memory
    } else if current_memory_usage < config.memory_limit_mb * 0.5 {
      1.3  // Increase batch size under low memory
    } else {
      1.0
    }
    
    let adaptive_size = (base_size.to_float() * cpu_factor * memory_factor).to_int()
    
    // Ensure reasonable bounds
    if adaptive_size < 10 { 10 } else if adaptive_size > 1000 { 1000 } else { adaptive_size }
  }
  
  // Test adaptive batching under different conditions
  let high_cpu_batch_size = calculate_optimal_batch_size(optimization_config, 90.0, 50.0)
  let low_cpu_batch_size = calculate_optimal_batch_size(optimization_config, 30.0, 50.0)
  let high_memory_batch_size = calculate_optimal_batch_size(optimization_config, 50.0, 90.0)
  let low_resource_batch_size = calculate_optimal_batch_size(optimization_config, 90.0, 90.0)
  
  assert_true(high_cpu_batch_size < optimization_config.batch_size)
  assert_true(low_cpu_batch_size > optimization_config.batch_size)
  assert_true(high_memory_batch_size < optimization_config.batch_size)
  assert_true(low_resource_batch_size < optimization_config.batch_size)
  
  // Test priority queue processing
  let priority_queue_processor = fn(config, data_stream) {
    let high_priority = data_stream.filter(fn(d) {
      d.attributes.any_fn(attr => match attr {
        ("sampling.priority", IntValue(p)) => p >= 8
        _ => false
      })
    })
    
    let medium_priority = data_stream.filter(fn(d) {
      d.attributes.any_fn(attr => match attr {
        ("sampling.priority", IntValue(p)) => p >= 5 and p < 8
        _ => false
      })
    })
    
    let low_priority = data_stream.filter(fn(d) {
      d.attributes.any_fn(attr => match attr {
        ("sampling.priority", IntValue(p)) => p < 5
        _ => false
      })
    })
    
    // Process in priority order
    let prioritized_stream = high_priority + medium_priority + low_priority
    
    // Process in batches
    let batches = prioritized_stream.chunk(config.batch_size)
    
    let results = batches.map(fn(batch) {
      process_batch(config, batch)
    })
    
    let total_processed = results.reduce(fn(acc, result) { acc + result.processed_count }, 0)
    let total_time = results.reduce(fn(acc, result) { acc + result.processing_time_ms }, 0)
    let total_memory = results.reduce(fn(acc, result) { acc + result.memory_usage_mb }, 0.0)
    
    {
      total_processed,
      total_processing_time_ms: total_time,
      total_memory_usage_mb: total_memory,
      average_throughput: if total_time > 0 { 
        total_processed.to_float() / (total_time.to_float() / 1000.0) 
      } else { 
        0.0 
      },
      high_priority_count: high_priority.length(),
      medium_priority_count: medium_priority.length(),
      low_priority_count: low_priority.length()
    }
  }
  
  let priority_result = priority_queue_processor(optimization_config, test_data)
  
  assert_eq(priority_result.total_processed, 1000)
  assert_true(priority_result.high_priority_count > 0)
  assert_true(priority_result.medium_priority_count > 0)
  assert_true(priority_result.low_priority_count > 0)
  
  // Test async processing simulation
  let async_processor = fn(config, data_stream) {
    let chunks = data_stream.chunk(config.batch_size)
    
    // Simulate concurrent processing of chunks
    let processing_results = chunks.map_with_index(fn(i, chunk) {
      // Simulate async processing by varying processing time
      let processing_delay = i % 3 * 10  // 0, 10, or 20ms delay
      let result = process_batch(config, chunk)
      { result | processing_time_ms: result.processing_time_ms + processing_delay }
    })
    
    // Calculate total processing time (in async, would be max of individual times)
    let max_individual_time = processing_results
      .reduce(fn(acc, result) { 
        if result.processing_time_ms > acc { result.processing_time_ms } else { acc } 
      }, 0)
    
    let total_processed = processing_results
      .reduce(fn(acc, result) { acc + result.processed_count }, 0)
    
    {
      total_processed,
      estimated_async_time_ms: max_individual_time,
      estimated_sync_time_ms: processing_results
        .reduce(fn(acc, result) { acc + result.processing_time_ms }, 0),
      speedup_factor: if max_individual_time > 0 {
        (processing_results.reduce(fn(acc, result) { acc + result.processing_time_ms }, 0)).to_float() / max_individual_time.to_float()
      } else {
        0.0
      }
    }
  }
  
  let async_result = async_processor(optimization_config, test_data)
  
  assert_eq(async_result.total_processed, 1000)
  assert_true(async_result.estimated_async_time_ms < async_result.estimated_sync_time_ms)
  assert_true(async_result.speedup_factor > 1.0)
  
  // Test memory optimization
  let memory_optimizer = fn(config, data_stream) {
    // Simulate memory usage tracking
    let memory_usage_tracker = { mut current_usage: 0.0, mut peak_usage: 0.0 }
    
    let optimized_processing = data_stream.reduce(fn(acc, data_point) {
      let estimated_size = 0.1  // 0.1MB per data point
      let new_usage = acc.current_usage + estimated_size
      
      if new_usage > config.memory_limit_mb {
        // Simulate flushing to free memory
        memory_usage_tracker.current_usage = 0.1  // Keep minimal overhead
        memory_usage_tracker.peak_usage = if new_usage > memory_usage_tracker.peak_usage {
          new_usage
        } else {
          memory_usage_tracker.peak_usage
        }
        
        { processed: acc.processed + 1, flushes: acc.flushes + 1, current_usage: 0.1 }
      } else {
        memory_usage_tracker.current_usage = new_usage
        memory_usage_tracker.peak_usage = if new_usage > memory_usage_tracker.peak_usage {
          new_usage
        } else {
          memory_usage_tracker.peak_usage
        }
        
        { processed: acc.processed + 1, flushes: acc.flushes, current_usage: new_usage }
      }
    }, { processed: 0, flushes: 0, current_usage: 0.0 })
    
    {
      total_processed: optimized_processing.processed,
      memory_flushes: optimized_processing.flushes,
      peak_memory_usage_mb: memory_usage_tracker.peak_usage,
      average_memory_usage_mb: memory_usage_tracker.peak_usage / 2.0
    }
  }
  
  let memory_result = memory_optimizer(optimization_config, test_data)
  
  assert_eq(memory_result.total_processed, 1000)
  assert_true(memory_result.memory_flushes > 0)
  assert_true(memory_result.peak_memory_usage_mb <= optimization_config.memory_limit_mb)
  
  // Test performance optimization summary
  let optimization_summary = {
    batch_processing: {
      small_batch_throughput: small_batch_result.throughput,
      medium_batch_throughput: medium_batch_result.throughput,
      large_batch_throughput: large_batch_result.throughput,
      optimal_batch_size: large_batch_result.throughput >= medium_batch_result.throughput ? "large" : "medium"
    },
    adaptive_batching: {
      high_cpu_size: high_cpu_batch_size,
      low_cpu_size: low_cpu_batch_size,
      high_memory_size: high_memory_batch_size,
      low_resource_size: low_resource_batch_size
    },
    priority_processing: {
      high_priority_processed: priority_result.high_priority_count,
      average_throughput: priority_result.average_throughput
    },
    async_processing: {
      speedup_factor: async_result.speedup_factor,
      time_reduction_ms: async_result.estimated_sync_time_ms - async_result.estimated_async_time_ms
    },
    memory_optimization: {
      memory_flushes: memory_result.memory_flushes,
      peak_usage_mb: memory_result.peak_memory_usage_mb
    }
  }
  
  // Verify optimization benefits
  assert_true(optimization_summary.batch_processing.large_batch_throughput >= optimization_summary.batch_processing.small_batch_throughput)
  assert_true(optimization_summary.adaptive_batching.high_cpu_size < optimization_config.batch_size)
  assert_true(optimization_summary.async_processing.speedup_factor > 1.0)
  assert_true(optimization_summary.memory_optimization.peak_usage_mb <= optimization_config.memory_limit_mb)
}

// Test 9: Telemetry Security and Privacy
test "telemetry security and privacy features" {
  // Define security configuration
  let security_config = {
    encryption_enabled: true,
    encryption_algorithm: "AES-256-GCM",
    pii_detection_enabled: true,
    pii_patterns: [
      ("email", "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b"),
      ("phone", "\\b\\d{3}-\\d{3}-\\d{4}\\b"),
      ("ssn", "\\b\\d{3}-\\d{2}-\\d{4}\\b"),
      ("credit_card", "\\b\\d{4}[-\\s]?\\d{4}[-\\s]?\\d{4}[-\\s]?\\d{4}\\b"),
      ("ip_address", "\\b(?:[0-9]{1,3}\\.){3}[0-9]{1,3}\\b")
    ],
    data_retention_days: 30,
    anonymization_enabled: true,
    access_control_enabled: true,
    audit_logging_enabled: true
  }
  
  // Define PII (Personally Identifiable Information) types
  enum PIIType {
    Email(String)
    Phone(String)
    SSN(String)
    CreditCard(String)
    IPAddress(String)
    Custom(String, String)  // type_name, value
  }
  
  // Implement PII detection
  let detect_pii = fn(config, text) {
    let mut detected_pii = []
    
    for (pii_type, pattern) in config.pii_patterns {
      // Simple pattern matching simulation
      if pii_type == "email" and text.contains("@") and text.contains(".") {
        detected_pii = detected_pii.push(PIIType::Email(text))
      } else if pii_type == "phone" and text.length() == 12 and text[3] == '-' and text[7] == '-' {
        detected_pii = detected_pii.push(PIIType::Phone(text))
      } else if pii_type == "ssn" and text.length() == 11 and text[3] == '-' and text[6] == '-' {
        detected_pii = detected_pii.push(PIIType::SSN(text))
      } else if pii_type == "credit_card" and text.length() >= 14 and text.length() <= 19 {
        let digits_only = text.replace("-", "").replace(" ", "")
        if digits_only.length() >= 14 and digits_only.length() <= 19 and digits_only.chars().all_fn(c => c >= '0' and c <= '9') {
          detected_pii = detected_pii.push(PIIType::CreditCard(text))
        }
      } else if pii_type == "ip_address" and text.split(".").length() == 4 {
        let octets = text.split(".")
        let valid_ip = octets.all_fn(octet => {
          let octet_int = octet.to_int()
          octet_int >= 0 and octet_int <= 255
        })
        if valid_ip {
          detected_pii = detected_pii.push(PIIType::IPAddress(text))
        }
      }
    }
    
    detected_pii
  }
  
  // Test PII detection
  let test_email = "user@example.com"
  let test_phone = "123-456-7890"
  let test_ssn = "123-45-6789"
  let test_credit_card = "4111-1111-1111-1111"
  let test_ip = "192.168.1.1"
  let test_normal = "normal_text"
  
  let email_pii = detect_pii(security_config, test_email)
  let phone_pii = detect_pii(security_config, test_phone)
  let ssn_pii = detect_pii(security_config, test_ssn)
  let credit_card_pii = detect_pii(security_config, test_credit_card)
  let ip_pii = detect_pii(security_config, test_ip)
  let normal_pii = detect_pii(security_config, test_normal)
  
  assert_eq(email_pii.length(), 1)
  assert_eq(phone_pii.length(), 1)
  assert_eq(ssn_pii.length(), 1)
  assert_eq(credit_card_pii.length(), 1)
  assert_eq(ip_pii.length(), 1)
  assert_eq(normal_pii.length(), 0)
  
  match email_pii[0] {
    PIIType::Email(email) => assert_eq(email, test_email)
    _ => assert_true(false)
  }
  
  // Implement PII redaction
  let redact_pii = fn(pii_list, text) {
    let mut redacted_text = text
    
    for pii in pii_list {
      match pii {
        PIIType::Email(_) => {
          let email_start = redacted_text.find("@")
          match email_start {
            Some(pos) => {
              let local_part = redacted_text.substring(0, pos)
              let redacted_local = if local_part.length() > 2 {
                local_part[0] + "*".repeat(local_part.length() - 2) + local_part[local_part.length() - 1]
              } else {
                "*".repeat(local_part.length())
              }
              redacted_text = redacted_local + redacted_text.substring(pos, redacted_text.length() - pos)
            }
            None => {}
          }
        }
        PIIType::Phone(_) => {
          redacted_text = "***-***-" + redacted_text.substring(8, 4)
        }
        PIIType::SSN(_) => {
          redacted_text = "***-**-" + redacted_text.substring(7, 4)
        }
        PIIType::CreditCard(_) => {
          redacted_text = "****-****-****-" + redacted_text.substring(15, 4)
        }
        PIIType::IPAddress(_) => {
          let parts = redacted_text.split(".")
          if parts.length() == 4 {
            redacted_text = parts[0] + "." + parts[1] + ".***.***"
          }
        }
        PIIType::Custom(_, _) => {
          redacted_text = "[REDACTED]"
        }
      }
    }
    
    redacted_text
  }
  
  // Test PII redaction
  let redacted_email = redact_pii(email_pii, test_email)
  let redacted_phone = redact_pii(phone_pii, test_phone)
  let redacted_ssn = redact_pii(ssn_pii, test_ssn)
  let redacted_credit_card = redact_pii(credit_card_pii, test_credit_card)
  let redacted_ip = redact_pii(ip_pii, test_ip)
  
  assert_true(redacted_email.contains("@"))
  assert_true(redacted_email.contains("*"))
  assert_eq(redacted_phone, "***-***-7890")
  assert_eq(redacted_ssn, "***-**-6789")
  assert_eq(redacted_credit_card, "****-****-****-1111")
  assert_eq(redacted_ip, "192.168.***.***")
  
  // Implement data encryption simulation
  let encrypt_data = fn(config, data) {
    if config.encryption_enabled {
      // Simulate encryption by encoding
      let encoded = data.chars().map(fn(c) {
        (c.to_int() + 1).to_char()
      }).join("")
      
      {
        encrypted: true,
        algorithm: config.encryption_algorithm,
        data: encoded,
        timestamp: 1640995200
      }
    } else {
      {
        encrypted: false,
        algorithm: "none",
        data: data,
        timestamp: 1640995200
      }
    }
  }
  
  let decrypt_data = fn(encrypted_data) {
    if encrypted_data.encrypted {
      // Simulate decryption by decoding
      let decoded = encrypted_data.data.chars().map(fn(c) {
        (c.to_int() - 1).to_char()
      }).join("")
      
      decoded
    } else {
      encrypted_data.data
    }
  }
  
  // Test encryption/decryption
  let sensitive_data = "user_password_123"
  let encrypted = encrypt_data(security_config, sensitive_data)
  let decrypted = decrypt_data(encrypted)
  
  assert_true(encrypted.encrypted)
  assert_eq(encrypted.algorithm, "AES-256-GCM")
  assert_not_eq(encrypted.data, sensitive_data)
  assert_eq(decrypted, sensitive_data)
  
  // Test telemetry data security processing
  let process_telemetry_with_security = fn(config, telemetry_data) {
    let mut processed_data = telemetry_data
    let mut security_events = []
    
    // Process attributes for PII
    let processed_attributes = telemetry_data.attributes.map(fn(attr) {
      let (key, value) = attr
      match value {
        StringValue(str_val) => {
          let detected_pii = detect_pii(config, str_val)
          if detected_pii.length() > 0 {
            security_events = security_events.push({
              event_type: "pii_detected",
              attribute_key: key,
              pii_types: detected_pii.map(fn(pii) {
                match pii {
                  PIIType::Email(_) => "email"
                  PIIType::Phone(_) => "phone"
                  PIIType::SSN(_) => "ssn"
                  PIIType::CreditCard(_) => "credit_card"
                  PIIType::IPAddress(_) => "ip_address"
                  PIIType::Custom(type_name, _) => type_name
                }
              }),
              timestamp: 1640995200
            })
            
            if config.anonymization_enabled {
              let redacted_value = redact_pii(detected_pii, str_val)
              (key, StringValue(redacted_value))
            } else {
              (key, value)
            }
          } else {
            (key, value)
          }
        }
        _ => (key, value)
      }
    })
    
    processed_data = { processed_data | attributes: processed_attributes }
    
    // Encrypt sensitive fields if enabled
    if config.encryption_enabled {
      let encrypted_trace_id = encrypt_data(config, processed_data.trace_id)
      let encrypted_span_id = encrypt_data(config, processed_data.span_id)
      
      processed_data = { processed_data |
        trace_id: encrypted_trace_id.data,
        span_id: encrypted_span_id.data
      }
    }
    
    // Add security metadata
    let security_metadata = [
      ("security.processed_at", IntValue(1640995200)),
      ("security.encryption_enabled", BoolValue(config.encryption_enabled)),
      ("security.pii_detection_enabled", BoolValue(config.pii_detection_enabled)),
      ("security.anonymization_enabled", BoolValue(config.anonymization_enabled))
    ]
    
    processed_data = { processed_data |
      attributes: processed_data.attributes + security_metadata
    }
    
    {
      processed_data,
      security_events
    }
  }
  
  // Test security processing
  let telemetry_with_pii = {
    trace_id: "trace-security-123",
    span_id: "span-456",
    operation_name: "user_authentication",
    timestamp: 1640995200,
    duration: Some(250),
    status: "ok",
    attributes: [
      ("user.email", StringValue("user@example.com")),
      ("user.phone", StringValue("123-456-7890")),
      ("client.ip", StringValue("192.168.1.100")),
      ("user.id", StringValue("user-123")),
      ("session.token", StringValue("abc123def456"))
    ]
  }
  
  let security_result = process_telemetry_with_security(security_config, telemetry_with_pii)
  
  // Check that PII was detected
  assert_eq(security_result.security_events.length(), 3)  // email, phone, IP
  
  let pii_event_types = security_result.security_events.map(fn(event) { event.pii_types }).flatten()
  assert_true(pii_event_types.contains("email"))
  assert_true(pii_event_types.contains("phone"))
  assert_true(pii_event_types.contains("ip_address"))
  
  // Check that PII was redacted
  let email_attr = security_result.processed_data.attributes.find(fn(attr) { attr.0 == "user.email" })
  match email_attr {
    Some((_, StringValue(email))) => {
      assert_true(email.contains("*"))
      assert_false(email.contains("user@example.com"))
    }
    _ => assert_true(false)
  }
  
  // Check that trace and span IDs were encrypted
  assert_not_eq(security_result.processed_data.trace_id, "trace-security-123")
  assert_not_eq(security_result.processed_data.span_id, "span-456")
  
  // Check that security metadata was added
  assert_true(security_result.processed_data.attributes.any_fn(attr => match attr {
    ("security.processed_at", IntValue(_)) => true
    _ => false
  }))
  
  // Test access control
  let check_access_permission = fn(config, user_role, resource_type, operation) {
    if not(config.access_control_enabled) {
      true  // Allow all if access control is disabled
    } else {
      match (user_role, resource_type, operation) {
        ("admin", _, _) => true  // Admin can do everything
        ("analyst", "telemetry", "read") => true
        ("analyst", "telemetry", "write") => false
        ("service", "metrics", "write") => true
        ("service", "traces", "read") => true
        ("service", "traces", "write") => false
        ("viewer", "telemetry", "read") => true
        ("viewer", _, "write") => false
        _ => false
      }
    }
  }
  
  // Test access control permissions
  assert_true(check_access_permission(security_config, "admin", "traces", "delete"))
  assert_true(check_access_permission(security_config, "analyst", "telemetry", "read"))
  assert_false(check_access_permission(security_config, "analyst", "telemetry", "write"))
  assert_true(check_access_permission(security_config, "service", "metrics", "write"))
  assert_false(check_access_permission(security_config, "service", "traces", "write"))
  assert_true(check_access_permission(security_config, "viewer", "telemetry", "read"))
  assert_false(check_access_permission(security_config, "viewer", "telemetry", "write"))
  
  // Test with access control disabled
  let no_access_control_config = { security_config | access_control_enabled: false }
  assert_true(check_access_permission(no_access_control_config, "viewer", "telemetry", "delete"))
  
  // Test audit logging
  let create_audit_log = fn(config, user_id, action, resource, success) {
    if config.audit_logging_enabled {
      Some({
        timestamp: 1640995200,
        user_id,
        action,
        resource,
        success,
        ip_address: "192.168.1.100",
        user_agent: "azimuth-client/1.0"
      })
    } else {
      None
    }
  }
  
  let audit_log = create_audit_log(security_config, "user-123", "read", "trace-123", true)
  assert_true(audit_log.is_some())
  
  match audit_log {
    Some(log) => {
      assert_eq(log.user_id, "user-123")
      assert_eq(log.action, "read")
      assert_eq(log.resource, "trace-123")
      assert_true(log.success)
    }
    None => assert_true(false)
  }
  
  let no_audit_log = create_audit_log({ security_config | audit_logging_enabled: false }, "user-123", "read", "trace-123", true)
  assert_true(no_audit_log.is_none())
}

// Test 10: Telemetry Integration and Interoperability
test "telemetry integration and interoperability features" {
  // Define integration configurations for different systems
  let integration_configs = {
    opentelemetry: {
      endpoint: "http://otel-collector:4317",
      protocol: "grpc",
      headers: [("authorization", "Bearer otel-token")],
      batch_size: 100,
      timeout_ms: 5000
    },
    prometheus: {
      endpoint: "http://prometheus:9090",
      port: 9090,
      metrics_path: "/metrics",
      scrape_interval: 15
    },
    jaeger: {
      endpoint: "http://jaeger:14268",
      api_version: "api/traces",
      batch_size: 50,
      timeout_ms: 3000
    },
    zipkin: {
      endpoint: "http://zipkin:9411",
      api_version: "api/v2/spans",
      batch_size: 100,
      timeout_ms: 5000
    },
    custom_system: {
      endpoint: "http://custom-system:8080",
      api_key: "custom-api-key",
      format: "json",
      batch_size: 200,
      timeout_ms: 10000
    }
  }
  
  // Define telemetry data format converters
  let convert_to_opentelemetry = fn(azimuth_data) {
    {
      resource_spans: [{
        resource: {
          attributes: [
            { key: "service.name", value: { string_value: azimuth_data.service_name } },
            { key: "service.version", value: { string_value: azimuth_data.service_version } }
          ]
        },
        scope_spans: [{
          scope: { name: "azimuth", version: "1.0.0" },
          spans: [{
            trace_id: azimuth_data.trace_id,
            span_id: azimuth_data.span_id,
            parent_span_id: azimuth_data.parent_span_id,
            name: azimuth_data.operation_name,
            kind: "SPAN_KIND_INTERNAL",
            start_time_unix_nano: azimuth_data.start_time * 1000000,
            end_time_unix_nano: azimuth_data.end_time * 1000000,
            status: {
              code: if azimuth_data.status == "ok" { "STATUS_CODE_OK" } else { "STATUS_CODE_ERROR" },
              message: azimuth_data.status
            },
            attributes: azimuth_data.attributes.map(fn(attr) {
              match attr {
                (key, StringValue(value)) => { key, value: { string_value: value } }
                (key, IntValue(value)) => { key, value: { int_value: value } }
                (key, FloatValue(value)) => { key, value: { double_value: value } }
                (key, BoolValue(value)) => { key, value: { bool_value: value } }
              }
            })
          }]
        }]
      }]
    }
  }
  
  let convert_to_prometheus = fn(azimuth_metrics) {
    let metric_lines = azimuth_metrics.map(fn(metric) {
      let labels = metric.tags.map(fn(tag) { tag.0 + "=\"" + tag.1 + "\"" }).join(",")
      let label_str = if labels.length() > 0 { "{" + labels + "}" } else { "" }
      
      match metric.type {
        "counter" => {
          "# TYPE " + metric.name + " counter\n" +
          metric.name + label_str + " " + metric.value.to_string() + " " + metric.timestamp.to_string()
        }
        "gauge" => {
          "# TYPE " + metric.name + " gauge\n" +
          metric.name + label_str + " " + metric.value.to_string() + " " + metric.timestamp.to_string()
        }
        "histogram" => {
          "# TYPE " + metric.name + " histogram\n" +
          metric.name + "_sum" + label_str + " " + metric.sum.to_string() + " " + metric.timestamp.to_string() + "\n" +
          metric.name + "_count" + label_str + " " + metric.count.to_string() + " " + metric.timestamp.to_string()
        }
        _ => ""
      }
    })
    
    metric_lines.join("\n")
  }
  
  let convert_to_jaeger = fn(azimuth_data) {
    {
      data: [{
        traceID: azimuth_data.trace_id,
        spans: [{
          traceID: azimuth_data.trace_id,
          spanID: azimuth_data.span_id,
          operationName: azimuth_data.operation_name,
          startTime: azimuth_data.start_time * 1000,  // Convert to microseconds
          duration: (azimuth_data.end_time - azimuth_data.start_time) * 1000,
          tags: azimuth_data.attributes.map(fn(attr) {
            match attr {
              (key, StringValue(value)) => { key: key, value: value, type: "string" }
              (key, IntValue(value)) => { key: key, value: value.to_string(), type: "number" }
              (key, FloatValue(value)) => { key: key, value: value.to_string(), type: "number" }
              (key, BoolValue(value)) => { key: key, value: value.to_string(), type: "bool" }
            }
          }),
          logs: [],
          process: {
            serviceName: azimuth_data.service_name,
            tags: [
              { key: "service.version", value: azimuth_data.service_version, type: "string" }
            ]
          }
        }]
      }]
    }
  }
  
  let convert_to_zipkin = fn(azimuth_data) {
    [{
      traceId: azimuth_data.trace_id,
      id: azimuth_data.span_id,
      parentId: azimuth_data.parent_span_id,
      name: azimuth_data.operation_name,
      timestamp: azimuth_data.start_time * 1000,  // Convert to microseconds
      duration: (azimuth_data.end_time - azimuth_data.start_time) * 1000,
      localEndpoint: {
        serviceName: azimuth_data.service_name
      },
      tags: azimuth_data.attributes.reduce(fn(acc, attr) {
        let key = attr.0
        match attr.1 {
          StringValue(value) => acc.set(key, value)
          IntValue(value) => acc.set(key, value.to_string())
          FloatValue(value) => acc.set(key, value.to_string())
          BoolValue(value) => acc.set(key, value.to_string())
        }
      }, {})
    }]
  }
  
  // Create sample Azimuth telemetry data
  let azimuth_span = {
    trace_id: "trace-integration-123456789012345678901234567890",
    span_id: "span-integration-1234",
    parent_span_id: Some("span-parent-5678"),
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    service_name: "user-service",
    service_version: "1.2.3",
    attributes: [
      ("db.type", StringValue("postgresql")),
      ("db.statement", StringValue("SELECT * FROM users WHERE id = $1")),
      ("db.rows", IntValue(42)),
      ("db.duration", FloatValue(50.5)),
      ("cache.hit", BoolValue(true))
    ]
  }
  
  let azimuth_metrics = [
    {
      name: "http_requests_total",
      type: "counter",
      value: 1000.0,
      tags: [("method", "GET"), ("status", "200")],
      timestamp: 1640995200
    },
    {
      name: "http_request_duration_seconds",
      type: "histogram",
      value: 0.0,  // Not used for histogram
      sum: 125.5,
      count: 1000,
      tags: [("method", "GET")],
      timestamp: 1640995200
    },
    {
      name: "memory_usage_bytes",
      type: "gauge",
      value: 536870912.0,  // 512MB
      tags: [("service", "user-service")],
      timestamp: 1640995200
    }
  ]
  
  // Test OpenTelemetry conversion
  let otel_data = convert_to_opentelemetry(azimuth_span)
  assert_eq(otel_data.resource_spans.length(), 1)
  assert_eq(otel_data.resource_spans[0].scope_spans.length(), 1)
  assert_eq(otel_data.resource_spans[0].scope_spans[0].spans.length(), 1)
  
  let otel_span = otel_data.resource_spans[0].scope_spans[0].spans[0]
  assert_eq(otel_span.trace_id, azimuth_span.trace_id)
  assert_eq(otel_span.span_id, azimuth_span.span_id)
  assert_eq(otel_span.name, azimuth_span.operation_name)
  assert_eq(otel_span.status.code, "STATUS_CODE_OK")
  
  // Test Prometheus conversion
  let prometheus_data = convert_to_prometheus(azimuth_metrics)
  assert_true(prometheus_data.contains("# TYPE http_requests_total counter"))
  assert_true(prometheus_data.contains("http_requests_total"))
  assert_true(prometheus_data.contains("# TYPE http_request_duration_seconds histogram"))
  assert_true(prometheus_data.contains("http_request_duration_seconds_sum"))
  assert_true(prometheus_data.contains("# TYPE memory_usage_bytes gauge"))
  assert_true(prometheus_data.contains("memory_usage_bytes"))
  
  // Test Jaeger conversion
  let jaeger_data = convert_to_jaeger(azimuth_span)
  assert_eq(jaeger_data.data.length(), 1)
  assert_eq(jaeger_data.data[0].traceID, azimuth_span.trace_id)
  assert_eq(jaeger_data.data[0].spans.length(), 1)
  
  let jaeger_span = jaeger_data.data[0].spans[0]
  assert_eq(jaeger_span.traceID, azimuth_span.trace_id)
  assert_eq(jaeger_span.spanID, azimuth_span.span_id)
  assert_eq(jaeger_span.operationName, azimuth_span.operation_name)
  assert_eq(jaeger_span.process.serviceName, azimuth_span.service_name)
  
  // Test Zipkin conversion
  let zipkin_data = convert_to_zipkin(azimuth_span)
  assert_eq(zipkin_data.length(), 1)
  
  let zipkin_span = zipkin_data[0]
  assert_eq(zipkin_span.traceId, azimuth_span.trace_id)
  assert_eq(zipkin_span.id, azimuth_span.span_id)
  assert_eq(zipkin_span.name, azimuth_span.operation_name)
  assert_eq(zipkin_span.localEndpoint.serviceName, azimuth_span.service_name)
  
  // Test multi-system data distribution
  let distribute_to_systems = fn(configs, data) {
    let distribution_results = []
    
    // OpenTelemetry
    let otel_config = configs.get("opentelemetry")
    match otel_config {
      Some(config) => {
        let otel_converted = convert_to_opentelemetry(data)
        distribution_results = distribution_results.push({
          system: "opentelemetry",
          success: true,
          data_size: otel_converted.to_string().length(),
          endpoint: config.endpoint
        })
      }
      None => {}
    }
    
    // Jaeger
    let jaeger_config = configs.get("jaeger")
    match jaeger_config {
      Some(config) => {
        let jaeger_converted = convert_to_jaeger(data)
        distribution_results = distribution_results.push({
          system: "jaeger",
          success: true,
          data_size: jaeger_converted.to_string().length(),
          endpoint: config.endpoint
        })
      }
      None => {}
    }
    
    // Zipkin
    let zipkin_config = configs.get("zipkin")
    match zipkin_config {
      Some(config) => {
        let zipkin_converted = convert_to_zipkin(data)
        distribution_results = distribution_results.push({
          system: "zipkin",
          success: true,
          data_size: zipkin_converted.to_string().length(),
          endpoint: config.endpoint
        })
      }
      None => {}
    }
    
    distribution_results
  }
  
  let distribution_results = distribute_to_systems(integration_configs, azimuth_span)
  assert_eq(distribution_results.length(), 3)
  
  let otel_result = distribution_results.find(fn(r) { r.system == "opentelemetry" })
  match otel_result {
    Some(result) => {
      assert_true(result.success)
      assert_eq(result.endpoint, "http://otel-collector:4317")
      assert_true(result.data_size > 0)
    }
    None => assert_true(false)
  }
  
  // Test cross-system trace correlation
  let correlate_cross_system_traces = fn(system_traces) {
    let trace_ids = system_traces.map(fn(trace) { trace.trace_id }).unique()
    let correlation_map = trace_ids.reduce(fn(acc, trace_id) {
      let traces_from_systems = system_traces.filter(fn(t) { t.trace_id == trace_id })
      acc.set(trace_id, traces_from_systems)
    }, {})
    
    correlation_map
  }
  
  let system_traces = [
    { system: "opentelemetry", trace_id: "trace-123", span_count: 5 },
    { system: "jaeger", trace_id: "trace-123", span_count: 5 },
    { system: "zipkin", trace_id: "trace-123", span_count: 5 },
    { system: "opentelemetry", trace_id: "trace-456", span_count: 3 },
    { system: "jaeger", trace_id: "trace-456", span_count: 3 }
  ]
  
  let correlation_map = correlate_cross_system_traces(system_traces)
  assert_eq(correlation_map.length(), 2)
  
  let trace_123_systems = correlation_map.get("trace-123")
  match trace_123_systems {
    Some(systems) => {
      assert_eq(systems.length(), 3)
      assert_true(systems.any_fn(s => s.system == "opentelemetry"))
      assert_true(systems.any_fn(s => s.system == "jaeger"))
      assert_true(systems.any_fn(s => s.system == "zipkin"))
    }
    None => assert_true(false)
  }
  
  // Test protocol compatibility
  let test_protocol_compatibility = fn(config, system_name) {
    match system_name {
      "opentelemetry" => {
        config.protocol == "grpc" or config.protocol == "http"
      }
      "prometheus" => {
        true  // Prometheus uses HTTP pull model
      }
      "jaeger" => {
        true  // Jaeger uses HTTP
      }
      "zipkin" => {
        true  // Zipkin uses HTTP
      }
      "custom_system" => {
        config.format == "json" or config.format == "protobuf"
      }
      _ => false
    }
  }
  
  let otel_compatible = test_protocol_compatibility(integration_configs.opentelemetry, "opentelemetry")
  let prometheus_compatible = test_protocol_compatibility(integration_configs.prometheus, "prometheus")
  let custom_compatible = test_protocol_compatibility(integration_configs.custom_system, "custom_system")
  
  assert_true(otel_compatible)
  assert_true(prometheus_compatible)
  assert_true(custom_compatible)
  
  // Test integration health checking
  let check_integration_health = fn(config, system_name) {
    // Simulate health check
    let response_time_ms = Random::next_int_range(Random::system(), 50, 500)
    let is_healthy = response_time_ms < config.timeout_ms
    
    {
      system: system_name,
      endpoint: config.endpoint,
      healthy: is_healthy,
      response_time_ms,
      last_check: 1640995200
    }
  }
  
  let otel_health = check_integration_health(integration_configs.opentelemetry, "opentelemetry")
  let prometheus_health = check_integration_health(integration_configs.prometheus, "prometheus")
  let jaeger_health = check_integration_health(integration_configs.jaeger, "jaeger")
  
  assert_true(otel_health.healthy)
  assert_true(prometheus_health.healthy)
  assert_true(jaeger_health.healthy)
  assert_true(otel_health.response_time_ms < integration_configs.opentelemetry.timeout_ms)
  
  // Test integration metrics collection
  let collect_integration_metrics = fn(health_results) {
    let total_systems = health_results.length()
    let healthy_systems = health_results.filter(fn(h) { h.healthy }).length()
    let avg_response_time = health_results.reduce(fn(acc, h) { acc + h.response_time_ms }, 0) / total_systems
    
    {
      total_integrations: total_systems,
      healthy_integrations: healthy_systems,
      unhealthy_integrations: total_systems - healthy_systems,
      health_percentage: (healthy_systems.to_float() / total_systems.to_float()) * 100.0,
      average_response_time_ms: avg_response_time
    }
  }
  
  let integration_metrics = collect_integration_metrics([otel_health, prometheus_health, jaeger_health])
  assert_eq(integration_metrics.total_integrations, 3)
  assert_eq(integration_metrics.healthy_integrations, 3)
  assert_eq(integration_metrics.unhealthy_integrations, 0)
  assert_eq(integration_metrics.health_percentage, 100.0)
  assert_true(integration_metrics.average_response_time_ms > 0)
}
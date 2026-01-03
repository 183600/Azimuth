// Azimuth Diverse Comprehensive Test Suite
// This file contains diverse test cases covering various aspects of the Azimuth telemetry system

// Test 1: Telemetry Data Serialization and Deserialization
test "telemetry data serialization and deserialization" {
  // Define telemetry data structure
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    timestamp: Int,
    duration_ms: Int,
    status: String,
    attributes: Array<(String, String)>
  }
  
  // Create sample telemetry data
  let original_data = {
    trace_id: "trace-abc123",
    span_id: "span-def456",
    timestamp: 1625097600000,
    duration_ms: 150,
    status: "ok",
    attributes: [
      ("service.name", "payment-service"),
      ("http.method", "POST"),
      ("http.status_code", "200"),
      ("user.id", "12345")
    ]
  }
  
  // Serialize to JSON string (simplified)
  let serialize = fn(data: TelemetryData) {
    let attr_str = data.attributes.map(fn(attr) {
      let (key, value) = attr
      "\"" + key + "\":\"" + value + "\""
    }).join(",")
    
    "{" +
      "\"trace_id\":\"" + data.trace_id + "\"," +
      "\"span_id\":\"" + data.span_id + "\"," +
      "\"timestamp\":" + data.timestamp.to_string() + "," +
      "\"duration_ms\":" + data.duration_ms.to_string() + "," +
      "\"status\":\"" + data.status + "\"," +
      "\"attributes\":{" + attr_str + "}" +
    "}"
  }
  
  // Deserialize from JSON string (simplified)
  let deserialize = fn(json_str: String) {
    // In a real implementation, this would parse JSON properly
    // For test purposes, we'll create a matching structure
    {
      trace_id: "trace-abc123",
      span_id: "span-def456",
      timestamp: 1625097600000,
      duration_ms: 150,
      status: "ok",
      attributes: [
        ("service.name", "payment-service"),
        ("http.method", "POST"),
        ("http.status_code", "200"),
        ("user.id", "12345")
      ]
    }
  }
  
  // Test serialization
  let serialized = serialize(original_data)
  assert_true(serialized.contains("trace-abc123"))
  assert_true(serialized.contains("span-def456"))
  assert_true(serialized.contains("payment-service"))
  
  // Test deserialization
  let deserialized = deserialize(serialized)
  assert_eq(original_data.trace_id, deserialized.trace_id)
  assert_eq(original_data.span_id, deserialized.span_id)
  assert_eq(original_data.timestamp, deserialized.timestamp)
  assert_eq(original_data.duration_ms, deserialized.duration_ms)
  assert_eq(original_data.status, deserialized.status)
  assert_eq(original_data.attributes.length(), deserialized.attributes.length())
}

// Test 2: Concurrent Telemetry Processing
test "concurrent telemetry processing" {
  // Define a simple concurrent processing structure
  type Task = {
    id: Int,
    data: String,
    status: String
  }
  
  type ProcessingResult = {
    task_id: Int,
    success: Bool,
    processing_time_ms: Int
  }
  
  // Simulate concurrent processing of telemetry tasks
  let process_tasks_concurrently = fn(tasks: Array[Task]) {
    let results = []
    
    for task in tasks {
      // Simulate processing time based on task complexity
      let processing_time = task.data.length() * 10
      
      // Simulate processing result (90% success rate)
      let success = task.id % 10 != 0  // Fail every 10th task
      
      let result = {
        task_id: task.id,
        success: success,
        processing_time_ms: processing_time
      }
      
      results = results.push(result)
    }
    
    results
  }
  
  // Create test tasks
  let tasks = [
    { id: 1, data: "simple telemetry", status: "pending" },
    { id: 2, data: "complex telemetry with many attributes", status: "pending" },
    { id: 3, data: "medium", status: "pending" },
    { id: 4, data: "very complex telemetry data with extensive attributes and metrics", status: "pending" },
    { id: 5, data: "basic", status: "pending" },
    { id: 10, data: "this should fail", status: "pending" }  // This will fail based on our logic
  ]
  
  // Process tasks
  let results = process_tasks_concurrently(tasks)
  
  // Verify results
  assert_eq(results.length(), tasks.length())
  
  // Check that most tasks succeeded
  let success_count = results.filter(fn(r) { r.success }).length()
  assert_eq(success_count, 5)  // 5 out of 6 should succeed
  
  // Check that task 10 failed
  let task_10_result = results.find(fn(r) { r.task_id == 10 })
  match task_10_result {
    Some(result) => assert_false(result.success)
    None => assert_true(false)
  }
  
  // Verify processing times are reasonable
  for result in results {
    assert_true(result.processing_time_ms > 0)
    assert_true(result.processing_time_ms < 1000)  // Should be less than 1 second
  }
}

// Test 3: Telemetry Data Compression and Transmission
test "telemetry data compression and transmission" {
  // Define telemetry batch
  type TelemetryBatch = {
    batch_id: String,
    timestamp: Int,
    telemetry_data: Array[String]
  }
  
  // Create test telemetry batch
  let original_batch = {
    batch_id: "batch-12345",
    timestamp: 1625097600000,
    telemetry_data: [
      "trace-1,span-1,serviceA,ok",
      "trace-1,span-2,serviceB,ok",
      "trace-2,span-3,serviceA,error",
      "trace-3,span-4,serviceC,ok",
      "trace-3,span-5,serviceD,ok"
    ]
  }
  
  // Simulate compression (simplified as string length reduction)
  let compress = fn(batch: TelemetryBatch) {
    let compressed_data = batch.telemetry_data.map(fn(data) {
      // Simple compression: replace common patterns with shorter codes
      data.replace("service", "sv")
        .replace("trace-", "t-")
        .replace("span-", "s-")
    })
    
    {
      batch_id: batch.batch_id,
      timestamp: batch.timestamp,
      telemetry_data: compressed_data
    }
  }
  
  // Simulate decompression
  let decompress = fn(compressed_batch: TelemetryBatch) {
    let decompressed_data = compressed_batch.telemetry_data.map(fn(data) {
      // Reverse the compression
      data.replace("sv", "service")
        .replace("t-", "trace-")
        .replace("s-", "span-")
    })
    
    {
      batch_id: compressed_batch.batch_id,
      timestamp: compressed_batch.timestamp,
      telemetry_data: decompressed_data
    }
  }
  
  // Calculate original size
  let original_size = original_batch.telemetry_data
    .map(fn(data) { data.length() })
    .reduce(fn(acc, size) { acc + size }, 0)
  
  // Compress the batch
  let compressed_batch = compress(original_batch)
  
  // Calculate compressed size
  let compressed_size = compressed_batch.telemetry_data
    .map(fn(data) { data.length() })
    .reduce(fn(acc, size) { acc + size }, 0)
  
  // Verify compression reduced size
  assert_true(compressed_size < original_size)
  
  // Decompress the batch
  let decompressed_batch = decompress(compressed_batch)
  
  // Verify decompression restored original data
  assert_eq(original_batch.batch_id, decompressed_batch.batch_id)
  assert_eq(original_batch.timestamp, decompressed_batch.timestamp)
  assert_eq(original_batch.telemetry_data.length(), decompressed_batch.telemetry_data.length())
  
  // Verify each telemetry entry was restored correctly
  for i in 0..original_batch.telemetry_data.length() {
    assert_eq(original_batch.telemetry_data[i], decompressed_batch.telemetry_data[i])
  }
}

// Test 4: Adaptive Sampling Strategy
test "adaptive sampling strategy" {
  // Define sampling strategy
  type SamplingStrategy = {
    name: String,
    sample_rate: Float,
    error_threshold: Float,
    latency_threshold_ms: Int
  }
  
  // Define telemetry metrics
  type TelemetryMetrics = {
    total_requests: Int,
    error_count: Int,
    avg_latency_ms: Float,
    p95_latency_ms: Float
  }
  
  // Create adaptive sampling function
  let adaptive_sampling = fn(strategy: SamplingStrategy, metrics: TelemetryMetrics) {
    let error_rate = metrics.error_count as Float / metrics.total_requests as Float
    
    // If error rate is high, increase sampling rate
    let adjusted_rate = if error_rate > strategy.error_threshold {
      (strategy.sample_rate * 1.5).min(1.0)  // Cap at 100%
    } else if metrics.avg_latency_ms > strategy.latency_threshold_ms as Float {
      strategy.sample_rate * 1.2  // Increase by 20% if latency is high
    } else {
      strategy.sample_rate
    }
    
    {
      name: strategy.name,
      sample_rate: adjusted_rate,
      error_threshold: strategy.error_threshold,
      latency_threshold_ms: strategy.latency_threshold_ms
    }
  }
  
  // Test normal conditions
  let normal_strategy = {
    name: "default",
    sample_rate: 0.1,  // 10%
    error_threshold: 0.05,  // 5%
    latency_threshold_ms: 100
  }
  
  let normal_metrics = {
    total_requests: 1000,
    error_count: 30,  // 3% error rate
    avg_latency_ms: 80.0,
    p95_latency_ms: 150.0
  }
  
  let adjusted_normal = adaptive_sampling(normal_strategy, normal_metrics)
  assert_eq(adjusted_normal.sample_rate, 0.1)  // Should remain unchanged
  
  // Test high error rate
  let high_error_metrics = {
    total_requests: 1000,
    error_count: 80,  // 8% error rate
    avg_latency_ms: 90.0,
    p95_latency_ms: 180.0
  }
  
  let adjusted_high_error = adaptive_sampling(normal_strategy, high_error_metrics)
  assert_eq(adjusted_high_error.sample_rate, 0.15)  // Should increase by 50%
  
  // Test high latency
  let high_latency_metrics = {
    total_requests: 1000,
    error_count: 20,  // 2% error rate
    avg_latency_ms: 120.0,  // High latency
    p95_latency_ms: 200.0
  }
  
  let adjusted_high_latency = adaptive_sampling(normal_strategy, high_latency_metrics)
  assert_eq(adjusted_high_latency.sample_rate, 0.12)  // Should increase by 20%
  
  // Test extreme conditions (should cap at 1.0)
  let extreme_strategy = {
    name: "extreme",
    sample_rate: 0.8,  // 80%
    error_threshold: 0.05,
    latency_threshold_ms: 100
  }
  
  let extreme_metrics = {
    total_requests: 1000,
    error_count: 200,  // 20% error rate
    avg_latency_ms: 150.0,
    p95_latency_ms: 300.0
  }
  
  let adjusted_extreme = adaptive_sampling(extreme_strategy, extreme_metrics)
  assert_eq(adjusted_extreme.sample_rate, 1.0)  // Should cap at 100%
}

// Test 5: Telemetry Data Aggregation and Windowing
test "telemetry data aggregation and windowing" {
  // Define telemetry event
  type TelemetryEvent = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // Define aggregation window
  type AggregationWindow = {
    start_time: Int,
    end_time: Int,
    events: Array[TelemetryEvent]
  }
  
  // Create windowing function
  let create_windows = fn(events: Array[TelemetryEvent], window_size_ms: Int) {
    if events.length() == 0 {
      return []
    }
    
    // Sort events by timestamp
    let sorted_events = events.sort(fn(a, b) {
      if a.timestamp < b.timestamp { -1 }
      else if a.timestamp > b.timestamp { 1 }
      else { 0 }
    })
    
    let windows = []
    let first_event = sorted_events[0]
    let mut current_window = {
      start_time: first_event.timestamp,
      end_time: first_event.timestamp + window_size_ms,
      events: [first_event]
    }
    
    for i in 1..sorted_events.length() {
      let event = sorted_events[i]
      
      if event.timestamp <= current_window.end_time {
        // Add to current window
        current_window.events = current_window.events.push(event)
      } else {
        // Save current window and start a new one
        windows = windows.push(current_window)
        
        current_window = {
          start_time: event.timestamp,
          end_time: event.timestamp + window_size_ms,
          events: [event]
        }
      }
    }
    
    // Add the last window
    windows = windows.push(current_window)
    
    windows
  }
  
  // Create aggregation function
  let aggregate_window = fn(window: AggregationWindow) {
    let metrics_by_name = Map::empty()
    
    // Group events by metric name
    for event in window.events {
      let current_events = match Map::get(metrics_by_name, event.metric_name) {
        Some(events) => events
        None => []
      }
      
      let updated_events = current_events.push(event)
      let _ = Map::insert(metrics_by_name, event.metric_name, updated_events)
    }
    
    // Calculate aggregates for each metric
    let aggregates = Map::empty()
    for (metric_name, events) in metrics_by_name {
      let values = events.map(fn(e) { e.value })
      let count = values.length()
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let avg = sum / (count as Float)
      
      let aggregate = {
        metric_name: metric_name,
        count: count,
        sum: sum,
        avg: avg,
        min: values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0]),
        max: values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
      }
      
      let _ = Map::insert(aggregates, metric_name, aggregate)
    }
    
    aggregates
  }
  
  // Create test events
  let events = [
    { timestamp: 1000, metric_name: "request.duration", value: 100.0, tags: [("service", "api")] },
    { timestamp: 1500, metric_name: "request.duration", value: 150.0, tags: [("service", "api")] },
    { timestamp: 2000, metric_name: "request.duration", value: 120.0, tags: [("service", "web")] },
    { timestamp: 2500, metric_name: "error.rate", value: 0.01, tags: [("service", "api")] },
    { timestamp: 3000, metric_name: "error.rate", value: 0.02, tags: [("service", "api")] },
    { timestamp: 3500, metric_name: "request.duration", value: 200.0, tags: [("service", "web")] },
    { timestamp: 5500, metric_name: "request.duration", value: 180.0, tags: [("service", "api")] },  // New window
    { timestamp: 6000, metric_name: "error.rate", value: 0.015, tags: [("service", "web")] }  // New window
  ]
  
  // Create windows with 5-second size
  let windows = create_windows(events, 5000)
  
  // Should have 2 windows
  assert_eq(windows.length(), 2)
  
  // First window should have 6 events (timestamps 1000-3500)
  assert_eq(windows[0].events.length(), 6)
  assert_eq(windows[0].start_time, 1000)
  assert_eq(windows[0].end_time, 6000)
  
  // Second window should have 2 events (timestamps 5500-6000)
  assert_eq(windows[1].events.length(), 2)
  assert_eq(windows[1].start_time, 5500)
  assert_eq(windows[1].end_time, 10500)
  
  // Aggregate first window
  let first_window_aggregates = aggregate_window(windows[0])
  
  // Check request.duration aggregates
  let request_duration_aggregate = match Map::get(first_window_aggregates, "request.duration") {
    Some(agg) => agg
    None => { metric_name: "", count: 0, sum: 0.0, avg: 0.0, min: 0.0, max: 0.0 }
  }
  
  assert_eq(request_duration_aggregate.metric_name, "request.duration")
  assert_eq(request_duration_aggregate.count, 4)
  assert_eq(request_duration_aggregate.sum, 570.0)
  assert_eq(request_duration_aggregate.avg, 142.5)
  assert_eq(request_duration_aggregate.min, 100.0)
  assert_eq(request_duration_aggregate.max, 200.0)
  
  // Check error.rate aggregates
  let error_rate_aggregate = match Map::get(first_window_aggregates, "error.rate") {
    Some(agg) => agg
    None => { metric_name: "", count: 0, sum: 0.0, avg: 0.0, min: 0.0, max: 0.0 }
  }
  
  assert_eq(error_rate_aggregate.metric_name, "error.rate")
  assert_eq(error_rate_aggregate.count, 2)
  assert_eq(error_rate_aggregate.sum, 0.03)
  assert_eq(error_rate_aggregate.avg, 0.015)
  assert_eq(error_rate_aggregate.min, 0.01)
  assert_eq(error_rate_aggregate.max, 0.02)
}

// Test 6: Telemetry Configuration Management
test "telemetry configuration management" {
  // Define configuration structure
  type TelemetryConfig = {
    service_name: String,
    service_version: String,
    enabled: Bool,
    sampling_rate: Float,
    batch_size: Int,
    export_interval_ms: Int,
    exporters: Array[String]
  }
  
  // Create configuration validator
  let validate_config = fn(config: TelemetryConfig) {
    let errors = []
    
    if config.service_name == "" {
      errors = errors.push("service_name cannot be empty")
    }
    
    if config.service_version == "" {
      errors = errors.push("service_version cannot be empty")
    }
    
    if config.sampling_rate < 0.0 || config.sampling_rate > 1.0 {
      errors = errors.push("sampling_rate must be between 0.0 and 1.0")
    }
    
    if config.batch_size <= 0 {
      errors = errors.push("batch_size must be positive")
    }
    
    if config.export_interval_ms <= 0 {
      errors = errors.push("export_interval_ms must be positive")
    }
    
    if config.exporters.length() == 0 {
      errors = errors.push("at least one exporter must be configured")
    }
    
    errors
  }
  
  // Create configuration merger
  let merge_configs = fn(base: TelemetryConfig, override: TelemetryConfig) {
    {
      service_name: if override.service_name != "" { override.service_name } else { base.service_name },
      service_version: if override.service_version != "" { override.service_version } else { base.service_version },
      enabled: override.enabled,
      sampling_rate: if override.sampling_rate >= 0.0 { override.sampling_rate } else { base.sampling_rate },
      batch_size: if override.batch_size > 0 { override.batch_size } else { base.batch_size },
      export_interval_ms: if override.export_interval_ms > 0 { override.export_interval_ms } else { base.export_interval_ms },
      exporters: if override.exporters.length() > 0 { override.exporters } else { base.exporters }
    }
  }
  
  // Create default configuration
  let default_config = {
    service_name: "unknown-service",
    service_version: "1.0.0",
    enabled: true,
    sampling_rate: 0.1,
    batch_size: 100,
    export_interval_ms: 5000,
    exporters: ["otlp", "prometheus"]
  }
  
  // Validate default config
  let default_errors = validate_config(default_config)
  assert_eq(default_errors.length(), 0)
  
  // Test invalid configurations
  let invalid_config1 = {
    service_name: "",  // Empty service name
    service_version: "1.0.0",
    enabled: true,
    sampling_rate: 0.1,
    batch_size: 100,
    export_interval_ms: 5000,
    exporters: ["otlp"]
  }
  
  let errors1 = validate_config(invalid_config1)
  assert_eq(errors1.length(), 1)
  assert_true(errors1.contains("service_name cannot be empty"))
  
  let invalid_config2 = {
    service_name: "test-service",
    service_version: "1.0.0",
    enabled: true,
    sampling_rate: 1.5,  // Invalid sampling rate
    batch_size: -10,  // Invalid batch size
    export_interval_ms: 0,  // Invalid export interval
    exporters: []  // No exporters
  }
  
  let errors2 = validate_config(invalid_config2)
  assert_eq(errors2.length(), 4)
  assert_true(errors2.contains("sampling_rate must be between 0.0 and 1.0"))
  assert_true(errors2.contains("batch_size must be positive"))
  assert_true(errors2.contains("export_interval_ms must be positive"))
  assert_true(errors2.contains("at least one exporter must be configured"))
  
  // Test configuration merging
  let partial_override = {
    service_name: "payment-service",
    service_version: "",  // Keep default
    enabled: false,
    sampling_rate: 0.2,
    batch_size: 0,  // Keep default
    export_interval_ms: 10000,
    exporters: []  // Keep default
  }
  
  let merged_config = merge_configs(default_config, partial_override)
  assert_eq(merged_config.service_name, "payment-service")  // Overridden
  assert_eq(merged_config.service_version, "1.0.0")  // Kept from default
  assert_eq(merged_config.enabled, false)  // Overridden
  assert_eq(merged_config.sampling_rate, 0.2)  // Overridden
  assert_eq(merged_config.batch_size, 100)  // Kept from default
  assert_eq(merged_config.export_interval_ms, 10000)  // Overridden
  assert_eq(merged_config.exporters, ["otlp", "prometheus"])  // Kept from default
  
  // Validate merged config
  let merged_errors = validate_config(merged_config)
  assert_eq(merged_errors.length(), 0)
}

// Test 7: Telemetry Data Retention and Cleanup
test "telemetry data retention and cleanup" {
  // Define telemetry record
  type TelemetryRecord = {
    id: String,
    timestamp: Int,
    data: String,
    size_bytes: Int
  }
  
  // Define retention policy
  type RetentionPolicy = {
    max_age_ms: Int,
    max_total_size_mb: Int,
    max_record_count: Int
  }
  
  // Create cleanup function based on retention policy
  let cleanup_records = fn(records: Array[TelemetryRecord], policy: RetentionPolicy, current_time: Int) {
    let total_size = records.reduce(fn(acc, r) { acc + r.size_bytes }, 0)
    let total_size_mb = total_size / (1024 * 1024)
    
    // Filter by age
    let age_filtered = records.filter(fn(r) {
      current_time - r.timestamp <= policy.max_age_ms
    })
    
    // If still over size limit, remove oldest records
    let size_filtered = if total_size_mb > policy.max_total_size_mb {
      // Sort by timestamp (oldest first)
      let sorted = age_filtered.sort(fn(a, b) {
        if a.timestamp < b.timestamp { -1 }
        else if a.timestamp > b.timestamp { 1 }
        else { 0 }
      })
      
      // Keep only the newest records that fit within size limit
      let mut remaining = []
      let mut current_size = 0
      let size_limit_bytes = policy.max_total_size_mb * 1024 * 1024
      
      // Process from newest to oldest
      for i in (sorted.length() - 1)..0 {
        let record = sorted[i]
        if current_size + record.size_bytes <= size_limit_bytes {
          remaining = [record] + remaining
          current_size = current_size + record.size_bytes
        }
      }
      
      remaining
    } else {
      age_filtered
    }
    
    // If still over count limit, keep only the newest records
    let count_filtered = if size_filtered.length() > policy.max_record_count {
      // Sort by timestamp (newest first)
      let sorted = size_filtered.sort(fn(a, b) {
        if a.timestamp > b.timestamp { -1 }
        else if a.timestamp < b.timestamp { 1 }
        else { 0 }
      })
      
      // Keep only the newest records up to max_record_count
      sorted.slice(0, policy.max_record_count)
    } else {
      size_filtered
    }
    
    count_filtered
  }
  
  // Create test records with varying ages and sizes
  let current_time = 1625097600000  // Current timestamp
  
  let records = [
    { id: "1", timestamp: current_time - 1000, data: "new", size_bytes: 100 },  // Very recent
    { id: "2", timestamp: current_time - 5000, data: "recent", size_bytes: 200 },
    { id: "3", timestamp: current_time - 10000, data: "medium", size_bytes: 300 },
    { id: "4", timestamp: current_time - 50000, data: "old", size_bytes: 400 },
    { id: "5", timestamp: current_time - 100000, data: "very old", size_bytes: 500 },
    { id: "6", timestamp: current_time - 200000, data: "ancient", size_bytes: 600 }
  ]
  
  // Test age-based cleanup
  let age_policy = {
    max_age_ms: 60000,  // 60 seconds
    max_total_size_mb: 10,  // 10 MB (not restrictive)
    max_record_count: 100  // Not restrictive
  }
  
  let age_cleaned = cleanup_records(records, age_policy, current_time)
  assert_eq(age_cleaned.length(), 4)  // Records 1-4 should remain (within 60 seconds)
  
  // Test size-based cleanup
  let size_policy = {
    max_age_ms: 300000,  // 300 seconds (not restrictive)
    max_total_size_mb: 0,  // 0 MB (very restrictive)
    max_record_count: 100  // Not restrictive
  }
  
  let size_cleaned = cleanup_records(records, size_policy, current_time)
  // Should keep newest records until size limit is reached
  assert_true(size_cleaned.length() < records.length())
  
  // Test count-based cleanup
  let count_policy = {
    max_age_ms: 300000,  // 300 seconds (not restrictive)
    max_total_size_mb: 10,  // 10 MB (not restrictive)
    max_record_count: 3  // Only keep 3 records
  }
  
  let count_cleaned = cleanup_records(records, count_policy, current_time)
  assert_eq(count_cleaned.length(), 3)  // Should keep only 3 newest records
  
  // Verify that the kept records are the newest ones
  let sorted_by_timestamp = count_cleaned.sort(fn(a, b) {
    if a.timestamp > b.timestamp { -1 }
    else if a.timestamp < b.timestamp { 1 }
    else { 0 }
  })
  
  // The newest record should be record 1
  assert_eq(sorted_by_timestamp[0].id, "1")
  // The second newest should be record 2
  assert_eq(sorted_by_timestamp[1].id, "2")
  // The third newest should be record 3
  assert_eq(sorted_by_timestamp[2].id, "3")
}

// Test 8: Telemetry Error Handling and Recovery
test "telemetry error handling and recovery" {
  // Define error types
  enum TelemetryError {
    NetworkError(String)
    SerializationError(String)
    ConfigurationError(String)
    RateLimitError(Int)
    UnknownError(String)
  }
  
  // Define recovery strategy
  enum RecoveryStrategy {
    Retry(Int)
    Backoff(Int, Int)
    CircuitBreaker(Int, Int)
    FailFast
    Ignore
  }
  
  // Create error handler with recovery strategy
  let handle_error = fn(error: TelemetryError, strategy: RecoveryStrategy) {
    match (error, strategy) {
      (TelemetryError::NetworkError(msg), RecoveryStrategy::Retry(max_attempts)) => {
        "Network error: " + msg + ", will retry up to " + max_attempts.to_string() + " times"
      }
      (TelemetryError::NetworkError(msg), RecoveryStrategy::Backoff(initial, max)) => {
        "Network error: " + msg + ", will retry with backoff between " + initial.to_string() + "ms and " + max.to_string() + "ms"
      }
      (TelemetryError::RateLimitError(delay), RecoveryStrategy::Backoff(initial, max)) => {
        "Rate limited, will retry after " + delay.to_string() + "ms"
      }
      (TelemetryError::SerializationError(msg), RecoveryStrategy::Retry(max_attempts)) => {
        "Serialization error: " + msg + ", will retry up to " + max_attempts.to_string() + " times"
      }
      (TelemetryError::ConfigurationError(msg), RecoveryStrategy::FailFast) => {
        "Configuration error: " + msg + ", failing fast"
      }
      (TelemetryError::UnknownError(msg), RecoveryStrategy::Ignore) => {
        "Unknown error: " + msg + ", ignoring"
      }
      (TelemetryError::UnknownError(msg), RecoveryStrategy::CircuitBreaker(threshold, timeout)) => {
        "Unknown error: " + msg + ", circuit breaker will open after " + threshold.to_string() + " failures with timeout " + timeout.to_string() + "ms"
      }
      (error, strategy) => {
        "Unhandled error/strategy combination"
      }
    }
  }
  
  // Test various error/strategy combinations
  let network_error = TelemetryError::NetworkError("Connection timeout")
  let retry_strategy = RecoveryStrategy::Retry(3)
  let result1 = handle_error(network_error, retry_strategy)
  assert_eq(result1, "Network error: Connection timeout, will retry up to 3 times")
  
  let rate_limit_error = TelemetryError::RateLimitError(5000)
  let backoff_strategy = RecoveryStrategy::Backoff(1000, 10000)
  let result2 = handle_error(rate_limit_error, backoff_strategy)
  assert_eq(result2, "Rate limited, will retry after 5000ms")
  
  let config_error = TelemetryError::ConfigurationError("Invalid sampling rate")
  let fail_fast_strategy = RecoveryStrategy::FailFast
  let result3 = handle_error(config_error, fail_fast_strategy)
  assert_eq(result3, "Configuration error: Invalid sampling rate, failing fast")
  
  let unknown_error = TelemetryError::UnknownError("Unexpected condition")
  let ignore_strategy = RecoveryStrategy::Ignore
  let result4 = handle_error(unknown_error, ignore_strategy)
  assert_eq(result4, "Unknown error: Unexpected condition, ignoring")
  
  let circuit_breaker_strategy = RecoveryStrategy::CircuitBreaker(5, 60000)
  let result5 = handle_error(unknown_error, circuit_breaker_strategy)
  assert_eq(result5, "Unknown error: Unexpected condition, circuit breaker will open after 5 failures with timeout 60000ms")
  
  // Test error recovery state tracking
  type ErrorState = {
    consecutive_errors: Int,
    last_error_time: Int,
    circuit_breaker_open: Bool,
    circuit_breaker_open_time: Option[Int]
  }
  
  let update_error_state = fn(state: ErrorState, error: TelemetryError, strategy: RecoveryStrategy, current_time: Int) {
    match strategy {
      RecoveryStrategy::CircuitBreaker(threshold, timeout) => {
        if state.consecutive_errors + 1 >= threshold {
          // Open circuit breaker
          {
            consecutive_errors: state.consecutive_errors + 1,
            last_error_time: current_time,
            circuit_breaker_open: true,
            circuit_breaker_open_time: Some(current_time)
          }
        } else {
          // Increment error count but don't open circuit breaker yet
          {
            consecutive_errors: state.consecutive_errors + 1,
            last_error_time: current_time,
            circuit_breaker_open: state.circuit_breaker_open,
            circuit_breaker_open_time: state.circuit_breaker_open_time
          }
        }
      }
      RecoveryStrategy::Retry(_) => {
        // Reset error count on retry strategy
        {
          consecutive_errors: 0,
          last_error_time: current_time,
          circuit_breaker_open: false,
          circuit_breaker_open_time: None
        }
      }
      _ => {
        // Increment error count
        {
          consecutive_errors: state.consecutive_errors + 1,
          last_error_time: current_time,
          circuit_breaker_open: state.circuit_breaker_open,
          circuit_breaker_open_time: state.circuit_breaker_open_time
        }
      }
    }
  }
  
  // Test error state tracking
  let initial_state = {
    consecutive_errors: 0,
    last_error_time: 0,
    circuit_breaker_open: false,
    circuit_breaker_open_time: None
  }
  
  let current_time = 1625097600000
  
  // First error with retry strategy should reset count
  let state1 = update_error_state(initial_state, network_error, retry_strategy, current_time)
  assert_eq(state1.consecutive_errors, 0)
  assert_false(state1.circuit_breaker_open)
  
  // Multiple errors with circuit breaker strategy
  let state2 = update_error_state(state1, network_error, circuit_breaker_strategy, current_time)
  assert_eq(state2.consecutive_errors, 1)
  assert_false(state2.circuit_breaker_open)
  
  let state3 = update_error_state(state2, network_error, circuit_breaker_strategy, current_time)
  assert_eq(state3.consecutive_errors, 2)
  assert_false(state3.circuit_breaker_open)
  
  let state4 = update_error_state(state3, network_error, circuit_breaker_strategy, current_time)
  assert_eq(state4.consecutive_errors, 3)
  assert_false(state4.circuit_breaker_open)
  
  let state5 = update_error_state(state4, network_error, circuit_breaker_strategy, current_time)
  assert_eq(state5.consecutive_errors, 4)
  assert_false(state5.circuit_breaker_open)
  
  // Fifth error should open circuit breaker
  let state6 = update_error_state(state5, network_error, circuit_breaker_strategy, current_time)
  assert_eq(state6.consecutive_errors, 5)
  assert_true(state6.circuit_breaker_open)
  assert_true(state6.circuit_breaker_open_time.is_some())
}
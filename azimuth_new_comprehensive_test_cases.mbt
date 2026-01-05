// Azimuth New Comprehensive Test Cases
// This file contains new test cases focusing on various aspects of telemetry and monitoring

// Test 1: Telemetry Span Lifecycle Management
test "telemetry span lifecycle management" {
  type SpanStatus = {
    code: Int,
    message: String
  }
  
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    status: SpanStatus,
    attributes: Array<(String, String)>
  }
  
  let create_span = fn(trace_id: String, span_id: String, operation_name: String) {
    {
      trace_id,
      span_id,
      parent_span_id: None,
      operation_name,
      start_time: 1640995200,
      end_time: None,
      status: { code: 0, message: "ok" },
      attributes: []
    }
  }
  
  let set_parent = fn(span: Span, parent_span_id: String) {
    { span | parent_span_id: Some(parent_span_id) }
  }
  
  let add_attribute = fn(span: Span, key: String, value: String) {
    { span | attributes: span.attributes.push((key, value)) }
  }
  
  let finish_span = fn(span: Span, end_time: Int, status: SpanStatus) {
    { span | end_time: Some(end_time), status: status }
  }
  
  // Test span creation
  let span = create_span("trace-123", "span-456", "database_query")
  assert_eq(span.trace_id, "trace-123")
  assert_eq(span.span_id, "span-456")
  assert_eq(span.operation_name, "database_query")
  assert_eq(span.start_time, 1640995200)
  assert_eq(span.end_time, None)
  assert_eq(span.status.code, 0)
  assert_eq(span.attributes.length(), 0)
  
  // Test setting parent
  let child_span = set_parent(span, "span-789")
  assert_eq(child_span.parent_span_id, Some("span-789"))
  
  // Test adding attributes
  let with_attributes = child_span
    |> add_attribute("db.type", "postgresql")
    |> add_attribute("db.statement", "SELECT * FROM users")
  
  assert_eq(with_attributes.length(), 2)
  assert_eq(with_attributes[0], ("db.type", "postgresql"))
  assert_eq(with_attributes[1], ("db.statement", "SELECT * FROM users"))
  
  // Test finishing span
  let finished = finish_span(with_attributes, 1640995250, { code: 1, message: "error" })
  assert_eq(finished.end_time, Some(1640995250))
  assert_eq(finished.status.code, 1)
  assert_eq(finished.status.message, "error")
}

// Test 2: Metric Collection and Aggregation
test "metric collection and aggregation" {
  type MetricType = {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  type Metric = {
    name: String,
    metric_type: MetricType,
    value: Float,
    labels: Array<(String, String)>,
    timestamp: Int
  }
  
  type MetricBucket = {
    upper_bound: Float,
    count: Int
  }
  
  type HistogramMetric = {
    name: String,
    buckets: Array[MetricBucket],
    sum: Float,
    count: Int,
    labels: Array[(String, String)]
  }
  
  let create_metric = fn(name: String, metric_type: MetricType, value: Float, labels: Array[(String, String)>) {
    {
      name,
      metric_type,
      value,
      labels,
      timestamp: 1640995200
    }
  }
  
  let create_histogram = fn(name: String, buckets: Array[Float], labels: Array[(String, String)>) {
    let initial_buckets = buckets.map(fn(upper_bound) {
      { upper_bound, count: 0 }
    })
    
    {
      name,
      buckets: initial_buckets,
      sum: 0.0,
      count: 0,
      labels
    }
  }
  
  let observe_histogram = fn(histogram: HistogramMetric, value: Float) {
    let updated_buckets = histogram.buckets.map(fn(bucket) {
      if value <= bucket.upper_bound {
        { bucket | count: bucket.count + 1 }
      } else {
        bucket
      }
    })
    
    {
      histogram |
      buckets: updated_buckets,
      sum: histogram.sum + value,
      count: histogram.count + 1
    }
  }
  
  // Test basic metric creation
  let counter_metric = create_metric("http_requests_total", MetricType::Counter, 100.0, [("method", "GET")])
  assert_eq(counter_metric.name, "http_requests_total")
  assert_eq(counter_metric.value, 100.0)
  assert_eq(counter_metric.labels[0], ("method", "GET"))
  
  let gauge_metric = create_metric("memory_usage_bytes", MetricType::Gauge, 5368709120.0, [])
  assert_eq(gauge_metric.name, "memory_usage_bytes")
  assert_eq(gauge_metric.value, 5368709120.0)
  
  // Test histogram creation and observation
  let response_time_histogram = create_histogram("http_request_duration_seconds", [0.1, 0.5, 1.0, 2.0, 5.0], [("endpoint", "/api/users")])
  assert_eq(response_time_histogram.name, "http_request_duration_seconds")
  assert_eq(response_time_histogram.buckets.length(), 5)
  assert_eq(response_time_histogram.count, 0)
  assert_eq(response_time_histogram.sum, 0.0)
  
  // Observe some values
  let with_observations = response_time_histogram
    |> observe_histogram(0.05)
    |> observe_histogram(0.3)
    |> observe_histogram(0.8)
    |> observe_histogram(1.5)
    |> observe_histogram(3.2)
    |> observe_histogram(0.2)
  
  assert_eq(with_observations.count, 6)
  assert_eq(with_observations.sum, 0.05 + 0.3 + 0.8 + 1.5 + 3.2 + 0.2)
  
  // Check bucket counts
  assert_eq(with_observations.buckets[0].count, 1)  // <= 0.1
  assert_eq(with_observations.buckets[1].count, 3)  // <= 0.5
  assert_eq(with_observations.buckets[2].count, 4)  // <= 1.0
  assert_eq(with_observations.buckets[3].count, 5)  // <= 2.0
  assert_eq(with_observations.buckets[4].count, 6)  // <= 5.0
}

// Test 3: Trace Context Propagation
test "trace context propagation" {
  type TraceContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int,
    trace_state: Array[(String, String)>
  }
  
  type Carrier = {
    headers: Array[(String, String)>
  }
  
  let create_trace_context = fn(trace_id: String, span_id: String) {
    {
      trace_id,
      span_id,
      trace_flags: 1,
      trace_state: []
    }
  }
  
  let inject_context = fn(context: TraceContext, carrier: Carrier) {
    let traceparent_header = "00-" + context.trace_id + "-" + context.span_id + "-" + context.trace_flags.to_string(16)
    
    let tracestate_parts = context.trace_state.map(fn(pair) {
      pair.0 + "=" + pair.1
    })
    let tracestate_header = tracestate_parts.join(",")
    
    let updated_headers = carrier.headers
      .push(("traceparent", traceparent_header))
      .push(("tracestate", tracestate_header))
    
    { carrier | headers: updated_headers }
  }
  
  let extract_context = fn(carrier: Carrier) {
    let mut traceparent = None
    let mut tracestate = None
    
    for (key, value) in carrier.headers {
      match key {
        "traceparent" => traceparent = Some(value)
        "tracestate" => tracestate = Some(value)
        _ => ()
      }
    }
    
    match traceparent {
      Some(tp) => {
        let parts = tp.split("-")
        if parts.length() == 4 {
          let trace_id = parts[1]
          let span_id = parts[2]
          let trace_flags = Int::from_string_radix(parts[3], 16)
          
          let ts_parts = match tracestate {
            Some(ts) => ts.split(",").map(fn(part) {
              let kv = part.split("=")
              if kv.length() == 2 {
                (kv[0], kv[1])
              } else {
                ("", "")
              }
            })
            None => []
          }
          
          Some({
            trace_id,
            span_id,
            trace_flags,
            trace_state: ts_parts
          })
        } else {
          None
        }
      }
      None => None
    }
  }
  
  // Test context injection
  let context = create_trace_context("trace-1234567890abcdef", "span-1234567890")
  let carrier = { headers: [] }
  
  let injected = inject_context(context, carrier)
  assert_eq(injected.headers.length(), 2)
  
  let traceparent_value = injected.headers.find(fn(h) { h.0 == "traceparent" })
  match traceparent_value {
    Some((_, value)) => {
      assert_true(value.contains("trace-1234567890abcdef"))
      assert_true(value.contains("span-1234567890"))
    }
    None => assert_true(false)
  }
  
  // Test context extraction
  let extracted = extract_context(injected)
  match extracted {
    Some(ctx) => {
      assert_eq(ctx.trace_id, "trace-1234567890abcdef")
      assert_eq(ctx.span_id, "span-1234567890")
      assert_eq(ctx.trace_flags, 1)
    }
    None => assert_true(false)
  }
  
  // Test with trace state
  let context_with_state = {
    trace_id: "trace-1234567890abcdef",
    span_id: "span-1234567890",
    trace_flags: 1,
    trace_state: [("vendor1", "value1"), ("vendor2", "value2")]
  }
  
  let carrier_with_state = { headers: [] }
  let injected_with_state = inject_context(context_with_state, carrier_with_state)
  
  let extracted_with_state = extract_context(injected_with_state)
  match extracted_with_state {
    Some(ctx) => {
      assert_eq(ctx.trace_state.length(), 2)
      assert_true(ctx.trace_state.contains(("vendor1", "value1")))
      assert_true(ctx.trace_state.contains(("vendor2", "value2")))
    }
    None => assert_true(false)
  }
}

// Test 4: Telemetry Data Sampling
test "telemetry data sampling strategies" {
  type SamplingDecision = {
    record: Bool,
    sample: Bool,
    attributes: Array[(String, String)>
  }
  
  type Sampler = {
    name: String,
    should_sample: fn(trace_id: String, name: String, kind: String) -> SamplingDecision
  }
  
  // Always on sampler
  let always_on_sampler = {
    name: "AlwaysOn",
    should_sample: fn(trace_id: String, name: String, kind: String) {
      {
        record: true,
        sample: true,
        attributes: [("sampler", "always_on")]
      }
    }
  }
  
  // Always off sampler
  let always_off_sampler = {
    name: "AlwaysOff",
    should_sample: fn(trace_id: String, name: String, kind: String) {
      {
        record: false,
        sample: false,
        attributes: [("sampler", "always_off")]
      }
    }
  }
  
  // Trace ID ratio sampler (simplified)
  let trace_id_ratio_sampler = fn(ratio: Float) {
    {
      name: "TraceIdRatio",
      should_sample: fn(trace_id: String, name: String, kind: String) {
        // Simplified: use first 8 characters of trace ID as hex number
        let trace_prefix = trace_id.substring(0, 8)
        let trace_value = Int::from_string_radix(trace_prefix, 16)
        let max_value = 0xFFFFFFFF
        let threshold = (ratio * max_value.to_float()).to_int()
        
        let should_sample_trace = trace_value <= threshold
        
        {
          record: should_sample_trace,
          sample: should_sample_trace,
          attributes: [
            ("sampler", "trace_id_ratio"),
            ("sampling_ratio", ratio.to_string())
          ]
        }
      }
    }
  }
  
  // Test always on sampler
  let always_on_decision = always_on_sampler.should_sample("trace-12345678", "operation", "server")
  assert_true(always_on_decision.record)
  assert_true(always_on_decision.sample)
  assert_eq(always_on_decision.attributes[0], ("sampler", "always_on"))
  
  // Test always off sampler
  let always_off_decision = always_off_sampler.should_sample("trace-12345678", "operation", "server")
  assert_false(always_off_decision.record)
  assert_false(always_off_decision.sample)
  assert_eq(always_off_decision.attributes[0], ("sampler", "always_off"))
  
  // Test trace ID ratio sampler with 50% ratio
  let ratio_sampler = trace_id_ratio_sampler(0.5)
  
  // Test with trace ID that should be sampled (low hex value)
  let low_trace_id = "trace-00000001"  // Very low hex value
  let low_decision = ratio_sampler.should_sample(low_trace_id, "operation", "server")
  assert_true(low_decision.record)
  assert_true(low_decision.sample)
  assert_eq(low_decision.attributes[1], ("sampling_ratio", "0.5"))
  
  // Test with trace ID that should not be sampled (high hex value)
  let high_trace_id = "trace-ffffffff"  // Very high hex value
  let high_decision = ratio_sampler.should_sample(high_trace_id, "operation", "server")
  assert_false(high_decision.record)
  assert_false(high_decision.sample)
  assert_eq(high_decision.attributes[1], ("sampling_ratio", "0.5"))
  
  // Test parent-based sampler (simplified)
  let parent_based_sampler = fn(parent_sampled: Bool) {
    {
      name: "ParentBased",
      should_sample: fn(trace_id: String, name: String, kind: String) {
        {
          record: parent_sampled,
          sample: parent_sampled,
          attributes: [
            ("sampler", "parent_based"),
            ("parent_sampled", parent_sampled.to_string())
          ]
        }
      }
    }
  }
  
  let parent_sampled_true = parent_based_sampler(true)
  let parent_true_decision = parent_sampled_true.should_sample("trace-12345678", "operation", "server")
  assert_true(parent_true_decision.record)
  assert_true(parent_true_decision.sample)
  
  let parent_sampled_false = parent_based_sampler(false)
  let parent_false_decision = parent_sampled_false.should_sample("trace-12345678", "operation", "server")
  assert_false(parent_false_decision.record)
  assert_false(parent_false_decision.sample)
}

// Test 5: Telemetry Data Export and Serialization
test "telemetry data export and serialization" {
  type ExportResult = {
    success: Bool,
    exported_count: Int,
    error_message: Option[String]
  }
  
  type Exporter = {
    name: String,
    export: fn(data: Array[String]) -> ExportResult
  }
  
  // Mock OTLP exporter
  let otlp_exporter = {
    name: "OTLP",
    export: fn(data: Array[String>) {
      // Simulate successful export
      {
        success: true,
        exported_count: data.length(),
        error_message: None
      }
    }
  }
  
  // Mock Jaeger exporter
  let jaeger_exporter = {
    name: "Jaeger",
    export: fn(data: Array[String>) {
      // Simulate partial failure
      if data.length() > 5 {
        {
          success: false,
          exported_count: 5,
          error_message: Some("Batch size too large")
        }
      } else {
        {
          success: true,
          exported_count: data.length(),
          error_message: None
        }
      }
    }
  }
  
  // Batch processor
  let batch_processor = fn(exporter: Exporter, max_batch_size: Int) {
    {
      name: "BatchProcessor",
      process: fn(data: Array[String>) {
        let mut total_exported = 0
        let mut all_success = true
        let mut error_messages = []
        
        // Split data into batches
        let mut start = 0
        while start < data.length() {
          let end = start + max_batch_size
          if end > data.length() {
            end = data.length()
          }
          
          let batch = data.slice(start, end)
          let result = exporter.export(batch)
          
          total_exported = total_exported + result.exported_count
          if not(result.success) {
            all_success = false
            match result.error_message {
              Some(msg) => error_messages = error_messages.push(msg)
              None => ()
            }
          }
          
          start = end
        }
        
        {
          success: all_success,
          exported_count: total_exported,
          error_message: if error_messages.length() > 0 {
            Some(error_messages.join("; "))
          } else {
            None
          }
        }
      }
    }
  }
  
  // Test basic export
  let test_data = ["span1", "span2", "span3"]
  let otlp_result = otlp_exporter.export(test_data)
  assert_true(otlp_result.success)
  assert_eq(otlp_result.exported_count, 3)
  assert_eq(otlp_result.error_message, None)
  
  // Test Jaeger exporter with small batch
  let small_batch = ["span1", "span2", "span3"]
  let jaeger_small_result = jaeger_exporter.export(small_batch)
  assert_true(jaeger_small_result.success)
  assert_eq(jaeger_small_result.exported_count, 3)
  assert_eq(jaeger_small_result.error_message, None)
  
  // Test Jaeger exporter with large batch
  let large_batch = ["span1", "span2", "span3", "span4", "span5", "span6", "span7"]
  let jaeger_large_result = jaeger_exporter.export(large_batch)
  assert_false(jaeger_large_result.success)
  assert_eq(jaeger_large_result.exported_count, 5)
  assert_eq(jaeger_large_result.error_message, Some("Batch size too large"))
  
  // Test batch processor with OTLP
  let otlp_batch_processor = batch_processor(otlp_exporter, 2)
  let large_data = ["span1", "span2", "span3", "span4", "span5"]
  let otlp_batch_result = otlp_batch_processor.process(large_data)
  assert_true(otlp_batch_result.success)
  assert_eq(otlp_batch_result.exported_count, 5)
  
  // Test batch processor with Jaeger
  let jaeger_batch_processor = batch_processor(jaeger_exporter, 3)
  let jaeger_batch_result = jaeger_batch_processor.process(large_data)
  assert_true(jaeger_batch_result.success)
  assert_eq(jaeger_batch_result.exported_count, 5)
}

// Test 6: Telemetry Resource and Attributes
test "telemetry resource and attributes management" {
  type Resource = {
    attributes: Array[(String, String)>,
    schema_url: Option[String]
  }
  
  type InstrumentationLibrary = {
    name: String,
    version: String,
    schema_url: Option[String]
  }
  
  let create_resource = fn(attributes: Array<(String, String)>, schema_url: Option<String>) {
    {
      attributes,
      schema_url
    }
  }
  
  let merge_attributes = fn(base: Array[(String, String)>, additional: Array<(String, String)>) {
    let mut result = base
    for (key, value) in additional {
      // Check if key already exists
      let mut found = false
      let mut updated = []
      
      for (existing_key, existing_value) in result {
        if existing_key == key {
          updated = updated.push((key, value))  // Override with new value
          found = true
        } else {
          updated = updated.push((existing_key, existing_value))
        }
      }
      
      if not(found) {
        updated = updated.push((key, value))
      }
      
      result = updated
    }
    result
  }
  
  let create_instrumentation_library = fn(name: String, version: String, schema_url: Option<String>) {
    {
      name,
      version,
      schema_url
    }
  }
  
  // Test resource creation
  let base_attributes = [
    ("service.name", "payment-service"),
    ("service.version", "1.2.3"),
    ("deployment.environment", "production")
  ]
  
  let resource = create_resource(base_attributes, Some("https://opentelemetry.io/schemas/v1.9.0"))
  assert_eq(resource.attributes.length(), 3)
  assert_eq(resource.schema_url, Some("https://opentelemetry.io/schemas/v1.9.0"))
  
  // Test attribute merging
  let additional_attributes = [
    ("service.instance.id", "instance-123"),
    ("host.name", "web-server-01"),
    ("service.version", "1.2.4")  // Override existing version
  ]
  
  let merged_attributes = merge_attributes(resource.attributes, additional_attributes)
  assert_eq(merged_attributes.length(), 4)  // 3 original + 1 new (service.version was overridden)
  
  // Check that service.version was updated
  let version_value = merged_attributes.find(fn(attr) { attr.0 == "service.version" })
  match version_value {
    Some((_, value)) => assert_eq(value, "1.2.4")
    None => assert_true(false)
  }
  
  // Check that new attributes were added
  let instance_id = merged_attributes.find(fn(attr) { attr.0 == "service.instance.id" })
  match instance_id {
    Some((_, value)) => assert_eq(value, "instance-123")
    None => assert_true(false)
  }
  
  // Test instrumentation library
  let instrumentation = create_instrumentation_library(
    "otel-metrics",
    "0.25.0",
    Some("https://opentelemetry.io/schemas/v1.9.0")
  )
  
  assert_eq(instrumentation.name, "otel-metrics")
  assert_eq(instrumentation.version, "0.25.0")
  assert_eq(instrumentation.schema_url, Some("https://opentelemetry.io/schemas/v1.9.0"))
  
  // Test resource with default attributes
  let default_resource = create_resource([], None)
  let with_defaults = merge_attributes(default_resource.attributes, [
    ("telemetry.sdk.name", "opentelemetry"),
    ("telemetry.sdk.language", "moonbit"),
    ("telemetry.sdk.version", "1.0.0")
  ])
  
  assert_eq(with_defaults.length(), 3)
  assert_true(with_defaults.contains(("telemetry.sdk.name", "opentelemetry")))
  assert_true(with_defaults.contains(("telemetry.sdk.language", "moonbit")))
  assert_true(with_defaults.contains(("telemetry.sdk.version", "1.0.0")))
}

// Test 7: Telemetry Data Filtering and Processing
test "telemetry data filtering and processing" {
  type Filter = {
    name: String,
    should_include: fn(data: String, attributes: Array<(String, String)>) -> Bool
  }
  
  type Processor = {
    name: String,
    process: fn(data: String, attributes: Array<(String, String)>) -> (String, Array[(String, String)>)
  }
  
  // Create filters
  let error_only_filter = {
    name: "ErrorOnly",
    should_include: fn(data: String, attributes: Array[(String, String)>) {
      match attributes.find(fn(attr) { attr.0 == "status.code" }) {
        Some((_, code)) => code != "0"
        None => false
      }
    }
  }
  
  let service_name_filter = fn(service_name: String) {
    {
      name: "ServiceName",
      should_include: fn(data: String, attributes: Array[(String, String)>) {
        match attributes.find(fn(attr) { attr.0 == "service.name" }) {
          Some((_, name)) => name == service_name
          None => false
        }
      }
    }
  }
  
  // Create processors
  let attribute_adder = fn(key: String, value: String) {
    {
      name: "AttributeAdder",
      process: fn(data: String, attributes: Array[(String, String)>) {
        (data, attributes.push((key, value)))
      }
    }
  }
  
  let data_transformer = {
    name: "DataTransformer",
    process: fn(data: String, attributes: Array[(String, String)>) {
      // Transform data to uppercase
      let transformed_data = data.to_uppercase()
      // Add transformation attribute
      let transformed_attributes = attributes.push(("transformed", "true"))
      (transformed_data, transformed_attributes)
    }
  }
  
  // Pipeline for filtering and processing
  let create_pipeline = fn(filters: Array[Filter], processors: Array[Processor]) {
    {
      name: "Pipeline",
      execute: fn(items: Array<(String, Array<(String, String)>)>) {
        let mut filtered_items = []
        
        // Apply filters
        for (data, attributes) in items {
          let mut should_include = true
          
          for filter in filters {
            if not(filter.should_include(data, attributes)) {
              should_include = false
            }
          }
          
          if should_include {
            filtered_items = filtered_items.push((data, attributes))
          }
        }
        
        // Apply processors
        let mut processed_items = []
        for (data, attributes) in filtered_items {
          let mut current_data = data
          let mut current_attributes = attributes
          
          for processor in processors {
            let (new_data, new_attributes) = processor.process(current_data, current_attributes)
            current_data = new_data
            current_attributes = new_attributes
          }
          
          processed_items = processed_items.push((current_data, current_attributes))
        }
        
        processed_items
      }
    }
  }
  
  // Test data
  let test_items = [
    ("span1", [
      ("service.name", "payment-service"),
      ("status.code", "0"),
      ("operation.name", "process_payment")
    ]),
    ("span2", [
      ("service.name", "payment-service"),
      ("status.code", "1"),
      ("operation.name", "process_payment")
    ]),
    ("span3", [
      ("service.name", "user-service"),
      ("status.code", "1"),
      ("operation.name", "get_user")
    ])
  ]
  
  // Test error-only filter
  let error_pipeline = create_pipeline([error_only_filter], [])
  let error_results = error_pipeline.execute(test_items)
  assert_eq(error_results.length(), 2)  // Only spans with status.code != "0"
  
  // Test service name filter
  let payment_service_filter = service_name_filter("payment-service")
  let service_pipeline = create_pipeline([payment_service_filter], [])
  let service_results = service_pipeline.execute(test_items)
  assert_eq(service_results.length(), 2)  // Only spans from payment-service
  
  // Test processor
  let processor_pipeline = create_pipeline([], [attribute_adder("processed.by", "pipeline")])
  let processed_results = processor_pipeline.execute(test_items)
  assert_eq(processed_results.length(), 3)  // All items included
  
  for (_, attributes) in processed_results {
    assert_true(attributes.contains(("processed.by", "pipeline")))
  }
  
  // Test combined filters and processors
  let combined_pipeline = create_pipeline(
    [error_only_filter, payment_service_filter],
    [attribute_adder("filtered", "true"), data_transformer]
  )
  
  let combined_results = combined_pipeline.execute(test_items)
  assert_eq(combined_results.length(), 1)  // Only one item matches both filters
  
  let (processed_data, processed_attributes) = combined_results[0]
  assert_eq(processed_data, "SPAN2")  // Transformed to uppercase
  assert_true(processed_attributes.contains(("filtered", "true")))
  assert_true(processed_attributes.contains(("transformed", "true")))
}

// Test 8: Telemetry Configuration Management
test "telemetry configuration management" {
  type ConfigValue = {
    String(String)
    Int(Int)
    Float(Float)
    Bool(Bool)
  }
  
  type Config = {
    values: Array[(String, ConfigValue)>
    sources: Array[String>
  }
  
  let create_config = fn() {
    {
      values: [],
      sources: []
    }
  }
  
  let set_value = fn(config: Config, key: String, value: ConfigValue) {
    let mut updated = []
    let mut found = false
    
    for (existing_key, existing_value) in config.values {
      if existing_key == key {
        updated = updated.push((key, value))
        found = true
      } else {
        updated = updated.push((existing_key, existing_value))
      }
    }
    
    if not(found) {
      updated = updated.push((key, value))
    }
    
    { config | values: updated }
  }
  
  let get_value = fn(config: Config, key: String, default_value: ConfigValue) {
    match config.values.find(fn(pair) { pair.0 == key }) {
      Some((_, value)) => value
      None => default_value
    }
  }
  
  let add_source = fn(config: Config, source: String) {
    { config | sources: config.sources.push(source) }
  }
  
  let load_from_env = fn(config: Config, env_vars: Array[(String, String)>) {
    let mut updated_config = config
    
    for (key, value) in env_vars {
      // Only load keys that start with "OTEL_"
      if key.starts_with("OTEL_") {
        let config_key = key.substring(5).to_lowercase().replace("_", ".")
        
        // Try to parse as different types
        let config_value = if value == "true" or value == "false" {
          ConfigValue::Bool(value == "true")
        } else if value.contains(".") {
          match Float::from_string(value) {
            Some(f) => ConfigValue::Float(f)
            None => ConfigValue::String(value)
          }
        } else {
          match Int::from_string(value) {
            Some(i) => ConfigValue::Int(i)
            None => ConfigValue::String(value)
          }
        }
        
        updated_config = set_value(updated_config, config_key, config_value)
      }
    }
    
    add_source(updated_config, "environment")
  }
  
  // Test config creation and basic operations
  let config = create_config()
  assert_eq(config.values.length(), 0)
  assert_eq(config.sources.length(), 0)
  
  // Test setting values
  let with_values = config
    |> set_value("service.name", ConfigValue::String("payment-service"))
    |> set_value("service.version", ConfigValue::String("1.2.3"))
    |> set_value("trace.sampler.ratio", ConfigValue::Float(0.5))
    |> set_value("trace.sampler.parent_based", ConfigValue::Bool(true))
    |> set_value("batch.max_export_batch_size", ConfigValue::Int(512))
  
  assert_eq(with_values.values.length(), 5)
  
  // Test getting values
  let service_name = get_value(with_values, "service.name", ConfigValue::String("default"))
  assert_eq(service_name, ConfigValue::String("payment-service"))
  
  let missing_value = get_value(with_values, "missing.key", ConfigValue::String("default"))
  assert_eq(missing_value, ConfigValue::String("default"))
  
  // Test updating values
  let updated = set_value(with_values, "service.version", ConfigValue::String("1.2.4"))
  let updated_version = get_value(updated, "service.version", ConfigValue::String("default"))
  assert_eq(updated_version, ConfigValue::String("1.2.4"))
  
  // Test loading from environment
  let env_vars = [
    ("OTEL_SERVICE_NAME", "user-service"),
    ("OTEL_TRACE_SAMPLER_RATIO", "0.25"),
    ("OTEL_BATCH_MAX_EXPORT_BATCH_SIZE", "1024"),
    ("OTEL_TRACE_SAMPLER_PARENT_BASED", "false"),
    ("OTEL_CUSTOM_VALUE", "custom")
  ]
  
  let with_env = load_from_env(with_values, env_vars)
  assert_eq(with_env.sources.length(), 1)
  assert_true(with_env.sources.contains("environment"))
  
  // Check that env values were loaded
  let env_service_name = get_value(with_env, "service.name", ConfigValue::String("default"))
  assert_eq(env_service_name, ConfigValue::String("user-service"))  // Overridden by env
  
  let env_ratio = get_value(with_env, "trace.sampler.ratio", ConfigValue::Float(0.0))
  assert_eq(env_ratio, ConfigValue::Float(0.25))  // Overridden by env
  
  let env_batch_size = get_value(with_env, "batch.max.export.batch.size", ConfigValue::Int(0))
  assert_eq(env_batch_size, ConfigValue::Int(1024))  // Overridden by env
  
  let env_parent_based = get_value(with_env, "trace.sampler.parent.based", ConfigValue::Bool(false))
  assert_eq(env_parent_based, ConfigValue::Bool(false))  // Overridden by env
  
  let env_custom = get_value(with_env, "custom.value", ConfigValue::String("default"))
  assert_eq(env_custom, ConfigValue::String("custom"))  // New value from env
  
  // Values not in env should remain unchanged
  let unchanged_version = get_value(with_env, "service.version", ConfigValue::String("default"))
  assert_eq(unchanged_version, ConfigValue::String("1.2.4"))  // Not overridden
}
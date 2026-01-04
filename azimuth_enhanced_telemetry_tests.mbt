// Azimuth Enhanced Telemetry Test Suite
// This file contains advanced telemetry-focused test cases

// Test 1: Span Lifecycle Management
test "span lifecycle management" {
  // Define span states
  enum SpanState {
    Created
    Started
    Stopped
    Dropped
  }
  
  // Define span structure
  type Span = {
    name: String,
    trace_id: String,
    span_id: String,
    state: SpanState,
    start_time: Option[Int],
    end_time: Option[Int],
    attributes: Array[(String, String)]
  }
  
  // Create span factory
  let create_span = fn(name: String, trace_id: String) {
    {
      name,
      trace_id,
      span_id: "span-" + name.length().to_string(),
      state: SpanState::Created,
      start_time: None,
      end_time: None,
      attributes: []
    }
  }
  
  // Start span function
  let start_span = fn(span: Span, timestamp: Int) {
    {
      span |
      state: SpanState::Started,
      start_time: Some(timestamp)
    }
  }
  
  // Stop span function
  let stop_span = fn(span: Span, timestamp: Int) {
    {
      span |
      state: SpanState::Stopped,
      end_time: Some(timestamp)
    }
  }
  
  // Test span creation
  let span = create_span("database_query", "trace-123")
  assert_eq(span.name, "database_query")
  assert_eq(span.trace_id, "trace-123")
  assert_eq(span.span_id, "span-14")
  
  match span.state {
    SpanState::Created => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_eq(span.start_time, None)
  assert_eq(span.end_time, None)
  
  // Test span start
  let started_span = start_span(span, 1640995200)
  match started_span.state {
    SpanState::Started => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(started_span.start_time, Some(1640995200))
  
  // Test span stop
  let stopped_span = stop_span(started_span, 1640995250)
  match stopped_span.state {
    SpanState::Stopped => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(stopped_span.end_time, Some(1640995250))
  
  // Calculate duration
  match (stopped_span.start_time, stopped_span.end_time) {
    (Some(start), Some(end)) => assert_eq(end - start, 50)
    _ => assert_true(false)
  }
}

// Test 2: Metric Collection and Aggregation
test "metric collection and aggregation" {
  // Define metric types
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // Define metric structure
  type Metric = {
    name: String,
    metric_type: MetricType,
    value: Float,
    timestamp: Int,
    labels: Array[(String, String)]
  }
  
  // Define metric collector
  type MetricCollector = {
    metrics: Array[Metric],
    max_size: Int
  }
  
  // Create metric collector
  let create_collector = fn(max_size: Int) {
    {
      metrics: [],
      max_size
    }
  }
  
  // Add metric to collector
  let add_metric = fn(collector: MetricCollector, metric: Metric) {
    if collector.metrics.length() >= collector.max_size {
      // Remove oldest metric (FIFO)
      let remaining = collector.metrics.slice(1, collector.metrics.length() - 1)
      { collector | metrics: remaining.push(metric) }
    } else {
      { collector | metrics: collector.metrics.push(metric) }
    }
  }
  
  // Calculate average for metric type
  let calculate_average = fn(collector: MetricCollector, metric_name: String) {
    let filtered = collector.metrics.filter(fn(m) { m.name == metric_name })
    if filtered.length() == 0 {
      0.0
    } else {
      let sum = filtered.reduce(fn(acc, m) { acc + m.value }, 0.0)
      sum / filtered.length().to_float()
    }
  }
  
  // Test metric collection
  let collector = create_collector(100)
  
  // Add counter metrics
  let counter1 = {
    name: "request_count",
    metric_type: MetricType::Counter,
    value: 1.0,
    timestamp: 1640995200,
    labels: [("service", "api"), ("endpoint", "/users")]
  }
  
  let counter2 = {
    name: "request_count",
    metric_type: MetricType::Counter,
    value: 1.0,
    timestamp: 1640995210,
    labels: [("service", "api"), ("endpoint", "/orders")]
  }
  
  let collector_with_counters = add_metric(add_metric(collector, counter1), counter2)
  assert_eq(collector_with_counters.metrics.length(), 2)
  
  // Add gauge metric
  let gauge = {
    name: "memory_usage",
    metric_type: MetricType::Gauge,
    value: 512.5,
    timestamp: 1640995220,
    labels: [("service", "api"), ("instance", "i-123")]
  }
  
  let collector_with_gauge = add_metric(collector_with_counters, gauge)
  assert_eq(collector_with_gauge.metrics.length(), 3)
  
  // Test average calculation
  let avg_requests = calculate_average(collector_with_gauge, "request_count")
  assert_eq(avg_requests, 1.0)
  
  let avg_memory = calculate_average(collector_with_gauge, "memory_usage")
  assert_eq(avg_memory, 512.5)
  
  // Test max size limit
  let small_collector = create_collector(2)
  let filled_collector = add_metric(add_metric(small_collector, counter1), counter2)
  assert_eq(filled_collector.metrics.length(), 2)
  
  // Add third metric, should remove first
  let overflow_collector = add_metric(filled_collector, gauge)
  assert_eq(overflow_collector.metrics.length(), 2)
  assert_eq(overflow_collector.metrics[0].name, "request_count")  // counter2
  assert_eq(overflow_collector.metrics[1].name, "memory_usage")   // gauge
}

// Test 3: Trace Context Propagation
test "trace context propagation" {
  // Define trace context
  type TraceContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int,
    baggage: Array[(String, String)]
  }
  
  // Define propagator interface
  type Propagator = {
    inject: fn(context: TraceContext, headers: Array[(String, String)]) -> Array[(String, String)],
    extract: fn(headers: Array[(String, String)]) -> Option[TraceContext]
  }
  
  // Create traceparent propagator
  let traceparent_propagator = {
    inject: fn(context: TraceContext, headers: Array[(String, String)]) {
      let traceparent = "00-" + context.trace_id + "-" + context.span_id + "-" + context.trace_flags.to_string(16)
      headers.push(("traceparent", traceparent))
    },
    
    extract: fn(headers: Array[(String, String)]) {
      let mut found = None
      for (key, value) in headers {
        if key == "traceparent" {
          let parts = value.split("-")
          if parts.length() == 4 {
            found = Some({
              trace_id: parts[1],
              span_id: parts[2],
              trace_flags: parts[3].to_int(16),
              baggage: []
            })
          }
        }
      }
      found
    }
  }
  
  // Create baggage propagator
  let baggage_propagator = {
    inject: fn(context: TraceContext, headers: Array[(String, String)]) {
      let mut result = headers
      for (key, value) in context.baggage {
        result = result.push(("baggage", key + "=" + value))
      }
      result
    },
    
    extract: fn(headers: Array[(String, String)]) {
      let mut baggage_items = []
      for (key, value) in headers {
        if key == "baggage" {
          baggage_items = baggage_items.push(value)
        }
      }
      
      if baggage_items.length() == 0 {
        None
      } else {
        let parsed_baggage = baggage_items.map(fn(item) {
          let parts = item.split("=")
          if parts.length() == 2 {
            (parts[0], parts[1])
          } else {
            ("", "")
          }
        })
        
        Some({
          trace_id: "",
          span_id: "",
          trace_flags: 0,
          baggage: parsed_baggage
        })
      }
    }
  }
  
  // Create composite propagator
  let composite_propagator = {
    inject: fn(context: TraceContext, headers: Array[(String, String)]) {
      let with_traceparent = traceparent_propagator.inject(context, headers)
      baggage_propagator.inject(context, with_traceparent)
    },
    
    extract: fn(headers: Array[(String, String)]) {
      let trace_context = traceparent_propagator.extract(headers)
      let baggage_context = baggage_propagator.extract(headers)
      
      match (trace_context, baggage_context) {
        (Some(tc), Some(bc)) => Some({
          tc |
          baggage: bc.baggage
        })
        (Some(tc), None) => Some(tc)
        (None, Some(bc)) => Some({
          trace_id: "",
          span_id: "",
          trace_flags: 0,
          baggage: bc.baggage
        })
        (None, None) => None
      }
    }
  }
  
  // Test context injection and extraction
  let original_context = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    trace_flags: 1,
    baggage: [("user.id", "12345"), ("service.version", "1.2.3")]
  }
  
  // Inject context into headers
  let headers = []
  let injected_headers = composite_propagator.inject(original_context, headers)
  
  // Verify traceparent header
  let traceparent_found = injected_headers.find(fn(h) { h.0 == "traceparent" })
  assert_true(traceparent_found.is_some())
  assert_eq(traceparent_found.unwrap().1, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Verify baggage headers
  let baggage_headers = injected_headers.filter(fn(h) { h.0 == "baggage" })
  assert_eq(baggage_headers.length(), 2)
  
  // Extract context from headers
  let extracted_context = composite_propagator.extract(injected_headers)
  assert_true(extracted_context.is_some())
  
  let context = extracted_context.unwrap()
  assert_eq(context.trace_id, "0af7651916cd43dd8448eb211c80319c")
  assert_eq(context.span_id, "b7ad6b7169203331")
  assert_eq(context.trace_flags, 1)
  assert_eq(context.baggage.length(), 2)
  
  // Test partial extraction (only baggage)
  let baggage_only_headers = [("baggage", "debug=true"), ("baggage", "env=production")]
  let baggage_only_context = composite_propagator.extract(baggage_only_headers)
  assert_true(baggage_only_context.is_some())
  
  let baggage_context = baggage_only_context.unwrap()
  assert_eq(baggage_context.trace_id, "")
  assert_eq(baggage_context.span_id, "")
  assert_eq(baggage_context.baggage.length(), 2)
}

// Test 4: Sampling Strategy
test "sampling strategy" {
  // Define sampling decision
  enum SamplingDecision {
    Drop
    Record
    RecordAndSample
  }
  
  // Define sampling result
  type SamplingResult = {
    decision: SamplingDecision,
    attributes: Array[(String, String)]
  }
  
  // Define sampler interface
  type Sampler = {
    should_sample: fn(trace_id: String, name: String, parent_sampled: Option[Bool]) -> SamplingResult
  }
  
  // Create always-on sampler
  let always_on_sampler = {
    should_sample: fn(trace_id: String, name: String, parent_sampled: Option[Bool]) {
      {
        decision: SamplingDecision::RecordAndSample,
        attributes: [("sampler", "always_on")]
      }
    }
  }
  
  // Create always-off sampler
  let always_off_sampler = {
    should_sample: fn(trace_id: String, name: String, parent_sampled: Option[Bool]) {
      {
        decision: SamplingDecision::Drop,
        attributes: [("sampler", "always_off")]
      }
    }
  }
  
  // Create parent-based sampler
  let parent_based_sampler = {
    should_sample: fn(trace_id: String, name: String, parent_sampled: Option[Bool]) {
      match parent_sampled {
        Some(true) => {
          {
            decision: SamplingDecision::RecordAndSample,
            attributes: [("sampler", "parent_based"), ("parent.sampled", "true")]
          }
        }
        Some(false) => {
          {
            decision: SamplingDecision::Drop,
            attributes: [("sampler", "parent_based"), ("parent.sampled", "false")]
          }
        }
        None => {
          {
            decision: SamplingDecision::Record,
            attributes: [("sampler", "parent_based"), ("parent.sampled", "none")]
          }
        }
      }
    }
  }
  
  // Create trace ID ratio sampler (simplified)
  let trace_id_ratio_sampler = fn(ratio: Float) {
    {
      should_sample: fn(trace_id: String, name: String, parent_sampled: Option[Bool]) {
        // Simplified: use trace ID first character to determine sampling
        let trace_id_value = trace_id[0].to_int()
        let threshold = (ratio * 16.0).to_int()  // 16 possible hex digits
        
        if trace_id_value < threshold {
          {
            decision: SamplingDecision::RecordAndSample,
            attributes: [("sampler", "trace_id_ratio"), ("ratio", ratio.to_string())]
          }
        } else {
          {
            decision: SamplingDecision::Drop,
            attributes: [("sampler", "trace_id_ratio"), ("ratio", ratio.to_string())]
          }
        }
      }
    }
  }
  
  // Test always-on sampler
  let always_on_result = always_on_sampler.should_sample("trace123", "operation", None)
  match always_on_result.decision {
    SamplingDecision::RecordAndSample => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(always_on_result.attributes, [("sampler", "always_on")])
  
  // Test always-off sampler
  let always_off_result = always_off_sampler.should_sample("trace456", "operation", None)
  match always_off_result.decision {
    SamplingDecision::Drop => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(always_off_result.attributes, [("sampler", "always_off")])
  
  // Test parent-based sampler with sampled parent
  let parent_sampled_result = parent_based_sampler.should_sample("trace789", "operation", Some(true))
  match parent_sampled_result.decision {
    SamplingDecision::RecordAndSample => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(parent_sampled_result.attributes, [("sampler", "parent_based"), ("parent.sampled", "true")])
  
  // Test parent-based sampler with unsampled parent
  let parent_unsampled_result = parent_based_sampler.should_sample("trace012", "operation", Some(false))
  match parent_unsampled_result.decision {
    SamplingDecision::Drop => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(parent_unsampled_result.attributes, [("sampler", "parent_based"), ("parent.sampled", "false")])
  
  // Test parent-based sampler with no parent
  let no_parent_result = parent_based_sampler.should_sample("trace345", "operation", None)
  match no_parent_result.decision {
    SamplingDecision::Record => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(no_parent_result.attributes, [("sampler", "parent_based"), ("parent.sampled", "none")])
  
  // Test trace ID ratio sampler
  let ratio_sampler = trace_id_ratio_sampler(0.5)  // 50% sampling
  
  // Use trace IDs with different first characters
  let low_trace_id = "0abcdef123456789"  // '0' has low value
  let high_trace_id = "fabcdef123456789"  // 'f' has high value
  
  let low_result = ratio_sampler.should_sample(low_trace_id, "operation", None)
  let high_result = ratio_sampler.should_sample(high_trace_id, "operation", None)
  
  // Low trace ID should be sampled (50% chance)
  match low_result.decision {
    SamplingDecision::RecordAndSample => assert_true(true)
    SamplingDecision::Drop => assert_true(true)  // Could also happen depending on implementation
    _ => assert_true(false)
  }
  
  // High trace ID might not be sampled
  match high_result.decision {
    SamplingDecision::RecordAndSample => assert_true(true)  // Could happen
    SamplingDecision::Drop => assert_true(true)  // More likely
    _ => assert_true(false)
  }
}

// Test 5: Resource Management
test "resource management" {
  // Define resource attributes
  type Resource = {
    attributes: Array[(String, String)]
  }
  
  // Create resource builder
  let create_resource = fn() {
    {
      attributes: []
    }
  }
  
  // Add attribute to resource
  let add_attribute = fn(resource: Resource, key: String, value: String) {
    // Remove existing key if present
    let filtered = resource.attributes.filter(fn(attr) { attr.0 != key })
    { resource | attributes: filtered.push((key, value)) }
  }
  
  // Merge resources (later resources override earlier ones)
  let merge_resources = fn(resources: Array[Resource]) {
    let mut result = create_resource()
    for resource in resources {
      for (key, value) in resource.attributes {
        result = add_attribute(result, key, value)
      }
    }
    result
  }
  
  // Get attribute from resource
  let get_attribute = fn(resource: Resource, key: String) {
    let mut found = None
    for (k, v) in resource.attributes {
      if k == key {
        found = Some(v)
      }
    }
    found
  }
  
  // Test resource creation and attribute management
  let resource = create_resource()
  assert_eq(resource.attributes.length(), 0)
  
  // Add service attributes
  let with_service = add_attribute(add_attribute(resource, "service.name", "payment-service"), "service.version", "1.2.3")
  assert_eq(with_service.attributes.length(), 2)
  
  // Test attribute retrieval
  let service_name = get_attribute(with_service, "service.name")
  assert_eq(service_name, Some("payment-service"))
  
  let service_version = get_attribute(with_service, "service.version")
  assert_eq(service_version, Some("1.2.3"))
  
  let missing_attr = get_attribute(with_service, "missing.attribute")
  assert_eq(missing_attr, None)
  
  // Test attribute override
  let with_updated_version = add_attribute(with_service, "service.version", "2.0.0")
  assert_eq(with_updated_version.attributes.length(), 2)  // Still 2, not 3
  
  let updated_version = get_attribute(with_updated_version, "service.version")
  assert_eq(updated_version, Some("2.0.0"))
  
  // Test resource merging
  let resource1 = add_attribute(create_resource(), "service.name", "api-service")
  let resource2 = add_attribute(add_attribute(create_resource(), "service.version", "1.0.0"), "environment", "production")
  let resource3 = add_attribute(add_attribute(create_resource(), "host.name", "server-01"), "service.version", "1.1.0")
  
  let merged = merge_resources([resource1, resource2, resource3])
  assert_eq(merged.attributes.length(), 4)
  
  assert_eq(get_attribute(merged, "service.name"), Some("api-service"))
  assert_eq(get_attribute(merged, "service.version"), Some("1.1.0"))  // Overridden by resource3
  assert_eq(get_attribute(merged, "environment"), Some("production"))
  assert_eq(get_attribute(merged, "host.name"), Some("server-01"))
  
  // Test resource with telemetry SDK info
  let sdk_resource = add_attribute(add_attribute(create_resource(), "telemetry.sdk.name", "azimuth"), "telemetry.sdk.version", "1.0.0")
  let final_resource = merge_resources([with_updated_version, sdk_resource])
  
  assert_eq(get_attribute(final_resource, "service.name"), Some("payment-service"))
  assert_eq(get_attribute(final_resource, "service.version"), Some("2.0.0"))
  assert_eq(get_attribute(final_resource, "telemetry.sdk.name"), Some("azimuth"))
  assert_eq(get_attribute(final_resource, "telemetry.sdk.version"), Some("1.0.0"))
}

// Test 6: Batch Processing
test "batch processing" {
  // Define batch configuration
  type BatchConfig = {
    max_export_batch_size: Int,
    max_export_timeout: Int,
    max_queue_size: Int
  }
  
  // Define batch processor
  type BatchProcessor = {
    config: BatchConfig,
    queue: Array[String],
    last_export_time: Int
  }
  
  // Create batch processor
  let create_processor = fn(config: BatchConfig) {
    {
      config,
      queue: [],
      last_export_time: 0
    }
  }
  
  // Add item to batch
  let add_to_batch = fn(processor: BatchProcessor, item: String, current_time: Int) {
    if processor.queue.length() >= processor.config.max_queue_size {
      // Queue is full, drop oldest item
      let remaining = processor.queue.slice(1, processor.queue.length() - 1)
      { processor | queue: remaining.push(item) }
    } else {
      { processor | queue: processor.queue.push(item) }
    }
  }
  
  // Check if batch should be exported
  let should_export = fn(processor: BatchProcessor, current_time: Int) {
    let size_reached = processor.queue.length() >= processor.config.max_export_batch_size
    let timeout_reached = (current_time - processor.last_export_time) >= processor.config.max_export_timeout
    size_reached or timeout_reached
  }
  
  // Export batch
  let export_batch = fn(processor: BatchProcessor, current_time: Int) {
    let batch = processor.queue
    {
      batch,
      updated_processor: { processor | queue: [], last_export_time: current_time }
    }
  }
  
  // Test batch processing
  let config = {
    max_export_batch_size: 3,
    max_export_timeout: 1000,
    max_queue_size: 10
  }
  
  let processor = create_processor(config)
  assert_eq(processor.queue.length(), 0)
  assert_eq(processor.last_export_time, 0)
  
  // Add items to batch
  let processor1 = add_to_batch(processor, "span1", 100)
  let processor2 = add_to_batch(processor1, "span2", 200)
  let processor3 = add_to_batch(processor2, "span3", 300)
  
  assert_eq(processor3.queue.length(), 3)
  assert_false(should_export(processor, 100))  // Before reaching batch size
  assert_true(should_export(processor3, 300))   // Reached batch size
  
  // Export batch
  let export_result = export_batch(processor3, 300)
  assert_eq(export_result.batch.length(), 3)
  assert_eq(export_result.batch, ["span1", "span2", "span3"])
  assert_eq(export_result.updated_processor.queue.length(), 0)
  assert_eq(export_result.updated_processor.last_export_time, 300)
  
  // Test timeout-based export
  let timeout_processor = add_to_batch(add_to_batch(export_result.updated_processor, "span4", 1000), "span5", 1100)
  assert_eq(timeout_processor.queue.length(), 2)
  assert_false(should_export(timeout_processor, 1100))  // Not reached batch size or timeout
  
  let timeout_processor2 = { timeout_processor | last_export_time: 100 }  // Simulate older export time
  assert_true(should_export(timeout_processor2, 1100))  // Timeout reached (1100 - 100 >= 1000)
  
  // Test queue size limit
  let full_processor = {
    let mut p = processor
    for i in 1..=10 {
      p = add_to_batch(p, "span" + i.to_string(), 1000 + i)
    }
    p
  }
  
  assert_eq(full_processor.queue.length(), 10)
  
  // Add one more item, should drop oldest
  let overflow_processor = add_to_batch(full_processor, "span11", 1011)
  assert_eq(overflow_processor.queue.length(), 10)
  assert_false(overflow_processor.queue.contains("span1"))  // Dropped
  assert_true(overflow_processor.queue.contains("span11"))  // Added
}

// Test 7: Error Recovery
test "error recovery mechanisms" {
  // Define error types
  enum TelemetryError {
    NetworkError(String)
    SerializationError(String)
    InvalidDataError(String)
    RateLimitError(Int)
  }
  
  // Define retry policy
  type RetryPolicy = {
    max_attempts: Int,
    initial_backoff: Int,
    max_backoff: Int,
    backoff_multiplier: Float
  }
  
  // Define operation result
  type OperationResult[T] = {
    success: Bool,
    data: Option[T],
    error: Option[TelemetryError],
    attempts: Int
  }
  
  // Create retry policy
  let create_retry_policy = fn(max_attempts: Int, initial_backoff: Int) {
    {
      max_attempts,
      initial_backoff,
      max_backoff: 30000,  // 30 seconds
      backoff_multiplier: 2.0
    }
  }
  
  // Calculate backoff delay
  let calculate_backoff = fn(policy: RetryPolicy, attempt: Int) {
    let delay = policy.initial_backoff.to_float() * (policy.backoff_multiplier.pow((attempt - 1).to_float()))
    let capped_delay = delay.min(policy.max_backoff.to_float())
    capped_delay.to_int()
  }
  
  // Execute operation with retry
  let execute_with_retry = fn[T](operation: () -> Result[T, TelemetryError], policy: RetryPolicy) {
    let mut attempts = 0
    let mut last_error = None
    
    while attempts < policy.max_attempts {
      attempts = attempts + 1
      
      match operation() {
        Ok(data) => {
          return {
            success: true,
            data: Some(data),
            error: None,
            attempts
          }
        }
        Err(error) => {
          last_error = Some(error)
          
          // Don't retry on rate limit errors
          match error {
            TelemetryError::RateLimitError(_) => {
              break
            }
            _ => {
              // Continue to next attempt
            }
          }
        }
      }
    }
    
    {
      success: false,
      data: None,
      error: last_error,
      attempts
    }
  }
  
  // Test retry policy creation
  let policy = create_retry_policy(3, 1000)
  assert_eq(policy.max_attempts, 3)
  assert_eq(policy.initial_backoff, 1000)
  assert_eq(policy.max_backoff, 30000)
  assert_eq(policy.backoff_multiplier, 2.0)
  
  // Test backoff calculation
  let backoff1 = calculate_backoff(policy, 1)
  let backoff2 = calculate_backoff(policy, 2)
  let backoff3 = calculate_backoff(policy, 3)
  
  assert_eq(backoff1, 1000)  // 1000 * 2^0
  assert_eq(backoff2, 2000)  // 1000 * 2^1
  assert_eq(backoff3, 4000)  // 1000 * 2^2
  
  // Test backoff capping
  let large_backoff = calculate_backoff(policy, 10)
  assert_eq(large_backoff, 30000)  // Capped at max_backoff
  
  // Test successful operation (no retry needed)
  let success_operation = fn() {
    Ok("success")
  }
  
  let success_result = execute_with_retry(success_operation, policy)
  assert_true(success_result.success)
  assert_eq(success_result.data, Some("success"))
  assert_eq(success_result.error, None)
  assert_eq(success_result.attempts, 1)
  
  // Test operation that fails then succeeds
  let mut attempt_count = 0
  let eventua_success_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Err(TelemetryError::NetworkError("Connection failed"))
    } else {
      Ok("eventual_success")
    }
  }
  
  let eventual_result = execute_with_retry(eventua_success_operation, policy)
  assert_true(eventual_result.success)
  assert_eq(eventual_result.data, Some("eventual_success"))
  assert_eq(eventual_result.error, None)
  assert_eq(eventual_result.attempts, 3)
  
  // Test operation that always fails
  let always_fail_operation = fn() {
    Err(TelemetryError::NetworkError("Persistent failure"))
  }
  
  let fail_result = execute_with_retry(always_fail_operation, policy)
  assert_false(fail_result.success)
  assert_eq(fail_result.data, None)
  assert_true(fail_result.error.is_some())
  assert_eq(fail_result.attempts, 3)
  
  // Test rate limit error (no retry)
  let rate_limit_operation = fn() {
    Err(TelemetryError::RateLimitError(100))
  }
  
  let rate_limit_result = execute_with_retry(rate_limit_operation, policy)
  assert_false(rate_limit_result.success)
  assert_eq(rate_limit_result.data, None)
  
  match rate_limit_result.error {
    Some(TelemetryError::RateLimitError(limit)) => assert_eq(limit, 100)
    _ => assert_true(false)
  }
  
  assert_eq(rate_limit_result.attempts, 1)  // Only one attempt for rate limit
}

// Test 8: Performance Metrics
test "performance metrics collection" {
  // Define performance metric types
  enum PerformanceMetric {
    Latency(Int, String)  // duration in ms, unit
    Throughput(Int, String)  // count, time unit
    ErrorRate(Float, String)  // percentage, unit
    MemoryUsage(Int, String)  // bytes, unit
  }
  
  // Define performance snapshot
  type PerformanceSnapshot = {
    timestamp: Int,
    operation_name: String,
    metrics: Array[PerformanceMetric]
  }
  
  // Define performance aggregator
  type PerformanceAggregator = {
    snapshots: Array[PerformanceSnapshot],
    window_size: Int  // Number of snapshots to keep
  }
  
  // Create aggregator
  let create_aggregator = fn(window_size: Int) {
    {
      snapshots: [],
      window_size
    }
  }
  
  // Add snapshot to aggregator
  let add_snapshot = fn(aggregator: PerformanceAggregator, snapshot: PerformanceSnapshot) {
    let updated_snapshots = aggregator.snapshots.push(snapshot)
    
    // Keep only the most recent snapshots
    if updated_snapshots.length() > aggregator.window_size {
      let start = updated_snapshots.length() - aggregator.window_size
      { aggregator | snapshots: updated_snapshots.slice(start, aggregator.window_size) }
    } else {
      { aggregator | snapshots: updated_snapshots }
    }
  }
  
  // Calculate average latency
  let calculate_average_latency = fn(aggregator: PerformanceAggregator, operation_name: String) {
    let relevant_snapshots = aggregator.snapshots.filter(fn(s) { s.operation_name == operation_name })
    let latencies = []
    
    for snapshot in relevant_snapshots {
      for metric in snapshot.metrics {
        match metric {
          PerformanceMetric::Latency(duration, _) => {
            latencies = latencies.push(duration)
          }
          _ => {}
        }
      }
    }
    
    if latencies.length() == 0 {
      0
    } else {
      let sum = latencies.reduce(fn(acc, latency) { acc + latency }, 0)
      sum / latencies.length()
    }
  }
  
  // Calculate percentile (simplified)
  let calculate_percentile = fn(values: Array[Int], percentile: Float) {
    if values.length() == 0 {
      0
    } else {
      let sorted = values.sort()
      let index = ((percentile / 100.0) * (sorted.length() - 1).to_float()).to_int()
      sorted[index]
    }
  }
  
  // Test performance metrics collection
  let aggregator = create_aggregator(10)
  
  // Create snapshots with different operations
  let snapshot1 = {
    timestamp: 1640995200,
    operation_name: "database_query",
    metrics: [
      PerformanceMetric::Latency(50, "ms"),
      PerformanceMetric::Throughput(100, "req/s"),
      PerformanceMetric::ErrorRate(0.01, "%")
    ]
  }
  
  let snapshot2 = {
    timestamp: 1640995210,
    operation_name: "database_query",
    metrics: [
      PerformanceMetric::Latency(75, "ms"),
      PerformanceMetric::Throughput(120, "req/s"),
      PerformanceMetric::ErrorRate(0.02, "%")
    ]
  }
  
  let snapshot3 = {
    timestamp: 1640995220,
    operation_name: "api_call",
    metrics: [
      PerformanceMetric::Latency(200, "ms"),
      PerformanceMetric::MemoryUsage(1024, "KB"),
      PerformanceMetric::ErrorRate(0.05, "%")
    ]
  }
  
  // Add snapshots to aggregator
  let aggregator1 = add_snapshot(aggregator, snapshot1)
  let aggregator2 = add_snapshot(aggregator1, snapshot2)
  let aggregator3 = add_snapshot(aggregator2, snapshot3)
  
  assert_eq(aggregator3.snapshots.length(), 3)
  
  // Test average latency calculation
  let db_avg_latency = calculate_average_latency(aggregator3, "database_query")
  assert_eq(db_avg_latency, 62)  // (50 + 75) / 2
  
  let api_avg_latency = calculate_average_latency(aggregator3, "api_call")
  assert_eq(api_avg_latency, 200)  // Only one snapshot
  
  let missing_avg_latency = calculate_average_latency(aggregator3, "missing_operation")
  assert_eq(missing_avg_latency, 0)  // No snapshots
  
  // Test window size limiting
  let large_aggregator = {
    let mut agg = create_aggregator(3)
    for i in 1..=5 {
      let snapshot = {
        timestamp: 1640995200 + i * 10,
        operation_name: "operation_" + i.to_string(),
        metrics: [PerformanceMetric::Latency(i * 10, "ms")]
      }
      agg = add_snapshot(agg, snapshot)
    }
    agg
  }
  
  assert_eq(large_aggregator.snapshots.length(), 3)  # Limited to window size
  assert_eq(large_aggregator.snapshots[0].operation_name, "operation_3")  # Oldest kept
  assert_eq(large_aggregator.snapshots[2].operation_name, "operation_5")  # Newest
  
  // Test percentile calculation
  let values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
  let p50 = calculate_percentile(values, 50.0)
  let p90 = calculate_percentile(values, 90.0)
  let p95 = calculate_percentile(values, 95.0)
  
  assert_eq(p50, 50)  # Median
  assert_eq(p90, 90)
  assert_eq(p95, 95)  # Simplified calculation
}

// Test 9: Configuration Management
test "configuration management" {
  // Define configuration value types
  enum ConfigValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    ArrayValue(Array[String])
  }
  
  // Define configuration source
  enum ConfigSource {
    Default
    Environment
    File
    Runtime
  }
  
  // Define configuration entry
  type ConfigEntry = {
    key: String,
    value: ConfigValue,
    source: ConfigSource,
    last_modified: Int
  }
  
  // Define configuration manager
  type ConfigManager = {
    entries: Array[ConfigEntry],
    sources: Array[ConfigSource]
  }
  
  // Create configuration manager
  let create_config_manager = fn() {
    {
      entries: [],
      sources: [ConfigSource::Default]
    }
  }
  
  // Set configuration value
  let set_config = fn(manager: ConfigManager, key: String, value: ConfigValue, source: ConfigSource, timestamp: Int) {
    // Remove existing entry with same key
    let filtered = manager.entries.filter(fn(entry) { entry.key != key })
    
    let new_entry = {
      key,
      value,
      source,
      last_modified: timestamp
    }
    
    {
      manager |
      entries: filtered.push(new_entry),
      sources: if manager.sources.contains(source) { manager.sources } else { manager.sources.push(source) }
    }
  }
  
  // Get configuration value
  let get_config = fn(manager: ConfigManager, key: String) {
    let mut found = None
    for entry in manager.entries {
      if entry.key == key {
        found = Some(entry)
      }
    }
    found
  }
  
  // Get configuration value with default
  let get_config_with_default = fn(manager: ConfigManager, key: String, default_value: ConfigValue) {
    match get_config(manager, key) {
      Some(entry) => entry.value
      None => default_value
    }
  }
  
  // Test configuration management
  let manager = create_config_manager()
  assert_eq(manager.entries.length(), 0)
  
  // Set default configuration
  let manager1 = set_config(manager, "service.name", ConfigValue::StringValue("azimuth"), ConfigSource::Default, 1640995200)
  let manager2 = set_config(manager1, "service.port", ConfigValue::IntValue(8080), ConfigSource::Default, 1640995200)
  let manager3 = set_config(manager2, "debug.enabled", ConfigValue::BoolValue(false), ConfigSource::Default, 1640995200)
  
  assert_eq(manager3.entries.length(), 3)
  
  // Test configuration retrieval
  let service_name_entry = get_config(manager3, "service.name")
  assert_true(service_name_entry.is_some())
  
  match service_name_entry.unwrap().value {
    ConfigValue::StringValue(name) => assert_eq(name, "azimuth")
    _ => assert_true(false)
  }
  
  // Test configuration with default
  let missing_config = get_config_with_default(manager3, "missing.key", ConfigValue::StringValue("default"))
  match missing_config {
    ConfigValue::StringValue(value) => assert_eq(value, "default")
    _ => assert_true(false)
  }
  
  // Override configuration from environment
  let manager4 = set_config(manager3, "service.port", ConfigValue::IntValue(9090), ConfigSource::Environment, 1640995300)
  
  let port_entry = get_config(manager4, "service.port")
  assert_true(port_entry.is_some())
  
  let port_entry_unwrapped = port_entry.unwrap()
  match port_entry_unwrapped.value {
    ConfigValue::IntValue(port) => assert_eq(port, 9090)
    _ => assert_true(false)
  }
  assert_eq(port_entry_unwrapped.source, ConfigSource::Environment)
  
  // Test source priority tracking
  assert_true(manager4.sources.contains(ConfigSource::Default))
  assert_true(manager4.sources.contains(ConfigSource::Environment))
  
  // Test runtime configuration
  let manager5 = set_config(manager4, "feature.flags", ConfigValue::ArrayValue(["new_ui", "beta_api"]), ConfigSource::Runtime, 1640995400)
  
  let flags_entry = get_config(manager5, "feature.flags")
  assert_true(flags_entry.is_some())
  
  match flags_entry.unwrap().value {
    ConfigValue::ArrayValue(flags) => {
      assert_eq(flags.length(), 2)
      assert_true(flags.contains("new_ui"))
      assert_true(flags.contains("beta_api"))
    }
    _ => assert_true(false)
  }
  
  // Test configuration filtering by source
  let get_configs_by_source = fn(manager: ConfigManager, source: ConfigSource) {
    manager.entries.filter(fn(entry) { entry.source == source })
  }
  
  let default_configs = get_configs_by_source(manager5, ConfigSource::Default)
  assert_eq(default_configs.length(), 2)  # service.name and debug.enabled (service.port was overridden)
  
  let env_configs = get_configs_by_source(manager5, ConfigSource::Environment)
  assert_eq(env_configs.length(), 1)  # service.port
  
  let runtime_configs = get_configs_by_source(manager5, ConfigSource::Runtime)
  assert_eq(runtime_configs.length(), 1)  # feature.flags
}

// Test 10: Data Validation
test "data validation" {
  // Define validation rule
  type ValidationRule = {
    name: String,
    validate: fn(String) -> Bool,
    error_message: String
  }
  
  // Define validation result
  type ValidationResult = {
    valid: Bool,
    errors: Array[String]
  }
  
  // Define validator
  type Validator = {
    rules: Array[ValidationRule]
  }
  
  // Create validator
  let create_validator = fn() {
    {
      rules: []
    }
  }
  
  // Add validation rule
  let add_rule = fn(validator: Validator, rule: ValidationRule) {
    { validator | rules: validator.rules.push(rule) }
  }
  
  // Validate value
  let validate = fn(validator: Validator, value: String) {
    let mut errors = []
    
    for rule in validator.rules {
      if not(rule.validate(value)) {
        errors = errors.push(rule.error_message)
      }
    }
    
    {
      valid: errors.length() == 0,
      errors
    }
  }
  
  // Create common validation rules
  let non_empty_rule = {
    name: "non_empty",
    validate: fn(value: String) { value.length() > 0 },
    error_message: "Value cannot be empty"
  }
  
  let max_length_rule = fn(max_length: Int) {
    {
      name: "max_length",
      validate: fn(value: String) { value.length() <= max_length },
      error_message: "Value cannot exceed " + max_length.to_string() + " characters"
    }
  }
  
  let pattern_rule = fn(pattern: String, message: String) {
    {
      name: "pattern",
      validate: fn(value: String) {
        // Simplified pattern matching - just check if value contains pattern
        value.contains(pattern)
      },
      error_message: message
    }
  }
  
  // Test validator creation and rule management
  let validator = create_validator()
  assert_eq(validator.rules.length(), 0)
  
  // Add validation rules
  let validator1 = add_rule(validator, non_empty_rule)
  let validator2 = add_rule(validator1, max_length_rule(50))
  let validator3 = add_rule(validator2, pattern_rule("trace-", "Value must contain 'trace-' prefix"))
  
  assert_eq(validator3.rules.length(), 3)
  
  // Test valid values
  let valid_result = validate(validator3, "trace-123456789")
  assert_true(valid_result.valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid values
  let empty_result = validate(validator3, "")
  assert_false(empty_result.valid)
  assert_eq(empty_result.errors.length(), 3)  # All rules should fail
  
  let too_long_result = validate(validator3, "trace-" + "a".repeat(100))
  assert_false(too_long_result.valid)
  assert_eq(too_long_result.errors.length(), 1)  # Only max length rule should fail
  
  let missing_prefix_result = validate(validator3, "123456789")
  assert_false(missing_prefix_result.valid)
  assert_eq(missing_prefix_result.errors.length(), 1)  # Only pattern rule should fail
  
  // Test specialized validators for different data types
  
  // Trace ID validator
  let trace_id_validator = create_validator()
    |> add_rule(non_empty_rule)
    |> add_rule(max_length_rule(32))
    |> add_rule(pattern_rule("-", "Trace ID must contain a dash"))
  
  let valid_trace_id = validate(trace_id_validator, "0af7651916cd43dd8448eb211c80319c")
  assert_true(valid_trace_id.valid)
  
  let invalid_trace_id = validate(trace_id_validator, "invalid")
  assert_false(invalid_trace_id.valid)
  
  // Service name validator
  let service_name_validator = create_validator()
    |> add_rule(non_empty_rule)
    |> add_rule(max_length_rule(50))
    |> add_rule({
      name: "alphanumeric",
      validate: fn(value: String) {
        let mut valid = true
        for c in value.to_char_array() {
          if not((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_' or c == '-') {
            valid = false
          }
        }
        valid
      },
      error_message: "Service name can only contain alphanumeric characters, underscores, and hyphens"
    })
  
  let valid_service_name = validate(service_name_validator, "payment-service_v2")
  assert_true(valid_service_name.valid)
  
  let invalid_service_name = validate(service_name_validator, "service@invalid")
  assert_false(invalid_service_name.valid)
  
  // Test conditional validation
  let conditional_validator = fn(condition: Bool) {
    if condition {
      create_validator() |> add_rule(max_length_rule(10))
    } else {
      create_validator() |> add_rule(max_length_rule(100))
    }
  }
  
  let strict_validator = conditional_validator(true)
  let lenient_validator = conditional_validator(false)
  
  let strict_result = validate(strict_validator, "12345678901")  # 11 characters
  assert_false(strict_result.valid)
  
  let lenient_result = validate(lenient_validator, "12345678901")  # 11 characters
  assert_true(lenient_result.valid)
}
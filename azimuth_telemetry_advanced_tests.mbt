// Azimuth Advanced Telemetry Test Suite
// This file contains advanced telemetry-specific test cases

// Test 1: Telemetry Data Generation
test "telemetry data generation and validation" {
  // Generate telemetry span data
  let generate_span_id = fn() {
    let timestamp = 1640995200  // Fixed timestamp for reproducible tests
    let random = 12345  // Deterministic "random" value for testing
    "span-" + timestamp.to_string() + "-" + random.to_string()
  }
  
  let generate_trace_id = fn() {
    let timestamp = 1640995200
    let service_id = "payment"
    let instance = 1
    "trace-" + service_id + "-" + timestamp.to_string() + "-" + instance.to_string()
  }
  
  let span_id = generate_span_id()
  let trace_id = generate_trace_id()
  
  // Validate generated IDs
  assert_true(span_id.starts_with("span-"))
  assert_true(trace_id.starts_with("trace-"))
  assert_true(span_id.length() > 10)
  assert_true(trace_id.length() > 10)
  
  // Create telemetry event
  let create_telemetry_event = fn(name: String, event_type: String) {
    {
      name,
      event_type,
      span_id: generate_span_id(),
      trace_id: generate_trace_id(),
      timestamp: 1640995200,
      attributes: []
    }
  }
  
  let event = create_telemetry_event("payment_processed", "business")
  assert_eq(event.name, "payment_processed")
  assert_eq(event.event_type, "business")
  assert_true(event.span_id.starts_with("span-"))
  assert_true(event.trace_id.starts_with("trace-"))
  assert_eq(event.attributes.length(), 0)
  
  // Add attributes to event
  let add_attribute = fn(event, key: String, value: String) {
    { event | 
      attributes: event.attributes.push((key, value))
    }
  }
  
  let event_with_attrs = add_attribute(event, "amount", "100.50")
  let event_final = add_attribute(event_with_attrs, "currency", "USD")
  
  assert_eq(event_final.attributes.length(), 2)
  assert_true(event_final.attributes.contains(("amount", "100.50")))
  assert_true(event_final.attributes.contains(("currency", "USD")))
}

// Test 2: Distributed Trace Context Propagation
test "distributed trace context propagation" {
  // Define trace context structure
  type TraceContext = {
    trace_id: String,
    span_id: String,
    baggage: Array[(String, String)],
    sampled: Bool
  }
  
  // Create root trace context
  let create_root_context = fn(service_name: String) {
    {
      trace_id: "trace-" + service_name + "-1640995200",
      span_id: "span-root-" + service_name,
      baggage: [],
      sampled: true
    }
  }
  
  // Extract context from headers (simulated)
  let extract_context = fn(headers: Array[(String, String)]) {
    let mut trace_id = None
    let mut span_id = None
    let mut baggage = []
    
    for (key, value) in headers {
      match key {
        "x-trace-id" => trace_id = Some(value)
        "x-span-id" => span_id = Some(value)
        "x-baggage" => {
          // Parse baggage items (simplified)
          let items = value.split(",")
          for item in items {
            let parts = item.split("=")
            if parts.length() == 2 {
              baggage = baggage.push((parts[0], parts[1]))
            }
          }
        }
        _ => ()  // Ignore other headers
      }
    }
    
    match (trace_id, span_id) {
      (Some(trace), Some(span)) => Some({
        trace_id: trace,
        span_id: span,
        baggage: baggage,
        sampled: true
      })
      _ => None
    }
  }
  
  // Inject context into headers (simulated)
  let inject_context = fn(context: TraceContext) {
    let mut headers = []
    headers = headers.push(("x-trace-id", context.trace_id))
    headers = headers.push(("x-span-id", context.span_id))
    
    if context.baggage.length() > 0 {
      let baggage_str = context.baggage
        .map(fn((k, v)) { k + "=" + v })
        .reduce(fn(acc, item) { acc + "," + item })
      headers = headers.push(("x-baggage", baggage_str))
    }
    
    headers
  }
  
  // Test context creation and propagation
  let root_context = create_root_context("payment-service")
  assert_eq(root_context.trace_id, "trace-payment-service-1640995200")
  assert_eq(root_context.span_id, "span-root-payment-service")
  assert_eq(root_context.baggage.length(), 0)
  assert_true(root_context.sampled)
  
  // Add baggage items
  let with_baggage = { root_context |
    baggage: root_context.baggage
      .push(("user.id", "12345"))
      .push(("request.id", "req-789"))
  }
  
  // Inject context into headers
  let headers = inject_context(with_baggage)
  
  // Verify headers contain trace information
  let trace_header = headers.find(fn((k, _)) { k == "x-trace-id" })
  let span_header = headers.find(fn((k, _)) { k == "x-span-id" })
  let baggage_header = headers.find(fn((k, _)) { k == "x-baggage" })
  
  assert_eq(trace_header, Some(("x-trace-id", "trace-payment-service-1640995200")))
  assert_eq(span_header, Some(("x-span-id", "span-root-payment-service")))
  assert_true(baggage_header.is_some())
  
  // Extract context from headers
  let extracted_context_opt = extract_context(headers)
  
  match extracted_context_opt {
    Some(extracted_context) => {
      assert_eq(extracted_context.trace_id, root_context.trace_id)
      assert_eq(extracted_context.span_id, root_context.span_id)
      assert_eq(extracted_context.baggage.length(), 2)
      assert_true(extracted_context.baggage.contains(("user.id", "12345")))
      assert_true(extracted_context.baggage.contains(("request.id", "req-789")))
    }
    None => assert_true(false)
  }
  
  // Create child context
  let create_child_context = fn(parent: TraceContext, child_name: String) {
    {
      trace_id: parent.trace_id,
      span_id: "span-" + child_name + "-" + parent.span_id,
      baggage: parent.baggage,
      sampled: parent.sampled
    }
  }
  
  let child_context = create_child_context(with_baggage, "database")
  assert_eq(child_context.trace_id, root_context.trace_id)
  assert_eq(child_context.span_id, "span-database-span-root-payment-service")
  assert_eq(child_context.baggage.length(), 2)
  assert_true(child_context.sampled)
}

// Test 3: Performance Metrics Collection
test "performance metrics collection and aggregation" {
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
    labels: Array[(String, String)],
    timestamp: Int
  }
  
  // Create metrics
  let create_counter = fn(name: String, value: Float, labels: Array[(String, String)]) {
    {
      name,
      metric_type: MetricType::Counter,
      value,
      labels,
      timestamp: 1640995200
    }
  }
  
  let create_gauge = fn(name: String, value: Float, labels: Array[(String, String)]) {
    {
      name,
      metric_type: MetricType::Gauge,
      value,
      labels,
      timestamp: 1640995200
    }
  }
  
  let create_histogram = fn(name: String, value: Float, labels: Array[(String, String)]) {
    {
      name,
      metric_type: MetricType::Histogram,
      value,
      labels,
      timestamp: 1640995200
    }
  }
  
  // Test metric creation
  let request_counter = create_counter("http_requests_total", 1000.0, [("method", "GET"), ("status", "200")])
  let active_connections = create_gauge("active_connections", 25.0, [("service", "api")])
  let response_time = create_histogram("http_request_duration_seconds", 0.150, [("endpoint", "/api/users")])
  
  assert_eq(request_counter.name, "http_requests_total")
  assert_eq(request_counter.value, 1000.0)
  assert_eq(request_counter.labels.length(), 2)
  
  assert_eq(active_connections.metric_type, MetricType::Gauge)
  assert_eq(active_connections.value, 25.0)
  
  assert_eq(response_time.metric_type, MetricType::Histogram)
  assert_eq(response_time.value, 0.150)
  
  // Aggregate metrics by name
  let aggregate_by_name = fn(metrics: Array[Metric]) {
    let mut groups = []
    
    for metric in metrics {
      let existing = groups.find(fn(m) { m.name == metric.name })
      match existing {
        Some(existing_metric) => {
          // For counters, sum values
          let updated_value = match existing_metric.metric_type {
            MetricType::Counter => existing_metric.value + metric.value
            MetricType::Gauge => metric.value  // Use latest gauge value
            _ => existing_metric.value  // Simplified for other types
          }
          
          // Update the metric (simplified)
          let index = groups.index_of(existing_metric)
          groups = groups.set(index, { existing_metric | value: updated_value })
        }
        None => {
          groups = groups.push(metric)
        }
      }
    }
    
    groups
  }
  
  // Create multiple metrics with the same name
  let counter1 = create_counter("http_requests_total", 100.0, [("method", "GET")])
  let counter2 = create_counter("http_requests_total", 200.0, [("method", "POST")])
  let counter3 = create_counter("http_requests_total", 150.0, [("method", "GET")])
  
  let all_metrics = [request_counter, counter1, counter2, counter3, active_connections, response_time]
  let aggregated = aggregate_by_name(all_metrics)
  
  // Find aggregated counter
  let aggregated_counter = aggregated.find(fn(m) { m.name == "http_requests_total" })
  match aggregated_counter {
    Some(counter) => assert_eq(counter.value, 1450.0)  // 1000 + 100 + 200 + 150
    None => assert_true(false)
  }
  
  // Filter metrics by label
  let filter_by_label = fn(metrics: Array[Metric], label_key: String, label_value: String) {
    metrics.filter(fn(metric) {
      metric.labels.find(fn((k, v)) { k == label_key and v == label_value }).is_some()
    })
  }
  
  let get_metrics = filter_by_label(all_metrics, "method", "GET")
  assert_eq(get_metrics.length(), 2)
  
  // Calculate metric statistics
  let calculate_stats = fn(metrics: Array[Metric]) {
    let values = metrics.map(fn(m) { m.value })
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let count = values.length().to_float()
    let avg = sum / count
    
    let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
    let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
    
    { sum, avg, min, max, count }
  }
  
  let response_times = [
    create_histogram("response_time", 0.100, []),
    create_histogram("response_time", 0.200, []),
    create_histogram("response_time", 0.150, []),
    create_histogram("response_time", 0.300, []),
    create_histogram("response_time", 0.125, [])
  ]
  
  let stats = calculate_stats(response_times)
  assert_eq(stats.sum, 0.875)
  assert_eq(stats.avg, 0.175)
  assert_eq(stats.min, 0.100)
  assert_eq(stats.max, 0.300)
  assert_eq(stats.count, 5.0)
}

// Test 4: Sampling Strategies
test "telemetry sampling strategies" {
  // Define sampling decision types
  enum SamplingDecision {
    RecordAndSample
    RecordOnly
    Drop
  }
  
  // Define sampling strategy types
  enum SamplingStrategy {
    AlwaysOn
    AlwaysOff
    Probability(Float)  // Sample rate between 0.0 and 1.0
    RateLimiting(Int)   // Max samples per second
  }
  
  // Define sampler structure
  type Sampler = {
    strategy: SamplingStrategy,
    samples_taken: Int,
    samples_seen: Int,
    rate_limit_tokens: Int,
    last_refill: Int
  }
  
  // Create sampler with strategy
  let create_sampler = fn(strategy: SamplingStrategy) {
    {
      strategy,
      samples_taken: 0,
      samples_seen: 0,
      rate_limit_tokens: 10,  // Default rate limit tokens
      last_refill: 1640995200
    }
  }
  
  // Make sampling decision
  let make_sampling_decision = fn(sampler: Sampler, trace_id: String) {
    match sampler.strategy {
      SamplingStrategy::AlwaysOn => (SamplingDecision::RecordAndSample, sampler)
      
      SamplingStrategy::AlwaysOff => (SamplingDecision::Drop, sampler)
      
      SamplingStrategy::Probability(rate) => {
        // Simple deterministic "random" based on trace ID for testing
        let hash = trace_id.length() % 100
        let threshold = (rate * 100.0).to_int()
        
        if hash < threshold {
          (SamplingDecision::RecordAndSample, { sampler | 
            samples_taken: sampler.samples_taken + 1,
            samples_seen: sampler.samples_seen + 1
          })
        } else {
          (SamplingDecision::RecordOnly, { sampler |
            samples_seen: sampler.samples_seen + 1
          })
        }
      }
      
      SamplingStrategy::RateLimiting(max_per_second) => {
        // Simplified rate limiting with token bucket
        let current_time = 1640995200
        let time_diff = current_time - sampler.last_refill
        
        // Refill tokens (simplified - 1 token per second)
        let new_tokens = if time_diff >= 1 { time_diff } else { 0 }
        let updated_tokens = min(sampler.rate_limit_tokens + new_tokens, max_per_second)
        
        if updated_tokens > 0 {
          (SamplingDecision::RecordAndSample, { sampler |
            samples_taken: sampler.samples_taken + 1,
            samples_seen: sampler.samples_seen + 1,
            rate_limit_tokens: updated_tokens - 1,
            last_refill: current_time
          })
        } else {
          (SamplingDecision::RecordOnly, { sampler |
            samples_seen: sampler.samples_seen + 1,
            last_refill: current_time
          })
        }
      }
    }
  }
  
  // Test AlwaysOn sampler
  let always_on_sampler = create_sampler(SamplingStrategy::AlwaysOn)
  let (decision1, _) = make_sampling_decision(always_on_sampler, "trace-123")
  assert_eq(decision1, SamplingDecision::RecordAndSample)
  
  // Test AlwaysOff sampler
  let always_off_sampler = create_sampler(SamplingStrategy::AlwaysOff)
  let (decision2, _) = make_sampling_decision(always_off_sampler, "trace-456")
  assert_eq(decision2, SamplingDecision::Drop)
  
  // Test probability sampler
  let prob_sampler = create_sampler(SamplingStrategy::Probability(0.5))  // 50% sampling
  
  // Use trace IDs with different lengths to get different "random" values
  let trace_a = "trace-short"  // length 10 -> 10 % 100 = 10
  let trace_b = "trace-very-long-trace-id-with-many-characters"  // length 44 -> 44 % 100 = 44
  let trace_c = "trace-much-much-much-much-much-longer-trace-id"  // length 50 -> 50 % 100 = 50
  let trace_d = "trace-extremely-long-trace-id-that-should-not-be-sampled"  // length 58 -> 58 % 100 = 58
  
  let (decision_a, sampler_a) = make_sampling_decision(prob_sampler, trace_a)
  let (decision_b, sampler_b) = make_sampling_decision(sampler_a, trace_b)
  let (decision_c, sampler_c) = make_sampling_decision(sampler_b, trace_c)
  let (decision_d, sampler_d) = make_sampling_decision(sampler_c, trace_d)
  
  // With 50% probability (threshold = 50), traces with hash < 50 should be sampled
  assert_eq(decision_a, SamplingDecision::RecordAndSample)  // 10 < 50
  assert_eq(decision_b, SamplingDecision::RecordOnly)       // 44 < 50
  assert_eq(decision_c, SamplingDecision::RecordOnly)       // 50 >= 50
  assert_eq(decision_d, SamplingDecision::RecordOnly)       // 58 >= 50
  
  assert_eq(sampler_d.samples_taken, 1)
  assert_eq(sampler_d.samples_seen, 4)
  
  // Test rate limiting sampler
  let rate_limit_sampler = create_sampler(SamplingStrategy::RateLimiting(3))
  
  let (decision_r1, sampler_r1) = make_sampling_decision(rate_limit_sampler, "trace-1")
  let (decision_r2, sampler_r2) = make_sampling_decision(sampler_r1, "trace-2")
  let (decision_r3, sampler_r3) = make_sampling_decision(sampler_r2, "trace-3")
  let (decision_r4, sampler_r4) = make_sampling_decision(sampler_r3, "trace-4")
  
  // First 3 should be sampled (initial tokens = 10, but limited to max_per_second = 3)
  assert_eq(decision_r1, SamplingDecision::RecordAndSample)
  assert_eq(decision_r2, SamplingDecision::RecordAndSample)
  assert_eq(decision_r3, SamplingDecision::RecordAndSample)
  
  // 4th should not be sampled (tokens exhausted)
  assert_eq(decision_r4, SamplingDecision::RecordOnly)
  
  assert_eq(sampler_r4.samples_taken, 3)
  assert_eq(sampler_r4.samples_seen, 4)
  
  // Test adaptive sampling based on trace length
  let adaptive_sampling = fn(sampler: Sampler, trace_id: String) {
    let base_decision = make_sampling_decision(sampler, trace_id)
    match base_decision {
      (SamplingDecision::RecordAndSample, updated_sampler) => {
        // For very long traces, use RecordOnly to save resources
        if trace_id.length() > 50 {
          (SamplingDecision::RecordOnly, updated_sampler)
        } else {
          (SamplingDecision::RecordAndSample, updated_sampler)
        }
      }
      other => other
    }
  }
  
  let adaptive_sampler = create_sampler(SamplingStrategy::Probability(0.8))
  
  let short_trace = "trace-short"
  let long_trace = "trace-this-is-a-very-long-trace-id-that-should-be-adaptively-sampled"
  
  let (adaptive_decision_short, _) = adaptive_sampling(adaptive_sampler, short_trace)
  let (adaptive_decision_long, _) = adaptive_sampling(adaptive_sampler, long_trace)
  
  // Short trace should follow normal sampling
  assert_eq(adaptive_decision_short, SamplingDecision::RecordAndSample)
  
  // Long trace should be downgraded to RecordOnly even if normally sampled
  assert_eq(adaptive_decision_long, SamplingDecision::RecordOnly)
}

// Test 5: Telemetry Data Serialization
test "telemetry data serialization formats" {
  // Define telemetry span structure
  type TelemetrySpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Array[(String, String)],
    logs: Array[(Int, String)]
  }
  
  // Create test span
  let test_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-parent"),
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    tags: [("service", "payment"), ("db.type", "postgresql")],
    logs: [(1640995220, "Query started"), (1640995240, "Query completed")]
  }
  
  // Serialize to JSON-like string (simplified)
  let serialize_to_json = fn(span: TelemetrySpan) {
    let tags_str = span.tags
      .map(fn((k, v)) { "\"" + k + "\":\"" + v + "\"" })
      .reduce(fn(acc, item) { acc + "," + item })
    
    let logs_str = span.logs
      .map(fn((time, msg)) { "{\"timestamp\":" + time.to_string() + ",\"message\":\"" + msg + "\"}" })
      .reduce(fn(acc, item) { acc + "," + item })
    
    let parent_str = match span.parent_span_id {
      Some(parent) => "\"" + parent + "\""
      None => "null"
    }
    
    "{" +
    "\"trace_id\":\"" + span.trace_id + "\"," +
    "\"span_id\":\"" + span.span_id + "\"," +
    "\"parent_span_id\":" + parent_str + "," +
    "\"operation_name\":\"" + span.operation_name + "\"," +
    "\"start_time\":" + span.start_time.to_string() + "," +
    "\"end_time\":" + span.end_time.to_string() + "," +
    "\"status\":\"" + span.status + "\"," +
    "\"tags\":{" + tags_str + "}," +
    "\"logs\":[" + logs_str + "]" +
    "}"
  }
  
  // Serialize to key-value format
  let serialize_to_kv = fn(span: TelemetrySpan) {
    let mut pairs = []
    pairs = pairs.push(("trace_id", span.trace_id))
    pairs = pairs.push(("span_id", span.span_id))
    
    match span.parent_span_id {
      Some(parent) => pairs = pairs.push(("parent_span_id", parent))
      None => ()
    }
    
    pairs = pairs.push(("operation_name", span.operation_name))
    pairs = pairs.push(("start_time", span.start_time.to_string()))
    pairs = pairs.push(("end_time", span.end_time.to_string()))
    pairs = pairs.push(("status", span.status))
    
    // Add tags
    for (key, value) in span.tags {
      pairs = pairs.push(("tag." + key, value))
    }
    
    // Add logs
    for (i, (time, msg)) in span.logs.enumerate() {
      pairs = pairs.push(("log." + i.to_string() + ".timestamp", time.to_string()))
      pairs = pairs.push(("log." + i.to_string() + ".message", msg))
    }
    
    pairs.map(fn((k, v)) { k + "=" + v })
      .reduce(fn(acc, item) { acc + "\n" + item })
  }
  
  // Test JSON serialization
  let json_str = serialize_to_json(test_span)
  assert_true(json_str.contains("\"trace_id\":\"trace-12345\""))
  assert_true(json_str.contains("\"span_id\":\"span-67890\""))
  assert_true(json_str.contains("\"parent_span_id\":\"span-parent\""))
  assert_true(json_str.contains("\"operation_name\":\"database_query\""))
  assert_true(json_str.contains("\"service\":\"payment\""))
  assert_true(json_str.contains("\"Query started\""))
  
  // Test KV serialization
  let kv_str = serialize_to_kv(test_span)
  assert_true(kv_str.contains("trace_id=trace-12345"))
  assert_true(kv_str.contains("span_id=span-67890"))
  assert_true(kv_str.contains("parent_span_id=span-parent"))
  assert_true(kv_str.contains("operation_name=database_query"))
  assert_true(kv_str.contains("tag.service=payment"))
  assert_true(kv_str.contains("log.0.message=Query started"))
  
  // Parse from JSON-like string (simplified)
  let parse_from_json = fn(json_str: String) {
    // Simplified parsing - in real implementation would use proper JSON parser
    let trace_id_start = json_str.index_of("\"trace_id\":\"") + 12
    let trace_id_end = json_str.index_of("\"", trace_id_start)
    let trace_id = json_str.substring(trace_id_start, trace_id_end - trace_id_start)
    
    let span_id_start = json_str.index_of("\"span_id\":\"") + 11
    let span_id_end = json_str.index_of("\"", span_id_start)
    let span_id = json_str.substring(span_id_start, span_id_end - span_id_start)
    
    let operation_start = json_str.index_of("\"operation_name\":\"") + 18
    let operation_end = json_str.index_of("\"", operation_start)
    let operation_name = json_str.substring(operation_start, operation_end - operation_start)
    
    // Create simplified span (omitting complex parsing for tags and logs)
    {
      trace_id,
      span_id,
      parent_span_id: None,  // Simplified
      operation_name,
      start_time: 0,  // Simplified
      end_time: 0,    // Simplified
      status: "unknown",  // Simplified
      tags: [],  // Simplified
      logs: []   // Simplified
    }
  }
  
  // Test JSON parsing
  let parsed_span = parse_from_json(json_str)
  assert_eq(parsed_span.trace_id, "trace-12345")
  assert_eq(parsed_span.span_id, "span-67890")
  assert_eq(parsed_span.operation_name, "database_query")
  
  // Test format conversion
  let convert_json_to_kv = fn(json_str: String) {
    let parsed = parse_from_json(json_str)
    serialize_to_kv(parsed)
  }
  
  let converted_kv = convert_json_to_kv(json_str)
  assert_true(converted_kv.contains("trace_id=trace-12345"))
  assert_true(converted_kv.contains("span_id=span-67890"))
  assert_true(converted_kv.contains("operation_name=database_query"))
  
  // Test compression simulation
  let compress_data = fn(data: String) {
    // Simulate compression by removing whitespace and using shorter representations
    data.replace(" ", "")
      .replace("\n", "")
      .replace("\"", "'")
      .replace(":", "=")
      .replace(",", ";")
  }
  
  let compressed_json = compress_data(json_str)
  assert_true(compressed_json.length() < json_str.length())
  assert_true(compressed_json.contains("trace_id=trace-12345"))
}

// Test 6: Cross-Service Telemetry Consistency
test "cross-service telemetry consistency" {
  // Define service telemetry structure
  type ServiceTelemetry = {
    service_name: String,
    service_version: String,
    trace_id: String,
    spans: Array[ServiceSpan],
    metrics: Array[ServiceMetric]
  }
  
  type ServiceSpan = {
    span_id: String,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    service_name: String
  }
  
  type ServiceMetric = {
    name: String,
    value: Float,
    service_name: String
  }
  
  // Create service telemetry data
  let create_service_span = fn(span_id: String, operation: String, service: String, start: Int, duration: Int) {
    {
      span_id,
      operation_name: operation,
      start_time: start,
      end_time: start + duration,
      service_name: service
    }
  }
  
  let create_service_metric = fn(name: String, value: Float, service: String) {
    {
      name,
      value,
      service_name: service
    }
  }
  
  // Create telemetry for multiple services
  let api_service = {
    service_name: "api-service",
    service_version: "1.2.3",
    trace_id: "trace-12345",
    spans: [
      create_service_span("span-1", "http_request", "api-service", 1640995200, 100),
      create_service_span("span-2", "auth_check", "api-service", 1640995210, 50)
    ],
    metrics: [
      create_service_metric("http_requests_total", 1.0, "api-service"),
      create_service_metric("request_duration_ms", 100.0, "api-service")
    ]
  }
  
  let payment_service = {
    service_name: "payment-service",
    service_version: "2.1.0",
    trace_id: "trace-12345",  // Same trace ID for correlation
    spans: [
      create_service_span("span-3", "process_payment", "payment-service", 1640995220, 200),
      create_service_span("span-4", "validate_card", "payment-service", 1640995230, 75)
    ],
    metrics: [
      create_service_metric("payments_processed", 1.0, "payment-service"),
      create_service_metric("payment_amount", 100.50, "payment-service")
    ]
  }
  
  let notification_service = {
    service_name: "notification-service",
    service_version: "1.0.5",
    trace_id: "trace-12345",  // Same trace ID for correlation
    spans: [
      create_service_span("span-5", "send_receipt", "notification-service", 1640995240, 150)
    ],
    metrics: [
      create_service_metric("notifications_sent", 1.0, "notification-service")
    ]
  }
  
  // Verify trace ID consistency across services
  assert_eq(api_service.trace_id, payment_service.trace_id)
  assert_eq(payment_service.trace_id, notification_service.trace_id)
  assert_eq(api_service.trace_id, "trace-12345")
  
  // Merge telemetry from multiple services
  let merge_telemetry = fn(services: Array[ServiceTelemetry]) {
    let mut all_spans = []
    let mut all_metrics = []
    let mut trace_id = None
    
    for service in services {
      all_spans = all_spans + service.spans
      all_metrics = all_metrics + service.metrics
      
      match trace_id {
        None => trace_id = Some(service.trace_id)
        Some(existing_id) => assert_eq(existing_id, service.trace_id)
      }
    }
    
    {
      trace_id: trace_id.unwrap_or(""),
      spans: all_spans,
      metrics: all_metrics,
      services: services.map(fn(s) { s.service_name })
    }
  }
  
  let services = [api_service, payment_service, notification_service]
  let merged = merge_telemetry(services)
  
  assert_eq(merged.trace_id, "trace-12345")
  assert_eq(merged.spans.length(), 5)  // 2 + 2 + 1
  assert_eq(merged.metrics.length(), 4)  // 2 + 2 + 1
  assert_eq(merged.services.length(), 3)
  assert_true(merged.services.contains("api-service"))
  assert_true(merged.services.contains("payment-service"))
  assert_true(merged.services.contains("notification-service"))
  
  // Analyze trace timeline
  let analyze_timeline = fn(spans: Array[ServiceSpan]) {
    let sorted_spans = spans.sort_by(fn(a, b) { a.start_time - b.start_time })
    
    let mut total_duration = 0
    let mut earliest_start = sorted_spans[0].start_time
    let mut latest_end = sorted_spans[0].end_time
    
    for span in sorted_spans {
      if span.start_time < earliest_start {
        earliest_start = span.start_time
      }
      if span.end_time > latest_end {
        latest_end = span.end_time
      }
      total_duration = total_duration + (span.end_time - span.start_time)
    }
    
    let trace_duration = latest_end - earliest_start
    
    {
      spans: sorted_spans,
      trace_duration,
      total_span_duration: total_duration,
      earliest_start,
      latest_end
    }
  }
  
  let timeline = analyze_timeline(merged.spans)
  assert_eq(timeline.spans.length(), 5)
  assert_eq(timeline.trace_duration, 240)  // From 1640995200 to 1640995440
  assert_eq(timeline.total_span_duration, 575)  // 100 + 50 + 200 + 75 + 150
  assert_eq(timeline.earliest_start, 1640995200)
  assert_eq(timeline.latest_end, 1640995440)
  
  // Verify span order
  assert_eq(timeline.spans[0].operation_name, "http_request")
  assert_eq(timeline.spans[1].operation_name, "auth_check")
  assert_eq(timeline.spans[2].operation_name, "process_payment")
  assert_eq(timeline.spans[3].operation_name, "validate_card")
  assert_eq(timeline.spans[4].operation_name, "send_receipt")
  
  // Check for overlapping spans (potential concurrency)
  let find_overlaps = fn(spans: Array[ServiceSpan]) {
    let mut overlaps = []
    
    for i in 0..spans.length() {
      for j in (i + 1)..spans.length() {
        let span_a = spans[i]
        let span_b = spans[j]
        
        // Check if spans overlap
        let overlap = not(span_a.end_time <= span_b.start_time or span_b.end_time <= span_a.start_time)
        
        if overlap {
          overlaps = overlaps.push((span_a, span_b))
        }
      }
    }
    
    overlaps
  }
  
  let overlaps = find_overlaps(merged.spans)
  
  // Based on our test data, there should be some overlaps
  // auth_check (1640995210-1640995260) overlaps with process_payment (1640995220-1640995420)
  assert_true(overlaps.length() > 0)
  
  // Verify service-specific metrics
  let get_metrics_by_service = fn(metrics: Array[ServiceMetric], service: String) {
    metrics.filter(fn(m) { m.service_name == service })
  }
  
  let api_metrics = get_metrics_by_service(merged.metrics, "api-service")
  let payment_metrics = get_metrics_by_service(merged.metrics, "payment-service")
  let notification_metrics = get_metrics_by_service(merged.metrics, "notification-service")
  
  assert_eq(api_metrics.length(), 2)
  assert_eq(payment_metrics.length(), 2)
  assert_eq(notification_metrics.length(), 1)
  
  // Calculate cross-service metrics
  let calculate_cross_service_metrics = fn(merged_telemetry) {
    let service_count = merged_telemetry.services.length().to_float()
    let total_span_count = merged_telemetry.spans.length().to_float()
    let spans_per_service = total_span_count / service_count
    
    let metric_count = merged_telemetry.metrics.length().to_float()
    let metrics_per_service = metric_count / service_count
    
    {
      service_count,
      total_spans: total_span_count,
      spans_per_service,
      total_metrics: metric_count,
      metrics_per_service,
      trace_duration: timeline.trace_duration
    }
  }
  
  let cross_metrics = calculate_cross_service_metrics(merged)
  assert_eq(cross_metrics.service_count, 3.0)
  assert_eq(cross_metrics.total_spans, 5.0)
  assert_eq(cross_metrics.spans_per_service, 5.0 / 3.0)
  assert_eq(cross_metrics.total_metrics, 4.0)
  assert_eq(cross_metrics.metrics_per_service, 4.0 / 3.0)
  assert_eq(cross_metrics.trace_duration, 240)
}

// Test 7: Telemetry Configuration Management
test "telemetry configuration management" {
  // Define configuration types
  type TelemetryConfig = {
    service_name: String,
    service_version: String,
    enabled: Bool,
    sampling_rate: Float,
    export_interval: Int,
    batch_size: Int,
    headers: Array[(String, String)],
    processors: Array[String]
  }
  
  // Create default configuration
  let create_default_config = fn(service_name: String) {
    {
      service_name,
      service_version: "1.0.0",
      enabled: true,
      sampling_rate: 1.0,  // 100% sampling
      export_interval: 60000,  // 60 seconds
      batch_size: 100,
      headers: [],
      processors: ["batch", "attribute"]
    }
  }
  
  // Update configuration with overrides
  let update_config = fn(config: TelemetryConfig, overrides: Array[(String, String)]) {
    let mut updated = config
    
    for (key, value) in overrides {
      match key {
        "service_version" => updated = { updated | service_version: value }
        "enabled" => updated = { updated | enabled: value == "true" }
        "sampling_rate" => updated = { updated | sampling_rate: value.to_float() }
        "export_interval" => updated = { updated | export_interval: value.to_int() }
        "batch_size" => updated = { updated | batch_size: value.to_int() }
        _ => ()  // Ignore unknown keys
      }
    }
    
    updated
  }
  
  // Validate configuration
  let validate_config = fn(config: TelemetryConfig) {
    let mut errors = []
    
    if config.service_name == "" {
      errors = errors.push("Service name cannot be empty")
    }
    
    if config.sampling_rate < 0.0 or config.sampling_rate > 1.0 {
      errors = errors.push("Sampling rate must be between 0.0 and 1.0")
    }
    
    if config.export_interval < 1000 {
      errors = errors.push("Export interval must be at least 1000ms")
    }
    
    if config.batch_size < 1 or config.batch_size > 10000 {
      errors = errors.push("Batch size must be between 1 and 10000")
    }
    
    {
      valid: errors.length() == 0,
      errors
    }
  }
  
  // Test default configuration
  let default_config = create_default_config("payment-service")
  assert_eq(default_config.service_name, "payment-service")
  assert_eq(default_config.service_version, "1.0.0")
  assert_true(default_config.enabled)
  assert_eq(default_config.sampling_rate, 1.0)
  assert_eq(default_config.export_interval, 60000)
  assert_eq(default_config.batch_size, 100)
  
  // Test configuration validation
  let validation_result = validate_config(default_config)
  assert_true(validation_result.valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // Test configuration updates
  let overrides = [
    ("service_version", "2.1.0"),
    ("sampling_rate", "0.5"),
    ("export_interval", "30000"),
    ("batch_size", "50")
  ]
  
  let updated_config = update_config(default_config, overrides)
  assert_eq(updated_config.service_version, "2.1.0")
  assert_eq(updated_config.sampling_rate, 0.5)
  assert_eq(updated_config.export_interval, 30000)
  assert_eq(updated_config.batch_size, 50)
  
  // Test invalid configuration
  let invalid_overrides = [
    ("sampling_rate", "1.5"),  // Invalid: > 1.0
    ("export_interval", "500"), // Invalid: < 1000
    ("batch_size", "0")         // Invalid: < 1
  ]
  
  let invalid_config = update_config(default_config, invalid_overrides)
  let invalid_validation = validate_config(invalid_config)
  assert_false(invalid_validation.valid)
  assert_eq(invalid_validation.errors.length(), 3)
  
  // Test configuration merging
  let merge_configs = fn(base: TelemetryConfig, override: TelemetryConfig) {
    {
      service_name: if override.service_name != "" { override.service_name } else { base.service_name },
      service_version: if override.service_version != "" { override.service_version } else { base.service_version },
      enabled: override.enabled,
      sampling_rate: if override.sampling_rate >= 0.0 { override.sampling_rate } else { base.sampling_rate },
      export_interval: if override.export_interval > 0 { override.export_interval } else { base.export_interval },
      batch_size: if override.batch_size > 0 { override.batch_size } else { base.batch_size },
      headers: base.headers + override.headers,
      processors: if override.processors.length() > 0 { override.processors } else { base.processors }
    }
  }
  
  let override_config = {
    service_name: "",
    service_version: "3.0.0",
    enabled: false,
    sampling_rate: 0.1,
    export_interval: 0,
    batch_size: 200,
    headers: [("x-custom-header", "value")],
    processors: ["filter", "sampling"]
  }
  
  let merged_config = merge_configs(default_config, override_config)
  assert_eq(merged_config.service_name, "payment-service")  // From base
  assert_eq(merged_config.service_version, "3.0.0")         // From override
  assert_false(merged_config.enabled)                       // From override
  assert_eq(merged_config.sampling_rate, 0.1)               // From override
  assert_eq(merged_config.export_interval, 60000)           // From base (override was 0)
  assert_eq(merged_config.batch_size, 200)                  // From override
  assert_eq(merged_config.headers.length(), 1)              // From override
  assert_eq(merged_config.processors.length(), 2)           // From override
  
  // Test configuration environment variable override
  let apply_env_overrides = fn(config: TelemetryConfig, env_vars: Array[(String, String)]) {
    let mut updated = config
    
    for (key, value) in env_vars {
      match key {
        "TELEMETRY_SERVICE_NAME" => updated = { updated | service_name: value }
        "TELEMETRY_SAMPLING_RATE" => {
          let rate = value.to_float()
          if rate >= 0.0 and rate <= 1.0 {
            updated = { updated | sampling_rate: rate }
          }
        }
        "TELEMETRY_ENABLED" => updated = { updated | enabled: value == "true" }
        "TELEMETRY_EXPORT_INTERVAL" => {
          let interval = value.to_int()
          if interval >= 1000 {
            updated = { updated | export_interval: interval }
          }
        }
        _ => ()  // Ignore other environment variables
      }
    }
    
    updated
  }
  
  let env_vars = [
    ("TELEMETRY_SAMPLING_RATE", "0.25"),
    ("TELEMETRY_ENABLED", "false"),
    ("TELEMETRY_EXPORT_INTERVAL", "120000"),
    ("OTHER_VAR", "ignored")
  ]
  
  let env_config = apply_env_overrides(default_config, env_vars)
  assert_eq(env_config.service_name, "payment-service")  // Unchanged
  assert_eq(env_config.sampling_rate, 0.25)              // From env
  assert_false(env_config.enabled)                       // From env
  assert_eq(env_config.export_interval, 120000)          // From env
  
  // Test configuration serialization
  let serialize_config = fn(config: TelemetryConfig) {
    let mut lines = []
    lines = lines.push("service_name=" + config.service_name)
    lines = lines.push("service_version=" + config.service_version)
    lines = lines.push("enabled=" + config.enabled.to_string())
    lines = lines.push("sampling_rate=" + config.sampling_rate.to_string())
    lines = lines.push("export_interval=" + config.export_interval.to_string())
    lines = lines.push("batch_size=" + config.batch_size.to_string())
    
    if config.headers.length() > 0 {
      for (key, value) in config.headers {
        lines = lines.push("header." + key + "=" + value)
      }
    }
    
    if config.processors.length() > 0 {
      let processors_str = config.processors.reduce(fn(acc, p) { acc + "," + p })
      lines = lines.push("processors=" + processors_str)
    }
    
    lines.reduce(fn(acc, line) { acc + "\n" + line })
  }
  
  let config_str = serialize_config(env_config)
  assert_true(config_str.contains("service_name=payment-service"))
  assert_true(config_str.contains("sampling_rate=0.25"))
  assert_true(config_str.contains("enabled=false"))
  assert_true(config_str.contains("export_interval=120000"))
}

// Test 8: Telemetry Data Retention and Cleanup
test "telemetry data retention and cleanup" {
  // Define telemetry data record
  type TelemetryRecord = {
    id: String,
    timestamp: Int,
    data_size: Int,
    access_count: Int,
    last_accessed: Int
  }
  
  // Define retention policy
  type RetentionPolicy = {
    max_age_seconds: Int,
    max_records: Int,
    max_storage_mb: Int,
    cleanup_interval_seconds: Int
  }
  
  // Create retention policy
  let create_retention_policy = fn(max_age: Int, max_records: Int, max_storage: Int) {
    {
      max_age_seconds: max_age,
      max_records,
      max_storage_mb: max_storage,
      cleanup_interval_seconds: 300  // 5 minutes
    }
  }
  
  // Create telemetry records
  let create_record = fn(id: String, timestamp: Int, size: Int) {
    {
      id,
      timestamp,
      data_size: size,
      access_count: 0,
      last_accessed: timestamp
    }
  }
  
  // Create test records with different timestamps
  let current_time = 1640995200  // Current time
  let records = [
    create_record("record-1", current_time - 86400, 1024),      // 1 day old
    create_record("record-2", current_time - 172800, 2048),     // 2 days old
    create_record("record-3", current_time - 259200, 1536),     // 3 days old
    create_record("record-4", current_time - 3600, 512),        // 1 hour old
    create_record("record-5", current_time - 604800, 3072),     // 1 week old
    create_record("record-6", current_time, 256)                // Current
  ]
  
  // Test retention policy
  let policy = create_retention_policy(172800, 5, 10)  // 2 days, 5 records, 10MB
  
  assert_eq(policy.max_age_seconds, 172800)
  assert_eq(policy.max_records, 5)
  assert_eq(policy.max_storage_mb, 10)
  
  // Check if record should be retained based on age
  let should_retain_by_age = fn(record: TelemetryRecord, policy: RetentionPolicy, current_time: Int) {
    let age = current_time - record.timestamp
    age <= policy.max_age_seconds
  }
  
  // Test age-based retention
  let record_1_age = current_time - records[0].timestamp
  let record_2_age = current_time - records[1].timestamp
  let record_3_age = current_time - records[2].timestamp
  let record_4_age = current_time - records[3].timestamp
  let record_5_age = current_time - records[4].timestamp
  let record_6_age = current_time - records[5].timestamp
  
  assert_eq(record_1_age, 86400)   // 1 day
  assert_eq(record_2_age, 172800)  // 2 days
  assert_eq(record_3_age, 259200)  // 3 days
  assert_eq(record_4_age, 3600)    // 1 hour
  assert_eq(record_5_age, 604800)  // 1 week
  assert_eq(record_6_age, 0)       // Current
  
  assert_true(should_retain_by_age(records[0], policy, current_time))   // 1 day old
  assert_true(should_retain_by_age(records[1], policy, current_time))   // 2 days old (boundary)
  assert_false(should_retain_by_age(records[2], policy, current_time))  // 3 days old
  assert_true(should_retain_by_age(records[3], policy, current_time))   // 1 hour old
  assert_false(should_retain_by_age(records[4], policy, current_time))  // 1 week old
  assert_true(should_retain_by_age(records[5], policy, current_time))   // Current
  
  // Filter records by age
  let filter_by_age = fn(records: Array[TelemetryRecord], policy: RetentionPolicy, current_time: Int) {
    records.filter(fn(record) {
      should_retain_by_age(record, policy, current_time)
    })
  }
  
  let age_filtered = filter_by_age(records, policy, current_time)
  assert_eq(age_filtered.length(), 4)  // records 0, 1, 3, 5
  
  // Filter records by count (keep most recent)
  let filter_by_count = fn(records: Array[TelemetryRecord], max_count: Int) {
    let sorted = records.sort_by(fn(a, b) { b.timestamp - a.timestamp })
    sorted.slice(0, max_count)
  }
  
  let count_filtered = filter_by_count(records, policy.max_records)
  assert_eq(count_filtered.length(), 5)
  
  // Verify the 5 most recent records are kept
  assert_eq(count_filtered[0].timestamp, current_time)           // record-6
  assert_eq(count_filtered[1].timestamp, current_time - 3600)    // record-4
  assert_eq(count_filtered[2].timestamp, current_time - 86400)   // record-1
  assert_eq(count_filtered[3].timestamp, current_time - 172800)  // record-2
  assert_eq(count_filtered[4].timestamp, current_time - 259200)  // record-3
  
  // Filter records by storage size
  let filter_by_storage = fn(records: Array[TelemetryRecord], max_storage_mb: Int) {
    let max_storage_bytes = max_storage_mb * 1024 * 1024
    let sorted = records.sort_by(fn(a, b) { b.timestamp - a.timestamp })
    
    let mut kept = []
    let mut current_size = 0
    
    for record in sorted {
      if current_size + record.data_size <= max_storage_bytes {
        kept = kept.push(record)
        current_size = current_size + record.data_size
      }
    }
    
    kept
  }
  
  let storage_filtered = filter_by_storage(records, 0)  // 0MB limit for testing
  assert_eq(storage_filtered.length(), 0)
  
  let storage_filtered_1mb = filter_by_storage(records, 1)  // 1MB limit
  // Calculate how many records fit in 1MB (1,048,576 bytes)
  let mut total_size = 0
  let mut count = 0
  for record in records.sort_by(fn(a, b) { b.timestamp - a.timestamp }) {
    if total_size + record.data_size <= 1048576 {
      total_size = total_size + record.data_size
      count = count + 1
    }
  }
  assert_eq(storage_filtered_1mb.length(), count)
  
  // Apply all retention policies
  let apply_retention_policies = fn(records: Array[TelemetryRecord], policy: RetentionPolicy, current_time: Int) {
    // First filter by age
    let age_filtered = filter_by_age(records, policy, current_time)
    
    // Then filter by count (most recent)
    let count_filtered = filter_by_count(age_filtered, policy.max_records)
    
    // Finally filter by storage
    let storage_filtered = filter_by_storage(count_filtered, policy.max_storage_mb)
    
    storage_filtered
  }
  
  let retained_records = apply_retention_policies(records, policy, current_time)
  
  // Verify retention results
  assert_true(retained_records.length() <= policy.max_records)
  
  // Calculate total storage used
  let total_storage = retained_records.reduce(fn(acc, record) { acc + record.data_size }, 0)
  let total_storage_mb = total_storage / (1024 * 1024)
  assert_true(total_storage_mb <= policy.max_storage_mb)
  
  // Verify all retained records are within age limit
  for record in retained_records {
    let age = current_time - record.timestamp
    assert_true(age <= policy.max_age_seconds)
  }
  
  // Test record access tracking
  let update_access = fn(record: TelemetryRecord, current_time: Int) {
    {
      id: record.id,
      timestamp: record.timestamp,
      data_size: record.data_size,
      access_count: record.access_count + 1,
      last_accessed: current_time
    }
  }
  
  let accessed_record = update_access(retained_records[0], current_time + 3600)
  assert_eq(accessed_record.access_count, 1)
  assert_eq(accessed_record.last_accessed, current_time + 3600)
  
  // Test cleanup based on access patterns (LRU)
  let filter_by_lru = fn(records: Array[TelemetryRecord], keep_count: Int) {
    let sorted_by_access = records.sort_by(fn(a, b) { b.last_accessed - a.last_accessed })
    sorted_by_access.slice(0, keep_count)
  }
  
  let lru_filtered = filter_by_lru(retained_records, 2)
  assert_eq(lru_filtered.length(), 2)
  
  // Create records with different access patterns
  let accessed_records = [
    update_access(records[0], current_time - 3600),  // Accessed 1 hour ago
    update_access(records[1], current_time - 7200),  // Accessed 2 hours ago
    update_access(records[2], current_time - 1800),  // Accessed 30 minutes ago
  ]
  
  let lru_result = filter_by_lru(accessed_records, 2)
  // Should keep the most recently accessed
  assert_eq(lru_result[0].id, records[2].id)  // Accessed 30 minutes ago
  assert_eq(lru_result[1].id, records[0].id)  // Accessed 1 hour ago
  
  // Test retention statistics
  let calculate_retention_stats = fn(original: Array[TelemetryRecord], retained: Array[TelemetryRecord]) {
    let original_count = original.length()
    let retained_count = retained.length()
    let removed_count = original_count - retained_count
    let retention_rate = retained_count.to_float() / original_count.to_float()
    
    let original_size = original.reduce(fn(acc, r) { acc + r.data_size }, 0)
    let retained_size = retained.reduce(fn(acc, r) { acc + r.data_size }, 0)
    let removed_size = original_size - retained_size
    
    {
      original_count,
      retained_count,
      removed_count,
      retention_rate,
      original_size_kb: original_size / 1024,
      retained_size_kb: retained_size / 1024,
      removed_size_kb: removed_size / 1024
    }
  }
  
  let stats = calculate_retention_stats(records, retained_records)
  assert_eq(stats.original_count, 6)
  assert_eq(stats.removed_count, stats.original_count - stats.retained_count)
  assert_true(stats.retention_rate <= 1.0)
  assert_eq(stats.retained_size_kb + stats.removed_size_kb, stats.original_size_kb)
}
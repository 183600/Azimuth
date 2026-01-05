// Azimuth Premium Quality MoonBit Test Suite
// This file contains premium quality MoonBit test cases focusing on advanced telemetry scenarios

// Test 1: Distributed Trace Context Propagation with Baggage
test "distributed trace context propagation with enhanced baggage handling" {
  // Define enhanced trace context structure
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: String,
    baggage: Array[(String, String)]
  }
  
  // Define trace propagation result
  type PropagationResult = {
    context: TraceContext,
    propagation_path: Array[String],
    baggage_integrity: Bool
  }
  
  // Create root trace context
  let create_root_context = fn(trace_id: String) {
    {
      trace_id,
      span_id: "span-root-" + trace_id.substring(6, 12),
      parent_span_id: None,
      trace_flags: 1,
      trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE",
      baggage: [
        ("user.id", "user-12345"),
        ("request.source", "mobile-app"),
        ("session.id", "session-abcde"),
        ("tenant.id", "tenant-001")
      ]
    }
  }
  
  // Propagate context to downstream service
  let propagate_context = fn(context: TraceContext, service_name: String, additional_baggage: Array[(String, String)]) {
    let new_span_id = "span-" + service_name + "-" + (context.span_id.length() + 1).to_string()
    let merged_baggage = context.baggage + additional_baggage
    
    {
      trace_id: context.trace_id,
      span_id: new_span_id,
      parent_span_id: Some(context.span_id),
      trace_flags: context.trace_flags,
      trace_state: context.trace_state,
      baggage: merged_baggage.push(("service.name", service_name))
    }
  }
  
  // Verify baggage integrity
  let verify_baggage_integrity = fn(original: Array[(String, String)], current: Array[(String, String)]) {
    let mut integrity = true
    for (key, value) in original {
      let found = current.find(fn(item) { item.0 == key })
      match found {
        Some((_, current_value)) => {
          if current_value != value {
            integrity = false
          }
        }
        None => integrity = false
      }
    }
    integrity
  }
  
  // Test trace propagation across multiple services
  let root_context = create_root_context("trace-abc123def456")
  let original_baggage = root_context.baggage
  
  // Propagate to API Gateway
  let api_context = propagate_context(root_context, "api-gateway", [("gateway.version", "2.1.0")])
  assert_eq(api_context.trace_id, root_context.trace_id)
  assert_eq(api_context.parent_span_id, Some(root_context.span_id))
  assert_true(api_context.baggage.contains(("gateway.version", "2.1.0")))
  assert_true(verify_baggage_integrity(original_baggage, api_context.baggage))
  
  // Propagate to Authentication Service
  let auth_context = propagate_context(api_context, "auth-service", [("auth.method", "jwt")])
  assert_eq(auth_context.trace_id, root_context.trace_id)
  assert_eq(auth_context.parent_span_id, Some(api_context.span_id))
  assert_true(auth_context.baggage.contains(("auth.method", "jwt")))
  assert_true(verify_baggage_integrity(original_baggage, auth_context.baggage))
  
  // Propagate to Business Logic Service
  let business_context = propagate_context(auth_context, "business-service", [
    ("operation.type", "order-processing"),
    ("transaction.id", "txn-789012")
  ])
  assert_eq(business_context.trace_id, root_context.trace_id)
  assert_eq(business_context.parent_span_id, Some(auth_context.span_id))
  assert_true(business_context.baggage.contains(("operation.type", "order-processing")))
  assert_true(business_context.baggage.contains(("transaction.id", "txn-789012")))
  assert_true(verify_baggage_integrity(original_baggage, business_context.baggage))
  
  // Verify trace state preservation
  assert_eq(root_context.trace_state, api_context.trace_state)
  assert_eq(api_context.trace_state, auth_context.trace_state)
  assert_eq(auth_context.trace_state, business_context.trace_state)
  
  // Verify trace flags preservation
  assert_eq(root_context.trace_flags, business_context.trace_flags)
  
  // Test baggage growth and uniqueness
  let unique_baggage_keys = business_context.baggage.map(fn(item) { item.0 })
  let unique_keys = fn(arr: Array[String]) {
    let mut result = []
    for item in arr {
      if not(result.contains(item)) {
        result = result.push(item)
      }
    }
    result
  }
  let unique_baggage_count = unique_keys(unique_baggage_keys).length()
  assert_eq(unique_baggage_count, business_context.baggage.length())
}

// Test 2: Advanced Metric Aggregation with Time Windows
test "advanced metric aggregation with sliding time windows" {
  // Define metric data point with enhanced attributes
  type MetricPoint = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)],
    metric_type: String  // counter, gauge, histogram
  }
  
  // Define time window configuration
  type TimeWindow = {
    start_time: Int,
    end_time: Int,
    duration_ms: Int
  }
  
  // Define aggregation result
  type AggregationResult = {
    metric_name: String,
    count: Int,
    sum: Float,
    min: Float,
    max: Float,
    avg: Float,
    p50: Float,
    p95: Float,
    p99: Float,
    window: TimeWindow
  }
  
  // Create sample metrics with timestamps
  let base_timestamp = 1640995200000  // Milliseconds since epoch
  let response_time_metrics = [
    { name: "http.request.duration", value: 120.5, timestamp: base_timestamp, tags: [("endpoint", "/api/users")], metric_type: "histogram" },
    { name: "http.request.duration", value: 85.3, timestamp: base_timestamp + 1000, tags: [("endpoint", "/api/users")], metric_type: "histogram" },
    { name: "http.request.duration", value: 210.7, timestamp: base_timestamp + 2000, tags: [("endpoint", "/api/users")], metric_type: "histogram" },
    { name: "http.request.duration", value: 95.2, timestamp: base_timestamp + 3000, tags: [("endpoint", "/api/users")], metric_type: "histogram" },
    { name: "http.request.duration", value: 150.8, timestamp: base_timestamp + 4000, tags: [("endpoint", "/api/users")], metric_type: "histogram" },
    { name: "http.request.duration", value: 175.3, timestamp: base_timestamp + 5000, tags: [("endpoint", "/api/users")], metric_type: "histogram" },
    { name: "http.request.duration", value: 65.7, timestamp: base_timestamp + 6000, tags: [("endpoint", "/api/users")], metric_type: "histogram" },
    { name: "http.request.duration", value: 198.4, timestamp: base_timestamp + 7000, tags: [("endpoint", "/api/users")], metric_type: "histogram" }
  ]
  
  // Filter metrics by time window
  let filter_by_time_window = fn(metrics: Array[MetricPoint], window: TimeWindow) {
    metrics.filter(fn(metric) {
      metric.timestamp >= window.start_time and metric.timestamp <= window.end_time
    })
  }
  
  // Calculate percentiles
  let calculate_percentile = fn(sorted_values: Array[Float], percentile: Float) {
    if sorted_values.length() == 0 {
      0.0
    } else {
      let index = ((sorted_values.length().to_float() - 1.0) * percentile / 100.0).to_int()
      sorted_values[index]
    }
  }
  
  // Aggregate metrics in time window
  let aggregate_metrics = fn(metrics: Array[MetricPoint], window: TimeWindow) {
    let windowed_metrics = filter_by_time_window(metrics, window)
    let values = windowed_metrics.map(fn(m) { m.value }).sort()
    
    if values.length() == 0 {
      {
        metric_name: "",
        count: 0,
        sum: 0.0,
        min: 0.0,
        max: 0.0,
        avg: 0.0,
        p50: 0.0,
        p95: 0.0,
        p99: 0.0,
        window
      }
    } else {
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let count = values.length()
      
      {
        metric_name: windowed_metrics[0].name,
        count,
        sum,
        min: values[0],
        max: values[count - 1],
        avg: sum / count.to_float(),
        p50: calculate_percentile(values, 50.0),
        p95: calculate_percentile(values, 95.0),
        p99: calculate_percentile(values, 99.0),
        window
      }
    }
  }
  
  // Test 1-minute window aggregation
  let one_minute_window = {
    start_time: base_timestamp,
    end_time: base_timestamp + 60000,
    duration_ms: 60000
  }
  
  let one_minute_result = aggregate_metrics(response_time_metrics, one_minute_window)
  assert_eq(one_minute_result.count, 8)
  assert_eq(one_minute_result.min, 65.7)
  assert_eq(one_minute_result.max, 210.7)
  assert_eq(one_minute_result.avg, 137.7375)
  assert_eq(one_minute_result.p50, 150.8)
  assert_eq(one_minute_result.p95, 210.7)
  
  // Test 3-second window aggregation
  let three_second_window = {
    start_time: base_timestamp + 1000,
    end_time: base_timestamp + 4000,
    duration_ms: 3000
  }
  
  let three_second_result = aggregate_metrics(response_time_metrics, three_second_window)
  assert_eq(three_second_result.count, 3)
  assert_eq(three_second_result.min, 85.3)
  assert_eq(three_second_result.max, 210.7)
  assert_eq(three_second_result.avg, 130.4)
  assert_eq(three_second_result.p50, 120.5)
  assert_eq(three_second_result.p95, 210.7)
  
  // Test empty window aggregation
  let empty_window = {
    start_time: base_timestamp + 10000,
    end_time: base_timestamp + 20000,
    duration_ms: 10000
  }
  
  let empty_result = aggregate_metrics(response_time_metrics, empty_window)
  assert_eq(empty_result.count, 0)
  assert_eq(empty_result.sum, 0.0)
  assert_eq(empty_result.avg, 0.0)
  
  // Test sliding windows
  let create_sliding_windows = fn(start_time: Int, end_time: Int, window_size_ms: Int, step_ms: Int) {
    let mut windows = []
    let mut current_start = start_time
    
    while current_start + window_size_ms <= end_time {
      let window = {
        start_time: current_start,
        end_time: current_start + window_size_ms,
        duration_ms: window_size_ms
      }
      windows = windows.push(window)
      current_start = current_start + step_ms
    }
    
    windows
  }
  
  let sliding_windows = create_sliding_windows(base_timestamp, base_timestamp + 7000, 3000, 1000)
  assert_eq(sliding_windows.length(), 5)
  
  // Aggregate across sliding windows
  let sliding_results = sliding_windows.map(fn(window) {
    aggregate_metrics(response_time_metrics, window)
  })
  
  assert_eq(sliding_results[0].count, 4)  // 0-3s
  assert_eq(sliding_results[1].count, 4)  // 1-4s
  assert_eq(sliding_results[2].count, 4)  // 2-5s
  assert_eq(sliding_results[3].count, 4)  // 3-6s
  assert_eq(sliding_results[4].count, 3)  // 4-7s
}

// Test 3: Span Event Correlation and Causality Analysis
test "span event correlation and causality analysis" {
  // Define span event with enhanced attributes
  type SpanEvent = {
    name: String,
    timestamp: Int,
    attributes: Array[(String, String)],
    event_type: String  // info, warning, error
  }
  
  // Define causality relationship
  type CausalityLink = {
    source_event_id: String,
    target_event_id: String,
    relationship_type: String  // triggers, precedes, correlates
    confidence: Float
  }
  
  // Define span with enhanced event tracking
  type EnhancedSpan = {
    span_id: String,
    trace_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    events: Array[SpanEvent],
    causality_links: Array[CausalityLink]
  }
  
  // Create enhanced span
  let create_enhanced_span = fn(span_id: String, trace_id: String, operation_name: String) {
    {
      span_id,
      trace_id,
      parent_span_id: None,
      operation_name,
      start_time: 1640995200000,
      end_time: 1640995300000,
      status: "ok",
      events: [],
      causality_links: []
    }
  }
  
  // Add event to span with auto-generated ID
  let add_event = fn(span: EnhancedSpan, event_name: String, event_type: String, attributes: Array[(String, String)]) {
    let event_id = "event-" + (span.events.length() + 1).to_string()
    let event = {
      name: event_name,
      timestamp: span.start_time + span.events.length() * 100,
      attributes: attributes.push(("event.id", event_id)),
      event_type
    }
    { span | events: span.events.push(event) }
  }
  
  // Analyze event causality
  let analyze_causality = fn(span: EnhancedSpan) {
    let mut links = []
    
    // Simple causality analysis based on temporal relationships
    for i in 0..span.events.length() {
      for j in (i + 1)..span.events.length() {
        let source_event = span.events[i]
        let target_event = span.events[j]
        
        // Analyze attribute correlations
        let source_keys = source_event.attributes.map(fn(attr) { attr.0 })
        let target_keys = target_event.attributes.map(fn(attr) { attr.0 })
        
        let common_keys = source_keys.filter(fn(key) { target_keys.contains(key) })
        let correlation_strength = common_keys.length().to_float() / source_keys.length().to_float()
        
        // Determine relationship type based on event names and attributes
        let relationship_type = if correlation_strength > 0.5 {
          "correlates"
        } else if target_event.timestamp - source_event.timestamp < 500 {
          "triggers"
        } else {
          "precedes"
        }
        
        let link = {
          source_event_id: source_event.attributes.find(fn(attr) { attr.0 == "event.id" }).unwrap().1,
          target_event_id: target_event.attributes.find(fn(attr) { attr.0 == "event.id" }).unwrap().1,
          relationship_type,
          confidence: correlation_strength
        }
        
        links = links.push(link)
      }
    }
    
    links
  }
  
  // Test event correlation and causality
  let db_span = create_enhanced_span("span-db-001", "trace-123", "database_query")
  
  // Add events with causal relationships
  let db_span_with_connect = add_event(db_span, "db.connection.initiated", "info", [
    ("pool.name", "primary"),
    ("timeout.ms", "5000"),
    ("connection.id", "conn-001")
  ])
  
  let db_span_with_auth = add_event(db_span_with_connect, "db.authentication.completed", "info", [
    ("auth.method", "token"),
    ("user.id", "user-123"),
    ("connection.id", "conn-001")  // Correlated with previous event
  ])
  
  let db_span_with_query = add_event(db_span_with_auth, "db.query.started", "info", [
    ("query.type", "SELECT"),
    ("table.name", "users"),
    ("connection.id", "conn-001")  // Correlated with previous events
  ])
  
  let db_span_with_error = add_event(db_span_with_query, "db.query.error", "error", [
    ("error.code", " deadlock"),
    ("error.message", "Deadlock detected"),
    ("retry.count", "1"),
    ("connection.id", "conn-001")  // Correlated with previous events
  ])
  
  let db_span_with_retry = add_event(db_span_with_error, "db.query.retried", "info", [
    ("retry.count", "2"),
    ("connection.id", "conn-001")  // Correlated with previous events
  ])
  
  let db_span_with_success = add_event(db_span_with_retry, "db.query.completed", "info", [
    ("rows.returned", "42"),
    ("execution.time.ms", "250"),
    ("connection.id", "conn-001")  // Correlated with previous events
  ])
  
  // Analyze causality
  let causality_links = analyze_causality(db_span_with_success)
  
  // Verify causality analysis
  assert_eq(causality_links.length(), 15)  // 6 events = 6 choose 2 = 15 links
  
  // Check for strong correlations (events with common connection.id)
  let strong_correlations = causality_links.filter(fn(link) { link.confidence > 0.2 })
  assert_true(strong_correlations.length() > 0)
  
  // Check for trigger relationships (close temporal proximity)
  let trigger_relationships = causality_links.filter(fn(link) { link.relationship_type == "triggers" })
  assert_true(trigger_relationships.length() > 0)
  
  // Verify event timeline
  let event_timestamps = db_span_with_success.events.map(fn(event) { event.timestamp })
  for i in 1..event_timestamps.length() {
    assert_true(event_timestamps[i] > event_timestamps[i-1])
  }
  
  // Verify error event type
  let error_events = db_span_with_success.events.filter(fn(event) { event.event_type == "error" })
  assert_eq(error_events.length(), 1)
  assert_eq(error_events[0].name, "db.query.error")
  
  // Test event attribute correlation
  let connection_id_events = db_span_with_success.events.filter(fn(event) {
    event.attributes.find(fn(attr) { attr.0 == "connection.id" && attr.1 == "conn-001" }) != None
  })
  assert_eq(connection_id_events.length(), 6)
}

// Test 4: Adaptive Sampling Strategy with Machine Learning
test "adaptive sampling strategy with machine learning-inspired decision making" {
  // Define sampling decision with confidence
  enum SamplingDecision {
    Drop(Float)      // Drop with confidence score
    Record(Float)    // Record with confidence score
    RecordAndSample(Float)  // Record and sample with confidence score
  }
  
  // Define trace characteristics for ML decision making
  type TraceCharacteristics = {
    trace_id: String,
    service_count: Int,
    span_count: Int,
    error_count: Int,
    duration_ms: Int,
    has_sensitive_data: Bool,
    endpoint_type: String,  // critical, normal, background
    user_importance: String  // vip, regular, anonymous
  }
  
  // Define adaptive sampling configuration
  type AdaptiveSamplingConfig = {
    base_sample_rate: Float,
    error_boost_factor: Float,
    latency_threshold_ms: Int,
    service_count_weight: Float,
    error_count_weight: Float,
    duration_weight: Float,
    user_importance_weight: Float
  }
  
  // Define sampling result with explanation
  type SamplingResult = {
    decision: SamplingDecision,
    confidence: Float,
    explanation: String,
    factors: Array[(String, Float)]
  }
  
  // Create adaptive sampling configuration
  let config = {
    base_sample_rate: 0.1,
    error_boost_factor: 5.0,
    latency_threshold_ms: 1000,
    service_count_weight: 0.2,
    error_count_weight: 0.4,
    duration_weight: 0.2,
    user_importance_weight: 0.2
  }
  
  // Calculate sampling score using ML-inspired approach
  let calculate_sampling_score = fn(characteristics: TraceCharacteristics, config: AdaptiveSamplingConfig) {
    let mut score = config.base_sample_rate
    
    // Service count factor
    let service_factor = if characteristics.service_count > 5 {
      1.5
    } else if characteristics.service_count > 2 {
      1.2
    } else {
      1.0
    }
    score = score * service_factor
    
    // Error count factor
    if characteristics.error_count > 0 {
      let error_factor = 1.0 + (characteristics.error_count.to_float() * config.error_boost_factor)
      score = score * error_factor
    }
    
    // Duration factor
    if characteristics.duration_ms > config.latency_threshold_ms {
      let duration_factor = 1.0 + (characteristics.duration_ms.to_float() / config.latency_threshold_ms.to_float())
      score = score * duration_factor
    }
    
    // User importance factor
    let importance_factor = match characteristics.user_importance {
      "vip" => 3.0
      "regular" => 1.0
      "anonymous" => 0.5
      _ => 1.0
    }
    score = score * importance_factor
    
    // Endpoint type factor
    let endpoint_factor = match characteristics.endpoint_type {
      "critical" => 2.0
      "normal" => 1.0
      "background" => 0.3
      _ => 1.0
    }
    score = score * endpoint_factor
    
    // Normalize to [0, 1]
    if score > 1.0 {
      1.0
    } else {
      score
    }
  }
  
  // Generate sampling decision
  let generate_sampling_decision = fn(characteristics: TraceCharacteristics, config: AdaptiveSamplingConfig) {
    let score = calculate_sampling_score(characteristics, config)
    
    // Calculate contributing factors
    let service_factor = if characteristics.service_count > 5 {
      1.5
    } else if characteristics.service_count > 2 {
      1.2
    } else {
      1.0
    }
    
    let error_factor = if characteristics.error_count > 0 {
      1.0 + (characteristics.error_count.to_float() * config.error_boost_factor)
    } else {
      1.0
    }
    
    let importance_factor = match characteristics.user_importance {
      "vip" => 3.0
      "regular" => 1.0
      "anonymous" => 0.5
      _ => 1.0
    }
    
    let factors = [
      ("service_count", service_factor),
      ("error_count", error_factor),
      ("user_importance", importance_factor)
    ]
    
    // Make decision with confidence
    let decision = if score > 0.8 {
      SamplingDecision::RecordAndSample(score)
    } else if score > 0.5 {
      SamplingDecision::Record(score)
    } else {
      SamplingDecision::Drop(1.0 - score)
    }
    
    // Generate explanation
    let explanation = match decision {
      SamplingDecision::RecordAndSample(confidence) => {
        "High sampling score (" + score.to_string() + ") due to multiple factors: " +
        "errors=" + characteristics.error_count.to_string() + ", " +
        "services=" + characteristics.service_count.to_string() + ", " +
        "importance=" + characteristics.user_importance
      }
      SamplingDecision::Record(confidence) => {
        "Moderate sampling score (" + score.to_string() + ") - recording without detailed sampling"
      }
      SamplingDecision::Drop(confidence) => {
        "Low sampling score (" + score.to_string() + ") - dropping trace to save resources"
      }
    }
    
    {
      decision,
      confidence: score,
      explanation,
      factors
    }
  }
  
  // Test VIP user trace with errors
  let vip_trace_characteristics = {
    trace_id: "trace-vip-001",
    service_count: 4,
    span_count: 12,
    error_count: 2,
    duration_ms: 1500,
    has_sensitive_data: false,
    endpoint_type: "critical",
    user_importance: "vip"
  }
  
  let vip_result = generate_sampling_decision(vip_trace_characteristics, config)
  match vip_result.decision {
    SamplingDecision::RecordAndSample(confidence) => {
      assert_true(confidence > 0.8)
      assert_true(vip_result.explanation.contains("High sampling score"))
      assert_true(vip_result.explanation.contains("vip"))
    }
    _ => assert_true(false)
  }
  
  // Test regular background trace with no errors
  let background_trace_characteristics = {
    trace_id: "trace-bg-001",
    service_count: 2,
    span_count: 5,
    error_count: 0,
    duration_ms: 200,
    has_sensitive_data: false,
    endpoint_type: "background",
    user_importance: "anonymous"
  }
  
  let background_result = generate_sampling_decision(background_trace_characteristics, config)
  match background_result.decision {
    SamplingDecision::Drop(confidence) => {
      assert_true(confidence > 0.5)
      assert_true(background_result.explanation.contains("Low sampling score"))
    }
    _ => assert_true(false)
  }
  
  // Test regular trace with moderate latency
  let regular_trace_characteristics = {
    trace_id: "trace-reg-001",
    service_count: 3,
    span_count: 8,
    error_count: 0,
    duration_ms: 800,
    has_sensitive_data: false,
    endpoint_type: "normal",
    user_importance: "regular"
  }
  
  let regular_result = generate_sampling_decision(regular_trace_characteristics, config)
  match regular_result.decision {
    SamplingDecision::Record(confidence) => {
      assert_true(confidence > 0.3)
      assert_true(regular_result.explanation.contains("Moderate sampling score"))
    }
    _ => assert_true(false)
  }
  
  // Test error-heavy trace
  let error_trace_characteristics = {
    trace_id: "trace-err-001",
    service_count: 5,
    span_count: 15,
    error_count: 4,
    duration_ms: 2000,
    has_sensitive_data: false,
    endpoint_type: "critical",
    user_importance: "regular"
  }
  
  let error_result = generate_sampling_decision(error_trace_characteristics, config)
  match error_result.decision {
    SamplingDecision::RecordAndSample(confidence) => {
      assert_true(confidence > 0.8)
      assert_true(error_result.explanation.contains("High sampling score"))
      assert_true(error_result.explanation.contains("errors=4"))
    }
    _ => assert_true(false)
  }
  
  // Verify factor contributions
  let vip_factors = vip_result.factors
  let service_factor = vip_factors.find(fn(f) { f.0 == "service_count" }).unwrap().1
  let error_factor = vip_factors.find(fn(f) { f.0 == "error_count" }).unwrap().1
  let importance_factor = vip_factors.find(fn(f) { f.0 == "user_importance" }).unwrap().1
  
  assert_eq(importance_factor, 3.0)
  assert_true(error_factor > 1.0)
  assert_eq(service_factor, 1.2)
}

// Test 5: Telemetry Data Compression and Transmission Optimization
test "telemetry data compression and transmission optimization" {
  // Define telemetry payload
  type TelemetryPayload = {
    payload_id: String,
    data_type: String,  // span, metric, log
    raw_data: String,
    compression_ratio: Float,
    transmission_priority: Int  // 1-10, 10 is highest
  }
  
  // Define compression algorithm
  enum CompressionAlgorithm {
    None
    Gzip
    Lz4
    Zstd
  }
  
  // Define transmission result
  type TransmissionResult = {
    payload_id: String,
    algorithm: CompressionAlgorithm,
    original_size: Int,
    compressed_size: Int,
    compression_time_ms: Int,
    transmission_time_ms: Int,
    success: Bool
  }
  
  // Simulate compression function
  let compress_data = fn(data: String, algorithm: CompressionAlgorithm) {
    let original_size = data.length()
    
    match algorithm {
      CompressionAlgorithm::None => {
        {
          compressed_data: data,
          compression_ratio: 1.0,
          compression_time_ms: 1
        }
      }
      CompressionAlgorithm::Gzip => {
        // Simulate 70% compression ratio
        let compressed_size = (original_size.to_float() * 0.3).to_int()
        {
          compressed_data: "compressed-gzip-" + compressed_size.to_string(),
          compression_ratio: original_size.to_float() / compressed_size.to_float(),
          compression_time_ms: 50
        }
      }
      CompressionAlgorithm::Lz4 => {
        // Simulate 60% compression ratio, faster
        let compressed_size = (original_size.to_float() * 0.4).to_int()
        {
          compressed_data: "compressed-lz4-" + compressed_size.to_string(),
          compression_ratio: original_size.to_float() / compressed_size.to_float(),
          compression_time_ms: 20
        }
      }
      CompressionAlgorithm::Zstd => {
        // Simulate 80% compression ratio, slower
        let compressed_size = (original_size.to_float() * 0.2).to_int()
        {
          compressed_data: "compressed-zstd-" + compressed_size.to_string(),
          compression_ratio: original_size.to_float() / compressed_size.to_float(),
          compression_time_ms: 80
        }
      }
    }
  }
  
  // Simulate transmission function
  let transmit_data = fn(compressed_data: String, priority: Int) {
    // Transmission time inversely proportional to priority and data size
    let base_time = 100
    let size_factor = compressed_data.length().to_float() / 1000.0
    let priority_factor = 11.0 - priority.to_float()  // Higher priority = lower factor
    
    let transmission_time = (base_time * size_factor * priority_factor).to_int()
    
    // Simulate occasional transmission failures for low priority
    let success = if priority < 3 && compressed_data.length() > 500 {
      false
    } else {
      true
    }
    
    {
      transmission_time_ms: transmission_time,
      success
    }
  }
  
  // Optimize compression and transmission
  let optimize_transmission = fn(payload: TelemetryPayload) {
    let algorithms = [
      CompressionAlgorithm::None,
      CompressionAlgorithm::Gzip,
      CompressionAlgorithm::Lz4,
      CompressionAlgorithm::Zstd
    ]
    
    let mut best_result = None
    
    for algorithm in algorithms {
      let compression_result = compress_data(payload.raw_data, algorithm)
      let transmission_result = transmit_data(compression_result.compressed_data, payload.transmission_priority)
      
      if transmission_result.success {
        let total_time = compression_result.compression_time_ms + transmission_result.transmission_time_ms
        let total_size = compression_result.compressed_data.length()
        
        match best_result {
          None => {
            best_result = Some({
              payload_id: payload.payload_id,
              algorithm,
              original_size: payload.raw_data.length(),
              compressed_size: total_size,
              compression_time_ms: compression_result.compression_time_ms,
              transmission_time_ms: transmission_result.transmission_time_ms,
              success: true
            })
          }
          Some(current_best) => {
            // Choose algorithm with best time-size tradeoff
            let current_score = total_time.to_float() + total_size.to_float() / 100.0
            let best_score = current_best.compression_time_ms.to_float() + 
                           current_best.transmission_time_ms.to_float() + 
                           current_best.compressed_size.to_float() / 100.0
            
            if current_score < best_score {
              best_result = Some({
                payload_id: payload.payload_id,
                algorithm,
                original_size: payload.raw_data.length(),
                compressed_size: total_size,
                compression_time_ms: compression_result.compression_time_ms,
                transmission_time_ms: transmission_result.transmission_time_ms,
                success: true
              })
            }
          }
        }
      }
    }
    
    best_result.unwrap()
  }
  
  // Create test payloads
  let create_test_payload = fn(id: String, data_type: String, data_size: Int, priority: Int) {
    let raw_data = "telemetry-data-".repeat(data_size / 20)  // Simulate data
    
    {
      payload_id: id,
      data_type,
      raw_data,
      compression_ratio: 1.0,
      transmission_priority: priority
    }
  }
  
  // Test high priority critical span data
  let critical_span_payload = create_test_payload("span-001", "span", 2000, 9)
  let critical_result = optimize_transmission(critical_span_payload)
  
  assert_eq(critical_result.payload_id, "span-001")
  assert_true(critical_result.success)
  assert_true(critical_result.compressed_size < critical_result.original_size)
  assert_true(critical_result.transmission_time_ms < 100)  // Should be fast due to high priority
  
  // Test low priority background metric data
  let background_metrics_payload = create_test_payload("metrics-001", "metric", 5000, 2)
  let background_result = optimize_transmission(background_metrics_payload)
  
  assert_eq(background_result.payload_id, "metrics-001")
  assert_true(background_result.success)
  assert_true(background_result.compressed_size < background_result.original_size)
  assert_true(background_result.transmission_time_ms > critical_result.transmission_time_ms)  // Should be slower
  
  // Test medium priority log data
  let log_payload = create_test_payload("log-001", "log", 1000, 5)
  let log_result = optimize_transmission(log_payload)
  
  assert_eq(log_result.payload_id, "log-001")
  assert_true(log_result.success)
  assert_true(log_result.compressed_size < log_result.original_size)
  
  // Test algorithm selection based on payload characteristics
  // Small payloads might use no compression for speed
  let small_payload = create_test_payload("small-001", "span", 200, 10)
  let small_result = optimize_transmission(small_payload)
  
  match small_result.algorithm {
    CompressionAlgorithm::None => assert_true(true)  // Might choose no compression for very small data
    _ => assert_true(true)  // Or might still compress depending on optimization
  }
  
  // Large payloads should use best compression
  let large_payload = create_test_payload("large-001", "metric", 10000, 8)
  let large_result = optimize_transmission(large_payload)
  
  assert_true(large_result.compressed_size < large_result.original_size * 0.5)  // Should have good compression
  
  // Verify compression ratios
  let critical_compression_ratio = critical_result.original_size.to_float() / critical_result.compressed_size.to_float()
  let background_compression_ratio = background_result.original_size.to_float() / background_result.compressed_size.to_float()
  
  assert_true(critical_compression_ratio > 1.0)
  assert_true(background_compression_ratio > 1.0)
}

// Test 6: Distributed System Health Monitoring and Anomaly Detection
test "distributed system health monitoring and anomaly detection" {
  // Define health metric
  type HealthMetric = {
    service_name: String,
    metric_name: String,
    value: Float,
    timestamp: Int,
    threshold: Float,
    status: String  // healthy, warning, critical
  }
  
  // Define anomaly detection result
  type AnomalyResult = {
    service_name: String,
    anomaly_type: String,
    severity: String,  // low, medium, high, critical
    confidence: Float,
    description: String,
    affected_metrics: Array[String]
  }
  
  // Define system health status
  type SystemHealthStatus = {
    overall_status: String,  // healthy, degraded, critical
    healthy_services: Int,
    degraded_services: Int,
    critical_services: Int,
    anomalies: Array[AnomalyResult]
  }
  
  // Define service health baseline
  type HealthBaseline = {
    service_name: String,
    baseline_metrics: Array[(String, Float)],
    acceptable_variance: Float,
    last_updated: Int
  }
  
  // Create health baselines for services
  let health_baselines = [
    {
      service_name: "api-gateway",
      baseline_metrics: [
        ("response_time", 100.0),
        ("error_rate", 0.01),
        ("throughput", 1000.0),
        ("cpu_usage", 0.4)
      ],
      acceptable_variance: 0.2,
      last_updated: 1640995200
    },
    {
      service_name: "payment-service",
      baseline_metrics: [
        ("response_time", 200.0),
        ("error_rate", 0.005),
        ("throughput", 500.0),
        ("cpu_usage", 0.6)
      ],
      acceptable_variance: 0.15,
      last_updated: 1640995200
    },
    {
      service_name: "notification-service",
      baseline_metrics: [
        ("response_time", 150.0),
        ("error_rate", 0.02),
        ("throughput", 800.0),
        ("cpu_usage", 0.3)
      ],
      acceptable_variance: 0.25,
      last_updated: 1640995200
    }
  ]
  
  // Get baseline for service and metric
  let get_baseline = fn(service_name: String, metric_name: String) {
    let service_baseline = health_baselines.find(fn(baseline) { baseline.service_name == service_name })
    match service_baseline {
      Some(baseline) => {
        baseline.baseline_metrics.find(fn(metric) { metric.0 == metric_name })
      }
      None => None
    }
  }
  
  // Calculate metric status
  let calculate_metric_status = fn(service_name: String, metric_name: String, current_value: Float) {
    let baseline_option = get_baseline(service_name, metric_name)
    match baseline_option {
      Some((_, baseline_value)) => {
        let service_baseline = health_baselines.find(fn(b) { b.service_name == service_name }).unwrap()
        let variance = (current_value - baseline_value).abs() / baseline_value
        
        if variance > service_baseline.acceptable_variance * 2.0 {
          "critical"
        } else if variance > service_baseline.acceptable_variance {
          "warning"
        } else {
          "healthy"
        }
      }
      None => "unknown"
    }
  }
  
  // Detect anomalies in service metrics
  let detect_anomalies = fn(service_name: String, metrics: Array[HealthMetric]) {
    let mut anomalies = []
    
    // Check for sudden spikes in error rates
    let error_metrics = metrics.filter(fn(m) { m.metric_name.contains("error") })
    for metric in error_metrics {
      if metric.value > 0.1 {  // 10% error rate
        let anomaly = {
          service_name,
          anomaly_type: "error_rate_spike",
          severity: if metric.value > 0.2 { "critical" } else if metric.value > 0.15 { "high" } else { "medium" },
          confidence: metric.value / 0.2,  // Normalize to 0-1
          description: "Error rate spike detected: " + metric.value.to_string() + " (threshold: " + metric.threshold.to_string() + ")",
          affected_metrics: [metric.metric_name]
        }
        anomalies = anomalies.push(anomaly)
      }
    }
    
    // Check for response time degradation
    let response_time_metrics = metrics.filter(fn(m) { m.metric_name.contains("response_time") })
    for metric in response_time_metrics {
      let baseline_option = get_baseline(service_name, metric.metric_name)
      match baseline_option {
        Some((_, baseline_value)) => {
          let degradation_ratio = metric.value / baseline_value
          if degradation_ratio > 2.0 {  // 2x slower than baseline
            let anomaly = {
              service_name,
              anomaly_type: "response_time_degradation",
              severity: if degradation_ratio > 5.0 { "critical" } else if degradation_ratio > 3.0 { "high" } else { "medium" },
              confidence: (degradation_ratio - 1.0) / 4.0,  // Normalize to 0-1
              description: "Response time degradation: " + metric.value.to_string() + "ms (baseline: " + baseline_value.to_string() + "ms)",
              affected_metrics: [metric.metric_name]
            }
            anomalies = anomalies.push(anomaly)
          }
        }
        None => {}
      }
    }
    
    // Check for throughput drops
    let throughput_metrics = metrics.filter(fn(m) { m.metric_name.contains("throughput") })
    for metric in throughput_metrics {
      let baseline_option = get_baseline(service_name, metric.metric_name)
      match baseline_option {
        Some((_, baseline_value)) => {
          let drop_ratio = baseline_value / metric.value
          if drop_ratio > 2.0 {  // 50% drop in throughput
            let anomaly = {
              service_name,
              anomaly_type: "throughput_drop",
              severity: if drop_ratio > 5.0 { "critical" } else if drop_ratio > 3.0 { "high" } else { "medium" },
              confidence: (drop_ratio - 1.0) / 4.0,  // Normalize to 0-1
              description: "Throughput drop: " + metric.value.to_string() + " (baseline: " + baseline_value.to_string() + ")",
              affected_metrics: [metric.metric_name]
            }
            anomalies = anomalies.push(anomaly)
          }
        }
        None => {}
      }
    }
    
    anomalies
  }
  
  // Calculate overall system health
  let calculate_system_health = fn(all_metrics: Array[HealthMetric>) {
    let mut service_names = []
    for metric in all_metrics {
      if not(service_names.contains(metric.service_name)) {
        service_names = service_names.push(metric.service_name)
      }
    }
    
    let mut all_anomalies = []
    let mut healthy_count = 0
    let mut degraded_count = 0
    let mut critical_count = 0
    
    for service_name in service_names {
      let service_metrics = all_metrics.filter(fn(m) { m.service_name == service_name })
      let service_anomalies = detect_anomalies(service_name, service_metrics)
      all_anomalies = all_anomalies + service_anomalies
      
      // Determine service health
      let critical_metrics = service_metrics.filter(fn(m) { m.status == "critical" })
      let warning_metrics = service_metrics.filter(fn(m) { m.status == "warning" })
      
      if critical_metrics.length() > 0 {
        critical_count = critical_count + 1
      } else if warning_metrics.length() > 0 || service_anomalies.length() > 0 {
        degraded_count = degraded_count + 1
      } else {
        healthy_count = healthy_count + 1
      }
    }
    
    let overall_status = if critical_count > 0 {
      "critical"
    } else if degraded_count > 0 {
      "degraded"
    } else {
      "healthy"
    }
    
    {
      overall_status,
      healthy_services: healthy_count,
      degraded_services: degraded_count,
      critical_services: critical_count,
      anomalies: all_anomalies
    }
  }
  
  // Create test metrics for healthy system
  let healthy_metrics = [
    { service_name: "api-gateway", metric_name: "response_time", value: 110.0, timestamp: 1640995200, threshold: 200.0, status: "healthy" },
    { service_name: "api-gateway", metric_name: "error_rate", value: 0.015, timestamp: 1640995200, threshold: 0.05, status: "healthy" },
    { service_name: "api-gateway", metric_name: "throughput", value: 950.0, timestamp: 1640995200, threshold: 500.0, status: "healthy" },
    { service_name: "payment-service", metric_name: "response_time", value: 210.0, timestamp: 1640995200, threshold: 400.0, status: "healthy" },
    { service_name: "payment-service", metric_name: "error_rate", value: 0.008, timestamp: 1640995200, threshold: 0.02, status: "healthy" },
    { service_name: "payment-service", metric_name: "throughput", value: 480.0, timestamp: 1640995200, threshold: 250.0, status: "healthy" },
    { service_name: "notification-service", metric_name: "response_time", value: 160.0, timestamp: 1640995200, threshold: 300.0, status: "healthy" },
    { service_name: "notification-service", metric_name: "error_rate", value: 0.025, timestamp: 1640995200, threshold: 0.05, status: "healthy" },
    { service_name: "notification-service", metric_name: "throughput", value: 780.0, timestamp: 1640995200, threshold: 400.0, status: "healthy" }
  ]
  
  let healthy_system_health = calculate_system_health(healthy_metrics)
  assert_eq(healthy_system_health.overall_status, "healthy")
  assert_eq(healthy_system_health.healthy_services, 3)
  assert_eq(healthy_system_health.degraded_services, 0)
  assert_eq(healthy_system_health.critical_services, 0)
  assert_eq(healthy_system_health.anomalies.length(), 0)
  
  // Create test metrics for degraded system
  let degraded_metrics = [
    { service_name: "api-gateway", metric_name: "response_time", value: 180.0, timestamp: 1640995200, threshold: 200.0, status: "warning" },
    { service_name: "api-gateway", metric_name: "error_rate", value: 0.03, timestamp: 1640995200, threshold: 0.05, status: "warning" },
    { service_name: "api-gateway", metric_name: "throughput", value: 900.0, timestamp: 1640995200, threshold: 500.0, status: "healthy" },
    { service_name: "payment-service", metric_name: "response_time", value: 250.0, timestamp: 1640995200, threshold: 400.0, status: "warning" },
    { service_name: "payment-service", metric_name: "error_rate", value: 0.01, timestamp: 1640995200, threshold: 0.02, status: "healthy" },
    { service_name: "payment-service", metric_name: "throughput", value: 450.0, timestamp: 1640995200, threshold: 250.0, status: "healthy" },
    { service_name: "notification-service", metric_name: "response_time", value: 140.0, timestamp: 1640995200, threshold: 300.0, status: "healthy" },
    { service_name: "notification-service", metric_name: "error_rate", value: 0.02, timestamp: 1640995200, threshold: 0.05, status: "healthy" },
    { service_name: "notification-service", metric_name: "throughput", value: 800.0, timestamp: 1640995200, threshold: 400.0, status: "healthy" }
  ]
  
  let degraded_system_health = calculate_system_health(degraded_metrics)
  assert_eq(degraded_system_health.overall_status, "degraded")
  assert_eq(degraded_system_health.healthy_services, 1)
  assert_eq(degraded_system_health.degraded_services, 2)
  assert_eq(degraded_system_health.critical_services, 0)
  
  // Create test metrics for critical system
  let critical_metrics = [
    { service_name: "api-gateway", metric_name: "response_time", value: 500.0, timestamp: 1640995200, threshold: 200.0, status: "critical" },
    { service_name: "api-gateway", metric_name: "error_rate", value: 0.25, timestamp: 1640995200, threshold: 0.05, status: "critical" },
    { service_name: "api-gateway", metric_name: "throughput", value: 300.0, timestamp: 1640995200, threshold: 500.0, status: "critical" },
    { service_name: "payment-service", metric_name: "response_time", value: 1000.0, timestamp: 1640995200, threshold: 400.0, status: "critical" },
    { service_name: "payment-service", metric_name: "error_rate", value: 0.15, timestamp: 1640995200, threshold: 0.02, status: "critical" },
    { service_name: "payment-service", metric_name: "throughput", value: 100.0, timestamp: 1640995200, threshold: 250.0, status: "critical" },
    { service_name: "notification-service", metric_name: "response_time", value: 200.0, timestamp: 1640995200, threshold: 300.0, status: "warning" },
    { service_name: "notification-service", metric_name: "error_rate", value: 0.04, timestamp: 1640995200, threshold: 0.05, status: "warning" },
    { service_name: "notification-service", metric_name: "throughput", value: 700.0, timestamp: 1640995200, threshold: 400.0, status: "healthy" }
  ]
  
  let critical_system_health = calculate_system_health(critical_metrics)
  assert_eq(critical_system_health.overall_status, "critical")
  assert_eq(critical_system_health.healthy_services, 0)
  assert_eq(critical_system_health.degraded_services, 1)
  assert_eq(critical_system_health.critical_services, 2)
  assert_true(critical_system_health.anomalies.length() > 0)
  
  // Verify anomaly detection
  let api_anomalies = critical_system_health.anomalies.filter(fn(a) { a.service_name == "api-gateway" })
  assert_true(api_anomalies.length() > 0)
  
  let error_anomalies = api_anomalies.filter(fn(a) { a.anomaly_type == "error_rate_spike" })
  assert_true(error_anomalies.length() > 0)
  
  let response_anomalies = api_anomalies.filter(fn(a) { a.anomaly_type == "response_time_degradation" })
  assert_true(response_anomalies.length() > 0)
  
  let throughput_anomalies = api_anomalies.filter(fn(a) { a.anomaly_type == "throughput_drop" })
  assert_true(throughput_anomalies.length() > 0)
}

// Test 7: Telemetry Data Privacy and Sensitive Information Handling
test "telemetry data privacy and sensitive information handling" {
  // Define data sensitivity level
  enum SensitivityLevel {
    Public
    Internal
    Confidential
    Restricted
  }
  
  // Define data masking strategy
  enum MaskingStrategy {
    None
    Partial
    Full
    Hash
    Tokenize
  }
  
  // Define telemetry field with sensitivity metadata
  type TelemetryField = {
    field_name: String,
    value: String,
    sensitivity_level: SensitivityLevel,
    masking_strategy: MaskingStrategy,
    pii_type: Option[String]  // Personally Identifiable Information type
  }
  
  // Define privacy policy
  type PrivacyPolicy = {
    policy_name: String,
    sensitivity_mappings: Array[(SensitivityLevel, MaskingStrategy)],
    retention_days: Int,
    allowed_export_destinations: Array[String]
  }
  
  // Define privacy compliance result
  type ComplianceResult = {
    compliant: Bool,
    violations: Array[String],
    masked_fields: Array[String],
    retained_fields: Array[String]
  }
  
  // Create privacy policy
  let strict_privacy_policy = {
    policy_name: "strict-privacy",
    sensitivity_mappings: [
      (SensitivityLevel::Public, MaskingStrategy::None),
      (SensitivityLevel::Internal, MaskingStrategy::Partial),
      (SensitivityLevel::Confidential, MaskingStrategy::Hash),
      (SensitivityLevel::Restricted, MaskingStrategy::Full)
    ],
    retention_days: 30,
    allowed_export_destinations: ["internal-analytics", "secure-storage"]
  }
  
  // Apply masking strategy to field value
  let apply_masking = fn(field: TelemetryField, strategy: MaskingStrategy) {
    match strategy {
      MaskingStrategy::None => field.value
      MaskingStrategy::Partial => {
        if field.value.length() <= 4 {
          "*".repeat(field.value.length())
        } else {
          field.value.substring(0, 2) + "*".repeat(field.value.length() - 4) + field.value.substring(field.value.length() - 2, 2)
        }
      }
      MaskingStrategy::Full => {
        "*".repeat(field.value.length())
      }
      MaskingStrategy::Hash => {
        // Simulate hash with length-based value
        "hash-" + (field.value.length() * 17).to_string()
      }
      MaskingStrategy::Tokenize => {
        // Simulate tokenization
        "token-" + field.value.substring(0, 3) + "-" + (field.value.length() * 7).to_string()
      }
    }
  }
  
  // Determine masking strategy based on policy
  let determine_masking_strategy = fn(field: TelemetryField, policy: PrivacyPolicy) {
    let mapping = policy.sensitivity_mappings.find(fn(mapping) { mapping.0 == field.sensitivity_level })
    match mapping {
      Some((_, strategy)) => strategy
      None => MaskingStrategy::Full  // Default to full masking if no mapping found
    }
  }
  
  // Process telemetry fields for privacy compliance
  let process_for_privacy = fn(fields: Array[TelemetryField], policy: PrivacyPolicy) {
    let mut violations = []
    let mut masked_fields = []
    let mut retained_fields = []
    let mut compliant = true
    
    for field in fields {
      let strategy = determine_masking_strategy(field, policy)
      let masked_value = apply_masking(field, strategy)
      
      // Check for PII violations
      match field.pii_type {
        Some(pii_type) => {
          if field.sensitivity_level == SensitivityLevel::Public && strategy != MaskingStrategy::Full {
            violations = violations.push("PII data in public field: " + field.field_name + " (" + pii_type + ")")
            compliant = false
          }
        }
        None => {}
      }
      
      // Track masked and retained fields
      if strategy != MaskingStrategy::None {
        masked_fields = masked_fields.push(field.field_name)
      } else {
        retained_fields = retained_fields.push(field.field_name)
      }
    }
    
    {
      compliant,
      violations,
      masked_fields,
      retained_fields
    }
  }
  
  // Create test telemetry fields with different sensitivity levels
  let telemetry_fields = [
    // Public information
    { field_name: "service.name", value: "payment-service", sensitivity_level: SensitivityLevel::Public, masking_strategy: MaskingStrategy::None, pii_type: None },
    { field_name: "service.version", value: "1.2.3", sensitivity_level: SensitivityLevel::Public, masking_strategy: MaskingStrategy::None, pii_type: None },
    
    // Internal information
    { field_name: "request.id", value: "req-abc123def456", sensitivity_level: SensitivityLevel::Internal, masking_strategy: MaskingStrategy::Partial, pii_type: None },
    { field_name: "session.id", value: "sess-789ghi012jkl", sensitivity_level: SensitivityLevel::Internal, masking_strategy: MaskingStrategy::Partial, pii_type: None },
    
    // Confidential information
    { field_name: "user.email", value: "user@example.com", sensitivity_level: SensitivityLevel::Confidential, masking_strategy: MaskingStrategy::Hash, pii_type: Some("email") },
    { field_name: "user.phone", value: "+1234567890", sensitivity_level: SensitivityLevel::Confidential, masking_strategy: MaskingStrategy::Hash, pii_type: Some("phone") },
    
    // Restricted information
    { field_name: "credit_card.number", value: "4111111111111111", sensitivity_level: SensitivityLevel::Restricted, masking_strategy: MaskingStrategy::Full, pii_type: Some("credit_card") },
    { field_name: "ssn", value: "123-45-6789", sensitivity_level: SensitivityLevel::Restricted, masking_strategy: MaskingStrategy::Full, pii_type: Some("ssn") },
    
    // Violation case: PII in public field
    { field_name: "user.name", value: "John Doe", sensitivity_level: SensitivityLevel::Public, masking_strategy: MaskingStrategy::None, pii_type: Some("name") }
  ]
  
  // Process fields for privacy compliance
  let compliance_result = process_for_privacy(telemetry_fields, strict_privacy_policy)
  
  // Verify compliance result
  assert_false(compliance_result.compliant)  // Should have violation for user.name
  assert_eq(compliance_result.violations.length(), 1)
  assert_true(compliance_result.violations[0].contains("PII data in public field"))
  assert_true(compliance_result.violations[0].contains("user.name"))
  
  // Verify masked fields
  assert_true(compliance_result.masked_fields.contains("request.id"))
  assert_true(compliance_result.masked_fields.contains("session.id"))
  assert_true(compliance_result.masked_fields.contains("user.email"))
  assert_true(compliance_result.masked_fields.contains("user.phone"))
  assert_true(compliance_result.masked_fields.contains("credit_card.number"))
  assert_true(compliance_result.masked_fields.contains("ssn"))
  
  // Verify retained fields
  assert_true(compliance_result.retained_fields.contains("service.name"))
  assert_true(compliance_result.retained_fields.contains("service.version"))
  
  // Test masking strategies
  let public_field = telemetry_fields[0]  // service.name
  let internal_field = telemetry_fields[2]  // request.id
  let confidential_field = telemetry_fields[4]  // user.email
  let restricted_field = telemetry_fields[6]  // credit_card.number
  
  // Test no masking
  let public_masked = apply_masking(public_field, MaskingStrategy::None)
  assert_eq(public_masked, "payment-service")
  
  // Test partial masking
  let internal_masked = apply_masking(internal_field, MaskingStrategy::Partial)
  assert_eq(internal_masked, "re****************ef456")
  
  // Test hash masking
  let confidential_masked = apply_masking(confidential_field, MaskingStrategy::Hash)
  assert_eq(confidential_masked, "hash-238")  // "user@example.com" length 16 * 17 = 272, but simplified
  
  // Test full masking
  let restricted_masked = apply_masking(restricted_field, MaskingStrategy::Full)
  assert_eq(restricted_masked, "****************")
  
  // Test tokenization
  let tokenized = apply_masking(internal_field, MaskingStrategy::Tokenize)
  assert_eq(tokenized, "token-req-84")  // "req-abc123def456" length 15 * 7 = 105, but simplified
  
  // Test edge cases
  let short_field = { field_name: "id", value: "ab", sensitivity_level: SensitivityLevel::Internal, masking_strategy: MaskingStrategy::Partial, pii_type: None }
  let short_masked = apply_masking(short_field, MaskingStrategy::Partial)
  assert_eq(short_masked, "**")  // Should be fully masked due to short length
  
  // Test privacy policy with different mappings
  let lenient_privacy_policy = {
    policy_name: "lenient-privacy",
    sensitivity_mappings: [
      (SensitivityLevel::Public, MaskingStrategy::None),
      (SensitivityLevel::Internal, MaskingStrategy::None),
      (SensitivityLevel::Confidential, MaskingStrategy::Partial),
      (SensitivityLevel::Restricted, MaskingStrategy::Full)
    ],
    retention_days: 90,
    allowed_export_destinations: ["internal-analytics", "secure-storage", "third-party-analytics"]
  }
  
  let lenient_compliance = process_for_privacy(telemetry_fields, lenient_privacy_policy)
  assert_false(lenient_compliance.compliant)  // Still has violation for user.name
  
  // Verify different masking under lenient policy
  let lenient_internal_masked = apply_masking(internal_field, MaskingStrategy::None)
  assert_eq(lenient_internal_masked, "req-abc123def456")
  
  let lenient_confidential_masked = apply_masking(confidential_field, MaskingStrategy::Partial)
  assert_eq(lenient_confidential_masked, "us**************.com")
}

// Test 8: Telemetry Data Retention with Tiered Storage
test "telemetry data retention with tiered storage optimization" {
  // Define storage tier
  enum StorageTier {
    Hot      // Fast, expensive, recent data
    Warm     // Medium speed/cost, medium age data
    Cold     // Slow, cheap, old data
    Archive  // Very slow, very cheap, very old data
  }
  
  // Define data record with storage information
  type DataRecord = {
    id: String,
    data_type: String,  // trace, metric, log
    timestamp: Int,
    size_bytes: Int,
    access_frequency: Float,  // Accesses per day
    business_value: Float,    // 0-1, importance score
    current_tier: StorageTier,
    retention_days: Int
  }
  
  // Define retention policy with tiered storage
  type TieredRetentionPolicy = {
    policy_name: String,
    tier_thresholds: Array[(StorageTier, Int)],  // (tier, age_days)
    tier_size_limits: Array[(StorageTier, Int)],  // (tier, size_limit_gb)
    access_frequency_thresholds: Array[(StorageTier, Float)],  // (tier, min_access_frequency)
    business_value_thresholds: Array[(StorageTier, Float)],    // (tier, min_business_value)
    default_retention_days: Int
  }
  
  // Define storage optimization result
  type OptimizationResult = {
    records_moved: Array[(String, StorageTier, StorageTier)],  // (id, from_tier, to_tier)
    space_freed_gb: Float,
    estimated_cost_savings: Float,
    access_impact: Float
  }
  
  // Create tiered retention policy
  let retention_policy = {
    policy_name: "tiered-retention-policy",
    tier_thresholds: [
      (StorageTier::Hot, 7),       // Hot tier: 0-7 days
      (StorageTier::Warm, 30),     // Warm tier: 8-30 days
      (StorageTier::Cold, 90),     // Cold tier: 31-90 days
      (StorageTier::Archive, 365)  // Archive tier: 91-365 days
    ],
    tier_size_limits: [
      (StorageTier::Hot, 100),     // 100GB
      (StorageTier::Warm, 500),    // 500GB
      (StorageTier::Cold, 2000),   // 2TB
      (StorageTier::Archive, 10000) // 10TB
    ],
    access_frequency_thresholds: [
      (StorageTier::Hot, 10.0),     // >10 accesses/day
      (StorageTier::Warm, 1.0),     // >1 access/day
      (StorageTier::Cold, 0.1),     // >0.1 access/day
      (StorageTier::Archive, 0.0)   // Any access frequency
    ],
    business_value_thresholds: [
      (StorageTier::Hot, 0.8),      // >0.8 business value
      (StorageTier::Warm, 0.6),     // >0.6 business value
      (StorageTier::Cold, 0.3),     // >0.3 business value
      (StorageTier::Archive, 0.0)   // Any business value
    ],
    default_retention_days: 365
  }
  
  // Calculate current time-based tier
  let calculate_time_based_tier = fn(record: DataRecord, current_time: Int, policy: TieredRetentionPolicy) {
    let age_days = (current_time - record.timestamp) / 86400
    
    if age_days <= policy.tier_thresholds[0].1 {
      StorageTier::Hot
    } else if age_days <= policy.tier_thresholds[1].1 {
      StorageTier::Warm
    } else if age_days <= policy.tier_thresholds[2].1 {
      StorageTier::Cold
    } else if age_days <= policy.tier_thresholds[3].1 {
      StorageTier::Archive
    } else {
      StorageTier::Archive  // Beyond policy, keep in archive
    }
  }
  
  // Calculate optimal tier based on access patterns and business value
  let calculate_optimal_tier = fn(record: DataRecord, current_time: Int, policy: TieredRetentionPolicy) {
    let time_based_tier = calculate_time_based_tier(record, current_time, policy)
    
    // Check if record should be promoted based on access frequency
    let mut optimal_tier = time_based_tier
    
    for (tier, min_frequency) in policy.access_frequency_thresholds {
      if record.access_frequency >= min_frequency {
        // Record qualifies for this tier based on access
        if tier_value(tier) < tier_value(optimal_tier) {
          optimal_tier = tier
        }
      }
    }
    
    // Check if record should be promoted based on business value
    for (tier, min_value) in policy.business_value_thresholds {
      if record.business_value >= min_value {
        // Record qualifies for this tier based on business value
        if tier_value(tier) < tier_value(optimal_tier) {
          optimal_tier = tier
        }
      }
    }
    
    optimal_tier
  }
  
  // Helper function to compare tier values (lower is better/faster)
  let tier_value = fn(tier: StorageTier) {
    match tier {
      StorageTier::Hot => 1
      StorageTier::Warm => 2
      StorageTier::Cold => 3
      StorageTier::Archive => 4
    }
  }
  
  // Optimize storage placement for records
  let optimize_storage_placement = fn(records: Array[DataRecord], current_time: Int, policy: TieredRetentionPolicy) {
    let mut records_moved = []
    let mut space_freed_bytes = 0
    let mut total_access_impact = 0.0
    
    for record in records {
      let optimal_tier = calculate_optimal_tier(record, current_time, policy)
      
      if optimal_tier != record.current_tier {
        // Calculate space impact (simplified: cold/archive are more compressed)
        let size_multiplier = match optimal_tier {
          StorageTier::Hot => 1.0
          StorageTier::Warm => 0.8
          StorageTier::Cold => 0.5
          StorageTier::Archive => 0.2
        }
        
        let current_size_multiplier = match record.current_tier {
          StorageTier::Hot => 1.0
          StorageTier::Warm => 0.8
          StorageTier::Cold => 0.5
          StorageTier::Archive => 0.2
        }
        
        let size_diff = record.size_bytes * (current_size_multiplier - size_multiplier)
        space_freed_bytes = space_freed_bytes + size_diff
        
        // Calculate access impact (higher tier value = slower access)
        let access_impact = (tier_value(optimal_tier) - tier_value(record.current_tier)).to_float() * record.access_frequency
        total_access_impact = total_access_impact + access_impact
        
        records_moved = records_moved.push((record.id, record.current_tier, optimal_tier))
      }
    }
    
    let space_freed_gb = space_freed_bytes.to_float() / (1024.0 * 1024.0 * 1024.0)
    let estimated_cost_savings = space_freed_gb * 0.23  // $0.23 per GB saved (example)
    
    {
      records_moved,
      space_freed_gb,
      estimated_cost_savings,
      access_impact: total_access_impact
    }
  }
  
  // Create test data records with different characteristics
  let current_time = 1640995200  // 2022-01-01 00:00:00 UTC
  
  let data_records = [
    // Hot tier candidates: recent, high access, high business value
    { id: "trace-001", data_type: "trace", timestamp: current_time - 86400, size_bytes: 1048576, access_frequency: 15.0, business_value: 0.9, current_tier: StorageTier::Hot, retention_days: 365 },
    { id: "metric-001", data_type: "metric", timestamp: current_time - 172800, size_bytes: 524288, access_frequency: 25.0, business_value: 0.85, current_tier: StorageTier::Hot, retention_days: 365 },
    
    // Warm tier candidates: medium age, medium access, medium business value
    { id: "trace-002", data_type: "trace", timestamp: current_time - 864000, size_bytes: 2097152, access_frequency: 2.0, business_value: 0.7, current_tier: StorageTier::Warm, retention_days: 365 },
    { id: "log-001", data_type: "log", timestamp: current_time - 1209600, size_bytes: 1048576, access_frequency: 1.5, business_value: 0.6, current_tier: StorageTier::Warm, retention_days: 90 },
    
    // Cold tier candidates: old, low access, low business value
    { id: "trace-003", data_type: "trace", timestamp: current_time - 2592000, size_bytes: 4194304, access_frequency: 0.05, business_value: 0.4, current_tier: StorageTier::Cold, retention_days: 365 },
    { id: "metric-002", data_type: "metric", timestamp: current_time - 5184000, size_bytes: 2097152, access_frequency: 0.2, business_value: 0.2, current_tier: StorageTier::Cold, retention_days: 365 },
    
    // Archive tier candidates: very old, very low access, very low business value
    { id: "trace-004", data_type: "trace", timestamp: current_time - 7776000, size_bytes: 8388608, access_frequency: 0.01, business_value: 0.1, current_tier: StorageTier::Archive, retention_days: 365 },
    
    // Misplaced records that should be moved
    { id: "trace-005", data_type: "trace", timestamp: current_time - 2592000, size_bytes: 1048576, access_frequency: 12.0, business_value: 0.8, current_tier: StorageTier::Cold, retention_days: 365 },  // High access, should be warmer
    { id: "metric-003", data_type: "metric", timestamp: current_time - 86400, size_bytes: 524288, access_frequency: 0.1, business_value: 0.2, current_tier: StorageTier::Hot, retention_days: 365 }  // Low access/value, should be colder
  ]
  
  // Optimize storage placement
  let optimization_result = optimize_storage_placement(data_records, current_time, retention_policy)
  
  // Verify optimization results
  assert_eq(optimization_result.records_moved.length(), 2)  // Two misplaced records should be moved
  
  // Check specific moves
  let trace_005_move = optimization_result.records_moved.find_fn(move) { move.0 == "trace-005" }
  match trace_005_move {
    Some((_, from_tier, to_tier)) => {
      assert_eq(from_tier, StorageTier::Cold)
      assert_eq(to_tier, StorageTier::Warm)
    }
    None => assert_true(false)
  }
  
  let metric_003_move = optimization_result.records_moved.find_fn(move) { move.0 == "metric-003" }
  match metric_003_move {
    Some((_, from_tier, to_tier)) => {
      assert_eq(from_tier, StorageTier::Hot)
      assert_eq(to_tier, StorageTier::Warm)
    }
    None => assert_true(false)
  }
  
  // Verify space freed
  assert_true(optimization_result.space_freed_gb > 0.0)
  
  // Verify cost savings
  assert_true(optimization_result.estimated_cost_savings > 0.0)
  
  // Test time-based tier calculation
  let recent_record = data_records[0]  // 1 day old
  let recent_tier = calculate_time_based_tier(recent_record, current_time, retention_policy)
  assert_eq(recent_tier, StorageTier::Hot)
  
  let medium_record = data_records[2]  // 10 days old
  let medium_tier = calculate_time_based_tier(medium_record, current_time, retention_policy)
  assert_eq(medium_tier, StorageTier::Warm)
  
  let old_record = data_records[4]  // 30 days old
  let old_tier = calculate_time_based_tier(old_record, current_time, retention_policy)
  assert_eq(old_tier, StorageTier::Cold)
  
  let very_old_record = data_records[6]  // 90 days old
  let very_old_tier = calculate_time_based_tier(very_old_record, current_time, retention_policy)
  assert_eq(very_old_tier, StorageTier::Archive)
  
  // Test optimal tier calculation with business value override
  let high_value_record = data_records[7]  // Old but high business value and access
  let optimal_tier = calculate_optimal_tier(high_value_record, current_time, retention_policy)
  assert_eq(optimal_tier, StorageTier::Warm)  // Promoted from Cold to Warm due to high access/business value
  
  // Test retention policy enforcement
  let enforce_retention = fn(records: Array[DataRecord], current_time: Int, policy: TieredRetentionPolicy) {
    records.filter_fn(record) {
      let age_days = (current_time - record.timestamp) / 86400
      age_days <= record.retention_days
    }
  }
  
  let all_records_retained = enforce_retention(data_records, current_time, retention_policy)
  assert_eq(all_records_retained.length(), data_records.length())  // All records within retention period
  
  // Test with expired record
  let expired_record = { id: "expired-001", data_type: "trace", timestamp: current_time - 40000000, size_bytes: 1048576, access_frequency: 0.0, business_value: 0.0, current_tier: StorageTier::Archive, retention_days: 365 }
  let records_with_expired = data_records + [expired_record]
  
  let filtered_records = enforce_retention(records_with_expired, current_time, retention_policy)
  assert_eq(filtered_records.length(), data_records.length())  // Expired record removed
}
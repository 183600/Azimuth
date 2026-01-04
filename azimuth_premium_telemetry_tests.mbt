// Azimuth Premium Telemetry Test Suite
// High-quality test cases focusing on advanced telemetry scenarios and edge cases

// Test 1: Distributed Trace Context Propagation
test "distributed trace context propagation across service boundaries" {
  // Define trace context structure
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)],
    sampled: Bool
  }
  
  // Create initial trace context
  let root_context = {
    trace_id: "trace-abc123",
    span_id: "span-def456",
    parent_span_id: None,
    baggage: [("user.id", "user-789"), ("request.id", "req-012")],
    sampled: true
  }
  
  // Simulate service boundary crossing
  let extract_context = fn(headers: Array[(String, String)>) {
    let trace_header = headers.find(fn(h) { h.0 == "traceparent" })
    let baggage_header = headers.find(fn(h) { h.0 == "baggage" })
    
    match trace_header {
      Some((_, trace_value)) => {
        let parts = trace_value.split("-")
        if parts.length() >= 3 {
          Some({
            trace_id: parts[1],
            span_id: parts[2],
            parent_span_id: Some(parts[2]),
            baggage: [],
            sampled: parts[0] == "00"
          })
        } else {
          None
        }
      }
      None => None
    }
  }
  
  let inject_context = fn(context: TraceContext) {
    let trace_header = "00-" + context.trace_id + "-" + context.span_id + "-01"
    let baggage_items = context.baggage.map(fn(item) { item.0 + "=" + item.1 })
    let baggage_header = baggage_items.join(",")
    
    [
      ("traceparent", trace_header),
      ("baggage", baggage_header)
    ]
  }
  
  // Test context injection
  let injected_headers = inject_context(root_context)
  assert_eq(injected_headers.length(), 2)
  assert_true(injected_headers[0].1.contains(root_context.trace_id))
  assert_true(injected_headers[0].1.contains(root_context.span_id))
  
  // Test context extraction
  let extracted_context = extract_context(injected_headers)
  assert_true(extracted_context.is_some())
  
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, root_context.trace_id)
      assert_eq(context.parent_span_id, Some(root_context.span_id))
      assert_true(context.sampled)
    }
    None => assert_true(false)
  }
  
  // Test baggage propagation
  let updated_context = match extracted_context {
    Some(ctx) => { ctx | baggage: root_context.baggage }
    None => root_context
  }
  
  assert_eq(updated_context.baggage.length(), 2)
  assert_true(updated_context.baggage.contains(("user.id", "user-789")))
  assert_true(updated_context.baggage.contains(("request.id", "req-012")))
}

// Test 2: Real-time Telemetry Stream Processing
test "real-time telemetry stream processing with windowing" {
  // Define telemetry event structure
  type TelemetryEvent = {
    timestamp: Int,
    event_type: String,
    service_name: String,
    duration_ms: Int,
    status_code: Int,
    attributes: Array[(String, String)]
  }
  
  // Create sliding window processor
  let sliding_window_processor = fn(events: Array[TelemetryEvent], window_size_ms: Int) {
    let mut windows = []
    let mut current_window_start = if events.length() > 0 { events[0].timestamp } else { 0 }
    
    for event in events {
      while event.timestamp >= current_window_start + window_size_ms {
        current_window_start = current_window_start + window_size_ms
      }
      
      let window_events = events.filter(fn(e) { 
        e.timestamp >= current_window_start && e.timestamp < current_window_start + window_size_ms
      })
      
      if window_events.length() > 0 && not(windows.any(fn(w) { w.0 == current_window_start })) {
        windows = windows.push((current_window_start, window_events))
      }
    }
    
    windows
  }
  
  // Generate test events
  let base_time = 1640995200000  // Millisecond timestamp
  let test_events = [
    { timestamp: base_time, event_type: "http_request", service_name: "api", duration_ms: 120, status_code: 200, attributes: [] },
    { timestamp: base_time + 500, event_type: "database_query", service_name: "db", duration_ms: 45, status_code: 200, attributes: [] },
    { timestamp: base_time + 1200, event_type: "http_request", service_name: "api", duration_ms: 200, status_code: 500, attributes: [] },
    { timestamp: base_time + 1800, event_type: "cache_hit", service_name: "cache", duration_ms: 5, status_code: 200, attributes: [] },
    { timestamp: base_time + 2500, event_type: "http_request", service_name: "api", duration_ms: 80, status_code: 200, attributes: [] }
  ]
  
  // Process with 1-second windows
  let windows = sliding_window_processor(test_events, 1000)
  assert_eq(windows.length(), 3)
  
  // Analyze first window (base_time to base_time + 1000)
  let first_window = windows[0]
  assert_eq(first_window.0, base_time)
  assert_eq(first_window.1.length(), 2)
  
  // Calculate window metrics
  let calculate_window_metrics = fn(window_events: Array[TelemetryEvent>) {
    let total_requests = window_events.length()
    let error_count = window_events.filter(fn(e) { e.status_code >= 400 }).length()
    let avg_duration = if total_requests > 0 {
      window_events.reduce(fn(acc, e) { acc + e.duration_ms }, 0) / total_requests
    } else {
      0
    }
    
    {
      total_events: total_requests,
      error_rate: if total_requests > 0 { (error_count * 100) / total_requests } else { 0 },
      avg_duration_ms: avg_duration
    }
  }
  
  let first_window_metrics = calculate_window_metrics(first_window.1)
  assert_eq(first_window_metrics.total_events, 2)
  assert_eq(first_window_metrics.error_rate, 0)
  assert_eq(first_window_metrics.avg_duration_ms, 82)  // (120 + 45) / 2
  
  // Test second window (contains error)
  let second_window = windows[1]
  let second_window_metrics = calculate_window_metrics(second_window.1)
  assert_eq(second_window_metrics.total_events, 2)
  assert_eq(second_window_metrics.error_rate, 50)  // 1 error out of 2 events
}

// Test 3: Telemetry Data Compression and Optimization
test "telemetry data compression and optimization strategies" {
  // Define telemetry batch structure
  type TelemetryBatch = {
    spans: Array[SpanData],
    metrics: Array[MetricData],
    resource_attributes: Array[(String, String)]
  }
  
  type SpanData = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  type MetricData = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    attributes: Array[(String, String)]
  }
  
  // Create data deduplicator
  let deduplicate_attributes = fn(attributes: Array[(String, String)]) {
    let mut seen = []
    let mut result = []
    
    for attr in attributes {
      if not(seen.contains(attr.0)) {
        seen = seen.push(attr.0)
        result = result.push(attr)
      }
    }
    
    result
  }
  
  // Create attribute compressor (common patterns)
  let compress_attributes = fn(attributes: Array[(String, String)>) {
    let common_prefixes = [
      ("service.", "s."),
      ("telemetry.", "t."),
      ("http.", "h."),
      ("db.", "d.")
    ]
    
    attributes.map(fn(attr) {
      let (key, value) = attr
      let mut compressed_key = key
      
      for (prefix, short_prefix) in common_prefixes {
        if key.starts_with(prefix) {
          compressed_key = short_prefix + key.substring(prefix.length(), key.length() - prefix.length())
          break
        }
      }
      
      (compressed_key, value)
    })
  }
  
  // Create batch optimizer
  let optimize_batch = fn(batch: TelemetryBatch) {
    let optimized_spans = batch.spans.map(fn(span) {
      let deduped_attrs = deduplicate_attributes(span.attributes)
      let compressed_attrs = compress_attributes(deduped_attrs)
      
      { span | attributes: compressed_attrs }
    })
    
    let optimized_metrics = batch.metrics.map(fn(metric) {
      let deduped_attrs = deduplicate_attributes(metric.attributes)
      let compressed_attrs = compress_attributes(deduped_attrs)
      
      { metric | attributes: compressed_attrs }
    })
    
    {
      spans: optimized_spans,
      metrics: optimized_metrics,
      resource_attributes: compress_attributes(batch.resource_attributes)
    }
  }
  
  // Create test batch with redundant attributes
  let test_batch = {
    spans: [
      {
        trace_id: "trace-123",
        span_id: "span-456",
        parent_span_id: None,
        name: "http_request",
        start_time: 1640995200,
        end_time: 1640995250,
        status: "ok",
        attributes: [
          ("service.name", "api-service"),
          ("service.version", "1.2.3"),
          ("service.name", "api-service"),  // Duplicate
          ("http.method", "GET"),
          ("http.status_code", "200"),
          ("telemetry.sdk.version", "1.0.0")
        ]
      }
    ],
    metrics: [
      {
        name: "http_requests_total",
        value: 100.0,
        unit: "count",
        timestamp: 1640995200,
        attributes: [
          ("service.name", "api-service"),
          ("http.method", "GET"),
          ("service.name", "api-service")  // Duplicate
        ]
      }
    ],
    resource_attributes: [
      ("service.name", "api-service"),
      ("service.instance.id", "instance-123"),
      ("telemetry.sdk.name", "opentelemetry"),
      ("service.name", "api-service")  // Duplicate
    ]
  }
  
  // Test optimization
  let optimized_batch = optimize_batch(test_batch)
  
  // Verify span optimization
  let optimized_span = optimized_batch.spans[0]
  assert_eq(optimized_span.attributes.length(), 5)  // Removed duplicate
  assert_true(optimized_span.attributes.contains(("s.name", "api-service")))
  assert_true(optimized_span.attributes.contains(("s.version", "1.2.3")))
  assert_true(optimized_span.attributes.contains(("h.method", "GET")))
  assert_true(optimized_span.attributes.contains(("h.status_code", "200")))
  assert_true(optimized_span.attributes.contains(("t.sdk.version", "1.0.0")))
  
  // Verify metric optimization
  let optimized_metric = optimized_batch.metrics[0]
  assert_eq(optimized_metric.attributes.length(), 2)  // Removed duplicate
  assert_true(optimized_metric.attributes.contains(("s.name", "api-service")))
  assert_true(optimized_metric.attributes.contains(("h.method", "GET")))
  
  // Verify resource attributes optimization
  assert_eq(optimized_batch.resource_attributes.length(), 3)  // Removed duplicate
  assert_true(optimized_batch.resource_attributes.contains(("s.name", "api-service")))
  assert_true(optimized_batch.resource_attributes.contains(("s.instance.id", "instance-123")))
  assert_true(optimized_batch.resource_attributes.contains(("t.sdk.name", "opentelemetry")))
}

// Test 4: Telemetry Anomaly Detection
test "telemetry anomaly detection and alerting" {
  // Define anomaly detection parameters
  type AnomalyThreshold = {
    metric_name: String,
    min_value: Option[Float],
    max_value: Option[Float],
    deviation_percentage: Float
  }
  
  type AnomalyAlert = {
    timestamp: Int,
    metric_name: String,
    actual_value: Float,
    expected_range: (Float, Float),
    severity: String
  }
  
  // Create anomaly detector
  let detect_anomalies = fn(metric_values: Array[(Int, Float)>, threshold: AnomalyThreshold, baseline_window: Int) {
    if metric_values.length() < baseline_window + 1 {
      return []
    }
    
    // Calculate baseline from first N values
    let baseline_values = metric_values.slice(0, baseline_window)
    let baseline_avg = baseline_values.reduce(fn(acc, v) { acc + v.1 }, 0.0) / (baseline_window as Float)
    
    // Calculate standard deviation
    let variance = baseline_values.reduce(fn(acc, v) { 
      let diff = v.1 - baseline_avg
      acc + (diff * diff)
    }, 0.0) / (baseline_window as Float)
    let std_dev = variance.sqrt()
    
    // Check for anomalies in remaining values
    let test_values = metric_values.slice(baseline_window, metric_values.length())
    let mut alerts = []
    
    for (timestamp, value) in test_values {
      let lower_bound = baseline_avg - (threshold.deviation_percentage * baseline_avg / 100.0)
      let upper_bound = baseline_avg + (threshold.deviation_percentage * baseline_avg / 100.0)
      
      // Check absolute thresholds
      let abs_min_violation = match threshold.min_value {
        Some(min) => value < min
        None => false
      }
      
      let abs_max_violation = match threshold.max_value {
        Some(max) => value > max
        None => false
      }
      
      // Check statistical anomaly
      let statistical_anomaly = value < lower_bound or value > upper_bound
      
      if abs_min_violation or abs_max_violation or statistical_anomaly {
        let severity = if abs_min_violation or abs_max_violation {
          "critical"
        } else if value < lower_bound - (2.0 * std_dev) or value > upper_bound + (2.0 * std_dev) {
          "high"
        } else {
          "medium"
        }
        
        alerts = alerts.push({
          timestamp,
          metric_name: threshold.metric_name,
          actual_value: value,
          expected_range: (lower_bound, upper_bound),
          severity
        })
      }
    }
    
    alerts
  }
  
  // Test with response time metrics
  let response_time_data = [
    (1640995200, 100.0),   // Baseline period starts
    (1640995260, 120.0),
    (1640995320, 95.0),
    (1640995380, 105.0),
    (1640995440, 110.0),
    (1640995500, 98.0),
    (1640995560, 102.0),
    (1640995620, 115.0),
    (1640995680, 1500.0),  // Anomaly - very high response time
    (1640995740, 90.0),    // Anomaly - very low response time
    (1640995800, 200.0)    // Anomaly - moderately high
  ]
  
  let response_time_threshold = {
    metric_name: "response_time_ms",
    min_value: Some(50.0),
    max_value: Some(500.0),
    deviation_percentage: 50.0
  }
  
  let alerts = detect_anomalies(response_time_data, response_time_threshold, 8)
  
  // Should detect 3 anomalies
  assert_eq(alerts.length(), 3)
  
  // Check critical alert (exceeds max threshold)
  let critical_alert = alerts.find(fn(a) { a.severity == "critical" })
  assert_true(critical_alert.is_some())
  match critical_alert {
    Some(alert) => {
      assert_eq(alert.actual_value, 1500.0)
      assert_eq(alert.metric_name, "response_time_ms")
    }
    None => assert_true(false)
  }
  
  // Check medium alert (statistical anomaly)
  let medium_alerts = alerts.filter(fn(a) { a.severity == "medium" })
  assert_eq(medium_alerts.length(), 2)
  
  // Test with error rate metrics
  let error_rate_data = [
    (1640995200, 0.01),
    (1640995260, 0.02),
    (1640995320, 0.015),
    (1640995380, 0.005),
    (1640995440, 0.012),
    (1640995500, 0.008),
    (1640995560, 0.018),
    (1640995620, 0.011),
    (1640995680, 0.15),    // Anomaly - high error rate
    (1640995740, 0.02)
  ]
  
  let error_rate_threshold = {
    metric_name: "error_rate",
    min_value: Some(0.0),
    max_value: Some(0.1),
    deviation_percentage: 200.0
  }
  
  let error_alerts = detect_anomalies(error_rate_data, error_rate_threshold, 8)
  assert_eq(error_alerts.length(), 1)
  assert_eq(error_alerts[0].actual_value, 0.15)
  assert_eq(error_alerts[0].severity, "critical")
}

// Test 5: Telemetry Memory Management
test "telemetry memory management and resource cleanup" {
  // Define memory pool for telemetry objects
  type MemoryPool[T] = {
    available: Array[T],
    in_use: Array[T],
    max_size: Int,
    created: Int,
    reused: Int
  }
  
  // Create memory pool manager
  let create_memory_pool = fn(max_size: Int) {
    {
      available: [],
      in_use: [],
      max_size,
      created: 0,
      reused: 0
    }
  }
  
  let acquire_from_pool = fn(pool: MemoryPool[T], factory: () -> T) {
    match pool.available.pop() {
      Some(item) => {
        {
          pool | 
          in_use: pool.in_use.push(item),
          reused: pool.reused + 1
        },
        Some(item)
      }
      None => {
        if pool.created < pool.max_size {
          let new_item = factory()
          {
            pool |
            in_use: pool.in_use.push(new_item),
            created: pool.created + 1
          },
          Some(new_item)
        } else {
          (pool, None)
        }
      }
    }
  }
  
  let release_to_pool = fn(pool: MemoryPool[T], item: T) {
    let updated_in_use = pool.in_use.filter(fn(i) { i != item })
    {
      pool |
      available: pool.available.push(item),
      in_use: updated_in_use
    }
  }
  
  // Test with telemetry span objects
  let span_factory = fn() {
    {
      trace_id: "",
      span_id: "",
      name: "",
      start_time: 0,
      end_time: 0,
      attributes: []
    }
  }
  
  let initial_pool = create_memory_pool(5)
  
  // Acquire first span (should create new)
  let (pool1, span1_opt) = acquire_from_pool(initial_pool, span_factory)
  assert_true(span1_opt.is_some())
  assert_eq(pool1.created, 1)
  assert_eq(pool1.reused, 0)
  assert_eq(pool1.in_use.length(), 1)
  
  // Acquire second span (should create new)
  let (pool2, span2_opt) = acquire_from_pool(pool1, span_factory)
  assert_true(span2_opt.is_some())
  assert_eq(pool2.created, 2)
  assert_eq(pool2.reused, 0)
  assert_eq(pool2.in_use.length(), 2)
  
  // Release first span back to pool
  let pool3 = release_to_pool(pool2, span1_opt.unwrap())
  assert_eq(pool3.available.length(), 1)
  assert_eq(pool3.in_use.length(), 1)
  
  // Acquire third span (should reuse from pool)
  let (pool4, span3_opt) = acquire_from_pool(pool3, span_factory)
  assert_true(span3_opt.is_some())
  assert_eq(pool4.created, 2)  // No new creation
  assert_eq(pool4.reused, 1)   // Reused one
  assert_eq(pool4.available.length(), 0)
  assert_eq(pool4.in_use.length(), 2)
  
  // Test memory pressure scenario
  let mut pressure_pool = pool4
  
  // Acquire remaining spans to fill pool
  for i in 0..3 {
    let (new_pool, span_opt) = acquire_from_pool(pressure_pool, span_factory)
    pressure_pool = new_pool
  }
  
  assert_eq(pressure_pool.created, 5)  // Should have created max 5 spans
  assert_eq(pressure_pool.in_use.length(), 5)
  
  // Try to acquire beyond pool limit
  let (final_pool, overflow_span) = acquire_from_pool(pressure_pool, span_factory)
  assert_true(overflow_span.is_none())  // Should fail due to pool limit
  assert_eq(final_pool.created, 5)     // No new creation
  assert_eq(final_pool.in_use.length(), 5)
  
  // Test cleanup simulation
  let cleanup_pool = fn(pool: MemoryPool[T]) {
    // Release all in-use items back to pool
    let mut cleaned_pool = pool
    for item in pool.in_use {
      cleaned_pool = release_to_pool(cleaned_pool, item)
    }
    cleaned_pool
  }
  
  let cleaned_pool = cleanup_pool(final_pool)
  assert_eq(cleaned_pool.in_use.length(), 0)
  assert_eq(cleaned_pool.available.length(), 5)
  
  // Test pool statistics
  let pool_efficiency = if cleaned_pool.created > 0 {
    (cleaned_pool.reused * 100) / cleaned_pool.created
  } else {
    0
  }
  
  assert_eq(pool_efficiency, 20)  // 1 reuse out of 5 created = 20%
}

// Test 6: Telemetry Data Correlation
test "telemetry data correlation across multiple signals" {
  // Define correlation identifiers
  type CorrelationId = String
  type TraceId = String
  type SpanId = String
  
  // Define correlated telemetry signals
  type CorrelatedSignals = {
    trace_id: TraceId,
    spans: Array[SpanReference],
    logs: Array[LogReference],
    metrics: Array[MetricReference],
    errors: Array[ErrorReference]
  }
  
  type SpanReference = {
    span_id: SpanId,
    parent_span_id: Option[SpanId],
    service_name: String,
    operation_name: String,
    start_time: Int,
    duration_ms: Int,
    status: String
  }
  
  type LogReference = {
    timestamp: Int,
    severity: String,
    message: String,
    span_id: Option[SpanId>,
    attributes: Array[(String, String)]
  }
  
  type MetricReference = {
    name: String,
    value: Float,
    timestamp: Int,
    span_id: Option[SpanId>,
    attributes: Array[(String, String)]
  }
  
  type ErrorReference = {
    error_id: String,
    error_type: String,
    message: String,
    timestamp: Int,
    span_id: Option[SpanId],
    stack_trace: String
  }
  
  // Create correlation engine
  let correlate_signals = fn(
    spans: Array[SpanReference], 
    logs: Array[LogReference], 
    metrics: Array[MetricReference], 
    errors: Array[ErrorReference]
  ) {
    // Group by trace_id
    let trace_groups = spans.group_by(fn(s) { s.trace_id })
    
    // Build correlated signals for each trace
    trace_groups.map(fn((trace_id, trace_spans)) {
      let span_ids = trace_spans.map(fn(s) { s.span_id })
      
      // Filter logs by span_id or trace_id
      let correlated_logs = logs.filter(fn(log) {
        match log.span_id {
          Some(span_id) => span_ids.contains(span_id)
          None => false
        }
      })
      
      // Filter metrics by span_id
      let correlated_metrics = metrics.filter(fn(metric) {
        match metric.span_id {
          Some(span_id) => span_ids.contains(span_id)
          None => false
        }
      })
      
      // Filter errors by span_id
      let correlated_errors = errors.filter(fn(error) {
        match error.span_id {
          Some(span_id) => span_ids.contains(span_id)
          None => false
        }
      })
      
      {
        trace_id,
        spans: trace_spans,
        logs: correlated_logs,
        metrics: correlated_metrics,
        errors: correlated_errors
      }
    })
  }
  
  // Create test data
  let test_spans = [
    {
      trace_id: "trace-123",
      span_id: "span-abc",
      parent_span_id: None,
      service_name: "api-gateway",
      operation_name: "process_request",
      start_time: 1640995200,
      duration_ms: 150,
      status: "ok"
    },
    {
      trace_id: "trace-123",
      span_id: "span-def",
      parent_span_id: Some("span-abc"),
      service_name: "user-service",
      operation_name: "authenticate_user",
      start_time: 1640995210,
      duration_ms: 80,
      status: "ok"
    },
    {
      trace_id: "trace-456",
      span_id: "span-ghi",
      parent_span_id: None,
      service_name: "payment-service",
      operation_name: "process_payment",
      start_time: 1640995300,
      duration_ms: 1200,
      status: "error"
    }
  ]
  
  let test_logs = [
    {
      timestamp: 1640995205,
      severity: "INFO",
      message: "Request received",
      span_id: Some("span-abc"),
      attributes: [("user.id", "user-789")]
    },
    {
      timestamp: 1640995215,
      severity: "DEBUG",
      message: "Authentication successful",
      span_id: Some("span-def"),
      attributes: [("auth.method", "jwt")]
    },
    {
      timestamp: 1640995310,
      severity: "ERROR",
      message: "Payment processing failed",
      span_id: Some("span-ghi"),
      attributes: [("error.code", "CARD_DECLINED")]
    }
  ]
  
  let test_metrics = [
    {
      name: "http_requests_total",
      value: 1.0,
      timestamp: 1640995200,
      span_id: Some("span-abc"),
      attributes: [("method", "POST"), ("status", "200")]
    },
    {
      name: "db_query_duration_ms",
      value: 45.0,
      timestamp: 1640995210,
      span_id: Some("span-def"),
      attributes: [("query.type", "SELECT")]
    },
    {
      name: "payment_failures_total",
      value: 1.0,
      timestamp: 1640995300,
      span_id: Some("span-ghi"),
      attributes: [("reason", "CARD_DECLINED")]
    }
  ]
  
  let test_errors = [
    {
      error_id: "error-123",
      error_type: "PaymentError",
      message: "Card declined",
      timestamp: 1640995310,
      span_id: Some("span-ghi"),
      stack_trace: "at PaymentService.process..."
    }
  ]
  
  // Test correlation
  let correlated_signals = correlate_signals(test_spans, test_logs, test_metrics, test_errors)
  assert_eq(correlated_signals.length(), 2)  // Two different traces
  
  // Test trace-123 correlation
  let trace_123 = correlated_signals.find(fn(cs) { cs.trace_id == "trace-123" })
  assert_true(trace_123.is_some())
  
  match trace_123 {
    Some(cs) => {
      assert_eq(cs.spans.length(), 2)
      assert_eq(cs.logs.length(), 2)
      assert_eq(cs.metrics.length(), 2)
      assert_eq(cs.errors.length(), 0)  // No errors in this trace
    }
    None => assert_true(false)
  }
  
  // Test trace-456 correlation
  let trace_456 = correlated_signals.find(fn(cs) { cs.trace_id == "trace-456" })
  assert_true(trace_456.is_some())
  
  match trace_456 {
    Some(cs) => {
      assert_eq(cs.spans.length(), 1)
      assert_eq(cs.logs.length(), 1)
      assert_eq(cs.metrics.length(), 1)
      assert_eq(cs.errors.length(), 1)  // One error in this trace
    }
    None => assert_true(false)
  }
  
  // Test correlation analysis
  let analyze_correlation = fn(correlated: CorrelatedSignals) {
    let total_duration = correlated.spans.reduce(fn(acc, span) { acc + span.duration_ms }, 0)
    let error_count = correlated.errors.length()
    let log_levels = correlated.logs.map(fn(log) { log.severity })
    let error_logs = log_levels.filter(fn(level) { level == "ERROR" }).length()
    
    {
      total_spans: correlated.spans.length(),
      total_duration_ms: total_duration,
      has_errors: error_count > 0,
      error_count: error_count,
      error_log_count: error_logs,
      services_involved: correlated.spans.map(fn(s) { s.service_name }).unique()
    }
  }
  
  let analysis_123 = analyze_correlation(trace_123.unwrap())
  assert_eq(analysis_123.total_spans, 2)
  assert_eq(analysis_123.total_duration_ms, 230)  // 150 + 80
  assert_false(analysis_123.has_errors)
  assert_eq(analysis_123.error_count, 0)
  assert_eq(analysis_123.services_involved.length(), 2)
  
  let analysis_456 = analyze_correlation(trace_456.unwrap())
  assert_eq(analysis_456.total_spans, 1)
  assert_eq(analysis_456.total_duration_ms, 1200)
  assert_true(analysis_456.has_errors)
  assert_eq(analysis_456.error_count, 1)
  assert_eq(analysis_456.error_log_count, 1)
  assert_eq(analysis_456.services_involved.length(), 1)
}

// Test 7: Adaptive Telemetry Sampling
test "adaptive telemetry sampling with dynamic rate adjustment" {
  // Define sampling strategy
  type SamplingStrategy = {
    base_rate: Float,
    min_rate: Float,
    max_rate: Float,
    adjustment_factor: Float,
    error_threshold: Float,
    latency_threshold: Int
  }
  
  type SamplingDecision = {
    sampled: Bool,
    rate: Float,
    reason: String
  }
  
  type SystemMetrics = {
    error_rate: Float,
    avg_latency_ms: Int,
    throughput: Int,
    resource_usage: Float
  }
  
  // Create adaptive sampler
  let create_adaptive_sampler = fn(strategy: SamplingStrategy) {
    let mut current_rate = strategy.base_rate
    
    fn(trace_id: String, system_metrics: SystemMetrics) -> SamplingDecision {
      // Adjust rate based on system metrics
      let adjusted_rate = if system_metrics.error_rate > strategy.error_threshold {
        // Increase sampling during high error rates
        let new_rate = current_rate * (1.0 + strategy.adjustment_factor)
        if new_rate > strategy.max_rate { strategy.max_rate } else { new_rate }
      } else if system_metrics.avg_latency_ms > strategy.latency_threshold {
        // Increase sampling during high latency
        let new_rate = current_rate * (1.0 + strategy.adjustment_factor)
        if new_rate > strategy.max_rate { strategy.max_rate } else { new_rate }
      } else if system_metrics.resource_usage > 0.8 {
        // Decrease sampling during high resource usage
        let new_rate = current_rate * (1.0 - strategy.adjustment_factor)
        if new_rate < strategy.min_rate { strategy.min_rate } else { new_rate }
      } else {
        // Gradually return to base rate
        let adjustment = (strategy.base_rate - current_rate) * 0.1
        current_rate + adjustment
      }
      
      current_rate = adjusted_rate
      
      // Make sampling decision
      let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      let normalized = (hash % 1000) as Float / 1000.0
      
      let sampled = normalized <= current_rate
      let reason = if sampled {
        "Sampled at rate " + current_rate.to_string()
      } else {
        "Not sampled at rate " + current_rate.to_string()
      }
      
      {
        sampled,
        rate: current_rate,
        reason
      }
    }
  }
  
  // Test adaptive sampling behavior
  let strategy = {
    base_rate: 0.1,      // 10% base sampling
    min_rate: 0.01,      // 1% minimum
    max_rate: 0.5,       // 50% maximum
    adjustment_factor: 0.5,  // 50% adjustment
    error_threshold: 0.05,  // 5% error rate threshold
    latency_threshold: 500   // 500ms latency threshold
  }
  
  let sampler = create_adaptive_sampler(strategy)
  
  // Test under normal conditions
  let normal_metrics = {
    error_rate: 0.01,
    avg_latency_ms: 100,
    throughput: 1000,
    resource_usage: 0.4
  }
  
  let normal_decision1 = sampler("trace-001", normal_metrics)
  let normal_decision2 = sampler("trace-002", normal_metrics)
  let normal_decision3 = sampler("trace-003", normal_metrics)
  
  // Should sample around base rate (10%)
  let normal_sampled_count = [normal_decision1, normal_decision2, normal_decision3]
    .filter(fn(d) { d.sampled })
    .length()
  
  assert_true(normal_sampled_count >= 0 && normal_sampled_count <= 3)
  
  // Test under high error conditions
  let high_error_metrics = {
    error_rate: 0.1,  // 10% error rate (above threshold)
    avg_latency_ms: 100,
    throughput: 1000,
    resource_usage: 0.4
  }
  
  let error_decision1 = sampler("trace-004", high_error_metrics)
  let error_decision2 = sampler("trace-005", high_error_metrics)
  let error_decision3 = sampler("trace-006", high_error_metrics)
  
  // Rate should increase due to high error rate
  assert_true(error_decision1.rate > normal_decision1.rate)
  assert_true(error_decision2.rate > normal_decision2.rate)
  assert_true(error_decision3.rate > normal_decision3.rate)
  
  // Test under high resource usage
  let high_resource_metrics = {
    error_rate: 0.01,
    avg_latency_ms: 100,
    throughput: 1000,
    resource_usage: 0.9  // 90% resource usage
  }
  
  let resource_decision1 = sampler("trace-007", high_resource_metrics)
  let resource_decision2 = sampler("trace-008", high_resource_metrics)
  let resource_decision3 = sampler("trace-009", high_resource_metrics)
  
  // Rate should decrease due to high resource usage
  assert_true(resource_decision1.rate < error_decision1.rate)
  assert_true(resource_decision2.rate < error_decision2.rate)
  assert_true(resource_decision3.rate < error_decision3.rate)
  
  // Test rate boundaries
  assert_true(resource_decision1.rate >= strategy.min_rate)
  assert_true(resource_decision1.rate <= strategy.max_rate)
  
  // Test sustained conditions (rate should stabilize)
  let stabilized_decision = sampler("trace-010", high_resource_metrics)
  assert_true(stabilized_decision.rate >= strategy.min_rate)
  assert_true(stabilized_decision.rate <= strategy.max_rate)
  
  // Test gradual recovery
  let recovery_metrics = {
    error_rate: 0.01,
    avg_latency_ms: 100,
    throughput: 1000,
    resource_usage: 0.4
  }
  
  let recovery_decision1 = sampler("trace-011", recovery_metrics)
  let recovery_decision2 = sampler("trace-012", recovery_metrics)
  let recovery_decision3 = sampler("trace-013", recovery_metrics)
  
  // Rate should gradually move toward base rate
  assert_true(recovery_decision3.rate > resource_decision3.rate)
}

// Test 8: Telemetry Pipeline Performance Optimization
test "telemetry pipeline performance optimization with batching" {
  // Define pipeline configuration
  type PipelineConfig = {
    batch_size: Int,
    batch_timeout_ms: Int,
    max_concurrent_batches: Int,
    compression_enabled: Bool,
    prioritization_enabled: Bool
  }
  
  type TelemetryItem = {
    id: String,
    timestamp: Int,
    priority: String,  // "high", "medium", "low"
    size_bytes: Int,
    processing_time_ms: Int
  }
  
  type BatchMetrics = {
    item_count: Int,
    total_size_bytes: Int,
    processing_time_ms: Int,
    compression_ratio: Option[Float]
  }
  
  // Create batch processor
  let create_batch_processor = fn(config: PipelineConfig) {
    let mut pending_items = []
    let mut batch_start_time = 0
    
    fn(item: TelemetryItem, current_time: Int) -> (Array[TelemetryItem], BatchMetrics) {
      pending_items = pending_items.push(item)
      
      if batch_start_time == 0 {
        batch_start_time = current_time
      }
      
      let batch_ready = pending_items.length() >= config.batch_size or 
                       (current_time - batch_start_time) >= config.batch_timeout_ms
      
      if batch_ready {
        // Sort by priority if enabled
        let sorted_items = if config.prioritization_enabled {
          pending_items.sort(fn(a, b) {
            let priority_order = ["high", "medium", "low"]
            let a_index = priority_order.find_index(fn(p) { p == a.priority }).unwrap_or(999)
            let b_index = priority_order.find_index(fn(p) { p == b.priority }).unwrap_or(999)
            
            if a_index < b_index { -1 } else if a_index > b_index { 1 } else { 0 }
          })
        } else {
          pending_items
        }
        
        let total_size = sorted_items.reduce(fn(acc, item) { acc + item.size_bytes }, 0)
        let processing_time = sorted_items.reduce(fn(acc, item) { acc + item.processing_time_ms }, 0)
        
        // Simulate compression
        let compression_ratio = if config.compression_enabled {
          Some(0.7)  // 30% compression
        } else {
          None
        }
        
        let metrics = {
          item_count: sorted_items.length(),
          total_size_bytes: total_size,
          processing_time_ms: processing_time,
          compression_ratio
        }
        
        // Reset for next batch
        pending_items = []
        batch_start_time = 0
        
        (sorted_items, metrics)
      } else {
        ([], {
          item_count: 0,
          total_size_bytes: 0,
          processing_time_ms: 0,
          compression_ratio: None
        })
      }
    }
  }
  
  // Test pipeline configuration
  let config = {
    batch_size: 5,
    batch_timeout_ms: 1000,
    max_concurrent_batches: 3,
    compression_enabled: true,
    prioritization_enabled: true
  }
  
  let processor = create_batch_processor(config)
  
  // Create test items with varying priorities
  let test_items = [
    { id: "item-1", timestamp: 1000, priority: "low", size_bytes: 100, processing_time_ms: 10 },
    { id: "item-2", timestamp: 1000, priority: "high", size_bytes: 150, processing_time_ms: 20 },
    { id: "item-3", timestamp: 1000, priority: "medium", size_bytes: 120, processing_time_ms: 15 },
    { id: "item-4", timestamp: 1000, priority: "high", size_bytes: 200, processing_time_ms: 25 },
    { id: "item-5", timestamp: 1000, priority: "low", size_bytes: 80, processing_time_ms: 8 },
    { id: "item-6", timestamp: 2000, priority: "medium", size_bytes: 130, processing_time_ms: 18 }
  ]
  
  // Process items and collect batches
  let mut all_batches = []
  let mut all_metrics = []
  
  for item in test_items {
    let (batch, metrics) = processor(item, item.timestamp)
    
    if batch.length() > 0 {
      all_batches = all_batches.push(batch)
      all_metrics = all_metrics.push(metrics)
    }
  }
  
  // Should have created one batch with first 5 items
  assert_eq(all_batches.length(), 1)
  assert_eq(all_metrics.length(), 1)
  
  let batch = all_batches[0]
  let metrics = all_metrics[0]
  
  // Verify batch size
  assert_eq(batch.length(), 5)
  assert_eq(metrics.item_count, 5)
  
  // Verify prioritization (high priority items should come first)
  assert_eq(batch[0].priority, "high")  // item-2
  assert_eq(batch[1].priority, "high")  // item-4
  assert_eq(batch[2].priority, "medium")  // item-3
  assert_eq(batch[3].priority, "low")   // item-1
  assert_eq(batch[4].priority, "low")   // item-5
  
  // Verify batch metrics
  assert_eq(metrics.total_size_bytes, 100 + 150 + 120 + 200 + 80)
  assert_eq(metrics.processing_time_ms, 10 + 20 + 15 + 25 + 8)
  assert_eq(metrics.compression_ratio, Some(0.7))
  
  // Test timeout-based batching
  let timeout_item = { id: "item-7", timestamp: 3000, priority: "low", size_bytes: 90, processing_time_ms: 12 }
  let (timeout_batch, timeout_metrics) = processor(timeout_item, timeout_item.timestamp)
  
  // Should create a batch due to timeout (item-6 + item-7)
  assert_eq(timeout_batch.length(), 2)
  assert_eq(timeout_metrics.item_count, 2)
  
  // Test performance optimization
  let calculate_throughput = fn(metrics: Array[BatchMetrics>, time_window_ms: Int) {
    let total_items = metrics.reduce(fn(acc, m) { acc + m.item_count }, 0)
    let total_size = metrics.reduce(fn(acc, m) { acc + m.total_size_bytes }, 0)
    
    {
      items_per_second: (total_items * 1000) / time_window_ms,
      bytes_per_second: (total_size * 1000) / time_window_ms,
      avg_processing_time_ms: if metrics.length() > 0 {
        metrics.reduce(fn(acc, m) { acc + m.processing_time_ms }, 0) / metrics.length()
      } else {
        0
      }
    }
  }
  
  let all_batch_metrics = all_metrics + [timeout_metrics]
  let performance = calculate_throughput(all_batch_metrics, 2000)  // 2 second window
  
  assert_true(performance.items_per_second > 0)
  assert_true(performance.bytes_per_second > 0)
  assert_true(performance.avg_processing_time_ms > 0)
  
  // Test compression effectiveness
  let total_compressed_size = all_batch_metrics
    .filter(fn(m) { m.compression_ratio.is_some() })
    .reduce(fn(acc, m) { 
      let ratio = m.compression_ratio.unwrap_or(1.0)
      acc + (m.total_size_bytes as Float * ratio) as Int
    }, 0)
  
  let total_original_size = all_batch_metrics
    .filter(fn(m) { m.compression_ratio.is_some() })
    .reduce(fn(acc, m) { acc + m.total_size_bytes }, 0)
  
  assert_true(total_compressed_size < total_original_size)
}

// Test 9: Telemetry Data Validation and Schema Enforcement
test "telemetry data validation and schema enforcement" {
  // Define validation schema
  type ValidationSchema = {
    required_fields: Array[String],
    field_types: Array[(String, String)],  // (field_name, type_name)
    field_constraints: Array[(String, String)]  // (field_name, constraint)
  }
  
  type ValidationError = {
    field: String,
    message: String,
    severity: String  // "error", "warning"
  }
  
  type ValidationResult = {
    valid: Bool,
    errors: Array[ValidationError],
    warnings: Array[ValidationError]
  }
  
  // Create validator function
  let create_validator = fn(schema: ValidationSchema) {
    fn(data: Array[(String, String)>) -> ValidationResult {
      let mut errors = []
      let mut warnings = []
      
      // Check required fields
      for field in schema.required_fields {
        let field_present = data.any(fn(item) { item.0 == field })
        if not(field_present) {
          errors = errors.push({
            field,
            message: "Required field is missing",
            severity: "error"
          })
        }
      }
      
      // Check field types
      for (field_name, expected_type) in schema.field_types {
        match data.find(fn(item) { item.0 == field_name }) {
          Some((_, value)) => {
            let type_valid = match expected_type {
              "string" => true,  // All values are strings in this simplified model
              "int" => {
                value.chars().all(fn(c) { c >= '0' and c <= '9' })
              }
              "float" => {
                let parts = value.split(".")
                parts.length() == 2 and 
                parts[0].chars().all(fn(c) { c >= '0' and c <= '9' }) and
                parts[1].chars().all(fn(c) { c >= '0' and c <= '9' })
              }
              "boolean" => value == "true" or value == "false",
              _ => false
            }
            
            if not(type_valid) {
              errors = errors.push({
                field: field_name,
                message: "Invalid type for field, expected " + expected_type,
                severity: "error"
              })
            }
          }
          None => {}  // Missing field already caught by required fields check
        }
      }
      
      // Check field constraints
      for (field_name, constraint) in schema.field_constraints {
        match data.find(fn(item) { item.0 == field_name }) {
          Some((_, value)) => {
            let constraint_valid = match constraint {
              s if s.starts_with("min_length:") => {
                let min_length = s.substring(11, s.length() - 11).to_int()
                value.length() >= min_length
              }
              s if s.starts_with("max_length:") => {
                let max_length = s.substring(11, s.length() - 11).to_int()
                value.length() <= max_length
              }
              s if s.starts_with("pattern:") => {
                let pattern = s.substring(8, s.length() - 8)
                // Simplified pattern matching
                if pattern == "trace_id" {
                  value.length() == 11 and value.starts_with("trace-")
                } else if pattern == "span_id" {
                  value.length() == 9 and value.starts_with("span-")
                } else {
                  false
                }
              }
              _ => false
            }
            
            if not(constraint_valid) {
              warnings = warnings.push({
                field: field_name,
                message: "Field does not satisfy constraint: " + constraint,
                severity: "warning"
              })
            }
          }
          None => {}  // Missing field already caught by required fields check
        }
      }
      
      {
        valid: errors.length() == 0,
        errors,
        warnings
      }
    }
  }
  
  // Create telemetry schema
  let telemetry_schema = {
    required_fields: ["trace_id", "span_id", "service_name", "operation_name"],
    field_types: [
      ("trace_id", "string"),
      ("span_id", "string"),
      ("service_name", "string"),
      ("operation_name", "string"),
      ("start_time", "int"),
      ("duration_ms", "int"),
      ("success", "boolean")
    ],
    field_constraints: [
      ("trace_id", "pattern:trace_id"),
      ("span_id", "pattern:span_id"),
      ("service_name", "min_length:1"),
      ("service_name", "max_length:50"),
      ("operation_name", "min_length:1"),
      ("operation_name", "max_length:100")
    ]
  }
  
  let validator = create_validator(telemetry_schema)
  
  // Test valid telemetry data
  let valid_data = [
    ("trace_id", "trace-abc123"),
    ("span_id", "span-def456"),
    ("service_name", "api-service"),
    ("operation_name", "process_request"),
    ("start_time", "1640995200"),
    ("duration_ms", "150"),
    ("success", "true")
  ]
  
  let valid_result = validator(valid_data)
  assert_true(valid_result.valid)
  assert_eq(valid_result.errors.length(), 0)
  assert_eq(valid_result.warnings.length(), 0)
  
  // Test missing required field
  let missing_field_data = [
    ("span_id", "span-def456"),
    ("service_name", "api-service"),
    ("operation_name", "process_request")
  ]
  
  let missing_field_result = validator(missing_field_data)
  assert_false(missing_field_result.valid)
  assert_eq(missing_field_result.errors.length(), 1)
  assert_eq(missing_field_result.errors[0].field, "trace_id")
  assert_eq(missing_field_result.errors[0].message, "Required field is missing")
  
  // Test invalid type
  let invalid_type_data = [
    ("trace_id", "trace-abc123"),
    ("span_id", "span-def456"),
    ("service_name", "api-service"),
    ("operation_name", "process_request"),
    ("start_time", "not_a_number"),  // Invalid type
    ("duration_ms", "150"),
    ("success", "true")
  ]
  
  let invalid_type_result = validator(invalid_type_data)
  assert_false(invalid_type_result.valid)
  assert_eq(invalid_type_result.errors.length(), 1)
  assert_eq(invalid_type_result.errors[0].field, "start_time")
  assert_true(invalid_type_result.errors[0].message.contains("Invalid type"))
  
  // Test constraint violations
  let constraint_violation_data = [
    ("trace_id", "invalid_trace"),  // Wrong pattern
    ("span_id", "span-def456"),
    ("service_name", ""),  // Too short
    ("operation_name", "process_request")
  ]
  
  let constraint_result = validator(constraint_violation_data)
  assert_false(constraint_result.valid)
  assert_eq(constraint_result.errors.length(), 1)  // Missing required fields
  assert_eq(constraint_result.warnings.length(), 2)  // Constraint violations
  
  let warning_fields = constraint_result.warnings.map(fn(w) { w.field })
  assert_true(warning_fields.contains("trace_id"))
  assert_true(warning_fields.contains("service_name"))
  
  // Test partial validation (some valid, some invalid)
  let partial_data = [
    ("trace_id", "trace-abc123"),
    ("span_id", "span-def456"),
    ("service_name", "api-service"),
    ("operation_name", "process_request"),
    ("start_time", "1640995200"),  // Valid
    ("duration_ms", "invalid"),     // Invalid type
    ("success", "maybe")            // Invalid type
  ]
  
  let partial_result = validator(partial_data)
  assert_false(partial_result.valid)
  assert_eq(partial_result.errors.length(), 2)  // Two type errors
  assert_eq(partial_result.warnings.length(), 0)
  
  // Test validation statistics
  let calculate_validation_stats = fn(results: Array[ValidationResult]) {
    let total = results.length()
    let valid_count = results.filter(fn(r) { r.valid }).length()
    let total_errors = results.reduce(fn(acc, r) { acc + r.errors.length() }, 0)
    let total_warnings = results.reduce(fn(acc, r) { acc + r.warnings.length() }, 0)
    
    {
      total_validated: total,
      valid_count: valid_count,
      invalid_count: total - valid_count,
      validation_rate: if total > 0 { (valid_count * 100) / total } else { 0 },
      total_errors: total_errors,
      total_warnings: total_warnings
    }
  }
  
  let all_results = [
    valid_result,
    missing_field_result,
    invalid_type_result,
    constraint_result,
    partial_result
  ]
  
  let stats = calculate_validation_stats(all_results)
  assert_eq(stats.total_validated, 5)
  assert_eq(stats.valid_count, 1)
  assert_eq(stats.invalid_count, 4)
  assert_eq(stats.validation_rate, 20)
  assert_eq(stats.total_errors, 5)
  assert_eq(stats.total_warnings, 2)
}

// Test 10: Telemetry Security and Privacy Controls
test "telemetry security and privacy controls with data masking" {
  // Define privacy policy
  type PrivacyPolicy = {
    sensitive_fields: Array[String],
    masking_rules: Array[(String, String)],  // (field_pattern, masking_strategy)
    retention_days: Int,
    anonymization_enabled: Bool
  }
  
  type SecurityContext = {
    authorized_roles: Array[String],
    data_classification: String,  // "public", "internal", "confidential"
    encryption_required: Bool,
    audit_logging: Bool
  }
  
  // Create privacy processor
  let create_privacy_processor = fn(policy: PrivacyPolicy) {
    let mask_value = fn(value: String, strategy: String) {
      match strategy {
        "full_mask" => "***"
        "partial_mask" => {
          if value.length() <= 4 {
            "***"
          } else {
            value.substring(0, 2) + "***" + value.substring(value.length() - 2, 2)
          }
        }
        "email_mask" => {
          let parts = value.split("@")
          if parts.length() == 2 {
            let username = parts[0]
            let domain = parts[1]
            let masked_username = if username.length() <= 2 {
              "***"
            } else {
              username.substring(0, 1) + "***" + username.substring(username.length() - 1, 1)
            }
            masked_username + "@" + domain
          } else {
            "***"
          }
        }
        "hash_mask" => {
          // Simplified hash simulation
          let hash = value.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
          "hash_" + (hash % 10000).to_string()
        }
        _ => value
      }
    }
    
    fn(attributes: Array[(String, String)], user_role: String) -> Array[(String, String)] {
      attributes.map(fn(attr) {
        let (key, value) = attr
        
        // Check if field is sensitive
        let is_sensitive = policy.sensitive_fields.any(fn(field) { 
          key.contains(field) or key == field 
        })
        
        if is_sensitive {
          // Find applicable masking rule
          let masking_rule = policy.masking_rules.find(fn(rule) { 
            key.contains(rule.0) or key == rule.0 
          })
          
          match masking_rule {
            Some((_, strategy)) => {
              (key, mask_value(value, strategy))
            }
            None => {
              (key, mask_value(value, "partial_mask"))
            }
          }
        } else {
          (key, value)
        }
      })
    }
  }
  
  // Create security validator
  let create_security_validator = fn(security_context: SecurityContext) {
    fn(attributes: Array[(String, String)>, user_role: String) -> (Bool, String) {
      // Check authorization
      let authorized = security_context.authorized_roles.contains(user_role)
      
      if not(authorized) {
        return (false, "User not authorized for this data classification")
      }
      
      // Check data classification requirements
      match security_context.data_classification {
        "confidential" => {
          if not(security_context.encryption_required) {
            return (false, "Encryption required for confidential data")
          }
          
          if not(security_context.audit_logging) {
            return (false, "Audit logging required for confidential data")
          }
        }
        "internal" => {
          if not(security_context.audit_logging) {
            return (false, "Audit logging required for internal data")
          }
        }
        "public" => {}  // No additional requirements
        _ => {
          return (false, "Unknown data classification")
        }
      }
      
      // Check for prohibited data
      let prohibited_patterns = ["password", "secret", "token", "key"]
      for (key, _) in attributes {
        if prohibited_patterns.any(fn(pattern) { key.contains(pattern) }) {
          return (false, "Prohibited field detected: " + key)
        }
      }
      
      (true, "Access granted")
    }
  }
  
  // Create privacy policy
  let privacy_policy = {
    sensitive_fields: ["email", "user_id", "phone", "ssn", "credit_card"],
    masking_rules: [
      ("email", "email_mask"),
      ("user_id", "hash_mask"),
      ("phone", "partial_mask"),
      ("ssn", "full_mask"),
      ("credit_card", "partial_mask")
    ],
    retention_days: 30,
    anonymization_enabled: true
  }
  
  // Create security context
  let security_context = {
    authorized_roles: ["admin", "analyst", "viewer"],
    data_classification: "confidential",
    encryption_required: true,
    audit_logging: true
  }
  
  let privacy_processor = create_privacy_processor(privacy_policy)
  let security_validator = create_security_validator(security_context)
  
  // Create test telemetry data with sensitive information
  let sensitive_data = [
    ("trace_id", "trace-abc123"),
    ("service_name", "user-service"),
    ("operation_name", "authenticate_user"),
    ("email", "john.doe@example.com"),
    ("user_id", "user-789456"),
    ("phone", "+1-555-123-4567"),
    ("ssn", "123-45-6789"),
    ("credit_card", "4532-1234-5678-9012"),
    ("request_id", "req-123456"),
    ("session_id", "sess-789012")
  ]
  
  // Test security validation
  let (admin_authorized, admin_message) = security_validator(sensitive_data, "admin")
  assert_true(admin_authorized)
  assert_eq(admin_message, "Access granted")
  
  let (viewer_authorized, viewer_message) = security_validator(sensitive_data, "viewer")
  assert_true(viewer_authorized)
  assert_eq(viewer_message, "Access granted")
  
  let (unauthorized, unauthorized_message) = security_validator(sensitive_data, "guest")
  assert_false(unauthorized)
  assert_eq(unauthorized_message, "User not authorized for this data classification")
  
  // Test privacy masking for different user roles
  let admin_masked_data = privacy_processor(sensitive_data, "admin")
  let viewer_masked_data = privacy_processor(sensitive_data, "viewer")
  
  // Admin should see masked data
  let admin_email = admin_masked_data.find(fn(item) { item.0 == "email" })
  assert_true(admin_email.is_some())
  assert_eq(admin_email.unwrap().1, "j***n@example.com")
  
  let admin_user_id = admin_masked_data.find(fn(item) { item.0 == "user_id" })
  assert_true(admin_user_id.is_some())
  assert_true(admin_user_id.unwrap().1.starts_with("hash_"))
  
  let admin_ssn = admin_masked_data.find(fn(item) { item.0 == "ssn" })
  assert_true(admin_ssn.is_some())
  assert_eq(admin_ssn.unwrap().1, "***")
  
  // Viewer should see the same masked data (masking is role-independent in this simplified model)
  let viewer_email = viewer_masked_data.find(fn(item) { item.0 == "email" })
  assert_true(viewer_email.is_some())
  assert_eq(viewer_email.unwrap().1, "j***n@example.com")
  
  // Non-sensitive fields should remain unchanged
  let admin_trace_id = admin_masked_data.find(fn(item) { item.0 == "trace_id" })
  assert_true(admin_trace_id.is_some())
  assert_eq(admin_trace_id.unwrap().1, "trace-abc123")
  
  let admin_service_name = admin_masked_data.find(fn(item) { item.0 == "service_name" })
  assert_true(admin_service_name.is_some())
  assert_eq(admin_service_name.unwrap().1, "user-service")
  
  // Test data retention simulation
  let check_retention = fn(timestamp: Int, current_time: Int, policy: PrivacyPolicy) {
    let age_days = (current_time - timestamp) / (24 * 60 * 60)
    age_days <= policy.retention_days
  }
  
  let current_time = 1640995200  // Example timestamp
  let old_timestamp = current_time - (40 * 24 * 60 * 60)  // 40 days ago
  
  assert_true(check_retention(current_time, current_time, privacy_policy))  // Current data
  assert_false(check_retention(old_timestamp, current_time, privacy_policy))  // Old data
  
  // Test anonymization
  let anonymize_data = fn(attributes: Array[(String, String)>, policy: PrivacyPolicy) {
    if policy.anonymization_enabled {
      attributes.map(fn(attr) {
        let (key, value) = attr
        
        // Replace user identifiers with anonymous equivalents
        if key.contains("user_id") or key.contains("session_id") {
          let hash = value.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
          (key, "anon_" + (hash % 100000).to_string())
        } else {
          (key, value)
        }
      })
    } else {
      attributes
    }
  }
  
  let anonymized_data = anonymize_data(admin_masked_data, privacy_policy)
  let anon_user_id = anonymized_data.find(fn(item) { item.0 == "user_id" })
  assert_true(anon_user_id.is_some())
  assert_true(anon_user_id.unwrap().1.starts_with("anon_"))
  
  let anon_session_id = anonymized_data.find(fn(item) { item.0 == "session_id" })
  assert_true(anon_session_id.is_some())
  assert_true(anon_session_id.unwrap().1.starts_with("anon_"))
  
  // Test security audit logging simulation
  let audit_log = { mut entries: [] }
  
  let log_access = fn(user_role: String, data: Array[(String, String)>, authorized: Bool) {
    let timestamp = 1640995200
    let data_class = "confidential"
    let field_count = data.length()
    
    audit_log.entries = audit_log.entries.push({
      timestamp,
      user_role,
      data_class,
      field_count,
      authorized
    })
  }
  
  log_access("admin", sensitive_data, true)
  log_access("guest", sensitive_data, false)
  
  assert_eq(audit_log.entries.length(), 2)
  assert_eq(audit_log.entries[0].user_role, "admin")
  assert_eq(audit_log.entries[0].authorized, true)
  assert_eq(audit_log.entries[1].user_role, "guest")
  assert_eq(audit_log.entries[1].authorized, false)
}
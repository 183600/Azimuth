// Azimuth Enhanced MoonBit Test Suite
// This file contains enhanced test cases for the Azimuth telemetry system

// Test 1: Telemetry Span Creation and Management
test "telemetry span creation and lifecycle management" {
  // Define a Span type for telemetry
  type Span = {
    name: String,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    start_time: Int,
    end_time: Option[Int],
    status: String,
    attributes: Array[(String, String)]
  }
  
  // Create a root span
  let root_span = {
    name: "http_request",
    trace_id: "trace-abc123",
    span_id: "span-def456",
    parent_span_id: None,
    start_time: 1640995200,
    end_time: None,
    status: "running",
    attributes: [
      ("http.method", "GET"),
      ("http.url", "/api/users"),
      ("http.status_code", "200")
    ]
  }
  
  // Verify root span properties
  assert_eq(root_span.name, "http_request")
  assert_eq(root_span.trace_id, "trace-abc123")
  assert_eq(root_span.span_id, "span-def456")
  assert_eq(root_span.parent_span_id, None)
  assert_eq(root_span.status, "running")
  assert_eq(root_span.attributes.length(), 3)
  
  // Create a child span
  let child_span = {
    name: "database_query",
    trace_id: "trace-abc123",
    span_id: "span-ghi789",
    parent_span_id: Some("span-def456"),
    start_time: 1640995201,
    end_time: None,
    status: "running",
    attributes: [
      ("db.statement", "SELECT * FROM users"),
      ("db.type", "postgresql")
    ]
  }
  
  // Verify child span properties
  assert_eq(child_span.name, "database_query")
  assert_eq(child_span.trace_id, "trace-abc123")  // Same trace as parent
  assert_eq(child_span.parent_span_id, Some("span-def456"))
  
  // Complete the child span
  let completed_child = { child_span | 
    end_time: Some(1640995203),
    status: "completed"
  }
  
  // Verify child span completion
  assert_eq(completed_child.end_time, Some(1640995203))
  assert_eq(completed_child.status, "completed")
  
  // Calculate span duration
  let calculate_duration = fn(span: Span) {
    match span.end_time {
      Some(end) => end - span.start_time
      None => 0  // Still running
    }
  }
  
  let child_duration = calculate_duration(completed_child)
  assert_eq(child_duration, 2)
  
  // Complete the root span
  let completed_root = { root_span | 
    end_time: Some(1640995205),
    status: "completed"
  }
  
  let root_duration = calculate_duration(completed_root)
  assert_eq(root_duration, 5)
}

// Test 2: Metric Collection and Aggregation
test "metric collection and aggregation operations" {
  // Define a Metric type
  type Metric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // Create metrics
  let metrics = [
    {
      name: "request_duration",
      value: 120.5,
      unit: "ms",
      timestamp: 1640995200,
      tags: [("endpoint", "/api/users"), ("method", "GET")]
    },
    {
      name: "request_duration",
      value: 85.2,
      unit: "ms",
      timestamp: 1640995201,
      tags: [("endpoint", "/api/users"), ("method", "GET")]
    },
    {
      name: "request_duration",
      value: 200.0,
      unit: "ms",
      timestamp: 1640995202,
      tags: [("endpoint", "/api/orders"), ("method", "POST")]
    },
    {
      name: "error_rate",
      value: 0.05,
      unit: "ratio",
      timestamp: 1640995200,
      tags: [("service", "api")]
    }
  ]
  
  // Filter metrics by name
  let filter_by_name = fn(metrics: Array[Metric], name: String) {
    let mut result = []
    for metric in metrics {
      if metric.name == name {
        result = result.push(metric)
      }
    }
    result
  }
  
  let duration_metrics = filter_by_name(metrics, "request_duration")
  assert_eq(duration_metrics.length(), 3)
  
  // Calculate average value
  let calculate_average = fn(values: Array[Float]) {
    let mut sum = 0.0
    for value in values {
      sum = sum + value
    }
    sum / values.length().to_float()
  }
  
  let duration_values = duration_metrics.map(fn(m) { m.value })
  let avg_duration = calculate_average(duration_values)
  assert_true(avg_duration > 130.0 and avg_duration < 140.0)
  
  // Find min and max values
  let find_min = fn(values: Array[Float]) {
    let mut min = values[0]
    for value in values {
      if value < min {
        min = value
      }
    }
    min
  }
  
  let find_max = fn(values: Array[Float]) {
    let mut max = values[0]
    for value in values {
      if value > max {
        max = value
      }
    }
    max
  }
  
  let min_duration = find_min(duration_values)
  let max_duration = find_max(duration_values)
  assert_eq(min_duration, 85.2)
  assert_eq(max_duration, 200.0)
  
  // Group metrics by tags
  let group_by_tag = fn(metrics: Array[Metric], tag_key: String) {
    let mut groups = []
    let mut processed_keys = []
    
    for metric in metrics {
      for (key, value) in metric.tags {
        if key == tag_key and not(processed_keys.contains(value)) {
          processed_keys = processed_keys.push(value)
          
          let mut group_metrics = []
          for m in metrics {
            for (k, v) in m.tags {
              if k == tag_key and v == value {
                group_metrics = group_metrics.push(m)
              }
            }
          }
          
          groups = groups.push((value, group_metrics))
        }
      }
    }
    
    groups
  }
  
  let grouped_by_endpoint = group_by_tag(metrics, "endpoint")
  assert_eq(grouped_by_endpoint.length(), 2)
  
  // Verify groups
  let users_group = grouped_by_endpoint.filter(fn(g) { g.0 == "/api/users" })[0]
  assert_eq(users_group.1.length(), 2)
  
  let orders_group = grouped_by_endpoint.filter(fn(g) { g.0 == "/api/orders" })[0]
  assert_eq(orders_group.1.length(), 1)
}

// Test 3: Log Event Processing and Correlation
test "log event processing and correlation" {
  // Define a LogEvent type
  type LogEvent = {
    timestamp: Int,
    level: String,
    message: String,
    trace_id: Option[String],
    span_id: Option[String],
    attributes: Array[(String, String)]
  }
  
  // Create log events
  let log_events = [
    {
      timestamp: 1640995200,
      level: "INFO",
      message: "Request received",
      trace_id: Some("trace-123"),
      span_id: Some("span-456"),
      attributes: [("request_id", "req-789")]
    },
    {
      timestamp: 1640995201,
      level: "DEBUG",
      message: "Processing request",
      trace_id: Some("trace-123"),
      span_id: Some("span-456"),
      attributes: [("step", "validation")]
    },
    {
      timestamp: 1640995202,
      level: "WARN",
      message: "Slow database query detected",
      trace_id: Some("trace-123"),
      span_id: Some("span-789"),
      attributes: [("duration_ms", "1500")]
    },
    {
      timestamp: 1640995203,
      level: "ERROR",
      message: "Database connection failed",
      trace_id: Some("trace-456"),
      span_id: Some("span-111"),
      attributes: [("error_code", "CONN_TIMEOUT")]
    }
  ]
  
  // Filter logs by trace ID
  let filter_by_trace = fn(logs: Array[LogEvent], trace_id: String) {
    let mut result = []
    for log in logs {
      match log.trace_id {
        Some(id) => if id == trace_id { result = result.push(log) }
        None => {}
      }
    }
    result
  }
  
  let trace_123_logs = filter_by_trace(log_events, "trace-123")
  assert_eq(trace_123_logs.length(), 3)
  
  let trace_456_logs = filter_by_trace(log_events, "trace-456")
  assert_eq(trace_456_logs.length(), 1)
  
  // Filter logs by level
  let filter_by_level = fn(logs: Array[LogEvent], level: String) {
    let mut result = []
    for log in logs {
      if log.level == level {
        result = result.push(log)
      }
    }
    result
  }
  
  let error_logs = filter_by_level(log_events, "ERROR")
  assert_eq(error_logs.length(), 1)
  assert_eq(error_logs[0].message, "Database connection failed")
  
  // Count logs by level
  let count_by_level = fn(logs: Array[LogEvent]) {
    let mut counts = []
    let mut processed_levels = []
    
    for log in logs {
      if not(processed_levels.contains(log.level)) {
        processed_levels = processed_levels.push(log.level)
        let count = filter_by_level(logs, log.level).length()
        counts = counts.push((log.level, count))
      }
    }
    
    counts
  }
  
  let level_counts = count_by_level(log_events)
  assert_eq(level_counts.length(), 4)
  
  // Verify specific counts
  let info_count = level_counts.filter(fn(c) { c.0 == "INFO" })[0].1
  let debug_count = level_counts.filter(fn(c) { c.0 == "DEBUG" })[0].1
  let warn_count = level_counts.filter(fn(c) { c.0 == "WARN" })[0].1
  let error_count = level_counts.filter(fn(c) { c.0 == "ERROR" })[0].1
  
  assert_eq(info_count, 1)
  assert_eq(debug_count, 1)
  assert_eq(warn_count, 1)
  assert_eq(error_count, 1)
  
  // Correlate logs with spans
  let correlate_with_span = fn(logs: Array[LogEvent], span_id: String) {
    let mut result = []
    for log in logs {
      match log.span_id {
        Some(id) => if id == span_id { result = result.push(log) }
        None => {}
      }
    }
    result
  }
  
  let span_456_logs = correlate_with_span(log_events, "span-456")
  assert_eq(span_456_logs.length(), 2)
  
  // Extract error messages
  let extract_errors = fn(logs: Array[LogEvent]) {
    let mut errors = []
    for log in logs {
      if log.level == "ERROR" {
        errors = errors.push(log.message)
      }
    }
    errors
  }
  
  let error_messages = extract_errors(log_events)
  assert_eq(error_messages.length(), 1)
  assert_eq(error_messages[0], "Database connection failed")
}

// Test 4: Trace Context Propagation
test "trace context propagation across service boundaries" {
  // Define a TraceContext type
  type TraceContext = {
    trace_id: String,
    span_id: String,
    baggage: Array[(String, String)],
    flags: Int
  }
  
  // Define a ServiceCall type
  type ServiceCall = {
    service_name: String,
    operation: String,
    incoming_context: Option[TraceContext],
    outgoing_context: Option[TraceContext],
    timestamp: Int
  }
  
  // Create initial trace context
  let initial_context = {
    trace_id: "trace-abc123",
    span_id: "span-def456",
    baggage: [
      ("user.id", "user-789"),
      ("request.id", "req-111"),
      ("tenant.id", "tenant-222")
    ],
    flags: 1
  }
  
  // Simulate service calls with context propagation
  let service_calls = [
    {
      service_name: "api-gateway",
      operation: "authenticate",
      incoming_context: None,
      outgoing_context: Some(initial_context),
      timestamp: 1640995200
    },
    {
      service_name: "user-service",
      operation: "get_user",
      incoming_context: Some(initial_context),
      outgoing_context: Some({
        trace_id: "trace-abc123",
        span_id: "span-ghi789",
        baggage: initial_context.baggage,
        flags: initial_context.flags
      }),
      timestamp: 1640995201
    },
    {
      service_name: "order-service",
      operation: "create_order",
      incoming_context: Some({
        trace_id: "trace-abc123",
        span_id: "span-ghi789",
        baggage: initial_context.baggage,
        flags: initial_context.flags
      }),
      outgoing_context: Some({
        trace_id: "trace-abc123",
        span_id: "span-jkl012",
        baggage: [
          ("user.id", "user-789"),
          ("request.id", "req-111"),
          ("tenant.id", "tenant-222"),
          ("order.id", "order-333")
        ],
        flags: 1
      }),
      timestamp: 1640995202
    }
  ]
  
  // Verify trace ID consistency across services
  let verify_trace_consistency = fn(calls: Array[ServiceCall]) {
    let mut trace_ids = []
    for call in calls {
      match call.outgoing_context {
        Some(ctx) => trace_ids = trace_ids.push(ctx.trace_id)
        None => {}
      }
    }
    
    // All trace IDs should be the same
    let first_id = trace_ids[0]
    for id in trace_ids {
      assert_eq(id, first_id)
    }
  }
  
  verify_trace_consistency(service_calls)
  
  // Verify baggage propagation
  let get_baggage_value = fn(context: TraceContext, key: String) {
    let mut found = None
    for (k, v) in context.baggage {
      if k == key {
        found = Some(v)
      }
    }
    found
  }
  
  // Verify initial baggage is preserved
  let final_context = service_calls[2].outgoing_context.unwrap()
  assert_eq(get_baggage_value(final_context, "user.id"), Some("user-789"))
  assert_eq(get_baggage_value(final_context, "request.id"), Some("req-111"))
  assert_eq(get_baggage_value(final_context, "tenant.id"), Some("tenant-222"))
  
  // Verify new baggage is added
  assert_eq(get_baggage_value(final_context, "order.id"), Some("order-333"))
  
  // Verify context propagation chain
  let build_trace_chain = fn(calls: Array[ServiceCall]) {
    let mut chain = []
    for call in calls {
      match (call.incoming_context, call.outgoing_context) {
        (Some(incoming), Some(outgoing)) => {
          chain = chain.push({
            service: call.service_name,
            operation: call.operation,
            incoming_span: incoming.span_id,
            outgoing_span: outgoing.span_id,
            trace_id: incoming.trace_id
          })
        }
        (None, Some(outgoing)) => {
          chain = chain.push({
            service: call.service_name,
            operation: call.operation,
            incoming_span: "none",
            outgoing_span: outgoing.span_id,
            trace_id: outgoing.trace_id
          })
        }
        _ => {}
      }
    }
    chain
  }
  
  let trace_chain = build_trace_chain(service_calls)
  assert_eq(trace_chain.length(), 3)
  
  // Verify chain structure
  assert_eq(trace_chain[0].service, "api-gateway")
  assert_eq(trace_chain[0].incoming_span, "none")
  assert_eq(trace_chain[0].outgoing_span, "span-def456")
  
  assert_eq(trace_chain[1].service, "user-service")
  assert_eq(trace_chain[1].incoming_span, "span-def456")
  assert_eq(trace_chain[1].outgoing_span, "span-ghi789")
  
  assert_eq(trace_chain[2].service, "order-service")
  assert_eq(trace_chain[2].incoming_span, "span-ghi789")
  assert_eq(trace_chain[2].outgoing_span, "span-jkl012")
}

// Test 5: Telemetry Data Serialization and Deserialization
test "telemetry data serialization and deserialization" {
  // Define a simplified telemetry data structure
  type TelemetryData = {
    data_type: String,
    timestamp: Int,
    payload: Array[(String, String)]
  }
  
  // Create sample telemetry data
  let span_data = {
    data_type: "span",
    timestamp: 1640995200,
    payload: [
      ("name", "http_request"),
      ("trace_id", "trace-123"),
      ("span_id", "span-456"),
      ("status", "completed"),
      ("duration_ms", "250")
    ]
  }
  
  let metric_data = {
    data_type: "metric",
    timestamp: 1640995201,
    payload: [
      ("name", "cpu_usage"),
      ("value", "75.5"),
      ("unit", "percent"),
      ("tags", "service:api,env:prod")
    ]
  }
  
  let log_data = {
    data_type: "log",
    timestamp: 1640995202,
    payload: [
      ("level", "ERROR"),
      ("message", "Database connection failed"),
      ("trace_id", "trace-789"),
      ("error_code", "CONN_TIMEOUT")
    ]
  }
  
  // Simulate serialization to string format
  let serialize_to_string = fn(data: TelemetryData) {
    let mut parts = []
    parts = parts.push(data.data_type)
    parts = parts.push(data.timestamp.to_string())
    
    for (key, value) in data.payload {
      parts = parts.push(key + "=" + value)
    }
    
    parts.join("|")
  }
  
  // Serialize the data
  let serialized_span = serialize_to_string(span_data)
  let serialized_metric = serialize_to_string(metric_data)
  let serialized_log = serialize_to_string(log_data)
  
  // Verify serialization format
  assert_true(serialized_span.starts_with("span|1640995200|"))
  assert_true(serialized_span.contains("name=http_request"))
  assert_true(serialized_span.contains("trace_id=trace-123"))
  
  assert_true(serialized_metric.starts_with("metric|1640995201|"))
  assert_true(serialized_metric.contains("name=cpu_usage"))
  assert_true(serialized_metric.contains("value=75.5"))
  
  assert_true(serialized_log.starts_with("log|1640995202|"))
  assert_true(serialized_log.contains("level=ERROR"))
  assert_true(serialized_log.contains("message=Database connection failed"))
  
  // Simulate deserialization from string format
  let deserialize_from_string = fn(serialized: String) {
    let parts = serialized.split("|")
    let data_type = parts[0]
    let timestamp = parts[1].to_int()
    
    let mut payload = []
    for i in 2..parts.length() {
      let pair = parts[i]
      let key_value = pair.split("=")
      if key_value.length() == 2 {
        payload = payload.push((key_value[0], key_value[1]))
      }
    }
    
    {
      data_type,
      timestamp,
      payload
    }
  }
  
  // Deserialize the data
  let deserialized_span = deserialize_from_string(serialized_span)
  let deserialized_metric = deserialize_from_string(serialized_metric)
  let deserialized_log = deserialize_from_string(serialized_log)
  
  // Verify deserialization
  assert_eq(deserialized_span.data_type, "span")
  assert_eq(deserialized_span.timestamp, 1640995200)
  assert_eq(deserialized_span.payload.length(), 5)
  
  assert_eq(deserialized_metric.data_type, "metric")
  assert_eq(deserialized_metric.timestamp, 1640995201)
  assert_eq(deserialized_metric.payload.length(), 4)
  
  assert_eq(deserialized_log.data_type, "log")
  assert_eq(deserialized_log.timestamp, 1640995202)
  assert_eq(deserialized_log.payload.length(), 4)
  
  // Verify round-trip integrity
  let get_payload_value = fn(payload: Array[(String, String)], key: String) {
    let mut found = None
    for (k, v) in payload {
      if k == key {
        found = Some(v)
      }
    }
    found
  }
  
  assert_eq(
    get_payload_value(deserialized_span.payload, "name"),
    get_payload_value(span_data.payload, "name")
  )
  assert_eq(
    get_payload_value(deserialized_span.payload, "trace_id"),
    get_payload_value(span_data.payload, "trace_id")
  )
  
  assert_eq(
    get_payload_value(deserialized_metric.payload, "name"),
    get_payload_value(metric_data.payload, "name")
  )
  assert_eq(
    get_payload_value(deserialized_metric.payload, "value"),
    get_payload_value(metric_data.payload, "value")
  )
  
  assert_eq(
    get_payload_value(deserialized_log.payload, "level"),
    get_payload_value(log_data.payload, "level")
  )
  assert_eq(
    get_payload_value(deserialized_log.payload, "message"),
    get_payload_value(log_data.payload, "message")
  )
}

// Test 6: Performance Metrics Collection and Analysis
test "performance metrics collection and analysis" {
  // Define performance metric types
  type PerformanceMetric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    metric_type: String,
    tags: Array[(String, String)]
  }
  
  // Create sample performance metrics
  let performance_metrics = [
    {
      name: "cpu_usage",
      value: 45.2,
      unit: "percent",
      timestamp: 1640995200,
      metric_type: "gauge",
      tags: [("service", "api"), ("instance", "i-123")]
    },
    {
      name: "memory_usage",
      value: 512.0,
      unit: "MB",
      timestamp: 1640995200,
      metric_type: "gauge",
      tags: [("service", "api"), ("instance", "i-123")]
    },
    {
      name: "request_count",
      value: 1250.0,
      unit: "count",
      timestamp: 1640995200,
      metric_type: "counter",
      tags: [("service", "api"), ("endpoint", "/users")]
    },
    {
      name: "response_time",
      value: 120.5,
      unit: "ms",
      timestamp: 1640995200,
      metric_type: "histogram",
      tags: [("service", "api"), ("endpoint", "/users")]
    },
    {
      name: "error_rate",
      value: 0.02,
      unit: "ratio",
      timestamp: 1640995200,
      metric_type: "gauge",
      tags: [("service", "api"), ("endpoint", "/users")]
    }
  ]
  
  // Group metrics by type
  let group_by_type = fn(metrics: Array[PerformanceMetric]) {
    let mut groups = []
    let mut processed_types = []
    
    for metric in metrics {
      if not(processed_types.contains(metric.metric_type)) {
        processed_types = processed_types.push(metric.metric_type)
        
        let mut type_metrics = []
        for m in metrics {
          if m.metric_type == metric.metric_type {
            type_metrics = type_metrics.push(m)
          }
        }
        
        groups = groups.push((metric.metric_type, type_metrics))
      }
    }
    
    groups
  }
  
  let grouped_by_type = group_by_type(performance_metrics)
  assert_eq(grouped_by_type.length(), 3)
  
  // Verify groups
  let gauge_metrics = grouped_by_type.filter(fn(g) { g.0 == "gauge" })[0].1
  let counter_metrics = grouped_by_type.filter(fn(g) { g.0 == "counter" })[0].1
  let histogram_metrics = grouped_by_type.filter(fn(g) { g.0 == "histogram" })[0].1
  
  assert_eq(gauge_metrics.length(), 3)
  assert_eq(counter_metrics.length(), 1)
  assert_eq(histogram_metrics.length(), 1)
  
  // Calculate percentiles for histogram metrics
  let calculate_percentile = fn(values: Array[Float], percentile: Float) {
    // Sort values
    let sorted = values.sort(fn(a, b) { a <= b })
    let index = ((sorted.length() - 1).to_float() * percentile / 100.0).to_int()
    sorted[index]
  }
  
  // Create sample response time values for percentile calculation
  let response_times = [50.0, 75.0, 100.0, 120.0, 150.0, 200.0, 300.0, 500.0]
  
  let p50 = calculate_percentile(response_times, 50.0)
  let p95 = calculate_percentile(response_times, 95.0)
  let p99 = calculate_percentile(response_times, 99.0)
  
  assert_eq(p50, 120.0)
  assert_eq(p95, 300.0)
  assert_eq(p99, 500.0)
  
  // Detect performance anomalies
  let detect_anomalies = fn(metrics: Array[PerformanceMetric], threshold: Float) {
    let mut anomalies = []
    
    for metric in metrics {
      if metric.name == "error_rate" and metric.value > threshold {
        anomalies = anomalies.push({
          metric: metric.name,
          value: metric.value,
          threshold,
          tags: metric.tags
        })
      } else if metric.name == "cpu_usage" and metric.value > 80.0 {
        anomalies = anomalies.push({
          metric: metric.name,
          value: metric.value,
          threshold: 80.0,
          tags: metric.tags
        })
      } else if metric.name == "memory_usage" and metric.value > 1024.0 {
        anomalies = anomalies.push({
          metric: metric.name,
          value: metric.value,
          threshold: 1024.0,
          tags: metric.tags
        })
      }
    }
    
    anomalies
  }
  
  let anomalies = detect_anomalies(performance_metrics, 0.01)
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].metric, "error_rate")
  assert_eq(anomalies[0].value, 0.02)
  assert_eq(anomalies[0].threshold, 0.01)
  
  // Calculate rate of change for counter metrics
  let calculate_rate = fn(current: Float, previous: Float, time_diff: Int) {
    if time_diff > 0 {
      (current - previous) / time_diff.to_float()
    } else {
      0.0
    }
  }
  
  let rate = calculate_rate(1250.0, 1000.0, 60)  // 250 requests in 60 seconds
  assert_true(rate > 4.0 and rate < 4.2)
  
  // Generate performance summary
  let generate_summary = fn(metrics: Array[PerformanceMetric]) {
    let cpu_metrics = metrics.filter(fn(m) { m.name == "cpu_usage" })
    let memory_metrics = metrics.filter(fn(m) { m.name == "memory_usage" })
    let error_metrics = metrics.filter(fn(m) { m.name == "error_rate" })
    
    {
      avg_cpu: if cpu_metrics.length() > 0 {
        cpu_metrics.map(fn(m) { m.value }).reduce(fn(acc, v) { acc + v }, 0.0) / cpu_metrics.length().to_float()
      } else { 0.0 },
      
      avg_memory: if memory_metrics.length() > 0 {
        memory_metrics.map(fn(m) { m.value }).reduce(fn(acc, v) { acc + v }, 0.0) / memory_metrics.length().to_float()
      } else { 0.0 },
      
      avg_error_rate: if error_metrics.length() > 0 {
        error_metrics.map(fn(m) { m.value }).reduce(fn(acc, v) { acc + v }, 0.0) / error_metrics.length().to_float()
      } else { 0.0 }
    }
  }
  
  let summary = generate_summary(performance_metrics)
  assert_eq(summary.avg_cpu, 45.2)
  assert_eq(summary.avg_memory, 512.0)
  assert_eq(summary.avg_error_rate, 0.02)
}

// Test 7: Distributed Tracing Consistency Validation
test "distributed tracing consistency validation" {
  // Define a trace structure
  type Trace = {
    trace_id: String,
    spans: Array[Span],
    root_span: String
  }
  
  type Span = {
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    service_name: String
  }
  
  // Create a valid trace with proper parent-child relationships
  let valid_trace = {
    trace_id: "trace-123",
    spans: [
      {
        span_id: "span-1",
        parent_span_id: None,
        operation_name: "http_request",
        start_time: 1000,
        end_time: 1500,
        service_name: "api-gateway"
      },
      {
        span_id: "span-2",
        parent_span_id: Some("span-1"),
        operation_name: "auth_check",
        start_time: 1100,
        end_time: 1200,
        service_name: "auth-service"
      },
      {
        span_id: "span-3",
        parent_span_id: Some("span-1"),
        operation_name: "data_fetch",
        start_time: 1200,
        end_time: 1400,
        service_name: "data-service"
      },
      {
        span_id: "span-4",
        parent_span_id: Some("span-3"),
        operation_name: "database_query",
        start_time: 1250,
        end_time: 1350,
        service_name: "database"
      }
    ],
    root_span: "span-1"
  }
  
  // Validate trace structure
  let validate_trace_structure = fn(trace: Trace) {
    let mut errors = []
    
    // Check if root span exists
    let root_span_exists = trace.spans.any(fn(s) { s.span_id == trace.root_span })
    if not(root_span_exists) {
      errors = errors.push("Root span not found")
    }
    
    // Check if all parent spans exist
    for span in trace.spans {
      match span.parent_span_id {
        Some(parent_id) => {
          let parent_exists = trace.spans.any(fn(s) { s.span_id == parent_id })
          if not(parent_exists) {
            errors = errors.push("Parent span " + parent_id + " not found for span " + span.span_id)
          }
        }
        None => {}
      }
    }
    
    // Check for cycles in span hierarchy
    let has_cycle = fn(trace: Trace) {
      let mut visited = []
      let mut stack = []
      
      // Start with root span
      stack = stack.push(trace.root_span)
      
      while stack.length() > 0 {
        let current = stack.pop()
        
        if visited.contains(current) {
          return true  // Cycle detected
        }
        
        visited = visited.push(current)
        
        // Find children of current span
        let children = trace.spans.filter(fn(s) {
          match s.parent_span_id {
            Some(parent) => parent == current
            None => false
          }
        })
        
        for child in children {
          stack = stack.push(child.span_id)
        }
      }
      
      false
    }
    
    if has_cycle(trace) {
      errors = errors.push("Cycle detected in span hierarchy")
    }
    
    // Check timestamp consistency
    for span in trace.spans {
      if span.start_time > span.end_time {
        errors = errors.push("Span " + span.span_id + " has start time after end time")
      }
    }
    
    errors
  }
  
  let validation_errors = validate_trace_structure(valid_trace)
  assert_eq(validation_errors.length(), 0)  // Valid trace should have no errors
  
  // Create an invalid trace with missing parent
  let invalid_trace = {
    trace_id: "trace-456",
    spans: [
      {
        span_id: "span-1",
        parent_span_id: None,
        operation_name: "http_request",
        start_time: 1000,
        end_time: 1500,
        service_name: "api-gateway"
      },
      {
        span_id: "span-2",
        parent_span_id: Some("span-nonexistent"),
        operation_name: "auth_check",
        start_time: 1100,
        end_time: 1200,
        service_name: "auth-service"
      }
    ],
    root_span: "span-1"
  }
  
  let invalid_validation_errors = validate_trace_structure(invalid_trace)
  assert_eq(invalid_validation_errors.length(), 1)
  assert_true(invalid_validation_errors[0].contains("Parent span span-nonexistent not found"))
  
  // Calculate trace duration
  let calculate_trace_duration = fn(trace: Trace) {
    let start_times = trace.spans.map(fn(s) { s.start_time })
    let end_times = trace.spans.map(fn(s) { s.end_time })
    
    let min_start = start_times.reduce(fn(acc, t) { if t < acc { t } else { acc } }, start_times[0])
    let max_end = end_times.reduce(fn(acc, t) { if t > acc { t } else { acc } }, end_times[0])
    
    max_end - min_start
  }
  
  let trace_duration = calculate_trace_duration(valid_trace)
  assert_eq(trace_duration, 500)  // 1500 - 1000
  
  // Analyze service participation
  let analyze_service_participation = fn(trace: Trace) {
    let mut services = []
    let mut processed_services = []
    
    for span in trace.spans {
      if not(processed_services.contains(span.service_name)) {
        processed_services = processed_services.push(span.service_name)
        
        let service_spans = trace.spans.filter(fn(s) { s.service_name == span.service_name })
        let service_duration = service_spans.reduce(fn(acc, s) { 
          acc + (s.end_time - s.start_time) 
        }, 0)
        
        services = services.push({
          service_name: span.service_name,
          span_count: service_spans.length(),
          total_duration: service_duration
        })
      }
    }
    
    services
  }
  
  let service_analysis = analyze_service_participation(valid_trace)
  assert_eq(service_analysis.length(), 4)
  
  // Verify specific services
  let api_gateway = service_analysis.filter(fn(s) { s.service_name == "api-gateway" })[0]
  assert_eq(api_gateway.span_count, 1)
  assert_eq(api_gateway.total_duration, 500)
  
  let database = service_analysis.filter(fn(s) { s.service_name == "database" })[0]
  assert_eq(database.span_count, 1)
  assert_eq(database.total_duration, 100)
  
  // Check for orphan spans (spans not connected to root)
  let find_orphan_spans = fn(trace: Trace) {
    let mut reachable = []
    let mut stack = []
    
    // Start with root span
    stack = stack.push(trace.root_span)
    
    while stack.length() > 0 {
      let current = stack.pop()
      
      if not(reachable.contains(current)) {
        reachable = reachable.push(current)
        
        // Find children of current span
        let children = trace.spans.filter(fn(s) {
          match s.parent_span_id {
            Some(parent) => parent == current
            None => false
          }
        })
        
        for child in children {
          stack = stack.push(child.span_id)
        }
      }
    }
    
    // Find spans not reachable from root
    trace.spans.filter(fn(s) { not(reachable.contains(s.span_id)) })
  }
  
  let orphan_spans = find_orphan_spans(valid_trace)
  assert_eq(orphan_spans.length(), 0)  // Valid trace should have no orphan spans
}

// Test 8: Telemetry Configuration Management
test "telemetry configuration management" {
  // Define configuration types
  type TelemetryConfig = {
    enabled: Bool,
    sampling_rate: Float,
    batch_size: Int,
    flush_interval: Int,
    exporters: Array[ExporterConfig]
  }
  
  type ExporterConfig = {
    name: String,
    exporter_type: String,
    endpoint: String,
    enabled: Bool,
    settings: Array[(String, String)]
  }
  
  // Create default configuration
  let default_config = {
    enabled: true,
    sampling_rate: 1.0,
    batch_size: 100,
    flush_interval: 5000,
    exporters: [
      {
        name: "jaeger",
        exporter_type: "jaeger",
        endpoint: "http://jaeger:14268/api/traces",
        enabled: true,
        settings: [
          ("service.name", "azimuth-service"),
          ("timeout", "5000")
        ]
      },
      {
        name: "prometheus",
        exporter_type: "prometheus",
        endpoint: "http://prometheus:9090",
        enabled: true,
        settings: [
          ("port", "8080"),
          ("path", "/metrics")
        ]
      },
      {
        name: "stdout",
        exporter_type: "console",
        endpoint: "",
        enabled: false,
        settings: [
          ("format", "json")
        ]
      }
    ]
  }
  
  // Validate configuration
  let validate_config = fn(config: TelemetryConfig) {
    let mut errors = []
    
    // Check sampling rate
    if config.sampling_rate < 0.0 or config.sampling_rate > 1.0 {
      errors = errors.push("Sampling rate must be between 0.0 and 1.0")
    }
    
    // Check batch size
    if config.batch_size <= 0 {
      errors = errors.push("Batch size must be positive")
    }
    
    // Check flush interval
    if config.flush_interval <= 0 {
      errors = errors.push("Flush interval must be positive")
    }
    
    // Check exporters
    if config.exporters.length() == 0 {
      errors = errors.push("At least one exporter must be configured")
    }
    
    // Check for enabled exporters
    let enabled_exporters = config.exporters.filter(fn(e) { e.enabled })
    if enabled_exporters.length() == 0 {
      errors = errors.push("At least one exporter must be enabled")
    }
    
    // Validate each exporter
    for exporter in config.exporters {
      if exporter.name == "" {
        errors = errors.push("Exporter name cannot be empty")
      }
      
      if exporter.exporter_type == "" {
        errors = errors.push("Exporter type cannot be empty")
      }
      
      // Check endpoint for non-console exporters
      if exporter.exporter_type != "console" and exporter.endpoint == "" {
        errors = errors.push("Exporter endpoint required for " + exporter.exporter_type)
      }
    }
    
    errors
  }
  
  let validation_errors = validate_config(default_config)
  assert_eq(validation_errors.length(), 0)  // Default config should be valid
  
  // Create invalid configuration
  let invalid_config = {
    enabled: true,
    sampling_rate: 1.5,  // Invalid: > 1.0
    batch_size: 0,       // Invalid: <= 0
    flush_interval: 0,   // Invalid: <= 0
    exporters: []        // Invalid: empty
  }
  
  let invalid_validation_errors = validate_config(invalid_config)
  assert_eq(invalid_validation_errors.length(), 5)
  
  // Merge configurations
  let merge_configs = fn(base: TelemetryConfig, override: TelemetryConfig) {
    {
      enabled: override.enabled,
      sampling_rate: override.sampling_rate,
      batch_size: override.batch_size,
      flush_interval: override.flush_interval,
      exporters: base.exporters.map(fn(base_exporter) {
        let override_exporter = override.exporters.find(fn(e) { e.name == base_exporter.name })
        match override_exporter {
          Some(oe) => oe
          None => base_exporter
        }
      })
    }
  }
  
  let config_override = {
    enabled: false,
    sampling_rate: 0.5,
    batch_size: 200,
    flush_interval: 10000,
    exporters: [
      {
        name: "stdout",
        exporter_type: "console",
        endpoint: "",
        enabled: true,
        settings: [
          ("format", "pretty")
        ]
      }
    ]
  }
  
  let merged_config = merge_configs(default_config, config_override)
  assert_eq(merged_config.enabled, false)
  assert_eq(merged_config.sampling_rate, 0.5)
  assert_eq(merged_config.batch_size, 200)
  assert_eq(merged_config.flush_interval, 10000)
  
  // Verify stdout exporter was overridden
  let stdout_exporter = merged_config.exporters.find(fn(e) { e.name == "stdout" })
  match stdout_exporter {
    Some(exporter) => {
      assert_eq(exporter.enabled, true)
      assert_eq(exporter.settings[0].1, "pretty")
    }
    None => assert_true(false)
  }
  
  // Verify other exporters were preserved
  let jaeger_exporter = merged_config.exporters.find(fn(e) { e.name == "jaeger" })
  match jaeger_exporter {
    Some(exporter) => {
      assert_eq(exporter.enabled, true)
      assert_eq(exporter.endpoint, "http://jaeger:14268/api/traces")
    }
    None => assert_true(false)
  }
  
  // Get effective configuration (considering environment variables)
  let get_effective_config = fn(config: TelemetryConfig, env_vars: Array[(String, String)]) {
    let mut effective = config
    
    // Simulate environment variable overrides
    for (key, value) in env_vars {
      match key {
        "TELEMETRY_ENABLED" => {
          effective = { effective | enabled: value == "true" }
        }
        "TELEMETRY_SAMPLING_RATE" => {
          effective = { effective | sampling_rate: value.to_float() }
        }
        "TELEMETRY_BATCH_SIZE" => {
          effective = { effective | batch_size: value.to_int() }
        }
        "TELEMETRY_FLUSH_INTERVAL" => {
          effective = { effective | flush_interval: value.to_int() }
        }
        _ => {}
      }
    }
    
    effective
  }
  
  let env_vars = [
    ("TELEMETRY_ENABLED", "false"),
    ("TELEMETRY_SAMPLING_RATE", "0.1"),
    ("TELEMETRY_BATCH_SIZE", "50")
  ]
  
  let effective_config = get_effective_config(default_config, env_vars)
  assert_eq(effective_config.enabled, false)
  assert_eq(effective_config.sampling_rate, 0.1)
  assert_eq(effective_config.batch_size, 50)
  assert_eq(effective_config.flush_interval, 5000)  // Not overridden
}

// Test 9: Resource Management and Cleanup
test "resource management and cleanup operations" {
  // Define resource types
  type Resource = {
    id: String,
    resource_type: String,
    created_at: Int,
    last_accessed: Int,
    size: Int,
    active: Bool
  }
  
  type ResourceManager = {
    max_resources: Int,
    max_memory: Int,
    cleanup_threshold: Int,
    resources: Array[Resource]
  }
  
  // Create resource manager
  let create_manager = fn(max_resources: Int, max_memory: Int, cleanup_threshold: Int) {
    {
      max_resources,
      max_memory,
      cleanup_threshold,
      resources: []
    }
  }
  
  let manager = create_manager(100, 1024, 80)
  
  // Add resources
  let add_resource = fn(manager: ResourceManager, resource: Resource) {
    let updated_resources = manager.resources.push(resource)
    { manager | resources: updated_resources }
  }
  
  let resource1 = {
    id: "res-1",
    resource_type: "span_buffer",
    created_at: 1640995200,
    last_accessed: 1640995200,
    size: 100,
    active: true
  }
  
  let resource2 = {
    id: "res-2",
    resource_type: "metric_buffer",
    created_at: 1640995201,
    last_accessed: 1640995201,
    size: 200,
    active: true
  }
  
  let manager_with_resources = add_resource(add_resource(manager, resource1), resource2)
  assert_eq(manager_with_resources.resources.length(), 2)
  
  // Calculate resource usage
  let calculate_usage = fn(manager: ResourceManager) {
    let active_resources = manager.resources.filter(fn(r) { r.active })
    let total_resources = active_resources.length()
    let total_memory = active_resources.reduce(fn(acc, r) { acc + r.size }, 0)
    
    {
      resource_usage: (total_resources.to_float() / manager.max_resources.to_float()) * 100.0,
      memory_usage: (total_memory.to_float() / manager.max_memory.to_float()) * 100.0,
      total_resources,
      total_memory
    }
  }
  
  let usage = calculate_usage(manager_with_resources)
  assert_eq(usage.total_resources, 2)
  assert_eq(usage.total_memory, 300)
  assert_eq(usage.resource_usage, 2.0)
  assert_eq(usage.memory_usage, (300.0 / 1024.0) * 100.0)
  
  // Identify resources for cleanup
  let identify_cleanup_candidates = fn(manager: ResourceManager, current_time: Int) {
    let active_resources = manager.resources.filter(fn(r) { r.active })
    
    // Sort by last accessed time (oldest first)
    let sorted_by_age = active_resources.sort(fn(a, b) { a.last_accessed <= b.last_accessed })
    
    // Calculate how many resources to clean up
    let usage_stats = calculate_usage(manager)
    let needs_resource_cleanup = usage_stats.resource_usage > manager.cleanup_threshold.to_float()
    let needs_memory_cleanup = usage_stats.memory_usage > manager.cleanup_threshold.to_float()
    
    let mut cleanup_candidates = []
    
    if needs_resource_cleanup or needs_memory_cleanup {
      let mut cleanup_count = 0
      let mut cleanup_memory = 0
      
      for resource in sorted_by_age {
        if needs_resource_cleanup and cleanup_count < (usage_stats.total_resources / 2) {
          cleanup_candidates = cleanup_candidates.push(resource)
          cleanup_count = cleanup_count + 1
        } else if needs_memory_cleanup and cleanup_memory < (usage_stats.total_memory / 2) {
          cleanup_candidates = cleanup_candidates.push(resource)
          cleanup_memory = cleanup_memory + resource.size
        }
      }
    }
    
    cleanup_candidates
  }
  
  let cleanup_candidates = identify_cleanup_candidates(manager_with_resources, 1640995300)
  assert_eq(cleanup_candidates.length(), 0)  // No cleanup needed yet
  
  // Add more resources to trigger cleanup
  let mut manager_full = manager_with_resources
  for i in 3..=90 {
    let new_resource = {
      id: "res-" + i.to_string(),
      resource_type: "buffer",
      created_at: 1640995200 + i,
      last_accessed: 1640995200 + i,
      size: 10,
      active: true
    }
    manager_full = add_resource(manager_full, new_resource)
  }
  
  let full_usage = calculate_usage(manager_full)
  assert_true(full_usage.resource_usage > 80.0)  // Should exceed threshold
  
  let full_cleanup_candidates = identify_cleanup_candidates(manager_full, 1640995300)
  assert_true(full_cleanup_candidates.length() > 0)  // Should have cleanup candidates
  
  // Perform cleanup
  let perform_cleanup = fn(manager: ResourceManager, cleanup_candidates: Array[Resource]) {
    let cleanup_ids = cleanup_candidates.map(fn(r) { r.id })
    let remaining_resources = manager.resources.filter(fn(r) { 
      not(cleanup_ids.contains(r.id)) 
    })
    
    // Mark cleaned up resources as inactive
    let updated_resources = remaining_resources.map(fn(r) { r })
    
    { manager | resources: updated_resources }
  }
  
  let manager_after_cleanup = perform_cleanup(manager_full, full_cleanup_candidates)
  let after_cleanup_usage = calculate_usage(manager_after_cleanup)
  assert_true(after_cleanup_usage.resource_usage < full_usage.resource_usage)
  
  // Resource access tracking
  let access_resource = fn(manager: ResourceManager, resource_id: String, access_time: Int) {
    let updated_resources = manager.resources.map(fn(r) {
      if r.id == resource_id {
        { r | last_accessed: access_time }
      } else {
        r
      }
    })
    
    { manager | resources: updated_resources }
  }
  
  // Access a resource to update its last accessed time
  let manager_after_access = access_resource(manager_after_cleanup, "res-50", 1640995400)
  
  // Verify the access time was updated
  let accessed_resource = manager_after_access.resources.find(fn(r) { r.id == "res-50" })
  match accessed_resource {
    Some(r) => assert_eq(r.last_accessed, 1640995400)
    None => assert_true(false)
  }
  
  // Resource expiration
  let expire_old_resources = fn(manager: ResourceManager, current_time: Int, max_age: Int) {
    let updated_resources = manager.resources.map(fn(r) {
      let age = current_time - r.last_accessed
      if age > max_age {
        { r | active: false }
      } else {
        r
      }
    })
    
    { manager | resources: updated_resources }
  }
  
  // Expire resources older than 100 seconds
  let manager_after_expiration = expire_old_resources(manager_after_access, 1640995400, 100)
  
  // Count expired resources
  let expired_count = manager_after_expiration.resources.filter(fn(r) { 
    not(r.active) 
  }).length()
  
  assert_true(expired_count > 0)  // Should have expired resources
}
// Azimuth Advanced Telemetry Test Suite
// This file contains advanced MoonBit test cases focusing on telemetry features

// Test 1: Telemetry Data Aggregation
test "telemetry data aggregation" {
  // Define telemetry metric types
  type Metric = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[String]
  }
  
  // Create sample metrics
  let metrics = [
    { name: "response_time", value: 120.5, timestamp: 1640995200, tags: ["service:api", "endpoint:/users"] },
    { name: "response_time", value: 85.3, timestamp: 1640995260, tags: ["service:api", "endpoint:/users"] },
    { name: "response_time", value: 200.1, timestamp: 1640995320, tags: ["service:api", "endpoint:/products"] },
    { name: "error_rate", value: 0.05, timestamp: 1640995200, tags: ["service:api", "endpoint:/users"] },
    { name: "error_rate", value: 0.02, timestamp: 1640995260, tags: ["service:api", "endpoint:/users"] }
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
  
  // Calculate average
  let calculate_average = fn(metrics: Array[Metric]) {
    if metrics.length() == 0 {
      0.0
    } else {
      let mut sum = 0.0
      for metric in metrics {
        sum = sum + metric.value
      }
      sum / metrics.length().to_float()
    }
  }
  
  // Calculate min and max
  let calculate_min_max = fn(metrics: Array[Metric]) {
    if metrics.length() == 0 {
      (0.0, 0.0)
    } else {
      let mut min = metrics[0].value
      let mut max = metrics[0].value
      
      for metric in metrics {
        if metric.value < min {
          min = metric.value
        }
        if metric.value > max {
          max = metric.value
        }
      }
      
      (min, max)
    }
  }
  
  // Test aggregation functions
  let response_time_metrics = filter_by_name(metrics, "response_time")
  assert_eq(response_time_metrics.length(), 3)
  
  let avg_response_time = calculate_average(response_time_metrics)
  assert_true(avg_response_time > 135.0 and avg_response_time < 136.0)
  
  let (min_response_time, max_response_time) = calculate_min_max(response_time_metrics)
  assert_eq(min_response_time, 85.3)
  assert_eq(max_response_time, 200.1)
  
  // Test error rate aggregation
  let error_rate_metrics = filter_by_name(metrics, "error_rate")
  assert_eq(error_rate_metrics.length(), 2)
  
  let avg_error_rate = calculate_average(error_rate_metrics)
  assert_true(avg_error_rate > 0.034 and avg_error_rate < 0.036)
  
  // Test aggregation by tags
  let filter_by_tag = fn(metrics: Array[Metric], tag: String) {
    let mut result = []
    for metric in metrics {
      if metric.tags.contains(tag) {
        result = result.push(metric)
      }
    }
    result
  }
  
  let users_endpoint_metrics = filter_by_tag(metrics, "endpoint:/users")
  assert_eq(users_endpoint_metrics.length(), 4)
  
  let users_avg_response_time = calculate_average(
    filter_by_name(users_endpoint_metrics, "response_time")
  )
  assert_eq(users_avg_response_time, (120.5 + 85.3) / 2.0)
}

// Test 2: Telemetry Data Serialization and Deserialization
test "telemetry data serialization and deserialization" {
  // Define telemetry span type
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  // Create a sample span
  let span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    tags: [
      ("service", "payment-service"),
      ("operation", "SELECT"),
      ("table", "transactions")
    ]
  }
  
  // Serialize span to string representation
  let serialize_span = fn(span: Span) {
    let parent_id = match span.parent_span_id {
      Some(id) => id
      None => "null"
    }
    
    let tags_str = span.tags.reduce(fn(acc, tag) {
      let (key, value) = tag
      acc + key + ":" + value + ","
    }, "").remove_suffix(",")
    
    "trace_id:" + span.trace_id + "|" +
    "span_id:" + span.span_id + "|" +
    "parent_span_id:" + parent_id + "|" +
    "operation_name:" + span.operation_name + "|" +
    "start_time:" + span.start_time.to_string() + "|" +
    "end_time:" + span.end_time.to_string() + "|" +
    "status:" + span.status + "|" +
    "tags:" + tags_str
  }
  
  // Deserialize string to span
  let deserialize_span = fn(serialized: String) {
    let parts = serialized.split("|")
    
    let parse_field = fn(part: String) {
      let field_parts = part.split(":")
      if field_parts.length() > 1 {
        field_parts[1]
      } else {
        ""
      }
    }
    
    let parse_tags = fn(tags_str: String) {
      if tags_str == "" {
        []
      } else {
        let tag_pairs = tags_str.split(",")
        let mut result = []
        for pair in tag_pairs {
          let kv = pair.split(":")
          if kv.length() == 2 {
            result = result.push((kv[0], kv[1]))
          }
        }
        result
      }
    }
    
    let parent_id_str = parse_field(parts[2])
    let parent_span_id = if parent_id_str == "null" {
      None
    } else {
      Some(parent_id_str)
    }
    
    {
      trace_id: parse_field(parts[0]),
      span_id: parse_field(parts[1]),
      parent_span_id,
      operation_name: parse_field(parts[3]),
      start_time: parse_field(parts[4]).to_int(),
      end_time: parse_field(parts[5]).to_int(),
      status: parse_field(parts[6]),
      tags: parse_tags(parse_field(parts[7]))
    }
  }
  
  // Test serialization
  let serialized = serialize_span(span)
  assert_true(serialized.contains("trace_id:trace-12345"))
  assert_true(serialized.contains("span_id:span-67890"))
  assert_true(serialized.contains("parent_span_id:span-11111"))
  assert_true(serialized.contains("operation_name:database_query"))
  assert_true(serialized.contains("tags:service:payment-service,operation:SELECT,table:transactions"))
  
  // Test deserialization
  let deserialized = deserialize_span(serialized)
  assert_eq(deserialized.trace_id, span.trace_id)
  assert_eq(deserialized.span_id, span.span_id)
  assert_eq(deserialized.parent_span_id, span.parent_span_id)
  assert_eq(deserialized.operation_name, span.operation_name)
  assert_eq(deserialized.start_time, span.start_time)
  assert_eq(deserialized.end_time, span.end_time)
  assert_eq(deserialized.status, span.status)
  assert_eq(deserialized.tags.length(), span.tags.length())
  
  // Test with null parent_span_id
  let span_without_parent = { span | parent_span_id: None }
  let serialized_without_parent = serialize_span(span_without_parent)
  let deserialized_without_parent = deserialize_span(serialized_without_parent)
  assert_eq(deserialized_without_parent.parent_span_id, None)
}

// Test 3: Telemetry Data Filtering and Querying
test "telemetry data filtering and querying" {
  // Define telemetry log entry type
  type LogEntry = {
    timestamp: Int,
    level: String,
    message: String,
    service: String,
    trace_id: Option[String],
    span_id: Option[String],
    attributes: Array[(String, String)]
  }
  
  // Create sample log entries
  let logs = [
    {
      timestamp: 1640995200,
      level: "INFO",
      message: "Request started",
      service: "api-service",
      trace_id: Some("trace-123"),
      span_id: Some("span-456"),
      attributes: [("method", "GET"), ("/users", "endpoint")]
    },
    {
      timestamp: 1640995210,
      level: "DEBUG",
      message: "Database connection established",
      service: "api-service",
      trace_id: Some("trace-123"),
      span_id: Some("span-789"),
      attributes: [("db", "postgresql")]
    },
    {
      timestamp: 1640995220,
      level: "WARN",
      message: "Slow query detected",
      service: "db-service",
      trace_id: Some("trace-123"),
      span_id: Some("span-789"),
      attributes: [("duration", "1500ms")]
    },
    {
      timestamp: 1640995230,
      level: "ERROR",
      message: "Connection timeout",
      service: "db-service",
      trace_id: Some("trace-456"),
      span_id: Some("span-111"),
      attributes: [("timeout", "30s")]
    },
    {
      timestamp: 1640995240,
      level: "INFO",
      message: "Request completed",
      service: "api-service",
      trace_id: Some("trace-123"),
      span_id: Some("span-456"),
      attributes: [("status", "200")]
    }
  ]
  
  // Filter by log level
  let filter_by_level = fn(logs: Array[LogEntry], level: String) {
    let mut result = []
    for log in logs {
      if log.level == level {
        result = result.push(log)
      }
    }
    result
  }
  
  // Filter by service
  let filter_by_service = fn(logs: Array[LogEntry], service: String) {
    let mut result = []
    for log in logs {
      if log.service == service {
        result = result.push(log)
      }
    }
    result
  }
  
  // Filter by trace ID
  let filter_by_trace_id = fn(logs: Array[LogEntry], trace_id: String) {
    let mut result = []
    for log in logs {
      match log.trace_id {
        Some(id) => if id == trace_id { result = result.push(log) }
        None => {}
      }
    }
    result
  }
  
  // Filter by time range
  let filter_by_time_range = fn(logs: Array[LogEntry], start: Int, end: Int) {
    let mut result = []
    for log in logs {
      if log.timestamp >= start and log.timestamp <= end {
        result = result.push(log)
      }
    }
    result
  }
  
  // Filter by attribute key-value
  let filter_by_attribute = fn(logs: Array[LogEntry], key: String, value: String) {
    let mut result = []
    for log in logs {
      let mut found = false
      for (k, v) in log.attributes {
        if k == key and v == value {
          found = true
        }
      }
      if found {
        result = result.push(log)
      }
    }
    result
  }
  
  // Test filtering functions
  let error_logs = filter_by_level(logs, "ERROR")
  assert_eq(error_logs.length(), 1)
  assert_eq(error_logs[0].message, "Connection timeout")
  
  let api_service_logs = filter_by_service(logs, "api-service")
  assert_eq(api_service_logs.length(), 3)
  
  let trace_123_logs = filter_by_trace_id(logs, "trace-123")
  assert_eq(trace_123_logs.length(), 4)
  
  let time_range_logs = filter_by_time_range(logs, 1640995210, 1640995230)
  assert_eq(time_range_logs.length(), 3)
  
  let db_logs = filter_by_attribute(logs, "db", "postgresql")
  assert_eq(db_logs.length(), 1)
  assert_eq(db_logs[0].message, "Database connection established")
  
  // Test compound queries
  let api_error_logs = filter_by_level(filter_by_service(logs, "api-service"), "ERROR")
  assert_eq(api_error_logs.length(), 0)
  
  let db_warn_logs = filter_by_level(filter_by_service(logs, "db-service"), "WARN")
  assert_eq(db_warn_logs.length(), 1)
  assert_eq(db_warn_logs[0].message, "Slow query detected")
  
  // Test query with multiple conditions
  let complex_query = fn(logs: Array[LogEntry]) {
    let mut result = []
    for log in logs {
      if log.service == "api-service" and 
         log.level != "ERROR" and 
         log.timestamp >= 1640995210 and
         log.timestamp <= 1640995230 {
        result = result.push(log)
      }
    }
    result
  }
  
  let complex_results = complex_query(logs)
  assert_eq(complex_results.length(), 1)
  assert_eq(complex_results[0].message, "Database connection established")
}

// Test 4: Telemetry Performance Monitoring
test "telemetry performance monitoring" {
  // Define performance metrics type
  type PerformanceMetric = {
    operation: String,
    duration: Int,
    cpu_usage: Float,
    memory_usage: Float,
    timestamp: Int
  }
  
  // Create sample performance metrics
  let metrics = [
    { operation: "database_query", duration: 150, cpu_usage: 0.25, memory_usage: 0.45, timestamp: 1640995200 },
    { operation: "api_call", duration: 80, cpu_usage: 0.15, memory_usage: 0.30, timestamp: 1640995260 },
    { operation: "cache_lookup", duration: 5, cpu_usage: 0.05, memory_usage: 0.10, timestamp: 1640995320 },
    { operation: "database_query", duration: 200, cpu_usage: 0.35, memory_usage: 0.50, timestamp: 1640995380 },
    { operation: "api_call", duration: 120, cpu_usage: 0.20, memory_usage: 0.35, timestamp: 1640995440 },
    { operation: "cache_lookup", duration: 3, cpu_usage: 0.03, memory_usage: 0.08, timestamp: 1640995500 }
  ]
  
  // Calculate percentiles
  let calculate_percentile = fn(values: Array[Int], percentile: Float) {
    if values.length() == 0 {
      0
    } else {
      let sorted_values = values.sort(fn(a, b) { a - b })
      let index = ((sorted_values.length() - 1).to_float() * percentile / 100.0).to_int()
      sorted_values[index]
    }
  }
  
  // Calculate moving average
  let calculate_moving_average = fn(values: Array[Int], window: Int) {
    if values.length() == 0 or window <= 0 {
      []
    } else {
      let mut result = []
      for i in 0..values.length() {
        let start = if i - window + 1 < 0 { 0 } else { i - window + 1 }
        let mut sum = 0
        let mut count = 0
        for j in start..=i {
          sum = sum + values[j]
          count = count + 1
        }
        result = result.push(sum / count)
      }
      result
    }
  }
  
  // Detect anomalies
  let detect_anomalies = fn(metrics: Array[PerformanceMetric], threshold: Float) {
    let mut result = []
    for metric in metrics {
      if metric.cpu_usage > threshold or metric.memory_usage > threshold {
        result = result.push(metric)
      }
    }
    result
  }
  
  // Calculate performance by operation
  let calculate_performance_by_operation = fn(metrics: Array[PerformanceMetric]) {
    let mut result = []
    let mut operations = []
    
    // Get unique operations
    for metric in metrics {
      if not(operations.contains(metric.operation)) {
        operations = operations.push(metric.operation)
      }
    }
    
    // Calculate stats for each operation
    for operation in operations {
      let operation_metrics = metrics.filter(fn(m) { m.operation == operation })
      let durations = operation_metrics.map(fn(m) { m.duration })
      let avg_duration = durations.reduce(fn(acc, d) { acc + d }, 0) / durations.length()
      let max_duration = durations.reduce(fn(acc, d) { if d > acc { d } else { acc } }, 0)
      let min_duration = durations.reduce(fn(acc, d) { if d < acc { d } else { acc } }, 100000)
      
      result = result.push({
        operation,
        avg_duration,
        min_duration,
        max_duration,
        count: operation_metrics.length()
      })
    }
    
    result
  }
  
  // Test performance calculations
  let durations = metrics.map(fn(m) { m.duration })
  let p50 = calculate_percentile(durations, 50.0)
  let p95 = calculate_percentile(durations, 95.0)
  let p99 = calculate_percentile(durations, 99.0)
  
  assert_eq(p50, 100)  // Median of [3, 5, 80, 120, 150, 200]
  assert_eq(p95, 200)
  assert_eq(p99, 200)
  
  // Test moving average
  let moving_avg = calculate_moving_average(durations, 3)
  assert_eq(moving_avg.length(), 6)
  assert_eq(moving_avg[0], 150)  // (150) / 1
  assert_eq(moving_avg[1], 115)  // (150 + 80) / 2
  assert_eq(moving_avg[2], 78)   // (150 + 80 + 5) / 3
  
  // Test anomaly detection
  let anomalies = detect_anomalies(metrics, 0.3)
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].operation, "database_query")
  assert_eq(anomalies[0].cpu_usage, 0.35)
  
  // Test performance by operation
  let performance_by_op = calculate_performance_by_operation(metrics)
  assert_eq(performance_by_op.length(), 3)
  
  let db_stats = performance_by_op.filter(fn(s) { s.operation == "database_query" })[0]
  assert_eq(db_stats.avg_duration, 175)  // (150 + 200) / 2
  assert_eq(db_stats.min_duration, 150)
  assert_eq(db_stats.max_duration, 200)
  assert_eq(db_stats.count, 2)
  
  let cache_stats = performance_by_op.filter(fn(s) { s.operation == "cache_lookup" })[0]
  assert_eq(cache_stats.avg_duration, 4)  // (5 + 3) / 2
  assert_eq(cache_stats.min_duration, 3)
  assert_eq(cache_stats.max_duration, 5)
  assert_eq(cache_stats.count, 2)
}

// Test 5: Telemetry Sampling Strategy
test "telemetry sampling strategy" {
  // Define sampling strategy types
  enum SamplingStrategy {
    Always
    Never
    Probability(Float)  // Sample rate between 0.0 and 1.0
    CountBased(Int)     // Sample every Nth item
    TimeBased(Int)      // Sample once per N seconds
  }
  
  // Define sampling decision
  type SamplingDecision = {
    should_sample: Bool,
    reason: String
  }
  
  // Create sampling function
  let should_sample = fn(strategy: SamplingStrategy, trace_id: String, timestamp: Int, count: Int) {
    match strategy {
      SamplingStrategy::Always => {
        { should_sample: true, reason: "always" }
      }
      SamplingStrategy::Never => {
        { should_sample: false, reason: "never" }
      }
      SamplingStrategy::Probability(rate) => {
        // Simple hash-based deterministic sampling
        let hash = trace_id.length() % 100
        let should = hash < (rate * 100.0).to_int()
        { should_sample: should, reason: "probability:" + rate.to_string() }
      }
      SamplingStrategy::CountBased(n) => {
        let should = count % n == 0
        { should_sample: should, reason: "count_based:" + n.to_string() }
      }
      SamplingStrategy::TimeBased(seconds) => {
        let should = timestamp % seconds == 0
        { should_sample: should, reason: "time_based:" + seconds.to_string() }
      }
    }
  }
  
  // Test Always strategy
  let always_decision = should_sample(SamplingStrategy::Always, "trace-123", 1640995200, 1)
  assert_true(always_decision.should_sample)
  assert_eq(always_decision.reason, "always")
  
  // Test Never strategy
  let never_decision = should_sample(SamplingStrategy::Never, "trace-123", 1640995200, 1)
  assert_false(never_decision.should_sample)
  assert_eq(never_decision.reason, "never")
  
  // Test Probability strategy
  let prob_decision_1 = should_sample(SamplingStrategy::Probability(0.5), "trace-123", 1640995200, 1)
  let prob_decision_2 = should_sample(SamplingStrategy::Probability(0.0), "trace-123", 1640995200, 1)
  let prob_decision_3 = should_sample(SamplingStrategy::Probability(1.0), "trace-123", 1640995200, 1)
  
  // Note: These assertions depend on the hash implementation
  // For this test, we're using trace_id.length() % 100
  // "trace-123" has length 9, so hash = 9
  assert_true(prob_decision_1.should_sample)  // 9 < 50
  assert_false(prob_decision_2.should_sample) // 9 >= 0
  assert_true(prob_decision_3.should_sample)  // 9 < 100
  
  // Test CountBased strategy
  let count_decision_1 = should_sample(SamplingStrategy::CountBased(10), "trace-123", 1640995200, 10)
  let count_decision_2 = should_sample(SamplingStrategy::CountBased(10), "trace-123", 1640995200, 5)
  
  assert_true(count_decision_1.should_sample)   // 10 % 10 == 0
  assert_false(count_decision_2.should_sample)  // 5 % 10 != 0
  
  // Test TimeBased strategy
  let time_decision_1 = should_sample(SamplingStrategy::TimeBased(60), "trace-123", 1640995200, 1)
  let time_decision_2 = should_sample(SamplingStrategy::TimeBased(60), "trace-123", 1640995230, 1)
  
  assert_true(time_decision_1.should_sample)    // 1640995200 % 60 == 0
  assert_false(time_decision_2.should_sample)   // 1640995230 % 60 != 0
  
  // Test sampling with multiple traces
  let sample_multiple_traces = fn(strategy: SamplingStrategy, traces: Array[String]) {
    let mut sampled_count = 0
    let mut total_count = 0
    
    for i in 0..traces.length() {
      let trace_id = traces[i]
      let decision = should_sample(strategy, trace_id, 1640995200 + i * 10, i + 1)
      total_count = total_count + 1
      if decision.should_sample {
        sampled_count = sampled_count + 1
      }
    }
    
    { sampled_count, total_count, rate: sampled_count.to_float() / total_count.to_float() }
  }
  
  let test_traces = ["trace-1", "trace-2", "trace-3", "trace-4", "trace-5", "trace-6", "trace-7", "trace-8", "trace-9", "trace-10"]
  
  // Test with CountBased(2) - should sample roughly 50%
  let count_result = sample_multiple_traces(SamplingStrategy::CountBased(2), test_traces)
  assert_eq(count_result.total_count, 10)
  assert_eq(count_result.sampled_count, 5)  // Every 2nd item
  assert_eq(count_result.rate, 0.5)
  
  // Test with CountBased(3) - should sample roughly 33%
  let count_result_3 = sample_multiple_traces(SamplingStrategy::CountBased(3), test_traces)
  assert_eq(count_result_3.total_count, 10)
  assert_eq(count_result_3.sampled_count, 4)  // Items 3, 6, 9
  assert_true(count_result_3.rate > 0.3 and count_result_3.rate < 0.4)
}

// Test 6: Telemetry Context Propagation
test "telemetry context propagation" {
  // Define context type
  type TelemetryContext = {
    trace_id: String,
    span_id: String,
    baggage: Array[(String, String)],
    flags: Int
  }
  
  // Define propagator interface
  type Propagator = {
    inject: (TelemetryContext, Array[(String, String)]) -> Array[(String, String)],
    extract: (Array[(String, String)]) -> TelemetryContext
  }
  
  // Create text map propagator
  let text_map_propagator = {
    inject: fn(context: TelemetryContext, carrier: Array[(String, String)]) {
      let mut result = carrier
      
      // Inject trace context
      result = result.push(("traceparent", "00-" + context.trace_id + "-" + context.span_id + "-" + context.flags.to_string(16)))
      
      // Inject baggage
      for (key, value) in context.baggage {
        result = result.push(("baggage-" + key, value))
      }
      
      result
    },
    
    extract: fn(carrier: Array[(String, String)]) {
      let mut trace_id = ""
      let mut span_id = ""
      let mut flags = 0
      let mut baggage = []
      
      // Extract trace context
      for (key, value) in carrier {
        if key == "traceparent" {
          let parts = value.split("-")
          if parts.length() >= 4 {
            trace_id = parts[1]
            span_id = parts[2]
            flags = parts[3].to_int(16)
          }
        } else if key.starts_with("baggage-") {
          let baggage_key = key.substring(8, key.length() - 8)  // Remove "baggage-" prefix
          baggage = baggage.push((baggage_key, value))
        }
      }
      
      {
        trace_id,
        span_id,
        baggage,
        flags
      }
    }
  }
  
  // Create initial context
  let initial_context = {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    baggage: [
      ("user-id", "12345"),
      ("request-id", "req-67890"),
      ("service", "api-gateway")
    ],
    flags: 1
  }
  
  // Test context injection
  let carrier = []
  let injected_carrier = text_map_propagator.inject(initial_context, carrier)
  
  assert_eq(injected_carrier.length(), 4)  // 1 traceparent + 3 baggage items
  assert_true(injected_carrier.contains(("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-1")))
  assert_true(injected_carrier.contains(("baggage-user-id", "12345")))
  assert_true(injected_carrier.contains(("baggage-request-id", "req-67890")))
  assert_true(injected_carrier.contains(("baggage-service", "api-gateway")))
  
  // Test context extraction
  let extracted_context = text_map_propagator.extract(injected_carrier)
  
  assert_eq(extracted_context.trace_id, initial_context.trace_id)
  assert_eq(extracted_context.span_id, initial_context.span_id)
  assert_eq(extracted_context.flags, initial_context.flags)
  assert_eq(extracted_context.baggage.length(), initial_context.baggage.length())
  
  // Verify baggage items
  for (key, value) in initial_context.baggage {
    assert_true(extracted_context.baggage.contains((key, value)))
  }
  
  // Test context propagation across multiple services
  let propagate_across_services = fn(initial_context: TelemetryContext, services: Array[String]) {
    let mut contexts = []
    let current_context = initial_context
    
    for service in services {
      // Extract context from previous service
      let carrier = []
      let injected = text_map_propagator.inject(current_context, carrier)
      
      // Simulate network call
      let received_carrier = injected
      
      // Extract context in new service
      let extracted = text_map_propagator.extract(received_carrier)
      
      // Add service-specific baggage
      let service_baggage = [
        ("service", service),
        ("timestamp", "1640995200")
      ]
      
      let updated_context = {
        trace_id: extracted.trace_id,
        span_id: extracted.span_id + "-" + service.length().to_string(),  // Simulate new span
        baggage: extracted.baggage + service_baggage,
        flags: extracted.flags
      }
      
      contexts = contexts.push((service, updated_context))
      current_context = updated_context
    }
    
    contexts
  }
  
  let services = ["user-service", "order-service", "payment-service"]
  let propagated_contexts = propagate_across_services(initial_context, services)
  
  assert_eq(propagated_contexts.length(), 3)
  
  // Verify trace ID remains the same across all services
  for (service, context) in propagated_contexts {
    assert_eq(context.trace_id, initial_context.trace_id)
    assert_true(context.baggage.contains(("service", service)))
    
    // Verify original baggage is preserved
    assert_true(context.baggage.contains(("user-id", "12345")))
    assert_true(context.baggage.contains(("request-id", "req-67890")))
  }
  
  // Verify span IDs are different
  let user_span = propagated_contexts.filter(fn(pair) { pair.0 == "user-service" })[0].1.span_id
  let order_span = propagated_contexts.filter(fn(pair) { pair.0 == "order-service" })[0].1.span_id
  let payment_span = propagated_contexts.filter(fn(pair) { pair.0 == "payment-service" })[0].1.span_id
  
  assert_not_eq(user_span, order_span)
  assert_not_eq(order_span, payment_span)
  assert_not_eq(user_span, payment_span)
}

// Test 7: Telemetry Data Batch Processing
test "telemetry data batch processing" {
  // Define telemetry event type
  type TelemetryEvent = {
    id: String,
    timestamp: Int,
    event_type: String,
    data: String,
    processed: Bool
  }
  
  // Define batch processor type
  type BatchProcessor = {
    batch_size: Int,
    flush_interval: Int,
    process_batch: (Array[TelemetryEvent]) -> Array[TelemetryEvent]
  }
  
  // Create sample events
  let create_events = fn(count: Int, base_timestamp: Int) {
    let mut events = []
    for i in 0..count {
      events = events.push({
        id: "event-" + i.to_string(),
        timestamp: base_timestamp + i * 10,
        event_type: if i % 2 == 0 { "metric" } else { "log" },
        data: "data-" + i.to_string(),
        processed: false
      })
    }
    events
  }
  
  // Create batch processor
  let create_batch_processor = fn(batch_size: Int, flush_interval: Int) {
    {
      batch_size,
      flush_interval,
      process_batch: fn(events: Array[TelemetryEvent]) {
        // Simulate processing - mark events as processed
        events.map(fn(event) { { event | processed: true } })
      }
    }
  }
  
  // Process events in batches
  let process_events = fn(events: Array[TelemetryEvent], processor: BatchProcessor) {
    let mut processed_events = []
    let mut batch = []
    let mut last_flush_time = 0
    
    for event in events {
      batch = batch.push(event)
      
      // Check if batch is full or flush interval has passed
      let should_flush = batch.length() >= processor.batch_size or 
                        (event.timestamp - last_flush_time) >= processor.flush_interval
      
      if should_flush {
        let processed_batch = processor.process_batch(batch)
        processed_events = processed_events + processed_batch
        batch = []
        last_flush_time = event.timestamp
      }
    }
    
    // Process remaining events in the last batch
    if batch.length() > 0 {
      let processed_batch = processor.process_batch(batch)
      processed_events = processed_events + processed_batch
    }
    
    processed_events
  }
  
  // Test batch processing
  let events = create_events(25, 1640995200)
  let processor = create_batch_processor(10, 100)  // Batch size 10, flush every 100 seconds
  
  let processed_events = process_events(events, processor)
  
  // Verify all events are processed
  assert_eq(processed_events.length(), events.length())
  
  for event in processed_events {
    assert_true(event.processed)
  }
  
  // Verify batch boundaries
  // With batch_size=10 and 25 events, we should have 3 batches: 10, 10, 5
  let batch_1 = processed_events.slice(0, 10)
  let batch_2 = processed_events.slice(10, 10)
  let batch_3 = processed_events.slice(20, 5)
  
  assert_eq(batch_1.length(), 10)
  assert_eq(batch_2.length(), 10)
  assert_eq(batch_3.length(), 5)
  
  // Test with time-based flushing
  let time_based_events = create_events(5, 1640995200)
  let time_based_processor = create_batch_processor(10, 50)  // Batch size 10, flush every 50 seconds
  
  let time_processed = process_events(time_based_events, time_based_processor)
  
  // With 5 events and flush interval of 50 seconds, events should be flushed by time
  // Events at timestamps: 1640995200, 1640995210, 1640995220, 1640995230, 1640995240
  // The 5th event (1640995240) should trigger a flush because 1640995240 - 1640995200 >= 40 (close to 50)
  assert_eq(time_processed.length(), 5)
  
  for event in time_processed {
    assert_true(event.processed)
  }
  
  // Test batch filtering
  let filter_processor = {
    batch_size: 5,
    flush_interval: 100,
    process_batch: fn(events: Array[TelemetryEvent]) {
      // Only process metric events
      events.filter(fn(event) { event.event_type == "metric" })
         .map(fn(event) { { event | processed: true } })
    }
  }
  
  let mixed_events = create_events(10, 1640995200)
  let filtered_processed = process_events(mixed_events, filter_processor)
  
  // Only metric events (even indices) should be processed
  let expected_count = 5  // 0, 2, 4, 6, 8
  assert_eq(filtered_processed.length(), expected_count)
  
  for event in filtered_processed {
    assert_eq(event.event_type, "metric")
    assert_true(event.processed)
  }
}

// Test 8: Telemetry Data Caching
test "telemetry data caching" {
  // Define cache entry type
  type CacheEntry[T] = {
    key: String,
    value: T,
    timestamp: Int,
    ttl: Int  // Time to live in seconds
  }
  
  // Define cache type
  type Cache[T] = {
    entries: Array[CacheEntry[T]],
    max_size: Int,
    default_ttl: Int
  }
  
  // Create cache
  let create_cache = fn(max_size: Int, default_ttl: Int) {
    {
      entries: [],
      max_size,
      default_ttl
    }
  }
  
  // Check if cache entry is expired
  let is_expired = fn(entry: CacheEntry[T], current_time: Int) {
    (current_time - entry.timestamp) > entry.ttl
  }
  
  // Get value from cache
  let cache_get = fn(cache: Cache[T], key: String, current_time: Int) {
    // Find entry by key
    let mut found_entry = None
    for entry in cache.entries {
      if entry.key == key and not(is_expired(entry, current_time)) {
        found_entry = Some(entry.value)
      }
    }
    found_entry
  }
  
  // Put value in cache
  let cache_put = fn(cache: Cache[T], key: String, value: T, current_time: Int, ttl: Option[Int]) {
    let actual_ttl = match ttl {
      Some(t) => t
      None => cache.default_ttl
    }
    
    // Remove existing entry with same key
    let mut filtered_entries = []
    for entry in cache.entries {
      if entry.key != key {
        filtered_entries = filtered_entries.push(entry)
      }
    }
    
    // Add new entry
    let new_entry = {
      key,
      value,
      timestamp: current_time,
      ttl: actual_ttl
    }
    
    let updated_entries = filtered_entries.push(new_entry)
    
    // Enforce max size (remove oldest entries if needed)
    let mut final_entries = updated_entries
    if final_entries.length() > cache.max_size {
      let sorted_by_timestamp = final_entries.sort(fn(a, b) { a.timestamp - b.timestamp })
      final_entries = sorted_by_timestamp.slice(final_entries.length() - cache.max_size, cache.max_size)
    }
    
    { cache | entries: final_entries }
  }
  
  // Clean up expired entries
  let cache_cleanup = fn(cache: Cache[T], current_time: Int) {
    let mut valid_entries = []
    for entry in cache.entries {
      if not(is_expired(entry, current_time)) {
        valid_entries = valid_entries.push(entry)
      }
    }
    { cache | entries: valid_entries }
  }
  
  // Test cache operations
  let cache = create_cache(5, 60)  // Max 5 entries, 60s TTL
  let current_time = 1640995200
  
  // Test put and get
  let cache1 = cache_put(cache, "trace-123", "span-data-1", current_time, None)
  let value1 = cache_get(cache1, "trace-123", current_time)
  assert_eq(value1, Some("span-data-1"))
  
  // Test missing key
  let missing = cache_get(cache1, "missing-key", current_time)
  assert_eq(missing, None)
  
  // Test multiple entries
  let cache2 = cache_put(cache1, "trace-456", "span-data-2", current_time, None)
  let cache3 = cache_put(cache2, "trace-789", "span-data-3", current_time, None)
  
  assert_eq(cache_get(cache3, "trace-123", current_time), Some("span-data-1"))
  assert_eq(cache_get(cache3, "trace-456", current_time), Some("span-data-2"))
  assert_eq(cache_get(cache3, "trace-789", current_time), Some("span-data-3"))
  
  // Test entry expiration
  let expired_time = current_time + 120  // 2 minutes later, past the 60s TTL
  let expired_value = cache_get(cache3, "trace-123", expired_time)
  assert_eq(expired_value, None)
  
  // Test cleanup
  let cleaned_cache = cache_cleanup(cache3, expired_time)
  assert_eq(cleaned_cache.entries.length(), 0)  // All entries should be expired
  
  // Test max size enforcement
  let time = current_time
  let cache_a = cache_put(cache, "key1", "value1", time, None)
  let cache_b = cache_put(cache_a, "key2", "value2", time + 1, None)
  let cache_c = cache_put(cache_b, "key3", "value3", time + 2, None)
  let cache_d = cache_put(cache_c, "key4", "value4", time + 3, None)
  let cache_e = cache_put(cache_d, "key5", "value5", time + 4, None)
  let cache_f = cache_put(cache_e, "key6", "value6", time + 5, None)  // This should evict the oldest
  
  assert_eq(cache_f.entries.length(), 5)  // Max size enforced
  assert_eq(cache_get(cache_f, "key1", time + 5), None)  // Oldest entry evicted
  assert_eq(cache_get(cache_f, "key6", time + 5), Some("value6"))  // Newest entry present
  
  // Test custom TTL
  let cache_g = cache_put(cache, "short-lived", "value", current_time, Some(10))  // 10s TTL
  assert_eq(cache_get(cache_g, "short-lived", current_time + 5), Some("value"))  // Still valid
  assert_eq(cache_get(cache_g, "short-lived", current_time + 15), None)  // Expired
  
  // Test cache with complex data
  type SpanData = {
    trace_id: String,
    span_id: String,
    operation: String,
    duration: Int
  }
  
  let span_cache = create_cache(3, 30)
  let span_data = {
    trace_id: "trace-123",
    span_id: "span-456",
    operation: "database_query",
    duration: 150
  }
  
  let span_cache1 = cache_put(span_cache, "span-key", span_data, current_time, None)
  let retrieved_span = cache_get(span_cache1, "span-key", current_time)
  
  match retrieved_span {
    Some(data) => {
      assert_eq(data.trace_id, "trace-123")
      assert_eq(data.span_id, "span-456")
      assert_eq(data.operation, "database_query")
      assert_eq(data.duration, 150)
    }
    None => assert_true(false)
  }
}

// Test 9: Telemetry Data Compression
test "telemetry data compression" {
  // Define telemetry data type
  type TelemetryData = {
    trace_id: String,
    spans: Array[SpanData],
    metrics: Array[MetricData],
    logs: Array[LogData]
  }
  
  type SpanData = {
    span_id: String,
    parent_span_id: Option[String],
    operation: String,
    start_time: Int,
    end_time: Int,
    tags: Array[(String, String)]
  }
  
  type MetricData = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  type LogData = {
    timestamp: Int,
    level: String,
    message: String,
    tags: Array[(String, String)]
  }
  
  // Create sample telemetry data
  let create_sample_data = fn() {
    let spans = [
      {
        span_id: "span-1",
        parent_span_id: None,
        operation: "http_request",
        start_time: 1640995200,
        end_time: 1640995250,
        tags: [("service", "api"), ("endpoint", "/users")]
      },
      {
        span_id: "span-2",
        parent_span_id: Some("span-1"),
        operation: "database_query",
        start_time: 1640995210,
        end_time: 1640995240,
        tags: [("service", "api"), ("query", "SELECT * FROM users")]
      }
    ]
    
    let metrics = [
      {
        name: "http_request_duration",
        value: 50.0,
        timestamp: 1640995250,
        tags: [("service", "api"), ("endpoint", "/users"), ("status", "200")]
      },
      {
        name: "database_query_duration",
        value: 30.0,
        timestamp: 1640995240,
        tags: [("service", "api"), ("query", "SELECT * FROM users")]
      }
    ]
    
    let logs = [
      {
        timestamp: 1640995200,
        level: "INFO",
        message: "Request started",
        tags: [("service", "api"), ("request_id", "req-123")]
      },
      {
        timestamp: 1640995250,
        level: "INFO",
        message: "Request completed",
        tags: [("service", "api"), ("request_id", "req-123")]
      }
    ]
    
    {
      trace_id: "trace-123",
      spans,
      metrics,
      logs
    }
  }
  
  // Serialize telemetry data to string
  let serialize_telemetry_data = fn(data: TelemetryData) {
    let serialize_span = fn(span: SpanData) {
      let parent_id = match span.parent_span_id {
        Some(id) => id
        None => ""
      }
      
      let tags_str = span.tags.reduce(fn(acc, tag) {
        let (k, v) = tag
        acc + k + ":" + v + ","
      }, "").remove_suffix(",")
      
      span.span_id + "|" + parent_id + "|" + span.operation + "|" + 
      span.start_time.to_string() + "|" + span.end_time.to_string() + "|" + tags_str
    }
    
    let serialize_metric = fn(metric: MetricData) {
      let tags_str = metric.tags.reduce(fn(acc, tag) {
        let (k, v) = tag
        acc + k + ":" + v + ","
      }, "").remove_suffix(",")
      
      metric.name + "|" + metric.value.to_string() + "|" + 
      metric.timestamp.to_string() + "|" + tags_str
    }
    
    let serialize_log = fn(log: LogData) {
      let tags_str = log.tags.reduce(fn(acc, tag) {
        let (k, v) = tag
        acc + k + ":" + v + ","
      }, "").remove_suffix(",")
      
      log.timestamp.to_string() + "|" + log.level + "|" + log.message + "|" + tags_str
    }
    
    let spans_str = data.spans.reduce(fn(acc, span) {
      acc + serialize_span(span) + ";"
    }, "").remove_suffix(";")
    
    let metrics_str = data.metrics.reduce(fn(acc, metric) {
      acc + serialize_metric(metric) + ";"
    }, "").remove_suffix(";")
    
    let logs_str = data.logs.reduce(fn(acc, log) {
      acc + serialize_log(log) + ";"
    }, "").remove_suffix(";")
    
    data.trace_id + "|" + spans_str + "|" + metrics_str + "|" + logs_str
  }
  
  // Simple compression using run-length encoding for repeated characters
  let compress_string = fn(input: String) {
    if input.length() == 0 {
      ""
    } else {
      let mut result = ""
      let mut count = 1
      let current_char = input[0]
      
      for i in 1..input.length() {
        if input[i] == current_char {
          count = count + 1
        } else {
          if count > 3 {
            result = result + current_char.to_string() + "[" + count.to_string() + "]"
          } else {
            for j in 0..count {
              result = result + current_char.to_string()
            }
          }
          current_char = input[i]
          count = 1
        }
      }
      
      // Handle the last character sequence
      if count > 3 {
        result = result + current_char.to_string() + "[" + count.to_string() + "]"
      } else {
        for j in 0..count {
          result = result + current_char.to_string()
        }
      }
      
      result
    }
  }
  
  // Decompress the run-length encoded string
  let decompress_string = fn(compressed: String) {
    let mut result = ""
    let i = 0
    
    while i < compressed.length() {
      let char = compressed[i]
      
      if i + 1 < compressed.length() and compressed[i + 1] == '[' {
        // Found a run-length encoding pattern
        let j = i + 2
        let mut num_str = ""
        
        while j < compressed.length() and compressed[j] != ']' {
          num_str = num_str + compressed[j].to_string()
        }
        
        if j < compressed.length() and compressed[j] == ']' {
          let count = num_str.to_int()
          for k in 0..count {
            result = result + char.to_string()
          }
          i = j + 1
        } else {
          // Invalid pattern, just add the character
          result = result + char.to_string()
          i = i + 1
        }
      } else {
        // Regular character
        result = result + char.to_string()
        i = i + 1
      }
    }
    
    result
  }
  
  // Test serialization
  let sample_data = create_sample_data()
  let serialized = serialize_telemetry_data(sample_data)
  
  assert_true(serialized.contains("trace-123"))
  assert_true(serialized.contains("span-1"))
  assert_true(serialized.contains("http_request"))
  assert_true(serialized.contains("database_query"))
  
  // Test compression
  let compressed = compress_string(serialized)
  let decompressed = decompress_string(compressed)
  
  assert_eq(serialized, decompressed)
  
  // Create a string with repeated characters to test compression effectiveness
  let repetitive_string = "aaaaabbbbcccccdddddeeeee" + "f".repeat(50) + "ggggg"
  let compressed_repetitive = compress_string(repetitive_string)
  let decompressed_repetitive = decompress_string(compressed_repetitive)
  
  assert_eq(repetitive_string, decompressed_repetitive)
  assert_true(compressed_repetitive.length() < repetitive_string.length())
  
  // Test compression ratio calculation
  let calculate_compression_ratio = fn(original: String, compressed: String) {
    if original.length() == 0 {
      0.0
    } else {
      (1.0 - (compressed.length().to_float() / original.length().to_float())) * 100.0
    }
  }
  
  let compression_ratio = calculate_compression_ratio(repetitive_string, compressed_repetitive)
  assert_true(compression_ratio > 50.0)  // Should achieve at least 50% compression
  
  // Test with real telemetry data
  let telemetry_compressed = compress_string(serialized)
  let telemetry_decompressed = decompress_string(telemetry_compressed)
  let telemetry_compression_ratio = calculate_compression_ratio(serialized, telemetry_compressed)
  
  assert_eq(serialized, telemetry_decompressed)
  // Real telemetry data might not compress as well as repetitive data
  assert_true(telemetry_compression_ratio >= 0.0)
  
  // Test batch compression
  let batch_data = [
    create_sample_data(),
    create_sample_data(),
    create_sample_data()
  ]
  
  let serialized_batch = batch_data.map(serialize_telemetry_data)
  let compressed_batch = serialized_batch.map(compress_string)
  let decompressed_batch = compressed_batch.map(decompress_string)
  
  assert_eq(serialized_batch.length(), decompressed_batch.length())
  for i in 0..serialized_batch.length() {
    assert_eq(serialized_batch[i], decompressed_batch[i])
  }
}
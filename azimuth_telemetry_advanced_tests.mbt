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
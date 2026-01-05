// Azimuth Telemetry Data Processing Tests
// This file contains test cases for telemetry data processing functionality

// Test 1: Telemetry Data Aggregation
test "telemetry data aggregation" {
  // Define telemetry data structure
  type TelemetryPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[String]
  }
  
  // Create sample telemetry data
  let telemetry_points = [
    { timestamp: 1640995200, metric_name: "cpu_usage", value: 45.2, tags: ["service:api", "region:us-west"] },
    { timestamp: 1640995260, metric_name: "cpu_usage", value: 48.7, tags: ["service:api", "region:us-west"] },
    { timestamp: 1640995320, metric_name: "cpu_usage", value: 52.1, tags: ["service:api", "region:us-west"] },
    { timestamp: 1640995200, metric_name: "memory_usage", value: 67.3, tags: ["service:api", "region:us-west"] },
    { timestamp: 1640995260, metric_name: "memory_usage", value: 69.8, tags: ["service:api", "region:us-west"] },
    { timestamp: 1640995320, metric_name: "memory_usage", value: 71.2, tags: ["service:api", "region:us-west"] }
  ]
  
  // Aggregate by metric name
  let aggregate_by_metric = fn(points: Array[TelemetryPoint]) {
    let mut result = []
    let mut processed = []
    
    for point in points {
      if not(processed.contains(point.metric_name)) {
        processed = processed.push(point.metric_name)
        
        let metric_points = points.filter(fn(p) { p.metric_name == point.metric_name })
        let sum = metric_points.reduce(fn(acc, p) { acc + p.value }, 0.0)
        let avg = sum / metric_points.length().to_float()
        let min = metric_points.reduce(fn(acc, p) { if p.value < acc { p.value } else { acc } }, metric_points[0].value)
        let max = metric_points.reduce(fn(acc, p) { if p.value > acc { p.value } else { acc } }, metric_points[0].value)
        
        result = result.push({
          metric_name: point.metric_name,
          count: metric_points.length(),
          avg: avg,
          min: min,
          max: max,
          sum: sum
        })
      }
    }
    
    result
  }
  
  let aggregated = aggregate_by_metric(telemetry_points)
  assert_eq(aggregated.length(), 2)
  
  // Check CPU usage aggregation
  let cpu_metric = aggregated.filter(fn(m) { m.metric_name == "cpu_usage" })[0]
  assert_eq(cpu_metric.count, 3)
  assert_eq(cpu_metric.avg, (45.2 + 48.7 + 52.1) / 3.0)
  assert_eq(cpu_metric.min, 45.2)
  assert_eq(cpu_metric.max, 52.1)
  assert_eq(cpu_metric.sum, 45.2 + 48.7 + 52.1)
  
  // Check memory usage aggregation
  let memory_metric = aggregated.filter(fn(m) { m.metric_name == "memory_usage" })[0]
  assert_eq(memory_metric.count, 3)
  assert_eq(memory_metric.avg, (67.3 + 69.8 + 71.2) / 3.0)
  assert_eq(memory_metric.min, 67.3)
  assert_eq(memory_metric.max, 71.2)
  assert_eq(memory_metric.sum, 67.3 + 69.8 + 71.2)
}

// Test 2: Time Window Telemetry Processing
test "time window telemetry processing" {
  type TelemetryEvent = {
    timestamp: Int,
    event_type: String,
    service: String,
    duration: Int,
    status: String
  }
  
  // Create sample events across different time windows
  let events = [
    { timestamp: 1640995200, event_type: "request", service: "api", duration: 120, status: "success" },
    { timestamp: 1640995250, event_type: "request", service: "api", duration: 150, status: "success" },
    { timestamp: 1640995310, event_type: "request", service: "api", duration: 200, status: "error" },
    { timestamp: 1640995370, event_type: "request", service: "web", duration: 80, status: "success" },
    { timestamp: 1640995430, event_type: "request", service: "web", duration: 95, status: "success" },
    { timestamp: 1640995490, event_type: "request", service: "web", duration: 110, status: "success" }
  ]
  
  // Process events in time windows
  let process_time_windows = fn(events: Array[TelemetryEvent], window_size: Int) {
    let mut windows = []
    
    // Find min and max timestamps
    let timestamps = events.map(fn(e) { e.timestamp })
    let min_time = timestamps.reduce(fn(acc, t) { if t < acc { t } else { acc } }, timestamps[0])
    let max_time = timestamps.reduce(fn(acc, t) { if t > acc { t } else { acc } }, timestamps[0])
    
    // Create windows
    let mut current_start = min_time
    while current_start <= max_time {
      let current_end = current_start + window_size
      let window_events = events.filter(fn(e) { 
        e.timestamp >= current_start and e.timestamp < current_end 
      })
      
      if window_events.length() > 0 {
        let total_requests = window_events.length()
        let successful_requests = window_events.filter(fn(e) { e.status == "success" }).length()
        let error_rate = (total_requests - successful_requests).to_float() / total_requests.to_float() * 100.0
        let avg_duration = window_events.reduce(fn(acc, e) { acc + e.duration }, 0) / total_requests
        
        windows = windows.push({
          window_start: current_start,
          window_end: current_end,
          total_requests: total_requests,
          successful_requests: successful_requests,
          error_rate: error_rate,
          avg_duration: avg_duration
        })
      }
      
      current_start = current_end
    }
    
    windows
  }
  
  let windows = process_time_windows(events, 300) // 5-minute windows
  
  // Should have multiple windows
  assert_true(windows.length() > 0)
  
  // Check first window (1640995200 to 1640995500)
  let first_window = windows[0]
  assert_eq(first_window.window_start, 1640995200)
  assert_eq(first_window.window_end, 1640995500)
  assert_eq(first_window.total_requests, 6)
  assert_eq(first_window.successful_requests, 5)
  
  // Calculate expected error rate: 1 error out of 6 requests = 16.67%
  let expected_error_rate = 1.0 / 6.0 * 100.0
  assert_true(first_window.error_rate > expected_error_rate - 0.1 and first_window.error_rate < expected_error_rate + 0.1)
  
  // Calculate expected average duration
  let total_duration = 120 + 150 + 200 + 80 + 95 + 110
  let expected_avg_duration = total_duration / 6
  assert_eq(first_window.avg_duration, expected_avg_duration)
}

// Test 3: Telemetry Data Filtering and Transformation
test "telemetry data filtering and transformation" {
  type LogEntry = {
    timestamp: Int,
    level: String,
    message: String,
    service: String,
    trace_id: String
  }
  
  // Create sample log entries
  let logs = [
    { timestamp: 1640995200, level: "INFO", message: "Service started", service: "auth", trace_id: "trace-001" },
    { timestamp: 1640995210, level: "DEBUG", message: "Cache initialized", service: "auth", trace_id: "trace-001" },
    { timestamp: 1640995220, level: "WARN", message: "High memory usage", service: "api", trace_id: "trace-002" },
    { timestamp: 1640995230, level: "ERROR", message: "Database connection failed", service: "api", trace_id: "trace-002" },
    { timestamp: 1640995240, level: "INFO", message: "Request processed", service: "web", trace_id: "trace-003" },
    { timestamp: 1640995250, level: "ERROR", message: "Service timeout", service: "web", trace_id: "trace-003" }
  ]
  
  // Filter logs by level
  let filter_by_level = fn(logs: Array[LogEntry], level: String) {
    logs.filter(fn(log) { log.level == level })
  }
  
  let error_logs = filter_by_level(logs, "ERROR")
  assert_eq(error_logs.length(), 2)
  assert_eq(error_logs[0].service, "api")
  assert_eq(error_logs[1].service, "web")
  
  // Filter logs by service
  let filter_by_service = fn(logs: Array[LogEntry], service: String) {
    logs.filter(fn(log) { log.service == service })
  }
  
  let api_logs = filter_by_service(logs, "api")
  assert_eq(api_logs.length(), 2)
  assert_eq(api_logs[0].level, "WARN")
  assert_eq(api_logs[1].level, "ERROR")
  
  // Filter logs by time range
  let filter_by_time_range = fn(logs: Array[LogEntry], start_time: Int, end_time: Int) {
    logs.filter(fn(log) { log.timestamp >= start_time and log.timestamp <= end_time })
  }
  
  let time_filtered_logs = filter_by_time_range(logs, 1640995210, 1640995240)
  assert_eq(time_filtered_logs.length(), 3)
  
  // Transform logs to summary format
  let transform_to_summary = fn(logs: Array[LogEntry]) {
    logs.map(fn(log) {
      {
        timestamp: log.timestamp,
        summary: log.level + ": " + log.message + " (" + log.service + ")",
        trace_id: log.trace_id
      }
    })
  }
  
  let summaries = transform_to_summary(logs)
  assert_eq(summaries.length(), logs.length())
  assert_eq(summaries[0].summary, "INFO: Service started (auth)")
  assert_eq(summaries[3].summary, "ERROR: Database connection failed (api)")
  
  // Group logs by trace ID
  let group_by_trace_id = fn(logs: Array[LogEntry]) {
    let mut result = []
    let mut processed_trace_ids = []
    
    for log in logs {
      if not(processed_trace_ids.contains(log.trace_id)) {
        processed_trace_ids = processed_trace_ids.push(log.trace_id)
        
        let trace_logs = logs.filter(fn(l) { l.trace_id == log.trace_id })
        let services = trace_logs.map(fn(l) { l.service })
        let unique_services = {
          let mut unique = []
          for service in services {
            if not(unique.contains(service)) {
              unique = unique.push(service)
            }
          }
          unique
        }
        
        result = result.push({
          trace_id: log.trace_id,
          log_count: trace_logs.length(),
          services: unique_services,
          has_errors: trace_logs.some(fn(l) { l.level == "ERROR" })
        })
      }
    }
    
    result
  }
  
  let grouped = group_by_trace_id(logs)
  assert_eq(grouped.length(), 3)
  
  // Check trace-001
  let trace_001 = grouped.filter(fn(g) { g.trace_id == "trace-001" })[0]
  assert_eq(trace_001.log_count, 2)
  assert_eq(trace_001.services.length(), 1)
  assert_true(trace_001.services.contains("auth"))
  assert_false(trace_001.has_errors)
  
  // Check trace-002
  let trace_002 = grouped.filter(fn(g) { g.trace_id == "trace-002" })[0]
  assert_eq(trace_002.log_count, 2)
  assert_eq(trace_002.services.length(), 1)
  assert_true(trace_002.services.contains("api"))
  assert_true(trace_002.has_errors)
  
  // Check trace-003
  let trace_003 = grouped.filter(fn(g) { g.trace_id == "trace-003" })[0]
  assert_eq(trace_003.log_count, 2)
  assert_eq(trace_003.services.length(), 1)
  assert_true(trace_003.services.contains("web"))
  assert_true(trace_003.has_errors)
}

// Test 4: Telemetry Data Sampling
test "telemetry data sampling strategies" {
  type MetricData = {
    id: String,
    timestamp: Int,
    value: Float,
    priority: String
  }
  
  // Create sample metrics data
  let metrics = [
    { id: "metric-001", timestamp: 1640995200, value: 10.5, priority: "high" },
    { id: "metric-002", timestamp: 1640995210, value: 15.2, priority: "low" },
    { id: "metric-003", timestamp: 1640995220, value: 8.7, priority: "medium" },
    { id: "metric-004", timestamp: 1640995230, value: 12.3, priority: "high" },
    { id: "metric-005", timestamp: 1640995240, value: 9.8, priority: "low" },
    { id: "metric-006", timestamp: 1640995250, value: 11.1, priority: "medium" },
    { id: "metric-007", timestamp: 1640995260, value: 14.6, priority: "high" },
    { id: "metric-008", timestamp: 1640995270, value: 7.9, priority: "low" },
    { id: "metric-009", timestamp: 1640995280, value: 13.4, priority: "medium" },
    { id: "metric-010", timestamp: 1640995290, value: 10.1, priority: "high" }
  ]
  
  // Random sampling strategy
  let random_sampling = fn(metrics: Array[MetricData], sample_rate: Float) {
    let sample_count = (metrics.length().to_float() * sample_rate).to_int()
    let mut sampled = []
    let mut indices = []
    
    // Generate random indices (simplified)
    let mut i = 0
    while i < sample_count and i < metrics.length() {
      let index = (i * 2) % metrics.length()  // Simplified "random" selection
      if not(indices.contains(index)) {
        indices = indices.push(index)
        sampled = sampled.push(metrics[index])
      }
      i = i + 1
    }
    
    sampled
  }
  
  let random_sampled = random_sampling(metrics, 0.3) // 30% sampling
  assert_eq(random_sampled.length(), 3)
  
  // Priority-based sampling
  let priority_sampling = fn(metrics: Array[MetricData], sample_rates: Array<(String, Float)>) {
    let mut result = []
    
    for (priority, rate) in sample_rates {
      let priority_metrics = metrics.filter(fn(m) { m.priority == priority })
      let sample_count = (priority_metrics.length().to_float() * rate).to_int()
      
      if sample_count > 0 and sample_count <= priority_metrics.length() {
        // Take first N metrics as sample
        let mut i = 0
        while i < sample_count {
          result = result.push(priority_metrics[i])
          i = i + 1
        }
      }
    }
    
    result
  }
  
  let priority_sampled = priority_sampling(metrics, [
    ("high", 1.0),   // 100% of high priority
    ("medium", 0.5), // 50% of medium priority
    ("low", 0.25)    // 25% of low priority
  ])
  
  // Verify priority sampling results
  let high_priority_sampled = priority_sampled.filter(fn(m) { m.priority == "high" })
  let medium_priority_sampled = priority_sampled.filter(fn(m) { m.priority == "medium" })
  let low_priority_sampled = priority_sampled.filter(fn(m) { m.priority == "low" })
  
  assert_eq(high_priority_sampled.length(), 4) // All high priority metrics
  assert_eq(medium_priority_sampled.length(), 1) // 50% of 2 medium priority metrics
  assert_eq(low_priority_sampled.length(), 1)   // 25% of 4 low priority metrics
  
  // Time-based sampling
  let time_based_sampling = fn(metrics: Array[MetricData], interval: Int) {
    let mut result = []
    let mut last_sample_time = -1
    
    for metric in metrics {
      if metric.timestamp - last_sample_time >= interval {
        result = result.push(metric)
        last_sample_time = metric.timestamp
      }
    }
    
    result
  }
  
  let time_sampled = time_based_sampling(metrics, 60) // Sample every 60 seconds
  assert_true(time_sampled.length() > 0)
  
  // Verify time-based sampling
  for i in 1..time_sampled.length() {
    assert_true(time_sampled[i].timestamp - time_sampled[i-1].timestamp >= 60)
  }
  
  // Reservoir sampling (fixed-size sample)
  let reservoir_sampling = fn(metrics: Array[MetricData], sample_size: Int) {
    let mut reservoir = []
    let mut i = 0
    
    // Fill reservoir with first sample_size items
    while i < sample_size and i < metrics.length() {
      reservoir = reservoir.push(metrics[i])
      i = i + 1
    }
    
    // For remaining items, randomly replace items in reservoir
    while i < metrics.length() {
      // Simplified: replace every nth item
      let replace_index = i % sample_size
      if replace_index < reservoir.length() {
        reservoir[replace_index] = metrics[i]
      }
      i = i + 1
    }
    
    reservoir
  }
  
  let reservoir_sampled = reservoir_sampling(metrics, 5)
  assert_eq(reservoir_sampled.length(), 5)
  
  // Verify all sampled items are unique
  let sampled_ids = reservoir_sampled.map(fn(m) { m.id })
  let unique_ids = {
    let mut unique = []
    for id in sampled_ids {
      if not(unique.contains(id)) {
        unique = unique.push(id)
      }
    }
    unique
  }
  assert_eq(unique_ids.length(), sampled_ids.length())
}

// Test 5: Telemetry Data Validation
test "telemetry data validation" {
  type TelemetryRecord = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    service_name: String,
    operation_name: String,
    duration: Int,
    status: String
  }
  
  // Validation functions
  let validate_timestamp = fn(timestamp: Int) {
    timestamp > 0 and timestamp <= 2147483647 // Valid Unix timestamp range
  }
  
  let validate_trace_id = fn(trace_id: String) {
    trace_id.length() > 0 and trace_id.length() <= 128 and trace_id.starts_with("trace-")
  }
  
  let validate_span_id = fn(span_id: String) {
    span_id.length() > 0 and span_id.length() <= 64 and span_id.starts_with("span-")
  }
  
  let validate_service_name = fn(service_name: String) {
    service_name.length() > 0 and service_name.length() <= 50 and 
    service_name.chars().all(fn(c) { c.is_alphanumeric() or c == '_' or c == '-' })
  }
  
  let validate_operation_name = fn(operation_name: String) {
    operation_name.length() > 0 and operation_name.length() <= 100
  }
  
  let validate_duration = fn(duration: Int) {
    duration >= 0 and duration <= 3600000 // Max 1 hour in milliseconds
  }
  
  let validate_status = fn(status: String) {
    status == "ok" or status == "error" or status == "timeout"
  }
  
  // Comprehensive validation
  let validate_telemetry_record = fn(record: TelemetryRecord) {
    let errors = []
    
    if not(validate_timestamp(record.timestamp)) {
      errors = errors.push("Invalid timestamp")
    }
    
    if not(validate_trace_id(record.trace_id)) {
      errors = errors.push("Invalid trace_id")
    }
    
    if not(validate_span_id(record.span_id)) {
      errors = errors.push("Invalid span_id")
    }
    
    if not(validate_service_name(record.service_name)) {
      errors = errors.push("Invalid service_name")
    }
    
    if not(validate_operation_name(record.operation_name)) {
      errors = errors.push("Invalid operation_name")
    }
    
    if not(validate_duration(record.duration)) {
      errors = errors.push("Invalid duration")
    }
    
    if not(validate_status(record.status)) {
      errors = errors.push("Invalid status")
    }
    
    {
      valid: errors.length() == 0,
      errors: errors
    }
  }
  
  // Test valid record
  let valid_record = {
    timestamp: 1640995200,
    trace_id: "trace-123456789",
    span_id: "span-987654321",
    service_name: "payment-service",
    operation_name: "process_payment",
    duration: 250,
    status: "ok"
  }
  
  let valid_result = validate_telemetry_record(valid_record)
  assert_true(valid_result.valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid record
  let invalid_record = {
    timestamp: -1, // Invalid timestamp
    trace_id: "", // Empty trace_id
    span_id: "invalid-span-id-too-long-123456789012345678901234567890123456789012345678901234567890", // Too long
    service_name: "invalid service name with spaces!", // Invalid characters
    operation_name: "", // Empty operation name
    duration: -100, // Negative duration
    status: "unknown" // Invalid status
  }
  
  let invalid_result = validate_telemetry_record(invalid_record)
  assert_false(invalid_result.valid)
  assert_eq(invalid_result.errors.length(), 7)
  assert_true(invalid_result.errors.contains("Invalid timestamp"))
  assert_true(invalid_result.errors.contains("Invalid trace_id"))
  assert_true(invalid_result.errors.contains("Invalid span_id"))
  assert_true(invalid_result.errors.contains("Invalid service_name"))
  assert_true(invalid_result.errors.contains("Invalid operation_name"))
  assert_true(invalid_result.errors.contains("Invalid duration"))
  assert_true(invalid_result.errors.contains("Invalid status"))
  
  // Test partial validation
  let partial_valid_record = {
    timestamp: 1640995200,
    trace_id: "trace-123",
    span_id: "span-456",
    service_name: "valid_service",
    operation_name: "valid_operation",
    duration: 100,
    status: "invalid_status" // Only status is invalid
  }
  
  let partial_result = validate_telemetry_record(partial_valid_record)
  assert_false(partial_result.valid)
  assert_eq(partial_result.errors.length(), 1)
  assert_true(partial_result.errors.contains("Invalid status"))
  
  // Batch validation
  let validate_batch = fn(records: Array[TelemetryRecord]) {
    let mut valid_count = 0
    let mut invalid_count = 0
    let mut all_errors = []
    
    for record in records {
      let result = validate_telemetry_record(record)
      if result.valid {
        valid_count = valid_count + 1
      } else {
        invalid_count = invalid_count + 1
        all_errors = all_errors.concat(result.errors)
      }
    }
    
    {
      total: records.length(),
      valid: valid_count,
      invalid: invalid_count,
      errors: all_errors
    }
  }
  
  let batch_records = [valid_record, invalid_record, partial_valid_record]
  let batch_result = validate_batch(batch_records)
  
  assert_eq(batch_result.total, 3)
  assert_eq(batch_result.valid, 1)
  assert_eq(batch_result.invalid, 2)
  assert_true(batch_result.errors.length() > 0)
}
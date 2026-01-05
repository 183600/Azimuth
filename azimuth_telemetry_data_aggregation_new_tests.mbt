// Azimuth Telemetry Data Aggregation Tests
// This file contains test cases for telemetry data aggregation functionality

// Test 1: Metric Aggregation
test "metric aggregation operations" {
  // Create metric provider
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_meter")
  
  // Create counter metrics
  let request_counter = Meter::create_counter(meter, "http_requests_total", Some("Total HTTP requests"), Some("count"))
  let error_counter = Meter::create_counter(meter, "http_errors_total", Some("Total HTTP errors"), Some("count"))
  
  // Simulate metric collection
  Counter::add(request_counter, 100.0, Some(Attributes::new()))
  Counter::add(error_counter, 5.0, Some(Attributes::new()))
  
  // Create histogram for response times
  let response_histogram = Meter::create_histogram(meter, "http_response_time", Some("HTTP response times"), Some("ms"))
  
  // Record different response times
  let response_times = [10.0, 25.0, 50.0, 75.0, 100.0, 150.0, 200.0, 300.0, 500.0, 1000.0]
  for time in response_times {
    Histogram::record(response_histogram, time, Some(Attributes::new()))
  }
  
  // Test metric aggregation functions
  let calculate_average = fn(values: Array[Float]) {
    let sum = values.reduce(fn(acc, x) { acc + x }, 0.0)
    sum / values.length().to_float()
  }
  
  let calculate_percentile = fn(values: Array[Float], percentile: Float) {
    let sorted = values.sort(fn(a, b) { a <= b })
    let index = ((sorted.length() - 1) as Float * percentile / 100.0) as Int
    sorted[index]
  }
  
  // Test aggregation calculations
  let avg_response_time = calculate_average(response_times)
  assert_true(avg_response_time > 200.0)
  assert_true(avg_response_time < 300.0)
  
  let p95_response_time = calculate_percentile(response_times, 95.0)
  assert_eq(p95_response_time, 1000.0)
  
  let p50_response_time = calculate_percentile(response_times, 50.0)
  assert_eq(p50_response_time, 150.0)
  
  // Test error rate calculation
  let error_rate = 5.0 / 100.0 * 100.0
  assert_eq(error_rate, 5.0)
}

// Test 2: Time Series Aggregation
test "time series aggregation" {
  // Simulate time series data points
  let time_series_data = [
    (1640995200, 10.0),   // 2022-01-01 00:00:00
    (1640995260, 15.0),   // 2022-01-01 00:01:00
    (1640995320, 12.0),   // 2022-01-01 00:02:00
    (1640995380, 18.0),   // 2022-01-01 00:03:00
    (1640995440, 20.0),   // 2022-01-01 00:04:00
    (1640995500, 25.0),   // 2022-01-01 00:05:00
    (1640995560, 22.0),   // 2022-01-01 00:06:00
    (1640995620, 30.0),   // 2022-01-01 00:07:00
    (1640995680, 28.0),   // 2022-01-01 00:08:00
    (1640995740, 35.0)    // 2022-01-01 00:09:00
  ]
  
  // Test time window aggregation
  let aggregate_by_window = fn(data: Array[(Int, Float)], window_size: Int) {
    let mut result = []
    let mut i = 0
    
    while i < data.length() {
      let window_start = data[i].0
      let window_end = window_start + window_size
      let mut window_sum = 0.0
      let mut window_count = 0
      
      // Collect data points within window
      let mut j = i
      while j < data.length() and data[j].0 < window_end {
        window_sum = window_sum + data[j].1
        window_count = window_count + 1
        j = j + 1
      }
      
      if window_count > 0 {
        result = result.push((window_start, window_sum / window_count.to_float()))
      }
      
      i = j
    }
    
    result
  }
  
  // Aggregate by 5-minute windows (300 seconds)
  let aggregated_5min = aggregate_by_window(time_series_data, 300)
  assert_eq(aggregated_5min.length(), 2)
  
  // First window: average of first 5 data points
  assert_eq(aggregated_5min[0].0, 1640995200)
  let first_window_avg = (10.0 + 15.0 + 12.0 + 18.0 + 20.0) / 5.0
  assert_eq(aggregated_5min[0].1, first_window_avg)
  
  // Second window: average of last 5 data points
  assert_eq(aggregated_5min[1].0, 1640995500)
  let second_window_avg = (25.0 + 22.0 + 30.0 + 28.0 + 35.0) / 5.0
  assert_eq(aggregated_5min[1].1, second_window_avg)
  
  // Test downsampling (reduce frequency)
  let downsample = fn(data: Array[(Int, Float)], factor: Int) {
    let mut result = []
    let mut i = 0
    
    while i < data.length() {
      result = result.push(data[i])
      i = i + factor
    }
    
    result
  }
  
  // Downsample by factor of 2 (keep every 2nd point)
  let downsampled = downsample(time_series_data, 2)
  assert_eq(downsampled.length(), 5)
  assert_eq(downsampled[0].1, 10.0)
  assert_eq(downsampled[1].1, 12.0)
  assert_eq(downsampled[2].1, 20.0)
  assert_eq(downsampled[3].1, 22.0)
  assert_eq(downsampled[4].1, 28.0)
}

// Test 3: Span Aggregation
test "span aggregation and analysis" {
  // Create span contexts
  let trace_id = "trace_12345"
  let spans = [
    {
      name: "HTTP GET /api/users",
      span_id: "span_1",
      parent_span_id: None,
      start_time: 1640995200,
      end_time: 1640995250,
      status: "ok"
    },
    {
      name: "Database Query",
      span_id: "span_2",
      parent_span_id: Some("span_1"),
      start_time: 1640995210,
      end_time: 1640995230,
      status: "ok"
    },
    {
      name: "Cache Lookup",
      span_id: "span_3",
      parent_span_id: Some("span_1"),
      start_time: 1640995205,
      end_time: 1640995210,
      status: "ok"
    },
    {
      name: "External API Call",
      span_id: "span_4",
      parent_span_id: Some("span_2"),
      start_time: 1640995220,
      end_time: 1640995240,
      status: "error"
    },
    {
      name: "Response Serialization",
      span_id: "span_5",
      parent_span_id: Some("span_1"),
      start_time: 1640995230,
      end_time: 1640995250,
      status: "ok"
    }
  ]
  
  // Calculate span durations
  let calculate_duration = fn(span: {name: String, span_id: String, parent_span_id: Option[String], start_time: Int, end_time: Int, status: String}) {
    span.end_time - span.start_time
  }
  
  let durations = spans.map(calculate_duration)
  assert_eq(durations, [50, 20, 5, 20, 20])
  
  // Test span tree analysis
  let find_root_spans = fn(all_spans: Array[{name: String, span_id: String, parent_span_id: Option[String], start_time: Int, end_time: Int, status: String}]) {
    all_spans.filter(fn(span) { span.parent_span_id == None })
  }
  
  let find_child_spans = fn(all_spans: Array[{name: String, span_id: String, parent_span_id: Option[String], start_time: Int, end_time: Int, status: String}], parent_id: String) {
    all_spans.filter(fn(span) { 
      match span.parent_span_id {
        Some(id) => id == parent_id
        None => false
      }
    })
  }
  
  // Find root spans
  let root_spans = find_root_spans(spans)
  assert_eq(root_spans.length(), 1)
  assert_eq(root_spans[0].name, "HTTP GET /api/users")
  
  // Find child spans of root
  let root_children = find_child_spans(spans, "span_1")
  assert_eq(root_children.length(), 3)
  assert_true(root_children.map(fn(s) { s.name }).contains("Database Query"))
  assert_true(root_children.map(fn(s) { s.name }).contains("Cache Lookup"))
  assert_true(root_children.map(fn(s) { s.name }).contains("Response Serialization"))
  
  // Find child spans of database query
  let db_children = find_child_spans(spans, "span_2")
  assert_eq(db_children.length(), 1)
  assert_eq(db_children[0].name, "External API Call")
  
  // Test span statistics
  let calculate_span_stats = fn(all_spans: Array[{name: String, span_id: String, parent_span_id: Option[String], start_time: Int, end_time: Int, status: String}]) {
    let durations = all_spans.map(calculate_duration)
    let total_duration = durations.reduce(fn(acc, x) { acc + x }, 0)
    let avg_duration = total_duration / durations.length()
    let max_duration = durations.reduce(fn(acc, x) { if x > acc { x } else { acc } }, 0)
    let min_duration = durations.reduce(fn(acc, x) { if x < acc { x } else { acc } }, durations[0])
    
    let error_count = all_spans.filter(fn(s) { s.status == "error" }).length()
    let error_rate = error_count.to_float() / all_spans.length().to_float() * 100.0
    
    {
      total_spans: all_spans.length(),
      total_duration,
      avg_duration,
      max_duration,
      min_duration,
      error_count,
      error_rate
    }
  }
  
  let span_stats = calculate_span_stats(spans)
  assert_eq(span_stats.total_spans, 5)
  assert_eq(span_stats.total_duration, 115)
  assert_eq(span_stats.avg_duration, 23)
  assert_eq(span_stats.max_duration, 50)
  assert_eq(span_stats.min_duration, 5)
  assert_eq(span_stats.error_count, 1)
  assert_eq(span_stats.error_rate, 20.0)
}

// Test 4: Log Aggregation
test "log aggregation and filtering" {
  // Create log records
  let log_records = [
    {
      timestamp: 1640995200,
      level: "INFO",
      message: "Request received",
      trace_id: Some("trace_1"),
      span_id: Some("span_1")
    },
    {
      timestamp: 1640995210,
      level: "DEBUG",
      message: "Cache lookup started",
      trace_id: Some("trace_1"),
      span_id: Some("span_2")
    },
    {
      timestamp: 1640995220,
      level: "WARN",
      message: "Cache miss occurred",
      trace_id: Some("trace_1"),
      span_id: Some("span_2")
    },
    {
      timestamp: 1640995230,
      level: "ERROR",
      message: "Database connection failed",
      trace_id: Some("trace_1"),
      span_id: Some("span_3")
    },
    {
      timestamp: 1640995240,
      level: "INFO",
      message: "Fallback to secondary database",
      trace_id: Some("trace_1"),
      span_id: Some("span_3")
    },
    {
      timestamp: 1640995250,
      level: "INFO",
      message: "Request completed",
      trace_id: Some("trace_1"),
      span_id: Some("span_1")
    }
  ]
  
  // Test log filtering by level
  let filter_by_level = fn(logs: Array[{timestamp: Int, level: String, message: String, trace_id: Option[String], span_id: Option[String]}], target_level: String) {
    logs.filter(fn(log) { log.level == target_level })
  }
  
  let info_logs = filter_by_level(log_records, "INFO")
  assert_eq(info_logs.length(), 3)
  assert_eq(info_logs[0].message, "Request received")
  assert_eq(info_logs[1].message, "Fallback to secondary database")
  assert_eq(info_logs[2].message, "Request completed")
  
  let error_logs = filter_by_level(log_records, "ERROR")
  assert_eq(error_logs.length(), 1)
  assert_eq(error_logs[0].message, "Database connection failed")
  
  // Test log filtering by time range
  let filter_by_time_range = fn(logs: Array[{timestamp: Int, level: String, message: String, trace_id: Option[String], span_id: Option[String]}], start_time: Int, end_time: Int) {
    logs.filter(fn(log) { log.timestamp >= start_time and log.timestamp <= end_time })
  }
  
  let time_filtered_logs = filter_by_time_range(log_records, 1640995210, 1640995230)
  assert_eq(time_filtered_logs.length(), 2)
  assert_eq(time_filtered_logs[0].message, "Cache lookup started")
  assert_eq(time_filtered_logs[1].message, "Cache miss occurred")
  
  // Test log aggregation by trace
  let group_by_trace = fn(logs: Array[{timestamp: Int, level: String, message: String, trace_id: Option[String], span_id: Option[String]}]) {
    let mut result = []
    let mut processed_traces = []
    
    for log in logs {
      match log.trace_id {
        Some(trace_id) => {
          if not(processed_traces.contains(trace_id)) {
            processed_traces = processed_traces.push(trace_id)
            let trace_logs = logs.filter(fn(l) { 
              match l.trace_id {
                Some(id) => id == trace_id
                None => false
              }
            })
            result = result.push((trace_id, trace_logs))
          }
        }
        None => {}
      }
    }
    
    result
  }
  
  let grouped_logs = group_by_trace(log_records)
  assert_eq(grouped_logs.length(), 1)
  assert_eq(grouped_logs[0].0, "trace_1")
  assert_eq(grouped_logs[0].1.length(), 6)
  
  // Test log level distribution
  let calculate_log_distribution = fn(logs: Array[{timestamp: Int, level: String, message: String, trace_id: Option[String], span_id: Option[String]}]) {
    let levels = ["DEBUG", "INFO", "WARN", "ERROR"]
    let mut distribution = []
    
    for level in levels {
      let count = logs.filter(fn(log) { log.level == level }).length()
      distribution = distribution.push((level, count))
    }
    
    distribution
  }
  
  let log_distribution = calculate_log_distribution(log_records)
  assert_eq(log_distribution, [
    ("DEBUG", 1),
    ("INFO", 3),
    ("WARN", 1),
    ("ERROR", 1)
  ])
}

// Test 5: Multi-Source Aggregation
test "multi-source telemetry aggregation" {
  // Simulate data from multiple sources
  let metrics_data = [
    ("service_a", "cpu_usage", 75.5),
    ("service_a", "memory_usage", 60.2),
    ("service_b", "cpu_usage", 45.8),
    ("service_b", "memory_usage", 80.1),
    ("service_c", "cpu_usage", 30.2),
    ("service_c", "memory_usage", 55.7)
  ]
  
  let traces_data = [
    ("service_a", "request_count", 1000),
    ("service_a", "error_count", 50),
    ("service_b", "request_count", 800),
    ("service_b", "error_count", 20),
    ("service_c", "request_count", 600),
    ("service_c", "error_count", 10)
  ]
  
  let logs_data = [
    ("service_a", "log_count", 2500),
    ("service_b", "log_count", 1800),
    ("service_c", "log_count", 1200)
  ]
  
  // Aggregate data by service
  let aggregate_by_service = fn(data: Array[(String, String, Float)]) {
    let mut result = []
    let mut services = []
    
    // Extract unique services
    for (service, _, _) in data {
      if not(services.contains(service)) {
        services = services.push(service)
      }
    }
    
    // Group data by service
    for service in services {
      let service_data = data.filter(fn(item) { item.0 == service })
      result = result.push((service, service_data))
    }
    
    result
  }
  
  let metrics_by_service = aggregate_by_service(metrics_data)
  assert_eq(metrics_by_service.length(), 3)
  
  // Verify service_a metrics
  let service_a_metrics = metrics_by_service.filter(fn(item) { item.0 == "service_a" })[0].1
  assert_eq(service_a_metrics.length(), 2)
  assert_true(service_a_metrics.contains(("service_a", "cpu_usage", 75.5)))
  assert_true(service_a_metrics.contains(("service_a", "memory_usage", 60.2)))
  
  // Calculate service health scores
  let calculate_health_score = fn(service: String, metrics: Array[(String, String, Float)], traces: Array[(String, String, Float)], logs: Array[(String, String, Float)]) {
    let service_metrics = metrics.filter(fn(m) { m.0 == service })
    let service_traces = traces.filter(fn(t) { t.0 == service })
    let service_logs = logs.filter(fn(l) { l.0 == service })
    
    // Calculate error rate from traces
    let mut request_count = 0.0
    let mut error_count = 0.0
    
    for (_, metric_name, value) in service_traces {
      if metric_name == "request_count" {
        request_count = value
      } else if metric_name == "error_count" {
        error_count = value
      }
    }
    
    let error_rate = if request_count > 0.0 { error_count / request_count * 100.0 } else { 0.0 }
    
    // Calculate average resource usage
    let mut cpu_usage = 0.0
    let mut memory_usage = 0.0
    
    for (_, metric_name, value) in service_metrics {
      if metric_name == "cpu_usage" {
        cpu_usage = value
      } else if metric_name == "memory_usage" {
        memory_usage = value
      }
    }
    
    let avg_resource_usage = (cpu_usage + memory_usage) / 2.0
    
    // Simple health score calculation (lower is better)
    let health_score = error_rate * 2.0 + avg_resource_usage * 0.5
    
    {
      service,
      error_rate,
      avg_resource_usage,
      health_score
    }
  }
  
  let service_a_health = calculate_health_score("service_a", metrics_data, traces_data, logs_data)
  let service_b_health = calculate_health_score("service_b", metrics_data, traces_data, logs_data)
  let service_c_health = calculate_health_score("service_c", metrics_data, traces_data, logs_data)
  
  // Verify health scores
  assert_eq(service_a_health.service, "service_a")
  assert_eq(service_a_health.error_rate, 5.0)  // 50/1000 * 100
  assert_eq(service_a_health.avg_resource_usage, 67.85)  // (75.5 + 60.2) / 2
  
  assert_eq(service_b_health.service, "service_b")
  assert_eq(service_b_health.error_rate, 2.5)  // 20/800 * 100
  assert_eq(service_b_health.avg_resource_usage, 62.95)  // (45.8 + 80.1) / 2
  
  assert_eq(service_c_health.service, "service_c")
  assert_eq(service_c_health.error_rate, 1.67)  // 10/600 * 100 (approximately)
  assert_eq(service_c_health.avg_resource_usage, 42.95)  // (30.2 + 55.7) / 2
  
  // Service C should have the best health score (lowest)
  assert_true(service_c_health.health_score < service_b_health.health_score)
  assert_true(service_b_health.health_score < service_a_health.health_score)
}
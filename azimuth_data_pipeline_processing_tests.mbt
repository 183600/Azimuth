// Azimuth Data Pipeline Processing Tests
// This file contains test cases for data pipeline processing functionality

// Test 1: Basic Data Pipeline Processing
test "basic data pipeline processing" {
  // Define data types for pipeline processing
  type TelemetryData = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    service_name: String,
    operation_name: String,
    duration_ms: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  type ProcessedData = {
    timestamp: Int,
    trace_id: String,
    service_name: String,
    avg_duration: Float,
    operation_count: Int,
    error_rate: Float
  }
  
  // Create sample telemetry data
  let telemetry_data = [
    {
      timestamp: 1640995200,
      trace_id: "trace-001",
      span_id: "span-001",
      service_name: "payment-service",
      operation_name: "process_payment",
      duration_ms: 150,
      status: "success",
      attributes: [("user.id", "user123"), ("amount", "100.00")]
    },
    {
      timestamp: 1640995205,
      trace_id: "trace-002",
      span_id: "span-002",
      service_name: "payment-service",
      operation_name: "process_payment",
      duration_ms: 200,
      status: "success",
      attributes: [("user.id", "user456"), ("amount", "250.50")]
    },
    {
      timestamp: 1640995210,
      trace_id: "trace-003",
      span_id: "span-003",
      service_name: "payment-service",
      operation_name: "process_payment",
      duration_ms: 100,
      status: "error",
      attributes: [("user.id", "user789"), ("amount", "75.25")]
    }
  ]
  
  // Define pipeline processing function
  let process_pipeline = fn(data: Array[TelemetryData]) {
    let mut service_data = {}
    
    // Group data by service name
    for item in data {
      let service = item.service_name
      let existing = match service_data[service] {
        Some(d) => d
        None => { 
          {
            total_duration: 0,
            operation_count: 0,
            error_count: 0,
            latest_timestamp: 0
          }
        }
      }
      
      let updated = {
        total_duration: existing.total_duration + item.duration_ms,
        operation_count: existing.operation_count + 1,
        error_count: existing.error_count + if item.status == "error" { 1 } else { 0 },
        latest_timestamp: if item.timestamp > existing.latest_timestamp { item.timestamp } else { existing.latest_timestamp }
      }
      
      service_data = service_data.set(service, updated)
    }
    
    // Convert to processed data
    let mut result = []
    for (service, data) in service_data {
      let avg_duration = (data.total_duration.to_float() / data.operation_count.to_float())
      let error_rate = (data.error_count.to_float() / data.operation_count.to_float()) * 100.0
      
      result = result.push({
        timestamp: data.latest_timestamp,
        trace_id: service + "-aggregated",
        service_name: service,
        avg_duration: avg_duration,
        operation_count: data.operation_count,
        error_rate: error_rate
      })
    }
    
    result
  }
  
  // Process the data through the pipeline
  let processed_data = process_pipeline(telemetry_data)
  
  // Verify the results
  assert_eq(processed_data.length(), 1)
  
  let payment_service_data = processed_data[0]
  assert_eq(payment_service_data.service_name, "payment-service")
  assert_eq(payment_service_data.operation_count, 3)
  assert_eq(payment_service_data.avg_duration, 150.0) // (150 + 200 + 100) / 3
  assert_eq(payment_service_data.error_rate, 33.333333333333336) // 1 error out of 3 operations
  assert_eq(payment_service_data.timestamp, 1640995210) // Latest timestamp
}

// Test 2: Data Pipeline Filtering and Transformation
test "data pipeline filtering and transformation" {
  // Define data types
  type RawMetric = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)],
    metric_type: String
  }
  
  type TransformedMetric = {
    name: String,
    value: Float,
    timestamp: Int,
    normalized_name: String,
    category: String,
    is_critical: Bool
  }
  
  // Create sample raw metrics
  let raw_metrics = [
    {
      name: "http_request_duration_seconds",
      value: 0.250,
      timestamp: 1640995200,
      tags: [("service", "api"), ("endpoint", "/users"), ("method", "GET")],
      metric_type: "histogram"
    },
    {
      name: "http_requests_total",
      value: 1250.0,
      timestamp: 1640995200,
      tags: [("service", "api"), ("endpoint", "/users"), ("method", "GET"), ("status", "200")],
      metric_type: "counter"
    },
    {
      name: "database_connection_pool_active",
      value: 8.0,
      timestamp: 1640995200,
      tags: [("service", "api"), ("database", "postgres")],
      metric_type: "gauge"
    },
    {
      name: "error_rate_percentage",
      value: 5.2,
      timestamp: 1640995200,
      tags: [("service", "api"), ("error_type", "timeout")],
      metric_type: "gauge"
    }
  ]
  
  // Define filtering and transformation pipeline
  let transform_metrics = fn(metrics: Array[RawMetric]) {
    // Filter only histogram and counter metrics
    let filtered = metrics.filter(fn(m) { 
      m.metric_type == "histogram" or m.metric_type == "counter" 
    })
    
    // Transform the filtered metrics
    filtered.map(fn(m) {
      let normalized_name = m.name.replace(".", "_").replace("-", "_")
      let category = match m.metric_type {
        "histogram" => "latency"
        "counter" => "throughput"
        _ => "other"
      }
      let is_critical = m.name.contains("error") or m.name.contains("timeout")
      
      {
        name: m.name,
        value: m.value,
        timestamp: m.timestamp,
        normalized_name: normalized_name,
        category: category,
        is_critical: is_critical
      }
    })
  }
  
  // Transform the metrics
  let transformed_metrics = transform_metrics(raw_metrics)
  
  // Verify the results
  assert_eq(transformed_metrics.length(), 2)
  
  let duration_metric = transformed_metrics[0]
  assert_eq(duration_metric.name, "http_request_duration_seconds")
  assert_eq(duration_metric.normalized_name, "http_request_duration_seconds")
  assert_eq(duration_metric.category, "latency")
  assert_false(duration_metric.is_critical)
  
  let request_metric = transformed_metrics[1]
  assert_eq(request_metric.name, "http_requests_total")
  assert_eq(request_metric.normalized_name, "http_requests_total")
  assert_eq(request_metric.category, "throughput")
  assert_false(request_metric.is_critical)
}

// Test 3: Data Pipeline Aggregation and Windowing
test "data pipeline aggregation and windowing" {
  // Define data types
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    metric_name: String
  }
  
  type AggregatedPoint = {
    window_start: Int,
    window_end: Int,
    metric_name: String,
    min_value: Float,
    max_value: Float,
    avg_value: Float,
    sum_value: Float,
    count: Int
  }
  
  // Create sample time series data
  let time_series_data = [
    { timestamp: 1640995200, value: 10.5, metric_name: "cpu_usage" },
    { timestamp: 1640995210, value: 12.3, metric_name: "cpu_usage" },
    { timestamp: 1640995220, value: 15.7, metric_name: "cpu_usage" },
    { timestamp: 1640995230, value: 14.2, metric_name: "cpu_usage" },
    { timestamp: 1640995240, value: 11.8, metric_name: "cpu_usage" },
    { timestamp: 1640995250, value: 9.6, metric_name: "cpu_usage" },
    { timestamp: 1640995260, value: 13.4, metric_name: "cpu_usage" },
    { timestamp: 1640995270, value: 16.9, metric_name: "cpu_usage" }
  ]
  
  // Define windowed aggregation function
  let aggregate_by_window = fn(data: Array[TimeSeriesPoint], window_size_seconds: Int) {
    // Group data by metric name and window
    let mut grouped_data = {}
    
    for point in data {
      let metric_name = point.metric_name
      let window_start = (point.timestamp / window_size_seconds) * window_size_seconds
      let window_end = window_start + window_size_seconds
      let window_key = metric_name + ":" + window_start.to_string()
      
      let existing = match grouped_data[window_key] {
        Some(d) => d
        None => {
          {
            window_start: window_start,
            window_end: window_end,
            metric_name: metric_name,
            min_value: point.value,
            max_value: point.value,
            sum_value: point.value,
            count: 1
          }
        }
      }
      
      let updated = {
        window_start: existing.window_start,
        window_end: existing.window_end,
        metric_name: existing.metric_name,
        min_value: if point.value < existing.min_value { point.value } else { existing.min_value },
        max_value: if point.value > existing.max_value { point.value } else { existing.max_value },
        sum_value: existing.sum_value + point.value,
        count: existing.count + 1
      }
      
      grouped_data = grouped_data.set(window_key, updated)
    }
    
    // Convert grouped data to array and calculate averages
    let mut result = []
    for (key, data) in grouped_data {
      let avg_value = data.sum_value / data.count.to_float()
      
      result = result.push({
        window_start: data.window_start,
        window_end: data.window_end,
        metric_name: data.metric_name,
        min_value: data.min_value,
        max_value: data.max_value,
        avg_value: avg_value,
        sum_value: data.sum_value,
        count: data.count
      })
    }
    
    // Sort by window start time
    result.sort_by(fn(a, b) { a.window_start - b.window_start })
  }
  
  // Aggregate the data with 60-second windows
  let aggregated_data = aggregate_by_window(time_series_data, 60)
  
  // Verify the results
  assert_eq(aggregated_data.length(), 2)
  
  let first_window = aggregated_data[0]
  assert_eq(first_window.window_start, 1640995200)
  assert_eq(first_window.window_end, 1640995260)
  assert_eq(first_window.metric_name, "cpu_usage")
  assert_eq(first_window.min_value, 9.6)
  assert_eq(first_window.max_value, 15.7)
  assert_eq(first_window.count, 6)
  
  let second_window = aggregated_data[1]
  assert_eq(second_window.window_start, 1640995260)
  assert_eq(second_window.window_end, 1640995320)
  assert_eq(second_window.metric_name, "cpu_usage")
  assert_eq(second_window.min_value, 13.4)
  assert_eq(second_window.max_value, 16.9)
  assert_eq(second_window.count, 2)
}

// Test 4: Data Pipeline Error Handling
test "data pipeline error handling" {
  // Define data types
  type PipelineResult[T] = {
    success: Bool,
    data: Option[T],
    errors: Array[String]
  }
  
  type ProcessingStep = {
    name: String,
    process: (Array[String]) -> PipelineResult[Array[String]]
  }
  
  // Create processing steps
  let validate_data = fn(data: Array[String]) {
    let mut errors = []
    let mut valid_data = []
    
    for item in data {
      if item.length() > 0 and item.length() <= 100 {
        valid_data = valid_data.push(item)
      } else {
        errors = errors.push("Invalid item length: " + item.length().to_string())
      }
    }
    
    {
      success: errors.length() == 0,
      data: Some(valid_data),
      errors: errors
    }
  }
  
  let transform_data = fn(data: Array[String]) {
    let mut errors = []
    let mut transformed_data = []
    
    for item in data {
      if item.contains("error") {
        errors = errors.push("Item contains error: " + item)
      } else {
        transformed_data = transformed_data.push(item.to_uppercase())
      }
    }
    
    {
      success: errors.length() == 0,
      data: Some(transformed_data),
      errors: errors
    }
  }
  
  let filter_data = fn(data: Array[String]) {
    let filtered = data.filter(fn(item) { not(item.starts_with("test")) })
    
    {
      success: true,
      data: Some(filtered),
      errors: []
    }
  }
  
  // Define pipeline execution function
  let execute_pipeline = fn(data: Array[String], steps: Array[ProcessingStep]) {
    let mut current_data = data
    let mut all_errors = []
    let mut pipeline_success = true
    
    for step in steps {
      let result = step.process(current_data)
      
      if result.success {
        match result.data {
          Some(d) => current_data = d
          None => {
            pipeline_success = false
            all_errors = all_errors.push("Step " + step.name + " returned no data")
          }
        }
      } else {
        pipeline_success = false
        all_errors = all_errors.extend(result.errors)
      }
    }
    
    {
      success: pipeline_success,
      data: Some(current_data),
      errors: all_errors
    }
  }
  
  // Create pipeline steps
  let pipeline_steps = [
    { name: "validate", process: validate_data },
    { name: "transform", process: transform_data },
    { name: "filter", process: filter_data }
  ]
  
  // Test with valid data
  let valid_data = ["item1", "item2", "item3"]
  let valid_result = execute_pipeline(valid_data, pipeline_steps)
  
  assert_true(valid_result.success)
  assert_eq(valid_result.errors.length(), 0)
  match valid_result.data {
    Some(d) => {
      assert_eq(d.length(), 3)
      assert_eq(d[0], "ITEM1")
      assert_eq(d[1], "ITEM2")
      assert_eq(d[2], "ITEM3")
    }
    None => assert_true(false)
  }
  
  // Test with invalid data
  let invalid_data = ["", "item_with_error", "test_item", "a".repeat(101)]
  let invalid_result = execute_pipeline(invalid_data, pipeline_steps)
  
  assert_false(invalid_result.success)
  assert_true(invalid_result.errors.length() > 0)
  
  // Check specific errors
  let errors = invalid_result.errors
  assert_true(errors.some(fn(e) { e.contains("Invalid item length") }))
  assert_true(errors.some(fn(e) { e.contains("Item contains error") }))
}

// Test 5: Data Pipeline Performance Monitoring
test "data pipeline performance monitoring" {
  // Define performance metrics
  type PerformanceMetrics = {
    total_processed: Int,
    processing_time_ms: Int,
    throughput_per_second: Float,
    memory_usage_mb: Float
  }
  
  // Define data processing function with performance tracking
  let process_with_performance_tracking = fn(data: Array[String], processing_fn: (String) -> String) {
    let start_time = 1640995200  // Simulated timestamp
    let start_memory = 50.0      // Simulated memory usage in MB
    
    let mut processed_data = []
    for item in data {
      let processed = processing_fn(item)
      processed_data = processed_data.push(processed)
    }
    
    let end_time = 1640995250  // Simulated timestamp (50ms later)
    let end_memory = 55.0      // Simulated memory usage in MB
    
    let processing_time = end_time - start_time
    let throughput = if processing_time > 0 {
      (data.length().to_float() / processing_time.to_float()) * 1000.0  // per second
    } else {
      0.0
    }
    let memory_usage = end_memory - start_memory
    
    {
      processed_data: processed_data,
      metrics: {
        total_processed: data.length(),
        processing_time_ms: processing_time,
        throughput_per_second: throughput,
        memory_usage_mb: memory_usage
      }
    }
  }
  
  // Define a simple processing function
  let simple_processing = fn(item: String) {
    "processed_" + item
  }
  
  // Create test data
  let test_data = []
  for i in 1..=100 {
    test_data = test_data.push("item_" + i.to_string())
  }
  
  // Process the data with performance tracking
  let result = process_with_performance_tracking(test_data, simple_processing)
  
  // Verify the processed data
  assert_eq(result.processed_data.length(), 100)
  assert_eq(result.processed_data[0], "processed_item_1")
  assert_eq(result.processed_data[99], "processed_item_100")
  
  // Verify the performance metrics
  assert_eq(result.metrics.total_processed, 100)
  assert_eq(result.metrics.processing_time_ms, 50)
  assert_eq(result.metrics.throughput_per_second, 2000.0)  // 100 items / 0.05 seconds
  assert_eq(result.metrics.memory_usage_mb, 5.0)
  
  // Test with a larger dataset
  let large_data = []
  for i in 1..=1000 {
    large_data = large_data.push("large_item_" + i.to_string())
  }
  
  let large_result = process_with_performance_tracking(large_data, simple_processing)
  
  assert_eq(large_result.metrics.total_processed, 1000)
  assert_eq(large_result.metrics.processing_time_ms, 50)
  assert_eq(large_result.metrics.throughput_per_second, 20000.0)  // 1000 items / 0.05 seconds
  assert_eq(large_result.metrics.memory_usage_mb, 50.0)  // More memory for larger dataset
}
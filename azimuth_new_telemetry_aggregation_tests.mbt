// Azimuth Telemetry System - New Telemetry Aggregation Tests
// This file contains test cases for telemetry data aggregation functionality

// Test 1: Telemetry Data Aggregation
test "telemetry data aggregation operations" {
  // Create telemetry data points
  let data_point1 = {
    timestamp: 1640995200,
    trace_id: "trace-001",
    span_id: "span-001",
    service_name: "payment-service",
    operation_name: "process_payment",
    duration: 250,
    status: "success"
  }
  
  let data_point2 = {
    timestamp: 1640995250,
    trace_id: "trace-002",
    span_id: "span-002",
    service_name: "payment-service",
    operation_name: "process_payment",
    duration: 300,
    status: "success"
  }
  
  let data_point3 = {
    timestamp: 1640995300,
    trace_id: "trace-003",
    span_id: "span-003",
    service_name: "payment-service",
    operation_name: "process_payment",
    duration: 180,
    status: "error"
  }
  
  // Create aggregation function
  let aggregate_telemetry = fn(data_points: Array[Dynamic]) {
    let mut total_duration = 0
    let mut success_count = 0
    let mut error_count = 0
    let mut min_duration = 999999
    let mut max_duration = 0
    
    for point in data_points {
      let duration = point.duration
      let status = point.status
      
      total_duration = total_duration + duration
      
      if status == "success" {
        success_count = success_count + 1
      } else {
        error_count = error_count + 1
      }
      
      if duration < min_duration {
        min_duration = duration
      }
      
      if duration > max_duration {
        max_duration = duration
      }
    }
    
    let avg_duration = if data_points.length() > 0 {
      total_duration / data_points.length()
    } else {
      0
    }
    
    {
      total_requests: data_points.length(),
      success_count: success_count,
      error_count: error_count,
      avg_duration: avg_duration,
      min_duration: if min_duration == 999999 { 0 } else { min_duration },
      max_duration: max_duration,
      error_rate: if data_points.length() > 0 {
        (error_count * 100) / data_points.length()
      } else {
        0
      }
    }
  }
  
  // Test aggregation
  let telemetry_data = [data_point1, data_point2, data_point3]
  let aggregated = aggregate_telemetry(telemetry_data)
  
  assert_eq(aggregated.total_requests, 3)
  assert_eq(aggregated.success_count, 2)
  assert_eq(aggregated.error_count, 1)
  assert_eq(aggregated.avg_duration, 243) // (250 + 300 + 180) / 3
  assert_eq(aggregated.min_duration, 180)
  assert_eq(aggregated.max_duration, 300)
  assert_eq(aggregated.error_rate, 33) // floor(1 * 100 / 3)
}

// Test 2: Time Window Aggregation
test "time window aggregation operations" {
  // Create time-series telemetry data
  let time_series_data = [
    { timestamp: 1640995200, value: 10, service: "api-gateway" },
    { timestamp: 1640995260, value: 15, service: "api-gateway" },
    { timestamp: 1640995320, value: 12, service: "api-gateway" },
    { timestamp: 1640995380, value: 18, service: "api-gateway" },
    { timestamp: 1640995440, value: 20, service: "api-gateway" },
    { timestamp: 1640995500, value: 14, service: "api-gateway" }
  ]
  
  // Create time window aggregation function
  let aggregate_by_time_window = fn(data: Array[Dynamic], window_size_seconds: Int) {
    let mut windows = []
    let mut processed_indices = []
    
    for i in 0..data.length() {
      if not(processed_indices.contains(i)) {
        let window_start = data[i].timestamp
        let window_end = window_start + window_size_seconds
        
        let mut window_data = []
        let mut window_sum = 0
        let mut window_count = 0
        
        for j in i..data.length() {
          if data[j].timestamp >= window_start and data[j].timestamp <= window_end {
            window_data = window_data.push(data[j])
            window_sum = window_sum + data[j].value
            window_count = window_count + 1
            processed_indices = processed_indices.push(j)
          }
        }
        
        let window_avg = if window_count > 0 {
          window_sum / window_count
        } else {
          0
        }
        
        windows = windows.push({
          window_start: window_start,
          window_end: window_end,
          data_points: window_count,
          sum: window_sum,
          average: window_avg,
          min: window_data.map_fn(d) { d.value }.reduce(fn(acc, val) { 
            if val < acc { val } else { acc } 
          }, 999999),
          max: window_data.map_fn(d) { d.value }.reduce(fn(acc, val) { 
            if val > acc { val } else { acc } 
          }, 0)
        })
      }
    }
    
    windows
  }
  
  // Test 2-minute window aggregation
  let windows = aggregate_by_time_window(time_series_data, 120) // 2 minutes
  
  assert_eq(windows.length(), 3) // 6 data points / 2 per window = 3 windows
  
  // First window (1640995200 to 1640995320)
  assert_eq(windows[0].window_start, 1640995200)
  assert_eq(windows[0].window_end, 1640995320)
  assert_eq(windows[0].data_points, 3)
  assert_eq(windows[0].sum, 37) // 10 + 15 + 12
  assert_eq(windows[0].average, 12) // floor(37 / 3)
  assert_eq(windows[0].min, 10)
  assert_eq(windows[0].max, 15)
  
  // Second window (1640995380 to 1640995500)
  assert_eq(windows[1].window_start, 1640995380)
  assert_eq(windows[1].window_end, 1640995500)
  assert_eq(windows[1].data_points, 3)
  assert_eq(windows[1].sum, 52) // 18 + 20 + 14
  assert_eq(windows[1].average, 17) // floor(52 / 3)
  assert_eq(windows[1].min, 14)
  assert_eq(windows[1].max, 20)
}

// Test 3: Service-level Aggregation
test "service-level aggregation operations" {
  // Create multi-service telemetry data
  let multi_service_data = [
    { service: "api-gateway", operation: "authenticate", duration: 50, status: "success" },
    { service: "api-gateway", operation: "authorize", duration: 30, status: "success" },
    { service: "payment-service", operation: "process_payment", duration: 250, status: "success" },
    { service: "payment-service", operation: "validate_card", duration: 100, status: "success" },
    { service: "notification-service", operation: "send_email", duration: 200, status: "success" },
    { service: "notification-service", operation: "send_sms", duration: 150, status: "error" },
    { service: "api-gateway", operation: "authenticate", duration: 60, status: "success" },
    { service: "payment-service", operation: "process_payment", duration: 300, status: "error" }
  ]
  
  // Create service-level aggregation function
  let aggregate_by_service = fn(data: Array[Dynamic]) {
    let mut services = []
    let mut processed_services = []
    
    for point in data {
      let service_name = point.service
      
      if not(processed_services.contains(service_name)) {
        let service_data = data.filter_fn(p) { p.service == service_name }
        
        let mut total_duration = 0
        let mut success_count = 0
        let mut error_count = 0
        let mut operations = []
        
        for p in service_data {
          total_duration = total_duration + p.duration
          
          if p.status == "success" {
            success_count = success_count + 1
          } else {
            error_count = error_count + 1
          }
          
          if not(operations.contains(p.operation)) {
            operations = operations.push(p.operation)
          }
        }
        
        services = services.push({
          service_name: service_name,
          total_requests: service_data.length(),
          success_count: success_count,
          error_count: error_count,
          avg_duration: total_duration / service_data.length(),
          operations: operations,
          error_rate: (error_count * 100) / service_data.length()
        })
        
        processed_services = processed_services.push(service_name)
      }
    }
    
    services
  }
  
  // Test service aggregation
  let service_stats = aggregate_by_service(multi_service_data)
  
  assert_eq(service_stats.length(), 3) // api-gateway, payment-service, notification-service
  
  // Check api-gateway stats
  let api_gateway = service_stats.filter_fn(s) { s.service_name == "api-gateway" }[0]
  assert_eq(api_gateway.total_requests, 3)
  assert_eq(api_gateway.success_count, 3)
  assert_eq(api_gateway.error_count, 0)
  assert_eq(api_gateway.avg_duration, 46) // floor((50 + 30 + 60) / 3)
  assert_eq(api_gateway.error_rate, 0)
  assert_true(api_gateway.operations.contains("authenticate"))
  assert_true(api_gateway.operations.contains("authorize"))
  
  // Check payment-service stats
  let payment_service = service_stats.filter_fn(s) { s.service_name == "payment-service" }[0]
  assert_eq(payment_service.total_requests, 3)
  assert_eq(payment_service.success_count, 2)
  assert_eq(payment_service.error_count, 1)
  assert_eq(payment_service.avg_duration, 216) // floor((250 + 100 + 300) / 3)
  assert_eq(payment_service.error_rate, 33) // floor(1 * 100 / 3)
  assert_true(payment_service.operations.contains("process_payment"))
  assert_true(payment_service.operations.contains("validate_card"))
  
  // Check notification-service stats
  let notification_service = service_stats.filter_fn(s) { s.service_name == "notification-service" }[0]
  assert_eq(notification_service.total_requests, 2)
  assert_eq(notification_service.success_count, 1)
  assert_eq(notification_service.error_count, 1)
  assert_eq(notification_service.avg_duration, 175) // floor((200 + 150) / 2)
  assert_eq(notification_service.error_rate, 50) // floor(1 * 100 / 2)
  assert_true(notification_service.operations.contains("send_email"))
  assert_true(notification_service.operations.contains("send_sms"))
}

// Test 4: Percentile Calculation
test "percentile calculation operations" {
  // Create latency data
  let latency_data = [10, 25, 20, 30, 15, 50, 40, 35, 45, 12, 18, 22, 28, 32, 48]
  
  // Calculate percentile function
  let calculate_percentile = fn(sorted_data: Array[Int], percentile: Int) {
    if sorted_data.length() == 0 {
      0
    } else {
      let index = (percentile * sorted_data.length()) / 100
      if index >= sorted_data.length() {
        sorted_data[sorted_data.length() - 1]
      } else {
        sorted_data[index]
      }
    }
  }
  
  // Sort the data
  let sorted_latency = latency_data.sort(fn(a, b) { a - b })
  assert_eq(sorted_latency, [10, 12, 15, 18, 20, 22, 25, 28, 30, 32, 35, 40, 45, 48, 50])
  
  // Calculate percentiles
  let p50 = calculate_percentile(sorted_latency, 50) // Median
  let p90 = calculate_percentile(sorted_latency, 90)
  let p95 = calculate_percentile(sorted_latency, 95)
  let p99 = calculate_percentile(sorted_latency, 99)
  
  assert_eq(p50, 25) // 50th percentile
  assert_eq(p90, 45) // 90th percentile
  assert_eq(p95, 48) // 95th percentile
  assert_eq(p99, 50) // 99th percentile
  
  // Test with empty data
  let empty_data: Array[Int] = []
  let empty_percentile = calculate_percentile(empty_data, 50)
  assert_eq(empty_percentile, 0)
  
  // Test with single data point
  let single_data = [100]
  let single_percentile = calculate_percentile(single_data, 95)
  assert_eq(single_percentile, 100)
}

// Test 5: Rate Calculation
test "rate calculation operations" {
  // Create time-series event data
  let event_data = [
    { timestamp: 1640995200, event_type: "request", service: "api-gateway" },
    { timestamp: 1640995210, event_type: "request", service: "api-gateway" },
    { timestamp: 1640995230, event_type: "request", service: "api-gateway" },
    { timestamp: 1640995260, event_type: "error", service: "api-gateway" },
    { timestamp: 1640995280, event_type: "request", service: "api-gateway" },
    { timestamp: 1640995310, event_type: "request", service: "api-gateway" },
    { timestamp: 1640995340, event_type: "request", service: "api-gateway" },
    { timestamp: 1640995370, event_type: "error", service: "api-gateway" }
  ]
  
  // Calculate rate function
  let calculate_rate = fn(data: Array[Dynamic], start_time: Int, end_time: Int, event_type: String) {
    let filtered_events = data.filter_fn(e) { 
      e.timestamp >= start_time and 
      e.timestamp <= end_time and 
      e.event_type == event_type 
    }
    
    let time_window = end_time - start_time
    if time_window > 0 {
      (filtered_events.length() * 60) / time_window // Events per minute
    } else {
      0
    }
  }
  
  // Calculate request rate for the first 2 minutes
  let request_rate = calculate_rate(event_data, 1640995200, 1640995320, "request")
  assert_eq(request_rate, 3) // 6 requests in 2 minutes = 3 per minute
  
  // Calculate error rate for the first 2 minutes
  let error_rate = calculate_rate(event_data, 1640995200, 1640995320, "error")
  assert_eq(error_rate, 0) // 0 errors in the first 2 minutes
  
  // Calculate error rate for the full period
  let total_error_rate = calculate_rate(event_data, 1640995200, 1640995400, "error")
  assert_eq(total_error_rate, 1) // 2 errors in ~3.33 minutes = ~0.6, rounded to 1
  
  // Test with no matching events
  let no_match_rate = calculate_rate(event_data, 1640995200, 1640995320, "nonexistent")
  assert_eq(no_match_rate, 0)
}

// Test 6: Trend Analysis
test "trend analysis operations" {
  // Create time-series metric data
  let metric_data = [
    { timestamp: 1640995200, value: 100 },
    { timestamp: 1640995260, value: 120 },
    { timestamp: 1640995320, value: 110 },
    { timestamp: 1640995380, value: 130 },
    { timestamp: 1640995440, value: 140 },
    { timestamp: 1640995500, value: 125 },
    { timestamp: 1640995560, value: 145 }
  ]
  
  // Calculate trend function
  let calculate_trend = fn(data: Array[Dynamic]) {
    if data.length() < 2 {
      "stable"
    } else {
      let first_half = data.slice(0, data.length() / 2)
      let second_half = data.slice(data.length() / 2, data.length())
      
      let first_avg = first_half.reduce(fn(acc, d) { acc + d.value }, 0) / first_half.length()
      let second_avg = second_half.reduce(fn(acc, d) { acc + d.value }, 0) / second_half.length()
      
      let change_percent = ((second_avg - first_avg) * 100) / first_avg
      
      if change_percent > 10 {
        "increasing"
      } else if change_percent < -10 {
        "decreasing"
      } else {
        "stable"
      }
    }
  }
  
  // Test trend calculation
  let trend = calculate_trend(metric_data)
  assert_eq(trend, "increasing") // Overall trend is upward
  
  // Test with decreasing data
  let decreasing_data = [
    { timestamp: 1640995200, value: 200 },
    { timestamp: 1640995260, value: 180 },
    { timestamp: 1640995320, value: 160 },
    { timestamp: 1640995380, value: 140 },
    { timestamp: 1640995440, value: 120 }
  ]
  
  let decreasing_trend = calculate_trend(decreasing_data)
  assert_eq(decreasing_trend, "decreasing")
  
  // Test with stable data
  let stable_data = [
    { timestamp: 1640995200, value: 100 },
    { timestamp: 1640995260, value: 105 },
    { timestamp: 1640995320, value: 95 },
    { timestamp: 1640995380, value: 102 },
    { timestamp: 1640995440, value: 98 }
  ]
  
  let stable_trend = calculate_trend(stable_data)
  assert_eq(stable_trend, "stable")
  
  // Test with insufficient data
  let insufficient_data = [{ timestamp: 1640995200, value: 100 }]
  let insufficient_trend = calculate_trend(insufficient_data)
  assert_eq(insufficient_trend, "stable")
}
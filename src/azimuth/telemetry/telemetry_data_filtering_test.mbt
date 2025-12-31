// 遥测数据过滤测试用例

test "telemetry_service_filtering" {
  // 测试按服务名称过滤遥测数据
  
  let telemetry_data = [
    ("auth-service", "login_attempt", 150),
    ("user-service", "profile_update", 85),
    ("payment-service", "transaction", 320),
    ("auth-service", "logout", 120),
    ("order-service", "create_order", 200),
    ("payment-service", "refund", 45)
  ]
  
  // 验证原始数据
  assert_eq(telemetry_data.length(), 6)
  
  // 过滤出auth-service的数据
  let auth_service_data = []
  let mut i = 0
  while i < telemetry_data.length() {
    if telemetry_data[i].0 == "auth-service" {
      auth_service_data.push(telemetry_data[i])
    }
    i = i + 1
  }
  
  assert_eq(auth_service_data.length(), 2)
  assert_eq(auth_service_data[0].0, "auth-service")
  assert_eq(auth_service_data[0].1, "login_attempt")
  assert_eq(auth_service_data[1].0, "auth-service")
  assert_eq(auth_service_data[1].1, "logout")
  
  // 过滤出payment-service的数据
  let payment_service_data = []
  i = 0
  while i < telemetry_data.length() {
    if telemetry_data[i].0 == "payment-service" {
      payment_service_data.push(telemetry_data[i])
    }
    i = i + 1
  }
  
  assert_eq(payment_service_data.length(), 2)
  assert_eq(payment_service_data[0].1, "transaction")
  assert_eq(payment_service_data[1].1, "refund")
}

test "telemetry_threshold_filtering" {
  // 测试按阈值过滤遥测数据
  
  let performance_metrics = [
    ("cpu_usage", 45.2),
    ("memory_usage", 78.9),
    ("disk_usage", 32.1),
    ("network_usage", 89.5),
    ("gpu_usage", 12.3)
  ]
  
  // 验证原始数据
  assert_eq(performance_metrics.length(), 5)
  
  // 过滤出使用率大于50%的指标
  let high_usage_metrics = []
  let mut i = 0
  while i < performance_metrics.length() {
    if performance_metrics[i].1 > 50.0 {
      high_usage_metrics.push(performance_metrics[i])
    }
    i = i + 1
  }
  
  assert_eq(high_usage_metrics.length(), 2)
  assert_eq(high_usage_metrics[0].0, "memory_usage")
  assert_eq(high_usage_metrics[0].1, 78.9)
  assert_eq(high_usage_metrics[1].0, "network_usage")
  assert_eq(high_usage_metrics[1].1, 89.5)
  
  // 过滤出使用率小于30%的指标
  let low_usage_metrics = []
  i = 0
  while i < performance_metrics.length() {
    if performance_metrics[i].1 < 30.0 {
      low_usage_metrics.push(performance_metrics[i])
    }
    i = i + 1
  }
  
  assert_eq(low_usage_metrics.length(), 2)
  assert_eq(low_usage_metrics[0].0, "disk_usage")
  assert_eq(low_usage_metrics[1].0, "gpu_usage")
}

test "telemetry_time_range_filtering" {
  // 测试按时间范围过滤遥测数据
  
  let time_series_data = [
    (1640995200L, "request", 100),
    (1640995260L, "request", 120),
    (1640995320L, "error", 5),
    (1640995380L, "request", 95),
    (1640995440L, "request", 110),
    (1640995500L, "error", 8)
  ]
  
  // 验证原始数据
  assert_eq(time_series_data.length(), 6)
  
  // 设置时间范围
  let start_time = 1640995260L
  let end_time = 1640995440L
  
  // 过滤出时间范围内的数据
  let filtered_data = []
  let mut i = 0
  while i < time_series_data.length() {
    if time_series_data[i].0 >= start_time && time_series_data[i].0 <= end_time {
      filtered_data.push(time_series_data[i])
    }
    i = i + 1
  }
  
  assert_eq(filtered_data.length(), 3)
  assert_eq(filtered_data[0].0, 1640995260L)
  assert_eq(filtered_data[1].0, 1640995320L)
  assert_eq(filtered_data[2].0, 1640995380L)
  
  // 进一步过滤出错误事件
  let error_events = []
  i = 0
  while i < filtered_data.length() {
    if filtered_data[i].1 == "error" {
      error_events.push(filtered_data[i])
    }
    i = i + 1
  }
  
  assert_eq(error_events.length(), 1)
  assert_eq(error_events[0].2, 5)
}

test "telemetry_attribute_filtering" {
  // 测试按属性过滤遥测数据
  
  let trace_data = [
    ("trace-001", [("http.method", "GET"), ("http.status_code", "200"), ("service.name", "api-gateway")]),
    ("trace-002", [("http.method", "POST"), ("http.status_code", "201"), ("service.name", "user-service")]),
    ("trace-003", [("http.method", "GET"), ("http.status_code", "404"), ("service.name", "api-gateway")]),
    ("trace-004", [("http.method", "POST"), ("http.status_code", "500"), ("service.name", "payment-service")]),
    ("trace-005", [("http.method", "GET"), ("http.status_code", "200"), ("service.name", "user-service")])
  ]
  
  // 验证原始数据
  assert_eq(trace_data.length(), 5)
  
  // 过滤出GET请求
  let get_requests = []
  let mut i = 0
  while i < trace_data.length() {
    let mut is_get = false
    let mut j = 0
    while j < trace_data[i].1.length() {
      if trace_data[i].1[j].0 == "http.method" && trace_data[i].1[j].1 == "GET" {
        is_get = true
        break
      }
      j = j + 1
    }
    if is_get {
      get_requests.push(trace_data[i])
    }
    i = i + 1
  }
  
  assert_eq(get_requests.length(), 3)
  
  // 进一步过滤出成功响应（状态码200）
  let successful_gets = []
  i = 0
  while i < get_requests.length() {
    let mut is_success = false
    let mut j = 0
    while j < get_requests[i].1.length() {
      if get_requests[i].1[j].0 == "http.status_code" && get_requests[i].1[j].1 == "200" {
        is_success = true
        break
      }
      j = j + 1
    }
    if is_success {
      successful_gets.push(get_requests[i])
    }
    i = i + 1
  }
  
  assert_eq(successful_gets.length(), 2)
  assert_eq(successful_gets[0].0, "trace-001")
  assert_eq(successful_gets[1].0, "trace-005")
}

test "telemetry_error_filtering" {
  // 测试错误数据过滤
  
  let log_entries = [
    ("INFO", "Service started successfully"),
    ("WARN", "High memory usage detected"),
    ("ERROR", "Database connection failed"),
    ("INFO", "Request processed"),
    ("ERROR", "Authentication service unavailable"),
    ("FATAL", "System crash detected"),
    ("WARN", "Disk space running low"),
    ("ERROR", "Payment processing timeout")
  ]
  
  // 验证原始数据
  assert_eq(log_entries.length(), 8)
  
  // 过滤出错误和致命错误
  let error_entries = []
  let mut i = 0
  while i < log_entries.length() {
    if log_entries[i].0 == "ERROR" || log_entries[i].0 == "FATAL" {
      error_entries.push(log_entries[i])
    }
    i = i + 1
  }
  
  assert_eq(error_entries.length(), 4)
  
  // 进一步过滤出包含特定关键词的错误
  let critical_errors = []
  i = 0
  while i < error_entries.length() {
    if error_entries[i].1.contains("connection") || 
       error_entries[i].1.contains("crash") || 
       error_entries[i].1.contains("timeout") {
      critical_errors.push(error_entries[i])
    }
    i = i + 1
  }
  
  assert_eq(critical_errors.length(), 3)
  assert_eq(critical_errors[0].1.contains("connection"), true)
  assert_eq(critical_errors[1].1.contains("crash"), true)
  assert_eq(critical_errors[2].1.contains("timeout"), true)
}
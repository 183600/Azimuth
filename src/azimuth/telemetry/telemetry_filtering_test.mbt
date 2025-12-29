// 遥测数据过滤和筛选测试用例

test "telemetry_data_filtering_by_service" {
  // 测试按服务名称过滤遥测数据
  
  let telemetry_data = [
    ("service1", "api", 200, 150.5),
    ("service2", "api", 404, 45.2),
    ("service1", "db", 200, 125.8),
    ("service3", "api", 500, 89.3),
    ("service1", "cache", 200, 2.1)
  ]
  
  // 过滤service1的数据
  let mut filtered_data = []
  let mut i = 0
  while i < telemetry_data.length() {
    if telemetry_data[i].0 == "service1" {
      filtered_data.push(telemetry_data[i])
    }
    i = i + 1
  }
  
  // 验证过滤结果
  assert_eq(filtered_data.length(), 3)
  assert_eq(filtered_data[0].0, "service1")
  assert_eq(filtered_data[1].0, "service1")
  assert_eq(filtered_data[2].0, "service1")
  assert_eq(filtered_data[0].1, "api")
  assert_eq(filtered_data[1].1, "db")
  assert_eq(filtered_data[2].1, "cache")
}

test "telemetry_data_filtering_by_status" {
  // 测试按状态码过滤遥测数据
  
  let telemetry_data = [
    ("api", 200, 120.5),
    ("api", 404, 25.3),
    ("api", 500, 89.7),
    ("db", 200, 45.8),
    ("api", 200, 156.2)
  ]
  
  // 过滤成功状态码(200)的数据
  let mut success_data = []
  let mut i = 0
  while i < telemetry_data.length() {
    if telemetry_data[i].1 == 200 {
      success_data.push(telemetry_data[i])
    }
    i = i + 1
  }
  
  // 验证过滤结果
  assert_eq(success_data.length(), 3)
  assert_eq(success_data[0].1, 200)
  assert_eq(success_data[1].1, 200)
  assert_eq(success_data[2].1, 200)
  
  // 过滤错误状态码的数据
  let mut error_data = []
  i = 0
  while i < telemetry_data.length() {
    if telemetry_data[i].1 != 200 {
      error_data.push(telemetry_data[i])
    }
    i = i + 1
  }
  
  // 验证错误数据
  assert_eq(error_data.length(), 2)
  assert_eq(error_data[0].1, 404)
  assert_eq(error_data[1].1, 500)
}

test "telemetry_data_filtering_by_response_time" {
  // 测试按响应时间过滤遥测数据
  
  let telemetry_data = [
    ("api", 200, 25.3),
    ("db", 200, 156.8),
    ("cache", 200, 1.2),
    ("api", 404, 45.6),
    ("external", 500, 289.3)
  ]
  
  // 过滤响应时间大于100ms的数据
  let mut slow_requests = []
  let mut i = 0
  while i < telemetry_data.length() {
    if telemetry_data[i].2 > 100.0 {
      slow_requests.push(telemetry_data[i])
    }
    i = i + 1
  }
  
  // 验证慢请求结果
  assert_eq(slow_requests.length(), 2)
  assert_eq(slow_requests[0].0, "db")
  assert_eq(slow_requests[1].0, "external")
  
  // 过滤响应时间小于50ms的数据
  let mut fast_requests = []
  i = 0
  while i < telemetry_data.length() {
    if telemetry_data[i].2 < 50.0 {
      fast_requests.push(telemetry_data[i])
    }
    i = i + 1
  }
  
  // 验证快请求结果
  assert_eq(fast_requests.length(), 2)
  assert_eq(fast_requests[0].0, "api")
  assert_eq(fast_requests[1].0, "cache")
}

test "telemetry_data_complex_filtering" {
  // 测试复合条件过滤遥测数据
  
  let telemetry_data = [
    ("api", "GET", 200, 25.3, "production"),
    ("api", "POST", 500, 156.8, "production"),
    ("api", "GET", 404, 45.6, "staging"),
    ("db", "SELECT", 200, 89.2, "production"),
    ("cache", "GET", 200, 1.2, "development"),
    ("api", "POST", 201, 78.9, "production")
  ]
  
  // 过滤生产环境的API GET请求
  let mut production_get_api = []
  let mut i = 0
  while i < telemetry_data.length() {
    if telemetry_data[i].0 == "api" && 
       telemetry_data[i].1 == "GET" && 
       telemetry_data[i].4 == "production" {
      production_get_api.push(telemetry_data[i])
    }
    i = i + 1
  }
  
  // 验证生产环境API GET请求
  assert_eq(production_get_api.length(), 1)
  assert_eq(production_get_api[0].0, "api")
  assert_eq(production_get_api[0].1, "GET")
  assert_eq(production_get_api[0].2, 200)
  assert_eq(production_get_api[0].4, "production")
  
  // 过滤响应时间大于50ms的错误请求
  let mut slow_error_requests = []
  i = 0
  while i < telemetry_data.length() {
    if telemetry_data[i].2 >= 400 && telemetry_data[i].3 > 50.0 {
      slow_error_requests.push(telemetry_data[i])
    }
    i = i + 1
  }
  
  // 验证慢错误请求
  assert_eq(slow_error_requests.length(), 2)
  assert_eq(slow_error_requests[0].2, 500)
  assert_eq(slow_error_requests[1].2, 404)
}

test "telemetry_data_range_filtering" {
  // 测试按范围过滤遥测数据
  
  let metrics_data = [
    ("cpu_usage", 75.5),
    ("memory_usage", 45.2),
    ("disk_usage", 89.7),
    ("network_io", 12.3),
    ("response_time", 156.8),
    ("error_rate", 2.1)
  ]
  
  // 过滤使用率在50%-80%之间的指标
  let mut medium_usage = []
  let mut i = 0
  while i < metrics_data.length() {
    if metrics_data[i].1 >= 50.0 && metrics_data[i].1 <= 80.0 {
      medium_usage.push(metrics_data[i])
    }
    i = i + 1
  }
  
  // 验证中等使用率指标
  assert_eq(medium_usage.length(), 2)
  assert_eq(medium_usage[0].0, "cpu_usage")
  assert_eq(medium_usage[1].0, "memory_usage")
  
  // 过滤高使用率指标(>80%)
  let mut high_usage = []
  i = 0
  while i < metrics_data.length() {
    if metrics_data[i].1 > 80.0 {
      high_usage.push(metrics_data[i])
    }
    i = i + 1
  }
  
  // 验证高使用率指标
  assert_eq(high_usage.length(), 2)
  assert_eq(high_usage[0].0, "disk_usage")
  assert_eq(high_usage[1].0, "response_time")
}
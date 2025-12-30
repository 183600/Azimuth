// 简单的新测试用例，用于验证语法

test "simple_string_operations" {
  // 测试基本字符串操作
  
  let service_name = "payment-service"
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  // 验证服务名称
  assert_eq(service_name.length(), 15)
  assert_eq(service_name.has_prefix("payment"), true)
  assert_eq(service_name.has_suffix("service"), true)
  
  // 验证追踪ID
  assert_eq(trace_id.length(), 32)
  assert_eq(trace_id.has_prefix("0af7"), true)
  assert_eq(trace_id.has_suffix("319c"), true)
  
  // 验证跨度ID
  assert_eq(span_id.length(), 16)
  assert_eq(span_id.has_prefix("b7ad"), true)
  assert_eq(span_id.has_suffix("3331"), true)
  
  // 创建完整的遥测标识
  let telemetry_id = service_name + ":" + trace_id + ":" + span_id
  assert_eq(telemetry_id, "payment-service:0af7651916cd43dd8448eb211c80319c:b7ad6b7169203331")
  assert_eq(telemetry_id.length(), 64)
}

test "simple_numeric_operations" {
  // 测试基本数值操作
  
  let metric_value = 1234.5
  let request_count = 1000
  let error_count = 50
  
  // 验证指标值
  assert_eq(metric_value > 1000.0, true)
  assert_eq(metric_value < 2000.0, true)
  
  // 计算错误率
  let error_rate = error_count.to_double() / request_count.to_double() * 100.0
  assert_eq(error_rate, 5.0)
  
  // 验证错误率范围
  assert_eq(error_rate > 0.0, true)
  assert_eq(error_rate < 10.0, true)
  
  // 创建统计报告
  let stats_report = "Requests: " + request_count.to_string() + 
                    ", Errors: " + error_count.to_string() + 
                    ", Error Rate: " + error_rate.to_string() + "%"
  
  assert_eq(stats_report.contains("Requests: 1000"), true)
  assert_eq(stats_report.contains("Errors: 50"), true)
  assert_eq(stats_report.contains("Error Rate: 5%"), true)
}

test "simple_array_operations" {
  // 测试基本数组操作
  
  let service_names = ["auth-service", "user-service", "payment-service", "order-service"]
  let response_times = [50.5, 25.3, 120.8, 75.2]
  
  // 验证数组长度
  assert_eq(service_names.length(), 4)
  assert_eq(response_times.length(), 4)
  
  // 验证数组内容
  assert_eq(service_names[0], "auth-service")
  assert_eq(service_names[3], "order-service")
  assert_eq(response_times[1], 25.3)
  assert_eq(response_times[2], 120.8)
  
  // 计算平均响应时间
  let mut total_time = 0.0
  let mut i = 0
  while i < response_times.length() {
    total_time = total_time + response_times[i]
    i = i + 1
  }
  
  let avg_time = total_time / response_times.length().to_double()
  assert_eq(avg_time > 60.0, true)
  assert_eq(avg_time < 80.0, true)
  
  // 找出最慢的服务
  let mut slowest_index = 0
  let mut max_time = response_times[0]
  
  i = 1
  while i < response_times.length() {
    if response_times[i] > max_time {
      max_time = response_times[i]
      slowest_index = i
    }
    i = i + 1
  }
  
  assert_eq(slowest_index, 2) // payment-service
  assert_eq(service_names[slowest_index], "payment-service")
  assert_eq(max_time, 120.8)
}
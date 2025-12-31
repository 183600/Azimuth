// 遥测数据聚合测试用例

test "telemetry_counter_aggregation" {
  // 测试计数器类型的遥测数据聚合
  
  let service_names = ["auth-service", "user-service", "payment-service"]
  let request_counts = [1500, 3200, 2800]
  let error_counts = [45, 128, 98]
  
  // 验证初始数据
  assert_eq(service_names.length(), 3)
  assert_eq(request_counts.length(), 3)
  assert_eq(error_counts.length(), 3)
  
  // 计算总请求数
  let mut total_requests = 0
  let mut i = 0
  while i < request_counts.length() {
    total_requests = total_requests + request_counts[i]
    i = i + 1
  }
  
  assert_eq(total_requests, 7500)
  
  // 计算总错误数
  let mut total_errors = 0
  i = 0
  while i < error_counts.length() {
    total_errors = total_errors + error_counts[i]
    i = i + 1
  }
  
  assert_eq(total_errors, 271)
  
  // 计算整体错误率
  let overall_error_rate = total_errors.to_double() / total_requests.to_double() * 100.0
  assert_eq(overall_error_rate > 3.0, true)
  assert_eq(overall_error_rate < 4.0, true)
  
  // 创建聚合报告
  let aggregation_report = "Total Requests: " + total_requests.to_string() + 
                          ", Total Errors: " + total_errors.to_string() + 
                          ", Error Rate: " + overall_error_rate.to_string() + "%"
  
  assert_eq(aggregation_report.contains("Total Requests: 7500"), true)
  assert_eq(aggregation_report.contains("Total Errors: 271"), true)
}

test "telemetry_histogram_aggregation" {
  // 测试直方图类型的遥测数据聚合
  
  let response_time_buckets = [
    (0.0, 10.0, 120),    // 0-10ms: 120 requests
    (10.0, 50.0, 480),   // 10-50ms: 480 requests
    (50.0, 100.0, 280),  // 50-100ms: 280 requests
    (100.0, 500.0, 95),  // 100-500ms: 95 requests
    (500.0, 1000.0, 20), // 500-1000ms: 20 requests
    (1000.0, 9999.0, 5)  // >1000ms: 5 requests
  ]
  
  // 验证桶数量
  assert_eq(response_time_buckets.length(), 6)
  
  // 计算总请求数
  let mut total_requests = 0
  let mut i = 0
  while i < response_time_buckets.length() {
    total_requests = total_requests + response_time_buckets[i].2
    i = i + 1
  }
  
  assert_eq(total_requests, 1000)
  
  // 计算小于100ms的请求百分比
  let mut fast_requests = 0
  i = 0
  while i < 3 {  // 前3个桶都是小于100ms的
    fast_requests = fast_requests + response_time_buckets[i].2
    i = i + 1
  }
  
  let fast_percentage = fast_requests.to_double() / total_requests.to_double() * 100.0
  assert_eq(fast_percentage, 88.0)
  
  // 计算大于500ms的请求百分比
  let mut slow_requests = 0
  i = 4  // 从第5个桶开始
  while i < response_time_buckets.length() {
    slow_requests = slow_requests + response_time_buckets[i].2
    i = i + 1
  }
  
  let slow_percentage = slow_requests.to_double() / total_requests.to_double() * 100.0
  assert_eq(slow_percentage, 2.5)
}

test "telemetry_gauge_aggregation" {
  // 测试仪表类型的遥测数据聚合
  
  let service_metrics = [
    ("cpu_usage", 65.5),
    ("memory_usage", 78.2),
    ("disk_usage", 45.8),
    ("network_usage", 32.1)
  ]
  
  // 验证指标数量
  assert_eq(service_metrics.length(), 4)
  
  // 计算平均资源使用率
  let mut total_usage = 0.0
  let mut i = 0
  while i < service_metrics.length() {
    total_usage = total_usage + service_metrics[i].1
    i = i + 1
  }
  
  let average_usage = total_usage / service_metrics.length().to_double()
  assert_eq(average_usage > 50.0, true)
  assert_eq(average_usage < 60.0, true)
  
  // 找出使用率最高的资源
  let mut max_usage = service_metrics[0].1
  let mut max_resource = service_metrics[0].0
  
  i = 1
  while i < service_metrics.length() {
    if service_metrics[i].1 > max_usage {
      max_usage = service_metrics[i].1
      max_resource = service_metrics[i].0
    }
    i = i + 1
  }
  
  assert_eq(max_resource, "memory_usage")
  assert_eq(max_usage, 78.2)
  
  // 找出使用率最低的资源
  let mut min_usage = service_metrics[0].1
  let mut min_resource = service_metrics[0].0
  
  i = 1
  while i < service_metrics.length() {
    if service_metrics[i].1 < min_usage {
      min_usage = service_metrics[i].1
      min_resource = service_metrics[i].0
    }
    i = i + 1
  }
  
  assert_eq(min_resource, "network_usage")
  assert_eq(min_usage, 32.1)
}

test "telemetry_time_series_aggregation" {
  // 测试时间序列数据聚合
  
  let time_series_data = [
    (1640995200L, 100.5), // 时间戳，值
    (1640995260L, 105.2),
    (1640995320L, 98.7),
    (1640995380L, 110.3),
    (1640995440L, 102.8)
  ]
  
  // 验证时间序列长度
  assert_eq(time_series_data.length(), 5)
  
  // 计算平均值
  let mut sum = 0.0
  let mut i = 0
  while i < time_series_data.length() {
    sum = sum + time_series_data[i].1
    i = i + 1
  }
  
  let average = sum / time_series_data.length().to_double()
  assert_eq(average > 100.0, true)
  assert_eq(average < 105.0, true)
  
  // 找出最大值
  let mut max_value = time_series_data[0].1
  let mut max_timestamp = time_series_data[0].0
  
  i = 1
  while i < time_series_data.length() {
    if time_series_data[i].1 > max_value {
      max_value = time_series_data[i].1
      max_timestamp = time_series_data[i].0
    }
    i = i + 1
  }
  
  assert_eq(max_value, 110.3)
  assert_eq(max_timestamp, 1640995380L)
  
  // 找出最小值
  let mut min_value = time_series_data[0].1
  let mut min_timestamp = time_series_data[0].0
  
  i = 1
  while i < time_series_data.length() {
    if time_series_data[i].1 < min_value {
      min_value = time_series_data[i].1
      min_timestamp = time_series_data[i].0
    }
    i = i + 1
  }
  
  assert_eq(min_value, 98.7)
  assert_eq(min_timestamp, 1640995320L)
  
  // 计算变化率
  let first_value = time_series_data[0].1
  let last_value = time_series_data[time_series_data.length() - 1].1
  let change_rate = (last_value - first_value) / first_value * 100.0
  
  assert_eq(change_rate > 2.0, true)
  assert_eq(change_rate < 3.0, true)
}
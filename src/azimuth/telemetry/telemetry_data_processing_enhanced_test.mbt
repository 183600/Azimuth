// 遥测数据处理增强测试用例
// 测试Azimuth Telemetry数据处理的高级功能

test "telemetry_data_aggregation" {
  // 测试遥测数据聚合功能
  
  let metric_values = [10.5, 15.2, 8.7, 12.3, 9.8, 11.1, 14.6, 7.9]
  
  // 计算平均值
  let sum = metric_values.fold(0.0, fn(acc, x) { acc + x })
  let avg = sum / Int64::from_int(metric_values.length())
  assert_eq(avg > 10.0 && avg < 12.0, true)
  
  // 查找最大值和最小值
  let max_val = metric_values.fold(0.0, fn(acc, x) { if x > acc { x } else { acc } })
  let min_val = metric_values.fold(999.0, fn(acc, x) { if x < acc { x } else { acc } })
  assert_eq(max_val, 15.2)
  assert_eq(min_val, 7.9)
  
  // 计算范围
  let range = max_val - min_val
  assert_eq(range > 7.0 && range < 8.0, true)
}

test "telemetry_data_filtering" {
  // 测试遥测数据过滤功能
  
  let raw_events = [
    ("user_login", "success", 120),
    ("user_logout", "success", 85),
    ("api_request", "error", 450),
    ("user_login", "failure", 230),
    ("api_request", "success", 150),
    ("user_logout", "error", 95)
  ]
  
  // 过滤成功的事件
  let success_events = raw_events.filter(fn(event) {
    let (_, status, _) = event
    status == "success"
  })
  assert_eq(success_events.length(), 3)
  
  // 过滤用户登录事件
  let login_events = raw_events.filter(fn(event) {
    let (event_type, _, _) = event
    event_type == "user_login"
  })
  assert_eq(login_events.length(), 2)
  
  // 过滤高延迟事件 (> 200ms)
  let high_latency_events = raw_events.filter(fn(event) {
    let (_, _, latency) = event
    latency > 200
  })
  assert_eq(high_latency_events.length(), 2)
}

test "telemetry_data_transformation" {
  // 测试遥测数据转换功能
  
  let raw_metrics = [
    ("cpu_usage", "75.5"),
    ("memory_usage", "68.2"),
    ("disk_usage", "45.8"),
    ("network_io", "120.3")
  ]
  
  // 转换字符串数值为浮点数
  let numeric_metrics = raw_metrics.map(fn(metric) {
    let (name, value_str) = metric
    let value = value_str.to_float()
    (name, value)
  })
  
  // 验证转换结果
  let cpu_value = numeric_metrics.find(fn(m) { m.0 == "cpu_usage" }).unwrap().1
  assert_eq(cpu_value > 75.0 && cpu_value < 76.0, true)
  
  let memory_value = numeric_metrics.find(fn(m) { m.0 == "memory_usage" }).unwrap().1
  assert_eq(memory_value > 68.0 && memory_value < 69.0, true)
  
  // 标准化数值 (0-100范围)
  let normalized_metrics = numeric_metrics.map(fn(metric) {
    let (name, value) = metric
    let normalized = if value > 100.0 { 100.0 } else { value }
    (name, normalized)
  })
  
  let network_normalized = normalized_metrics.find(fn(m) { m.0 == "network_io" }).unwrap().1
  assert_eq(network_normalized, 100.0)
}

test "telemetry_time_series_processing" {
  // 测试时间序列数据处理
  
  let time_series_data = [
    (1001L, 10.5),
    (1002L, 12.3),
    (1003L, 11.8),
    (1004L, 13.2),
    (1005L, 14.1)
  ]
  
  // 计算时间差
  let time_diffs = time_series_data.window(2).map(fn(window) {
    let (_, current_time) = window[0]
    let (_, next_time) = window[1]
    next_time - current_time
  })
  
  // 验证时间间隔一致性
  let all_ones = time_diffs.all(fn(diff) { diff == 1L })
  assert_eq(all_ones, true)
  
  // 计算变化率
  let value_changes = time_series_data.window(2).map(fn(window) {
    let (_, current_value) = window[0]
    let (_, next_value) = window[1]
    next_value - current_value
  })
  
  let total_change = value_changes.fold(0.0, fn(acc, change) { acc + change })
  let first_value = time_series_data[0].1
  let last_value = time_series_data[time_series_data.length() - 1].1
  assert_eq(total_change > 3.5 && total_change < 3.6, true)
  assert_eq(last_value - first_value, total_change)
}
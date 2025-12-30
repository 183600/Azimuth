// 指标聚合和统计测试用例

test "metrics_aggregation_functions" {
  // 测试指标聚合函数
  
  let metric_values = [10.5, 20.3, 15.7, 25.1, 18.9, 12.4, 22.6, 19.8]
  let metric_names = ["cpu_usage", "memory_usage", "disk_io", "network_throughput"]
  
  // 验证数据
  assert_eq(metric_values.length(), 8)
  assert_eq(metric_names.length(), 4)
  assert_eq(metric_values[0], 10.5)
  assert_eq(metric_values[7], 19.8)
  
  // 计算总和
  let mut sum = 0.0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  
  // 验证总和
  assert_eq(sum, 145.3)
  
  // 计算平均值
  let average = sum / metric_values.length().to_double()
  assert_eq(average, 18.1625)
  
  // 找出最大值和最小值
  let mut max_value = metric_values[0]
  let mut min_value = metric_values[0]
  
  i = 1
  while i < metric_values.length() {
    if metric_values[i] > max_value {
      max_value = metric_values[i]
    }
    if metric_values[i] < min_value {
      min_value = metric_values[i]
    }
    i = i + 1
  }
  
  // 验证最大值和最小值
  assert_eq(max_value, 25.1)
  assert_eq(min_value, 10.5)
  
  // 计算范围
  let range = max_value - min_value
  assert_eq(range, 14.6)
}

test "time_window_aggregation" {
  // 测试时间窗口聚合
  
  let time_windows = ["1m", "5m", "15m", "1h", "6h", "24h"]
  let window_sizes = [60, 300, 900, 3600, 21600, 86400] // 秒
  
  // 验证时间窗口
  assert_eq(time_windows.length(), 6)
  assert_eq(window_sizes.length(), 6)
  assert_eq(time_windows[0], "1m")
  assert_eq(time_windows[5], "24h")
  
  // 验证窗口大小
  assert_eq(window_sizes[0], 60) // 1分钟
  assert_eq(window_sizes[2], 900) // 15分钟
  assert_eq(window_sizes[5], 86400) // 24小时
  
  // 模拟时间序列数据点
  let data_points = []
  let mut timestamp = 1640995200L // 开始时间戳
  let mut i = 0
  while i < 100 {
    data_points.push((timestamp, i.to_double() * 1.5))
    timestamp = timestamp + 60L // 每分钟一个数据点
    i = i + 1
  }
  
  // 验证数据点
  assert_eq(data_points.length(), 100)
  assert_eq(data_points[0].0, 1640995200L)
  assert_eq(data_points[0].1, 0.0)
  assert_eq(data_points[99].0, 1640995200L + 99L * 60L)
  assert_eq(data_points[99].1, 99.0 * 1.5)
  
  // 计算5分钟窗口的聚合
  let window_5m_size = 300 // 5分钟 = 300秒
  let mut window_count = 0
  let mut window_sum = 0.0
  
  i = 0
  while i < data_points.length() {
    let point_timestamp = data_points[i].0
    let point_value = data_points[i].1
    
    // 简化：每5个点（5分钟）聚合一次
    if i % 5 == 0 && i > 0 {
      window_count = window_count + 1
    }
    
    window_sum = window_sum + point_value
    i = i + 1
  }
  
  // 验证聚合结果
  assert_eq(window_count, 19) // 100个点，每5个一组，共19个完整窗口
  assert_eq(window_sum > 0.0, true)
}

test "percentile_calculation" {
  // 测试百分位数计算
  
  let response_times = [
    10.2, 15.5, 12.8, 25.1, 8.9, 18.7, 22.3, 14.6, 
    19.4, 11.3, 16.9, 13.2, 20.8, 9.7, 17.5, 21.1,
    23.6, 7.8, 24.9, 26.2, 6.5, 27.4, 5.9, 28.7
  ]
  
  // 验证响应时间数据
  assert_eq(response_times.length(), 24)
  assert_eq(response_times[0], 10.2)
  assert_eq(response_times[23], 28.7)
  
  // 排序响应时间（简化：假设已排序）
  let sorted_times = [
    5.9, 6.5, 7.8, 8.9, 9.7, 10.2, 11.3, 12.8,
    13.2, 14.6, 15.5, 16.9, 17.5, 18.7, 19.4, 20.8,
    21.1, 22.3, 23.6, 24.9, 25.1, 26.2, 27.4, 28.7
  ]
  
  // 验证排序结果
  assert_eq(sorted_times.length(), 24)
  assert_eq(sorted_times[0], 5.9) // 最小值
  assert_eq(sorted_times[23], 28.7) // 最大值
  
  // 计算百分位数
  let p50_index = (sorted_times.length() * 50) / 100 - 1 // P50 (中位数)
  let p95_index = (sorted_times.length() * 95) / 100 - 1 // P95
  let p99_index = (sorted_times.length() * 99) / 100 - 1 // P99
  
  // 验证索引计算
  assert_eq(p50_index, 11)
  assert_eq(p95_index, 22)
  assert_eq(p99_index, 23)
  
  // 获取百分位数值
  let p50_value = sorted_times[p50_index]
  let p95_value = sorted_times[p95_index]
  let p99_value = sorted_times[p99_index]
  
  // 验证百分位数值
  assert_eq(p50_value, 16.9)
  assert_eq(p95_value, 27.4)
  assert_eq(p99_value, 28.7)
  
  // 验证百分位数关系
  assert_eq(p50_value < p95_value, true)
  assert_eq(p95_value < p99_value, true)
  assert_eq(p99_value <= sorted_times[23], true)
}

test "rate_calculation_and_trend_analysis" {
  // 测试比率计算和趋势分析
  
  let time_periods = ["00:00", "01:00", "02:00", "03:00", "04:00", "05:00"]
  let request_counts = [1200, 1450, 1680, 1320, 1890, 2100]
  let error_counts = [24, 29, 34, 26, 38, 42]
  
  // 验证数据
  assert_eq(time_periods.length(), 6)
  assert_eq(request_counts.length(), 6)
  assert_eq(error_counts.length(), 6)
  
  // 计算每个时间段的错误率
  let error_rates = []
  let mut i = 0
  while i < request_counts.length() {
    let error_rate = error_counts[i].to_double() / request_counts[i].to_double() * 100.0
    error_rates.push(error_rate)
    i = i + 1
  }
  
  // 验证错误率计算
  assert_eq(error_rates.length(), 6)
  assert_eq(error_rates[0], 2.0) // 24/1200 * 100
  assert_eq(error_rates[5], 2.0) // 42/2100 * 100
  
  // 计算请求增长率
  let request_growth_rates = []
  i = 1
  while i < request_counts.length() {
    let growth_rate = (request_counts[i].to_double() - request_counts[i-1].to_double()) / request_counts[i-1].to_double() * 100.0
    request_growth_rates.push(growth_rate)
    i = i + 1
  }
  
  // 验证增长率计算
  assert_eq(request_growth_rates.length(), 5)
  assert_eq(request_growth_rates[0] > 0.0, true) // 1200 -> 1450 增长
  assert_eq(request_growth_rates[4] > 0.0, true) // 1890 -> 2100 增长
  
  // 计算平均增长率
  let mut total_growth = 0.0
  i = 0
  while i < request_growth_rates.length() {
    total_growth = total_growth + request_growth_rates[i]
    i = i + 1
  }
  
  let average_growth = total_growth / request_growth_rates.length().to_double()
  assert_eq(average_growth > 0.0, true) // 整体呈增长趋势
  
  // 识别趋势方向
  let trend_direction = if average_growth > 5.0 { "strong_growth" } 
                       else if average_growth > 0.0 { "moderate_growth" }
                       else if average_growth < -5.0 { "strong_decline" }
                       else { "stable" }
  
  // 验证趋势判断逻辑
  assert_eq(trend_direction == "strong_growth" || trend_direction == "moderate_growth", true)
}
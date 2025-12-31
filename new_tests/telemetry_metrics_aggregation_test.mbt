// 指标聚合测试用例

test "metrics_aggregation_counter" {
  // 测试计数器指标的聚合
  
  let metric_name = "http_requests_total"
  let metric_labels = [
    ("method", "GET"),
    ("status", "200"),
    ("service", "api-gateway")
  ]
  
  // 验证指标配置
  assert_eq(metric_name, "http_requests_total")
  assert_eq(metric_labels.length(), 3)
  
  // 模拟计数器数据点
  let counter_values = [10, 25, 15, 30, 20, 35, 40, 45, 50, 55]
  let time_intervals = [1000, 1010, 1020, 1030, 1040, 1050, 1060, 1070, 1080, 1090]
  
  assert_eq(counter_values.length(), 10)
  assert_eq(time_intervals.length(), 10)
  
  // 计算累加和
  let mut cumulative_sum = 0
  let mut i = 0
  
  while i < counter_values.length() {
    cumulative_sum = cumulative_sum + counter_values[i]
    i = i + 1
  }
  
  assert_eq(cumulative_sum, 325)
  
  // 计算增长率
  let first_value = counter_values[0]
  let last_value = counter_values[counter_values.length() - 1]
  let growth_rate = ((last_value - first_value) * 100) / first_value
  
  assert_eq(growth_rate, 450)  // (55-10)*100/10 = 450%
  
  // 计算平均值
  let average_value = cumulative_sum / counter_values.length()
  assert_eq(average_value, 32)  // 325/10 = 32.5, 整数除法
  
  // 计算最大值和最小值
  let mut max_value = counter_values[0]
  let mut min_value = counter_values[0]
  i = 1
  
  while i < counter_values.length() {
    if counter_values[i] > max_value {
      max_value = counter_values[i]
    }
    if counter_values[i] < min_value {
      min_value = counter_values[i]
    }
    i = i + 1
  }
  
  assert_eq(max_value, 55)
  assert_eq(min_value, 10)
  
  // 验证计数器单调递增
  let mut is_monotonic = true
  i = 1
  while i < counter_values.length() {
    if counter_values[i] < counter_values[i - 1] {
      is_monotonic = false
      break
    }
    i = i + 1
  }
  
  assert_eq(is_monotonic, false)  // 这个序列不是严格单调的
}

test "metrics_aggregation_gauge" {
  // 测试仪表指标的聚合
  
  let metric_name = "memory_usage_bytes"
  let metric_labels = [
    ("instance", "server-01"),
    ("region", "us-west-1")
  ]
  
  // 验证指标配置
  assert_eq(metric_name, "memory_usage_bytes")
  assert_eq(metric_labels.length(), 2)
  
  // 模拟仪表数据点
  let gauge_values = [1024, 2048, 1536, 3072, 2560, 3584, 4096, 3840, 4608, 5120]
  let time_intervals = [1000, 1010, 1020, 1030, 1040, 1050, 1060, 1070, 1080, 1090]
  
  assert_eq(gauge_values.length(), 10)
  assert_eq(time_intervals.length(), 10)
  
  // 计算平均值
  let mut sum = 0
  let mut i = 0
  
  while i < gauge_values.length() {
    sum = sum + gauge_values[i]
    i = i + 1
  }
  
  let average_value = sum / gauge_values.length()
  assert_eq(average_value, 3174)  // 31744/10 = 3174.4
  
  // 计算最大值和最小值
  let mut max_value = gauge_values[0]
  let mut min_value = gauge_values[0]
  i = 1
  
  while i < gauge_values.length() {
    if gauge_values[i] > max_value {
      max_value = gauge_values[i]
    }
    if gauge_values[i] < min_value {
      min_value = gauge_values[i]
    }
    i = i + 1
  }
  
  assert_eq(max_value, 5120)
  assert_eq(min_value, 1024)
  
  // 计算范围（极差）
  let range = max_value - min_value
  assert_eq(range, 4096)
  
  // 计算中位数
  let sorted_values = [1024, 1536, 2048, 2560, 3072, 3584, 3840, 4096, 4608, 5120]
  let median_value = (sorted_values[4] + sorted_values[5]) / 2  // 偶数个元素
  assert_eq(median_value, 3328)  // (3072 + 3584)/2 = 3328
  
  // 计算方差
  let mut variance_sum = 0
  i = 0
  while i < gauge_values.length() {
    let diff = gauge_values[i] - average_value
    variance_sum = variance_sum + diff * diff
    i = i + 1
  }
  
  let variance = variance_sum / gauge_values.length()
  assert_eq(variance, 1517478)  // 近似值
  
  // 验证仪表值可以增减
  let mut has_increase = false
  let mut has_decrease = false
  i = 1
  
  while i < gauge_values.length() {
    if gauge_values[i] > gauge_values[i - 1] {
      has_increase = true
    }
    if gauge_values[i] < gauge_values[i - 1] {
      has_decrease = true
    }
    i = i + 1
  }
  
  assert_eq(has_increase, true)
  assert_eq(has_decrease, true)
}

test "metrics_aggregation_histogram" {
  // 测试直方图指标的聚合
  
  let metric_name = "request_duration_seconds"
  let metric_labels = [
    ("endpoint", "/api/users"),
    ("method", "GET")
  ]
  
  // 验证指标配置
  assert_eq(metric_name, "request_duration_seconds")
  assert_eq(metric_labels.length(), 2)
  
  // 直方图桶边界
  let bucket_bounds = [0.1, 0.5, 1.0, 2.5, 5.0, 10.0]
  
  // 模拟观察值
  let observations = [0.05, 0.15, 0.8, 1.2, 3.5, 0.3, 0.7, 2.0, 8.0, 0.9]
  
  assert_eq(bucket_bounds.length(), 6)
  assert_eq(observations.length(), 10)
  
  // 计算每个桶的计数
  let bucket_counts = [2, 5, 7, 8, 9, 10]  // 预期的桶计数
  
  // 验证桶计数递增
  let mut buckets_monotonic = true
  let mut i = 1
  
  while i < bucket_counts.length() {
    if bucket_counts[i] < bucket_counts[i - 1] {
      buckets_monotonic = false
      break
    }
    i = i + 1
  }
  
  assert_eq(buckets_monotonic, true)
  
  // 验证总观察数
  let total_observations = bucket_counts[bucket_counts.length() - 1]
  assert_eq(total_observations, observations.length())
  
  // 计算观察值的总和
  let mut sum_observations = 0.0
  let mut i = 0
  
  while i < observations.length() {
    sum_observations = sum_observations + observations[i]
    i = i + 1
  }
  
  assert_eq(sum_observations > 0.0, true)
  
  // 计算平均值
  let average_duration = sum_observations / Int64::from(observations.length())
  assert_eq(average_duration > 0.0, true)
  
  // 验证分位数计算
  let p50_index = (total_observations * 50) / 100
  let p95_index = (total_observations * 95) / 100
  let p99_index = (total_observations * 99) / 100
  
  assert_eq(p50_index, 5)   // 10*50/100 = 5
  assert_eq(p95_index, 9)   // 10*95/100 = 9.5 -> 9
  assert_eq(p99_index, 9)   // 10*99/100 = 9.9 -> 9
  
  // 验证分位数有效性
  assert_eq(p50_index < observations.length(), true)
  assert_eq(p95_index < observations.length(), true)
  assert_eq(p99_index < observations.length(), true)
}

test "metrics_aggregation_summary" {
  // 测试摘要指标的聚合
  
  let metric_name = "rpc_duration_seconds"
  let metric_labels = [
    ("service", "user-service"),
    ("method", "GetUser")
  ]
  
  // 验证指标配置
  assert_eq(metric_name, "rpc_duration_seconds")
  assert_eq(metric_labels.length(), 2)
  
  // 摘要的分位数配置
  let quantiles = [
    (0.5, 0.05),
    (0.9, 0.1),
    (0.95, 0.2),
    (0.99, 0.3)
  ]
  
  // 模拟观察值
  let observations = [0.01, 0.02, 0.03, 0.04, 0.05, 0.08, 0.1, 0.15, 0.2, 0.3]
  
  assert_eq(quantiles.length(), 4)
  assert_eq(observations.length(), 10)
  
  // 计算观察值总和
  let mut sum_observations = 0.0
  let mut i = 0
  
  while i < observations.length() {
    sum_observations = sum_observations + observations[i]
    i = i + 1
  }
  
  assert_eq(sum_observations, 0.98)
  
  // 验证观察数
  let count_observations = observations.length()
  assert_eq(count_observations, 10)
  
  // 验证分位数配置
  let mut quantiles_sorted = true
  let mut i = 1
  
  while i < quantiles.length() {
    if quantiles[i].0 < quantiles[i - 1].0 {
      quantiles_sorted = false
      break
    }
    i = i + 1
  }
  
  assert_eq(quantiles_sorted, true)
  
  // 验证分位数范围
  let mut all_quantiles_valid = true
  i = 0
  
  while i < quantiles.length() {
    let quantile_value = quantiles[i].0
    if quantile_value < 0.0 or quantile_value > 1.0 {
      all_quantiles_valid = false
      break
    }
    i = i + 1
  }
  
  assert_eq(all_quantiles_valid, true)
  
  // 验证分位数误差
  let mut max_error = 0.0
  i = 0
  
  while i < quantiles.length() {
    let error = quantiles[i].1
    if error > max_error {
      max_error = error
    }
    i = i + 1
  }
  
  assert_eq(max_error, 0.3)
  assert_eq(max_error > 0.0, true)
  
  // 计算平均值
  let average_value = sum_observations / Int64::from(count_observations)
  assert_eq(average_value, 0.098)  // 0.98/10 = 0.098
}

test "metrics_aggregation_time_window" {
  // 测试时间窗口内的指标聚合
  
  let metric_name = "cpu_usage_percent"
  let window_size_seconds = 60
  let current_time = 10000
  
  // 验证窗口配置
  assert_eq(metric_name, "cpu_usage_percent")
  assert_eq(window_size_seconds, 60)
  assert_eq(current_time, 10000)
  
  // 时间窗口内的数据点
  let time_series_data = [
    (9950, 25.5),
    (9960, 30.2),
    (9970, 28.7),
    (9980, 35.1),
    (9990, 32.8),
    (10000, 40.5)
  ]
  
  assert_eq(time_series_data.length(), 6)
  
  // 验证所有数据点在时间窗口内
  let window_start = current_time - window_size_seconds
  let mut all_points_in_window = true
  let mut i = 0
  
  while i < time_series_data.length() {
    let timestamp = time_series_data[i].0
    let value = time_series_data[i].1
    
    if timestamp < window_start or timestamp > current_time {
      all_points_in_window = false
      break
    }
    
    i = i + 1
  }
  
  assert_eq(all_points_in_window, true)
  
  // 计算窗口内的平均值
  let mut sum_values = 0.0
  i = 0
  
  while i < time_series_data.length() {
    sum_values = sum_values + time_series_data[i].1
    i = i + 1
  }
  
  let average_value = sum_values / Int64::from(time_series_data.length())
  assert_eq(average_value > 25.0, true)
  assert_eq(average_value < 41.0, true)
  
  // 计算最大值和最小值
  let mut max_value = time_series_data[0].1
  let mut min_value = time_series_data[0].1
  i = 1
  
  while i < time_series_data.length() {
    let current_value = time_series_data[i].1
    
    if current_value > max_value {
      max_value = current_value
    }
    if current_value < min_value {
      min_value = current_value
    }
    
    i = i + 1
  }
  
  assert_eq(max_value, 40.5)
  assert_eq(min_value, 25.5)
  
  // 计算变化率
  let first_value = time_series_data[0].1
  let last_value = time_series_data[time_series_data.length() - 1].1
  let change_rate = ((last_value - first_value) * 100.0) / first_value
  
  assert_eq(change_rate > 0.0, true)  // 确认是正增长
  
  // 验证时间戳递增
  let mut timestamps_increasing = true
  i = 1
  
  while i < time_series_data.length() {
    if time_series_data[i].0 <= time_series_data[i - 1].0 {
      timestamps_increasing = false
      break
    }
    i = i + 1
  }
  
  assert_eq(timestamps_increasing, true)
}
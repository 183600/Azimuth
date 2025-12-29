// 遥测指标聚合测试用例

test "counter_metrics_aggregation" {
  // 测试计数器指标聚合
  
  let service_metrics = [
    ("http_requests_total", 150),
    ("http_requests_total", 200),
    ("http_requests_total", 175),
    ("http_requests_total", 180)
  ]
  
  // 验证指标数量
  assert_eq(service_metrics.length(), 4)
  
  // 聚合计数器指标
  let mut total_requests = 0
  for metric in service_metrics {
    total_requests = total_requests + metric.1
  }
  
  // 验证聚合结果
  assert_eq(total_requests, 705)
  assert_eq(total_requests > 700, true)
  assert_eq(total_requests < 800, true)
  
  // 计算平均值
  let avg_requests = total_requests / service_metrics.length()
  assert_eq(avg_requests, 176)
}

test "gauge_metrics_aggregation" {
  // 测试仪表指标聚合
  
  let memory_readings = [
    1024.5,  // MB
    1056.8,  // MB
    987.2,   // MB
    1102.3,  // MB
    998.7    // MB
  ]
  
  // 验证读数数量
  assert_eq(memory_readings.length(), 5)
  
  // 计算最小值
  let mut min_memory = memory_readings[0]
  let mut i = 1
  while i < memory_readings.length() {
    if memory_readings[i] < min_memory {
      min_memory = memory_readings[i]
    }
    i = i + 1
  }
  assert_eq(min_memory, 987.2)
  
  // 计算最大值
  let mut max_memory = memory_readings[0]
  i = 1
  while i < memory_readings.length() {
    if memory_readings[i] > max_memory {
      max_memory = memory_readings[i]
    }
    i = i + 1
  }
  assert_eq(max_memory, 1102.3)
  
  // 计算平均值
  let mut total_memory = 0.0
  for reading in memory_readings {
    total_memory = total_memory + reading
  }
  let avg_memory = total_memory / memory_readings.length()
  
  assert_eq(avg_memory > 1000.0, true)
  assert_eq(avg_memory < 1100.0, true)
}

test "histogram_metrics_aggregation" {
  // 测试直方图指标聚合
  
  let response_time_buckets = [
    ("0.1", 50),    // 0-100ms
    ("0.5", 120),   // 100-500ms
    ("1.0", 80),    // 500ms-1s
    ("5.0", 30),    // 1-5s
    ("inf", 10)     // >5s
  ]
  
  // 验证桶数量
  assert_eq(response_time_buckets.length(), 5)
  
  // 验证快速响应 (<500ms)
  let fast_responses = response_time_buckets[0].1 + response_time_buckets[1].1
  assert_eq(fast_responses, 170)
  
  // 验证慢响应 (>1s)
  let slow_responses = response_time_buckets[3].1 + response_time_buckets[4].1
  assert_eq(slow_responses, 40)
  
  // 计算总响应数
  let mut total_responses = 0
  for bucket in response_time_buckets {
    total_responses = total_responses + bucket.1
  }
  assert_eq(total_responses, 290)
  
  // 计算快速响应百分比
  let fast_percentage = (fast_responses * 100) / total_responses
  assert_eq(fast_percentage, 58)
}

test "summary_metrics_aggregation" {
  // 测试摘要指标聚合
  
  let latency_quantiles = [
    ("0.5", 0.150),  // 50th percentile: 150ms
    ("0.9", 0.450),  // 90th percentile: 450ms
    ("0.95", 0.680), // 95th percentile: 680ms
    ("0.99", 1.200)  // 99th percentile: 1.2s
  ]
  
  // 验证分位数数量
  assert_eq(latency_quantiles.length(), 4)
  
  // 验证中位数
  assert_eq(latency_quantiles[0].0, "0.5")
  assert_eq(latency_quantiles[0].1, 0.150)
  
  // 验证99分位数
  assert_eq(latency_quantiles[3].0, "0.99")
  assert_eq(latency_quantiles[3].1, 1.200)
  
  // 验证分位数递增
  let mut i = 1
  while i < latency_quantiles.length() {
    assert_eq(latency_quantiles[i].1 > latency_quantiles[i-1].1, true)
    i = i + 1
  }
  
  // 计算延迟范围
  let min_latency = latency_quantiles[0].1
  let max_latency = latency_quantiles[3].1
  let latency_range = max_latency - min_latency
  
  assert_eq(latency_range > 1.0, true)
  assert_eq(latency_range < 1.1, true)
}

test "time_series_aggregation" {
  // 测试时间序列聚合
  
  let time_series_data = [
    ("2023-01-01T00:00:00Z", 100),
    ("2023-01-01T01:00:00Z", 120),
    ("2023-01-01T02:00:00Z", 95),
    ("2023-01-01T03:00:00Z", 110),
    ("2023-01-01T04:00:00Z", 105)
  ]
  
  // 验证时间序列点数量
  assert_eq(time_series_data.length(), 5)
  
  // 验证时间戳格式
  assert_eq(time_series_data[0].0.has_prefix("2023-01-01"), true)
  assert_eq(time_series_data[0].0.has_suffix("00:00:00Z"), true)
  
  // 计算时间窗口内的总和
  let mut total_value = 0
  for point in time_series_data {
    total_value = total_value + point.1
  }
  assert_eq(total_value, 530)
  
  // 计算滑动平均值 (窗口大小为3)
  let moving_averages = []
  let mut i = 2
  while i < time_series_data.length() {
    let window_sum = time_series_data[i-2].1 + time_series_data[i-1].1 + time_series_data[i].1
    let window_avg = window_sum / 3
    moving_averages.push(window_avg)
    i = i + 1
  }
  
  // 验证移动平均值
  assert_eq(moving_averages.length(), 3)
  assert_eq(moving_averages[0], 105) // (100+120+95)/3
  assert_eq(moving_averages[1], 108) // (120+95+110)/3
  assert_eq(moving_averages[2], 103) // (95+110+105)/3
}
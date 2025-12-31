// 遥测数据聚合和统计测试用例
// 测试遥测数据的聚合计算和统计功能

test "metric_aggregation_sum_count" {
  // 测试指标聚合：求和与计数
  
  let metric_values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
  
  // 计算总和
  let mut sum = 0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  
  // 计算平均值
  let average = sum / metric_values.length()
  
  // 验证聚合结果
  assert_eq(sum, 550)
  assert_eq(average, 55)
  assert_eq(metric_values.length(), 10)
  
  // 验证边界值
  assert_eq(metric_values[0], 10)
  assert_eq(metric_values[metric_values.length() - 1], 100)
  assert_eq(sum > metric_values[0], true)
  assert_eq(sum > average, true)
}

test "percentile_calculation" {
  // 测试百分位数计算
  
  let response_times = [
    100, 150, 200, 250, 300, 350, 400, 450, 500, 550,
    600, 650, 700, 750, 800, 850, 900, 950, 1000, 1050
  ]
  
  // 计算P50（中位数）
  let p50_index = (response_times.length() * 50) / 100
  let p50_value = response_times[p50_index]
  
  // 计算P95
  let p95_index = (response_times.length() * 95) / 100
  let p95_value = response_times[p95_index]
  
  // 计算P99
  let p99_index = (response_times.length() * 99) / 100
  let p99_value = response_times[p99_index]
  
  // 验证百分位数
  assert_eq(p50_value, 550)  // 中位数
  assert_eq(p95_value, 1000)
  assert_eq(p99_value, 1050)
  
  // 验证百分位数关系
  assert_eq(p50_value <= p95_value, true)
  assert_eq(p95_value <= p99_value, true)
  assert_eq(p99_value <= response_times[response_times.length() - 1], true)
}

test "time_window_aggregation" {
  // 测试时间窗口聚合
  
  let time_series_data = [
    (1000, 50),   // (timestamp, value)
    (1500, 75),
    (2000, 60),
    (2500, 80),
    (3000, 90),
    (3500, 70),
    (4000, 85),
    (4500, 95),
    (5000, 65),
    (5500, 100)
  ]
  
  let window_start = 2000
  let window_end = 4000
  
  // 聚合窗口内的数据
  let mut window_sum = 0
  let mut window_count = 0
  let mut window_values = []
  
  let mut i = 0
  while i < time_series_data.length() {
    let (timestamp, value) = time_series_data[i]
    
    if timestamp >= window_start && timestamp <= window_end {
      window_sum = window_sum + value
      window_count = window_count + 1
      window_values.push(value)
    }
    
    i = i + 1
  }
  
  // 计算窗口统计
  let window_average = if window_count > 0 {
    window_sum / window_count
  } else {
    0
  }
  
  // 验证窗口聚合结果
  assert_eq(window_count, 5)  // 2000, 2500, 3000, 3500, 4000
  assert_eq(window_sum, 60 + 80 + 90 + 70 + 85)
  assert_eq(window_average, 77)
  
  // 验证窗口边界
  assert_eq(window_values[0], 60)  // 2000ms
  assert_eq(window_values[window_values.length() - 1], 85)  // 4000ms
}

test "histogram_aggregation" {
  // 测试直方图聚合
  
  let request_durations = [50, 120, 200, 350, 450, 550, 650, 750, 850, 950]
  let bucket_boundaries = [100, 500, 1000]
  
  // 初始化桶计数
  let mut bucket_counts = [0, 0, 0, 0]  // 最后一个是infinity bucket
  
  // 将请求分配到桶中
  let mut i = 0
  while i < request_durations.length() {
    let duration = request_durations[i]
    let mut bucket_index = 0
    
    if duration <= bucket_boundaries[0] {
      bucket_index = 0
    } else if duration <= bucket_boundaries[1] {
      bucket_index = 1
    } else if duration <= bucket_boundaries[2] {
      bucket_index = 2
    } else {
      bucket_index = 3
    }
    
    bucket_counts[bucket_index] = bucket_counts[bucket_index] + 1
    i = i + 1
  }
  
  // 验证桶计数
  assert_eq(bucket_counts[0], 1)  // <= 100ms: [50]
  assert_eq(bucket_counts[1], 3)  // <= 500ms: [120, 200, 350]
  assert_eq(bucket_counts[2], 5)  // <= 1000ms: [450, 550, 650, 750, 850]
  assert_eq(bucket_counts[3], 1)  // > 1000ms: [950]
  
  // 验证总计数
  let mut total_count = 0
  i = 0
  while i < bucket_counts.length() {
    total_count = total_count + bucket_counts[i]
    i = i + 1
  }
  assert_eq(total_count, request_durations.length())
}

test "rate_calculation" {
  // 测试速率计算
  
  let event_counts = [
    (1000, 10),   // (timestamp, cumulative_count)
    (2000, 25),
    (3000, 45),
    (4000, 60),
    (5000, 80)
  ]
  
  // 计算速率（事件/秒）
  let mut rates = []
  
  let mut i = 1
  while i < event_counts.length() {
    let (prev_time, prev_count) = event_counts[i - 1]
    let (curr_time, curr_count) = event_counts[i]
    
    let time_diff = curr_time - prev_time
    let count_diff = curr_count - prev_count
    let rate = count_diff.to_float() / time_diff.to_float()
    
    rates.push(rate)
    i = i + 1
  }
  
  // 验证速率计算
  assert_eq(rates.length(), 4)
  assert_eq(rates[0], (25 - 10).to_float() / (2000 - 1000).to_float())  // 0.015
  assert_eq(rates[1], (45 - 25).to_float() / (3000 - 2000).to_float())  // 0.020
  assert_eq(rates[2], (60 - 45).to_float() / (4000 - 3000).to_float())  // 0.015
  assert_eq(rates[3], (80 - 60).to_float() / (5000 - 4000).to_float())  // 0.020
  
  // 验证速率合理性
  let mut i = 0
  while i < rates.length() {
    assert_eq(rates[i] >= 0.0, true)
    assert_eq(rates[i] <= 1.0, true)  // 假设最大速率为1事件/秒
    i = i + 1
  }
}

test "exponential_moving_average" {
  // 测试指数移动平均
  
  let metric_values = [10, 20, 15, 25, 30, 18, 22, 28, 35, 40]
  let alpha = 0.2  // 平滑因子
  
  // 计算指数移动平均
  let mut ema_values = []
  let ema = metric_values[0].to_float()  // 初始EMA为第一个值
  ema_values.push(ema)
  
  let mut i = 1
  while i < metric_values.length() {
    let current_value = metric_values[i].to_float()
    let new_ema = alpha * current_value + (1.0 - alpha) * ema_values[ema_values.length() - 1]
    ema_values.push(new_ema)
    i = i + 1
  }
  
  // 验证EMA计算
  assert_eq(ema_values.length(), metric_values.length())
  assert_eq(ema_values[0], 10.0)  // 初始值
  
  // 验证EMA平滑特性（应该比原始数据更平滑）
  let ema_variance = calculate_variance(ema_values)
  let raw_variance = calculate_variance(metric_values.map(fn(x) { x.to_float() }))
  
  assert_eq(ema_variance <= raw_variance, true)
  assert_eq(ema_values[ema_values.length() - 1] > 10.0, true)
  assert_eq(ema_values[ema_values.length() - 1] < 40.0, true)
}

// 辅助函数：计算方差
fn calculate_variance(values : Array[Float]) -> Float {
  let mut sum = 0.0
  let mut i = 0
  while i < values.length() {
    sum = sum + values[i]
    i = i + 1
  }
  
  let mean = sum / values.length().to_float()
  
  let mut variance_sum = 0.0
  i = 0
  while i < values.length() {
    let diff = values[i] - mean
    variance_sum = variance_sum + diff * diff
    i = i + 1
  }
  
  variance_sum / values.length().to_float()
}
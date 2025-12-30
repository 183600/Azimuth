// 遥测指标聚合测试
// 测试各种指标聚合功能和算法

test "counter_aggregation_test" {
  // 测试计数器聚合
  
  let counter_name = "http_requests_total"
  let mut counter_values = [10L, 15L, 20L, 25L, 30L]
  
  // 计算总和
  let mut sum = 0L
  for value in counter_values {
    sum = sum + value
  }
  assert_eq(sum, 100L)
  
  // 计算平均值
  let average = sum.to_double() / counter_values.length().to_double()
  assert_eq(average, 20.0)
  
  // 查找最大值和最小值
  let mut max_value = counter_values[0]
  let mut min_value = counter_values[0]
  
  for value in counter_values {
    if value > max_value {
      max_value = value
    }
    if value < min_value {
      min_value = value
    }
  }
  
  assert_eq(max_value, 30L)
  assert_eq(min_value, 10L)
  
  // 创建聚合结果
  let aggregation_result = counter_name + ":sum=" + sum.to_string() + ":avg=" + average.to_string() + ":max=" + max_value.to_string() + ":min=" + min_value.to_string()
  assert_eq(aggregation_result.contains("sum=100"), true)
  assert_eq(aggregation_result.contains("avg=20.0"), true)
  assert_eq(aggregation_result.contains("max=30"), true)
  assert_eq(aggregation_result.contains("min=10"), true)
}

test "histogram_aggregation_test" {
  // 测试直方图聚合
  
  let metric_name = "response_time_seconds"
  let histogram_values = [0.1, 0.2, 0.3, 0.5, 1.0, 2.0, 5.0]
  let bucket_boundaries = [0.1, 0.5, 1.0, 2.5, 5.0, 10.0]
  
  // 计算每个桶的计数
  let mut bucket_counts = [0, 0, 0, 0, 0, 0, 0] // 最后一个是+∞桶
  
  for value in histogram_values {
    let mut bucket_index = 0
    for boundary in bucket_boundaries {
      if value <= boundary {
        bucket_counts[bucket_index] = bucket_counts[bucket_index] + 1
        break
      }
      bucket_index = bucket_index + 1
    }
    if bucket_index == bucket_boundaries.length() {
      bucket_counts[bucket_counts.length() - 1] = bucket_counts[bucket_counts.length() - 1] + 1
    }
  }
  
  // 验证桶计数
  assert_eq(bucket_counts[0], 1) // <= 0.1
  assert_eq(bucket_counts[1], 3) // <= 0.5 (0.2, 0.3, 0.5)
  assert_eq(bucket_counts[2], 4) // <= 1.0 (1.0)
  assert_eq(bucket_counts[3], 5) // <= 2.5 (2.0)
  assert_eq(bucket_counts[4], 6) // <= 5.0 (5.0)
  assert_eq(bucket_counts[5], 6) // <= 10.0
  assert_eq(bucket_counts[6], 7) // 总数
  
  // 计算百分位数
  let sorted_values = [0.1, 0.2, 0.3, 0.5, 1.0, 2.0, 5.0]
  let p50_index = (sorted_values.length() * 50) / 100
  let p95_index = (sorted_values.length() * 95) / 100
  let p99_index = (sorted_values.length() * 99) / 100
  
  let p50 = sorted_values[p50_index]
  let p95 = sorted_values[p95_index]
  let p99 = sorted_values[p99_index]
  
  assert_eq(p50, 0.5)
  assert_eq(p95, 5.0)
  assert_eq(p99, 5.0)
  
  // 创建直方图聚合结果
  let histogram_result = metric_name + ":p50=" + p50.to_string() + ":p95=" + p95.to_string() + ":p99=" + p99.to_string() + ":count=" + sorted_values.length().to_string()
  assert_eq(histogram_result.contains("p50=0.5"), true)
  assert_eq(histogram_result.contains("p95=5.0"), true)
  assert_eq(histogram_result.contains("count=7"), true)
}

test "gauge_aggregation_test" {
  // 测试仪表盘聚合
  
  let gauge_name = "cpu_usage_percent"
  let gauge_values = [25.5, 30.2, 45.8, 60.1, 35.7, 40.3, 55.9]
  
  // 计算当前值（最后一个值）
  let current_value = gauge_values[gauge_values.length() - 1]
  assert_eq(current_value, 55.9)
  
  // 计算时间窗口内的平均值
  let mut sum = 0.0
  for value in gauge_values {
    sum = sum + value
  }
  let window_average = sum / gauge_values.length().to_double()
  assert_eq(window_average > 40.0, true)
  assert_eq(window_average < 45.0, true)
  
  // 计算变化率
  let first_value = gauge_values[0]
  let change_rate = (current_value - first_value) / first_value * 100.0
  assert_eq(change_rate > 100.0, true) // 从25.5到55.9，增长率超过100%
  
  // 查找峰值
  let mut peak_value = gauge_values[0]
  let mut peak_time = 0
  for i = 0; i < gauge_values.length(); i = i + 1 {
    if gauge_values[i] > peak_value {
      peak_value = gauge_values[i]
      peak_time = i
    }
  }
  assert_eq(peak_value, 60.1)
  assert_eq(peak_time, 3)
  
  // 创建仪表盘聚合结果
  let gauge_result = gauge_name + ":current=" + current_value.to_string() + ":avg=" + window_average.to_string() + ":peak=" + peak_value.to_string() + ":change=" + change_rate.to_string() + "%"
  assert_eq(gauge_result.contains("current=55.9"), true)
  assert_eq(gauge_result.contains("peak=60.1"), true)
}

test "multi_dimensional_aggregation_test" {
  // 测试多维聚合
  
  let metric_name = "request_duration"
  let dimensions = ["method:GET", "method:POST", "method:PUT"]
  let values = [100.0, 200.0, 150.0]
  
  // 按方法分组聚合
  let mut dimension_sums = []
  let mut dimension_counts = []
  
  for i = 0; i < dimensions.length(); i = i + 1 {
    dimension_sums.push(values[i])
    dimension_counts.push(1)
  }
  
  // 计算每个维度的平均值
  let mut dimension_averages = []
  for i = 0; i < dimension_sums.length(); i = i + 1 {
    let avg = dimension_sums[i] / dimension_counts[i].to_double()
    dimension_averages.push(avg)
  }
  
  // 验证多维聚合结果
  assert_eq(dimension_averages[0], 100.0) // GET
  assert_eq(dimension_averages[1], 200.0) // POST
  assert_eq(dimension_averages[2], 150.0) // PUT
  
  // 创建多维聚合结果
  let mut multi_dim_result = metric_name + "{"
  for i = 0; i < dimensions.length(); i = i + 1 {
    multi_dim_result = multi_dim_result + dimensions[i] + "=" + dimension_averages[i].to_string()
    if i < dimensions.length() - 1 {
      multi_dim_result = multi_dim_result + ","
    }
  }
  multi_dim_result = multi_dim_result + "}"
  
  assert_eq(multi_dim_result.contains("method:GET=100.0"), true)
  assert_eq(multi_dim_result.contains("method:POST=200.0"), true)
  assert_eq(multi_dim_result.contains("method:PUT=150.0"), true)
}

test "time_window_aggregation_test" {
  // 测试时间窗口聚合
  
  let metric_name = "throughput_ops_per_second"
  let time_windows = ["60s", "300s", "900s", "3600s"]
  let window_values = [100.0, 95.5, 88.2, 75.8]
  
  // 计算滑动窗口平均值
  let mut sliding_sum = 0.0
  let mut sliding_avg = []
  
  for i = 0; i < window_values.length(); i = i + 1 {
    sliding_sum = sliding_sum + window_values[i]
    let avg = sliding_sum / (i + 1).to_double()
    sliding_avg.push(avg)
  }
  
  // 验证滑动窗口平均值
  assert_eq(sliding_avg[0], 100.0)
  assert_eq(sliding_avg[1], 97.75) // (100 + 95.5) / 2
  assert_eq(sliding_avg[2], 94.56666666666666) // (100 + 95.5 + 88.2) / 3
  assert_eq(sliding_avg[3], 89.875) // (100 + 95.5 + 88.2 + 75.8) / 4
  
  // 计算趋势（简单线性回归）
  let n = window_values.length().to_double()
  let mut sum_x = 0.0
  let mut sum_y = 0.0
  let mut sum_xy = 0.0
  let mut sum_x2 = 0.0
  
  for i = 0; i < window_values.length(); i = i + 1 {
    let x = i.to_double()
    let y = window_values[i]
    sum_x = sum_x + x
    sum_y = sum_y + y
    sum_xy = sum_xy + x * y
    sum_x2 = sum_x2 + x * x
  }
  
  let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
  assert_eq(slope < 0.0, true) // 负斜率，表示下降趋势
  
  // 创建时间窗口聚合结果
  let window_result = metric_name + ":windows=" + time_windows.length().to_string() + ":trend=" + slope.to_string() + ":latest=" + window_values[window_values.length() - 1].to_string()
  assert_eq(window_result.contains("windows=4"), true)
  assert_eq(window_result.contains("latest=75.8"), true)
}

test "rate_aggregation_test" {
  // 测试速率聚合
  
  let counter_name = "errors_total"
  let counter_values = [100L, 150L, 200L, 280L, 350L]
  let time_intervals = [60, 60, 60, 60] // 秒
  
  // 计算每个时间间隔的速率
  let mut rates = []
  for i = 1; i < counter_values.length(); i = i + 1 {
    let delta = counter_values[i] - counter_values[i - 1]
    let rate = delta.to_double() / time_intervals[i - 1].to_double()
    rates.push(rate)
  }
  
  // 验证速率计算
  assert_eq(rates[0], 50.0 / 60.0) // (150-100)/60
  assert_eq(rates[1], 50.0 / 60.0) // (200-150)/60
  assert_eq(rates[2], 80.0 / 60.0) // (280-200)/60
  assert_eq(rates[3], 70.0 / 60.0) // (350-280)/60
  
  // 计算平均速率
  let mut sum_rates = 0.0
  for rate in rates {
    sum_rates = sum_rates + rate
  }
  let avg_rate = sum_rates / rates.length().to_double()
  assert_eq(avg_rate > 1.0, true)
  assert_eq(avg_rate < 2.0, true)
  
  // 计算加速率（速率的变化）
  let mut acceleration = []
  for i = 1; i < rates.length(); i = i + 1 {
    let accel = rates[i] - rates[i - 1]
    acceleration.push(accel)
  }
  
  // 验证加速度计算
  assert_eq(acceleration[0], 0.0) // 速率没有变化
  assert_eq(acceleration[1], 30.0 / 60.0) // 速率增加
  assert_eq(acceleration[2], -10.0 / 60.0) // 速率减少
  
  // 创建速率聚合结果
  let rate_result = counter_name + ":avg_rate=" + avg_rate.to_string() + ":max_rate=" + rates[2].to_string() + ":acceleration=" + acceleration[1].to_string()
  assert_eq(rate_result.contains("avg_rate="), true)
  assert_eq(rate_result.contains("max_rate="), true)
}
// 遥测数据聚合和统计测试用例

test "telemetry_metric_aggregation_sum" {
  // 测试遥测指标求和聚合
  
  let metric_values = [10.5, 20.3, 15.7, 8.2, 12.9, 18.4, 9.6, 14.1, 11.8, 16.5]
  let expected_sum = 128.0
  
  // 验证输入数据
  assert_eq(metric_values.length(), 10)
  assert_eq(metric_values.length() > 0, true)
  
  // 计算求和聚合
  let mut calculated_sum = 0.0
  let mut i = 0
  while i < metric_values.length() {
    calculated_sum = calculated_sum + metric_values[i]
    i = i + 1
  }
  
  // 验证求和结果
  assert_eq(calculated_sum, expected_sum)
  
  // 验证聚合精度
  let precision_threshold = 0.001
  let sum_diff = calculated_sum - expected_sum
  assert_eq(sum_diff.abs() < precision_threshold, true)
  
  // 验证空值处理
  let values_with_null = [10.5, 20.3, 0.0, 8.2, 12.9, 0.0, 9.6, 14.1, 11.8, 16.5]
  let mut sum_without_nulls = 0.0
  i = 0
  while i < values_with_null.length() {
    if values_with_null[i] > 0.0 {
      sum_without_nulls = sum_without_nulls + values_with_null[i]
    }
    i = i + 1
  }
  
  // 验证空值过滤结果
  assert_eq(sum_without_nulls, expected_sum)
  assert_eq(sum_without_nulls == calculated_sum, true)
}

test "telemetry_metric_aggregation_average" {
  // 测试遥测指标平均值聚合
  
  let metric_values = [12.5, 18.3, 14.7, 16.2, 19.9, 13.4, 17.6, 15.1, 20.8, 11.5]
  let expected_average = 16.0
  
  // 验证输入数据
  assert_eq(metric_values.length(), 10)
  assert_eq(metric_values.length() > 0, true)
  
  // 计算平均值聚合
  let mut sum = 0.0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  
  let calculated_average = sum / metric_values.length().to_float()
  
  // 验证平均值结果
  assert_eq(calculated_average, expected_average)
  
  // 验证平均值范围
  let min_value = 11.5
  let max_value = 20.8
  assert_eq(calculated_average >= min_value, true)
  assert_eq(calculated_average <= max_value, true)
  
  // 验证加权平均
  let weights = [1.0, 2.0, 1.5, 1.2, 2.5, 1.8, 1.3, 2.1, 1.7, 1.9]
  let mut weighted_sum = 0.0
  let mut total_weight = 0.0
  i = 0
  
  while i < metric_values.length() and i < weights.length() {
    weighted_sum = weighted_sum + metric_values[i] * weights[i]
    total_weight = total_weight + weights[i]
    i = i + 1
  }
  
  let weighted_average = weighted_sum / total_weight
  
  // 验证加权平均结果
  assert_eq(weighted_average > 0.0, true)
  assert_eq(total_weight > 0.0, true)
  assert_eq(weighted_average != calculated_average, true)  // 加权平均应不同于简单平均
}

test "telemetry_metric_aggregation_percentiles" {
  // 测试遥测指标百分位数聚合
  
  let metric_values = [5.2, 8.7, 12.3, 15.8, 19.4, 22.1, 25.6, 29.3, 32.9, 36.5]
  let percentiles = [50, 90, 95, 99]
  
  // 验证输入数据
  assert_eq(metric_values.length(), 10)
  assert_eq(percentiles.length(), 4)
  
  // 对数据进行排序
  let sorted_values = [5.2, 8.7, 12.3, 15.8, 19.4, 22.1, 25.6, 29.3, 32.9, 36.5]
  
  // 计算百分位数
  let mut percentile_results = []
  let mut i = 0
  
  while i < percentiles.length() {
    let percentile = percentiles[i]
    let index = (percentile * sorted_values.length()) / 100
    
    let percentile_value = 
      if index >= sorted_values.length() {
        sorted_values[sorted_values.length() - 1]
      } else {
        sorted_values[index]
      }
    
    percentile_results.push(percentile_value)
    i = i + 1
  }
  
  // 验证百分位数结果
  assert_eq(percentile_results.length(), 4)
  assert_eq(percentile_results[0], 19.4)  // 50th percentile (median)
  assert_eq(percentile_results[1], 32.9)  // 90th percentile
  assert_eq(percentile_results[2], 36.5)  // 95th percentile
  assert_eq(percentile_results[3], 36.5)  // 99th percentile
  
  // 验证百分位数单调性
  let mut j = 1
  while j < percentile_results.length() {
    assert_eq(percentile_results[j] >= percentile_results[j - 1], true)
    j = j + 1
  }
  
  // 验证百分位数范围
  let min_value = sorted_values[0]
  let max_value = sorted_values[sorted_values.length() - 1]
  i = 0
  while i < percentile_results.length() {
    assert_eq(percentile_results[i] >= min_value, true)
    assert_eq(percentile_results[i] <= max_value, true)
    i = i + 1
  }
}

test "telemetry_histogram_aggregation" {
  // 测试遥测直方图聚合
  
  let bucket_boundaries = [0.0, 10.0, 20.0, 30.0, 40.0, 50.0, Double.infinity]
  let observations = [5.2, 15.8, 25.3, 8.7, 32.1, 12.9, 28.4, 18.6, 35.7, 22.3]
  
  // 验证直方图配置
  assert_eq(bucket_boundaries.length(), 7)
  assert_eq(observations.length(), 10)
  
  // 初始化直方图计数
  let mut bucket_counts = []
  let mut i = 0
  while i < bucket_boundaries.length() - 1 {
    bucket_counts.push(0)
    i = i + 1
  }
  
  // 统计观测值落入的桶
  let mut j = 0
  while j < observations.length() {
    let value = observations[j]
    let mut bucket_index = 0
    
    let mut i = 0
    while i < bucket_boundaries.length() - 1 {
      if value >= bucket_boundaries[i] and value < bucket_boundaries[i + 1] {
        bucket_index = i
        break
      } else if value >= bucket_boundaries[bucket_boundaries.length() - 1] {
        bucket_index = bucket_boundaries.length() - 2
        break
      }
      i = i + 1
    }
    
    bucket_counts[bucket_index] = bucket_counts[bucket_index] + 1
    j = j + 1
  }
  
  // 验证直方图计数
  assert_eq(bucket_counts.length(), 6)
  assert_eq(bucket_counts[0], 2)  // [0, 10) 桶：5.2, 8.7
  assert_eq(bucket_counts[1], 3)  // [10, 20) 桶：15.8, 12.9, 18.6
  assert_eq(bucket_counts[2], 3)  // [20, 30) 桶：25.3, 28.4, 22.3
  assert_eq(bucket_counts[3], 2)  // [30, 40) 桶：32.1, 35.7
  assert_eq(bucket_counts[4], 0)  // [40, 50) 桶：空
  assert_eq(bucket_counts[5], 0)  // [50, ∞) 桶：空
  
  // 验证总计数
  let mut total_count = 0
  i = 0
  while i < bucket_counts.length() {
    total_count = total_count + bucket_counts[i]
    i = i + 1
  }
  assert_eq(total_count, observations.length())
  
  // 验证累积计数
  let mut cumulative_counts = []
  let mut cumulative_sum = 0
  i = 0
  while i < bucket_counts.length() {
    cumulative_sum = cumulative_sum + bucket_counts[i]
    cumulative_counts.push(cumulative_sum)
    i = i + 1
  }
  
  // 验证累积计数单调性
  let mut i = 1
  while i < cumulative_counts.length() {
    assert_eq(cumulative_counts[i] >= cumulative_counts[i - 1], true)
    i = i + 1
  }
  assert_eq(cumulative_counts[cumulative_counts.length() - 1], total_count)
}

test "telemetry_time_series_aggregation" {
  // 测试遥测时间序列聚合
  
  let time_series_data = [
    (1609459200, 100.5),  // 2021-01-01 00:00:00
    (1609459260, 105.2),  // 2021-01-01 00:01:00
    (1609459320, 98.7),   // 2021-01-01 00:02:00
    (1609459380, 102.3),  // 2021-01-01 00:03:00
    (1609459440, 107.8),  // 2021-01-01 00:04:00
    (1609459500, 95.4),   // 2021-01-01 00:05:00
    (1609459560, 103.1),  // 2021-01-01 00:06:00
    (1609459620, 99.6),   // 2021-01-01 00:07:00
    (1609459680, 106.2),  // 2021-01-01 00:08:00
    (1609459740, 101.9)   // 2021-01-01 00:09:00
  ]
  let aggregation_window_seconds = 300  // 5分钟窗口
  
  // 验证时间序列数据
  assert_eq(time_series_data.length(), 10)
  assert_eq(aggregation_window_seconds, 300)
  
  // 计算时间窗口聚合
  let window_start_time = 1609459200
  let window_end_time = window_start_time + aggregation_window_seconds
  
  let mut window_values = []
  let mut i = 0
  while i < time_series_data.length() {
    let timestamp = time_series_data[i].0
    let value = time_series_data[i].1
    
    if timestamp >= window_start_time and timestamp < window_end_time {
      window_values.push(value)
    }
    
    i = i + 1
  }
  
  // 验证窗口数据
  assert_eq(window_values.length(), 5)  // 前5个数据点在5分钟窗口内
  
  // 计算窗口统计
  let mut window_sum = 0.0
  let mut window_min = window_values[0]
  let mut window_max = window_values[0]
  i = 0
  
  while i < window_values.length() {
    let value = window_values[i]
    window_sum = window_sum + value
    
    if value < window_min {
      window_min = value
    }
    if value > window_max {
      window_max = value
    }
    
    i = i + 1
  }
  
  let window_average = window_sum / window_values.length().to_float()
  
  // 验证窗口统计结果
  assert_eq(window_values.length(), 5)
  assert_eq(window_min, 95.4)
  assert_eq(window_max, 107.8)
  assert_eq(window_average, 102.9)
  
  // 验证时间对齐
  let aligned_window_start = (window_start_time / aggregation_window_seconds) * aggregation_window_seconds
  assert_eq(aligned_window_start, window_start_time)
  
  // 验证窗口边界
  let next_window_start = window_start_time + aggregation_window_seconds
  let next_window_end = next_window_start + aggregation_window_seconds
  assert_eq(next_window_start, 1609459500)
  assert_eq(next_window_end, 1609459800)
}
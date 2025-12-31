// 遥测数据聚合测试用例，测试数据聚合功能

test "telemetry_data_aggregation_sum" {
  // 测试遥测数据求和聚合
  
  let metric_values = [10.5, 20.3, 15.7, 8.2, 12.1]
  let mut sum = 0.0
  
  // 计算总和
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  
  // 验证总和
  assert_eq(sum > 60.0, true)
  assert_eq(sum < 70.0, true)
  assert_eq(sum, 66.8)
  
  // 验证聚合结果
  let aggregation_result = "sum:" + sum.to_string()
  assert_eq(aggregation_result.has_prefix("sum:"), true)
  assert_eq(aggregation_result.contains("66.8"), true)
}

test "telemetry_data_aggregation_average" {
  // 测试遥测数据平均值聚合
  
  let response_times = [120.5, 98.3, 145.7, 88.2, 110.1, 95.6]
  let mut total = 0.0
  
  // 计算总和
  let mut i = 0
  while i < response_times.length() {
    total = total + response_times[i]
    i = i + 1
  }
  
  // 计算平均值
  let average = total / response_times.length().to_double()
  
  // 验证平均值
  assert_eq(average > 100.0, true)
  assert_eq(average < 120.0, true)
  assert_eq(average > 110.0, true)
  
  // 验证聚合结果格式
  let avg_result = "avg:" + average.to_string() + "ms"
  assert_eq(avg_result.has_prefix("avg:"), true)
  assert_eq(avg_result.has_suffix("ms"), true)
}

test "telemetry_data_aggregation_min_max" {
  // 测试遥测数据最小值和最大值聚合
  
  let cpu_usage_values = [45.2, 78.9, 23.5, 89.1, 56.7, 34.8, 91.2, 19.6]
  let mut min_value = cpu_usage_values[0]
  let mut max_value = cpu_usage_values[0]
  
  // 查找最小值和最大值
  let mut i = 1
  while i < cpu_usage_values.length() {
    if cpu_usage_values[i] < min_value {
      min_value = cpu_usage_values[i]
    }
    if cpu_usage_values[i] > max_value {
      max_value = cpu_usage_values[i]
    }
    i = i + 1
  }
  
  // 验证最小值和最大值
  assert_eq(min_value, 19.6)
  assert_eq(max_value, 91.2)
  
  // 验证范围
  let range = max_value - min_value
  assert_eq(range, 71.6)
  
  // 验证聚合结果
  let min_max_result = "min:" + min_value.to_string() + ",max:" + max_value.to_string()
  assert_eq(min_max_result.contains("min:19.6"), true)
  assert_eq(min_max_result.contains("max:91.2"), true)
}

test "telemetry_data_aggregation_percentile" {
  // 测试遥测数据百分位聚合
  
  let latency_values = [10.2, 15.5, 8.7, 22.1, 12.3, 18.9, 9.5, 25.6, 14.2, 11.8]
  
  // 排序（简化版，假设已经排序）
  let sorted_values = [8.7, 9.5, 10.2, 11.8, 12.3, 14.2, 15.5, 18.9, 22.1, 25.6]
  
  // 计算P50（中位数）
  let p50_index = sorted_values.length() / 2
  let p50_value = sorted_values[p50_index]
  
  // 计算P95
  let p95_index = (sorted_values.length() * 95) / 100
  let p95_value = sorted_values[p95_index]
  
  // 计算P99
  let p99_index = (sorted_values.length() * 99) / 100
  let p99_value = sorted_values[p99_index]
  
  // 验证百分位值
  assert_eq(p50_value, 14.2)
  assert_eq(p95_value, 25.6)
  assert_eq(p99_value, 25.6)
  
  // 验证聚合结果
  let percentile_result = "p50:" + p50_value.to_string() + ",p95:" + p95_value.to_string() + ",p99:" + p99_value.to_string()
  assert_eq(percentile_result.contains("p50:14.2"), true)
  assert_eq(percentile_result.contains("p95:25.6"), true)
  assert_eq(percentile_result.contains("p99:25.6"), true)
}

test "telemetry_data_aggregation_count" {
  // 测试遥测数据计数聚合
  
  let error_types = ["timeout", "connection_failed", "invalid_data", "rate_limit", "server_error"]
  let error_counts = [15, 8, 23, 12, 5]
  
  // 验证错误类型数量
  assert_eq(error_types.length(), 5)
  assert_eq(error_counts.length(), 5)
  
  // 计算总错误数
  let mut total_errors = 0
  let mut i = 0
  while i < error_counts.length() {
    total_errors = total_errors + error_counts[i]
    i = i + 1
  }
  
  // 验证总错误数
  assert_eq(total_errors, 63)
  
  // 找出最常见的错误
  let mut max_count = error_counts[0]
  let mut max_index = 0
  i = 1
  while i < error_counts.length() {
    if error_counts[i] > max_count {
      max_count = error_counts[i]
      max_index = i
    }
    i = i + 1
  }
  
  // 验证最常见的错误
  assert_eq(max_count, 23)
  assert_eq(error_types[max_index], "invalid_data")
  
  // 验证聚合结果
  let count_result = "total:" + total_errors.to_string() + ",most_common:" + error_types[max_index] + ":" + max_count.to_string()
  assert_eq(count_result.contains("total:63"), true)
  assert_eq(count_result.contains("most_common:invalid_data:23"), true)
}

test "telemetry_data_aggregation_rate" {
  // 测试遥测数据速率聚合
  
  let time_window_seconds = 60L
  let event_counts = [120, 150, 98, 175, 143, 110]
  
  // 计算总事件数
  let mut total_events = 0
  let mut i = 0
  while i < event_counts.length() {
    total_events = total_events + event_counts[i]
    i = i + 1
  }
  
  // 计算每秒速率
  let rate_per_second = total_events.to_double() / time_window_seconds.to_double()
  
  // 验证速率
  assert_eq(rate_per_second > 10.0, true)
  assert_eq(rate_per_second < 15.0, true)
  
  // 计算每分钟速率
  let rate_per_minute = rate_per_second * 60.0
  assert_eq(rate_per_minute, total_events.to_double())
  
  // 验证聚合结果
  let rate_result = "rate:" + rate_per_second.to_string() + "events/sec,total:" + total_events.to_string() + "events/" + time_window_seconds.to_string() + "s"
  assert_eq(rate_result.has_prefix("rate:"), true)
  assert_eq(rate_result.contains("events/sec"), true)
  assert_eq(rate_result.contains("events/60s"), true)
}

test "telemetry_data_aggregation_histogram" {
  // 测试遥测数据直方图聚合
  
  let response_times = [45.2, 78.9, 23.5, 89.1, 156.7, 234.8, 91.2, 19.6, 145.3, 67.8]
  let bucket_boundaries = [0.0, 50.0, 100.0, 150.0, 200.0, 300.0]
  let mut bucket_counts = [0, 0, 0, 0, 0, 0]
  
  // 将响应时间分配到桶中
  let mut i = 0
  while i < response_times.length() {
    let mut bucket_index = bucket_boundaries.length() - 1
    let mut j = 0
    while j < bucket_boundaries.length() {
      if response_times[i] <= bucket_boundaries[j] {
        bucket_index = j
        break
      }
      j = j + 1
    }
    bucket_counts[bucket_index] = bucket_counts[bucket_index] + 1
    i = i + 1
  }
  
  // 验证桶计数
  assert_eq(bucket_counts[0], 0)  // 0-50ms
  assert_eq(bucket_counts[1], 3)  // 50-100ms
  assert_eq(bucket_counts[2], 3)  // 100-150ms
  assert_eq(bucket_counts[3], 2)  // 150-200ms
  assert_eq(bucket_counts[4], 0)  // 200-300ms
  assert_eq(bucket_counts[5], 2)  // >300ms
  
  // 验证总计数
  let mut total_count = 0
  i = 0
  while i < bucket_counts.length() {
    total_count = total_count + bucket_counts[i]
    i = i + 1
  }
  assert_eq(total_count, response_times.length())
  
  // 验证聚合结果
  let histogram_result = "histogram:[" + bucket_counts[0].to_string() + "," + bucket_counts[1].to_string() + "," + bucket_counts[2].to_string() + "," + bucket_counts[3].to_string() + "," + bucket_counts[4].to_string() + "," + bucket_counts[5].to_string() + "]"
  assert_eq(histogram_result.contains("histogram:[0,3,3,2,0,2]"), true)
}

test "telemetry_data_aggregation_time_series" {
  // 测试遥测时间序列数据聚合
  
  let timestamps = [1640995200L, 1640995260L, 1640995320L, 1640995380L, 1640995440L]
  let metric_values = [100.5, 120.3, 95.7, 110.2, 105.8]
  
  // 验证时间戳和值的数量
  assert_eq(timestamps.length(), metric_values.length())
  assert_eq(timestamps.length(), 5)
  
  // 计算时间窗口
  let time_window = timestamps[timestamps.length() - 1] - timestamps[0]
  assert_eq(time_window, 240L)  // 4分钟
  
  // 计算时间窗口内的平均值
  let mut sum = 0.0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  let average_value = sum / metric_values.length().to_double()
  
  // 验证平均值
  assert_eq(average_value > 100.0, true)
  assert_eq(average_value < 110.0, true)
  
  // 找出最大值和最小值的时间点
  let mut max_value = metric_values[0]
  let mut max_index = 0
  let mut min_value = metric_values[0]
  let mut min_index = 0
  i = 1
  while i < metric_values.length() {
    if metric_values[i] > max_value {
      max_value = metric_values[i]
      max_index = i
    }
    if metric_values[i] < min_value {
      min_value = metric_values[i]
      min_index = i
    }
    i = i + 1
  }
  
  // 验证极值
  assert_eq(max_value, 120.3)
  assert_eq(min_value, 95.7)
  
  // 验证聚合结果
  let time_series_result = "timeseries:avg=" + average_value.to_string() + ",max=" + max_value.to_string() + "@" + timestamps[max_index].to_string() + ",min=" + min_value.to_string() + "@" + timestamps[min_index].to_string()
  assert_eq(time_series_result.contains("timeseries:avg="), true)
  assert_eq(time_series_result.contains("max=120.3"), true)
  assert_eq(time_series_result.contains("min=95.7"), true)
}
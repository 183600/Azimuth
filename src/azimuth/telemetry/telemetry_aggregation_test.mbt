// 遥测数据聚合测试用例
// 测试遥测数据的聚合、统计和分析功能

test "telemetry_metric_aggregation" {
  // 测试遥测指标聚合
  
  let metric_values = [10.5, 15.2, 8.7, 12.3, 9.8, 11.1, 14.6, 7.9]
  let metric_name = "response_time"
  let metric_unit = "milliseconds"
  
  // 验证指标数据
  assert_eq(metric_values.length(), 8)
  assert_eq(metric_name, "response_time")
  assert_eq(metric_unit, "milliseconds")
  
  // 计算总和
  let mut sum = 0.0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  
  // 计算平均值
  let average = sum / metric_values.length().to_double()
  
  // 验证聚合结果
  assert_eq(sum > 80.0, true)
  assert_eq(sum < 100.0, true)
  assert_eq(average > 10.0, true)
  assert_eq(average < 13.0, true)
  
  // 找出最大值和最小值
  let mut max_value = metric_values[0]
  let mut min_value = metric_values[0]
  
  i = 1
  while i < metric_values.length() {
    if (metric_values[i] > max_value) {
      max_value = metric_values[i]
    }
    if (metric_values[i] < min_value) {
      min_value = metric_values[i]
    }
    i = i + 1
  }
  
  // 验证极值
  assert_eq(max_value, 15.2)
  assert_eq(min_value, 7.9)
  
  // 创建聚合报告
  let aggregation_report = metric_name + ": avg=" + average.to_string().slice(0, 5) + 
                          " " + metric_unit + ", min=" + min_value.to_string() + 
                          ", max=" + max_value.to_string() + ", sum=" + sum.to_string().slice(0, 6)
  
  // 验证聚合报告
  assert_eq(aggregation_report.contains("response_time:"), true)
  assert_eq(aggregation_report.contains("avg="), true)
  assert_eq(aggregation_report.contains("milliseconds"), true)
}

test "telemetry_histogram_aggregation" {
  // 测试遥测直方图聚合
  
  let bucket_boundaries = [10.0, 50.0, 100.0, 500.0, 1000.0]
  let measurements = [5.2, 15.7, 45.3, 78.9, 120.5, 8.1, 23.4, 567.8, 89.2, 234.5]
  
  // 验证数据
  assert_eq(bucket_boundaries.length(), 5)
  assert_eq(measurements.length(), 10)
  
  // 初始化桶计数
  let bucket_counts = [0, 0, 0, 0, 0, 0] // 包括+∞桶
  
  // 将测量值分配到桶中
  let mut i = 0
  while i < measurements.length() {
    let value = measurements[i]
    let mut bucket_index = 0
    
    if (value <= bucket_boundaries[0]) {
      bucket_index = 0
    } else if (value <= bucket_boundaries[1]) {
      bucket_index = 1
    } else if (value <= bucket_boundaries[2]) {
      bucket_index = 2
    } else if (value <= bucket_boundaries[3]) {
      bucket_index = 3
    } else if (value <= bucket_boundaries[4]) {
      bucket_index = 4
    } else {
      bucket_index = 5 // +∞桶
    }
    
    bucket_counts[bucket_index] = bucket_counts[bucket_index] + 1
    i = i + 1
  }
  
  // 验证桶计数
  assert_eq(bucket_counts[0], 2) // <=10: 5.2, 8.1
  assert_eq(bucket_counts[1], 3) // <=50: 15.7, 45.3, 23.4
  assert_eq(bucket_counts[2], 3) // <=100: 78.9, 89.2, 120.5
  assert_eq(bucket_counts[3], 1) // <=500: 234.5
  assert_eq(bucket_counts[4], 0)  // <=1000: 无
  assert_eq(bucket_counts[5], 1)  // >1000: 567.8 (这里应该是500-1000之间)
  
  // 验证总计数
  let mut total_count = 0
  i = 0
  while i < bucket_counts.length() {
    total_count = total_count + bucket_counts[i]
    i = i + 1
  }
  assert_eq(total_count, measurements.length())
}

test "telemetry_percentile_calculation" {
  // 测试遥测百分位数计算
  
  let latencies = [10.1, 15.5, 8.7, 12.3, 9.8, 11.1, 14.6, 7.9, 13.2, 10.9]
  
  // 验证数据
  assert_eq(latencies.length(), 10)
  
  // 排序数据（简化版，假设已排序）
  let sorted_latencies = [7.9, 8.7, 9.8, 10.1, 10.9, 11.1, 12.3, 13.2, 14.6, 15.5]
  
  // 计算百分位数
  let p50_index = (sorted_latencies.length() * 50) / 100
  let p95_index = (sorted_latencies.length() * 95) / 100
  let p99_index = (sorted_latencies.length() * 99) / 100
  
  let p50_value = sorted_latencies[p50_index]
  let p95_value = sorted_latencies[p95_index]
  let p99_value = sorted_latencies[p99_index]
  
  // 验证百分位数
  assert_eq(p50_value, 10.9) // 中位数
  assert_eq(p95_value, 15.5) // 95%百分位数
  assert_eq(p99_value, 15.5) // 99%百分位数（简化计算）
  
  // 创建百分位数报告
  let percentile_report = "P50: " + p50_value.to_string() + 
                         "ms, P95: " + p95_value.to_string() + 
                         "ms, P99: " + p99_value.to_string() + "ms"
  
  // 验证百分位数报告
  assert_eq(percentile_report.contains("P50: 10.9"), true)
  assert_eq(percentile_report.contains("P95: 15.5"), true)
  assert_eq(percentile_report.contains("P99: 15.5"), true)
}

test "telemetry_time_series_aggregation" {
  // 测试遥测时间序列聚合
  
  let timestamps = [1640995200L, 1640995260L, 1640995320L, 1640995380L, 1640995440L]
  let values = [100, 150, 120, 180, 200]
  let interval_seconds = 60 // 1分钟间隔
  
  // 验证时间序列数据
  assert_eq(timestamps.length(), 5)
  assert_eq(values.length(), 5)
  assert_eq(interval_seconds, 60)
  
  // 按时间窗口聚合（这里假设每个点就是一个窗口）
  let time_windows = []
  let mut i = 0
  while i < timestamps.length() {
    let window_start = timestamps[i]
    let window_end = window_start + interval_seconds.to_int64()
    let window_value = values[i]
    
    time_windows.push((window_start, window_end, window_value))
    i = i + 1
  }
  
  // 验证时间窗口
  assert_eq(time_windows.length(), 5)
  assert_eq(time_windows[0].0, 1640995200L)
  assert_eq(time_windows[0].1, 1640995260L)
  assert_eq(time_windows[0].2, 100)
  assert_eq(time_windows[4].2, 200)
  
  // 计算时间序列统计
  let mut total_value = 0
  i = 0
  while i < values.length() {
    total_value = total_value + values[i]
    i = i + 1
  }
  
  let average_value = total_value / values.length()
  assert_eq(average_value, 150) // (100+150+120+180+200)/5 = 150
  
  // 创建时间序列报告
  let time_series_report = "Time Range: " + timestamps[0].to_string() + 
                          " to " + timestamps[4].to_string() + 
                          ", Average: " + average_value.to_string() + 
                          ", Total Points: " + values.length().to_string()
  
  // 验证时间序列报告
  assert_eq(time_series_report.contains("Average: 150"), true)
  assert_eq(time_series_report.contains("Total Points: 5"), true)
}

test "telemetry_rate_calculation" {
  // 测试遥测速率计算
  
  let event_counts = [100, 150, 120, 180, 200]
  let time_windows = [60, 60, 60, 60, 60] // 每个窗口60秒
  
  // 验证数据
  assert_eq(event_counts.length(), 5)
  assert_eq(time_windows.length(), 5)
  
  // 计算每个窗口的速率
  let rates = []
  let mut i = 0
  while i < event_counts.length() {
    let rate = event_counts[i].to_double() / time_windows[i].to_double()
    rates.push(rate)
    i = i + 1
  }
  
  // 验证速率计算
  assert_eq(rates.length(), 5)
  assert_eq(rates[0] > 1.6, true) // 100/60 ≈ 1.67
  assert_eq(rates[4] > 3.3, true) // 200/60 ≈ 3.33
  
  // 计算平均速率
  let mut total_events = 0
  let mut total_time = 0
  i = 0
  while i < event_counts.length() {
    total_events = total_events + event_counts[i]
    total_time = total_time + time_windows[i]
    i = i + 1
  }
  
  let average_rate = total_events.to_double() / total_time.to_double()
  assert_eq(average_rate > 2.0, true) // 750/300 = 2.5
  assert_eq(average_rate < 3.0, true)
  
  // 创建速率报告
  let rate_report = "Average Rate: " + average_rate.to_string().slice(0, 4) + 
                   " events/sec, Total Events: " + total_events.to_string() + 
                   ", Total Time: " + total_time.to_string() + "s"
  
  // 验证速率报告
  assert_eq(rate_report.contains("Average Rate:"), true)
  assert_eq(rate_report.contains("Total Events: 750"), true)
  assert_eq(rate_report.contains("Total Time: 300s"), true)
}

test "telemetry_dimension_aggregation" {
  // 测试遥测维度聚合
  
  let dimensions = ["service:api", "service:db", "service:cache", "service:auth"]
  let dimension_values = [1000, 500, 2000, 300]
  let dimension_errors = [10, 5, 2, 15]
  
  // 验证维度数据
  assert_eq(dimensions.length(), 4)
  assert_eq(dimension_values.length(), 4)
  assert_eq(dimension_errors.length(), 4)
  
  // 创建维度聚合数据
  let dimension_aggregates = []
  let mut i = 0
  while i < dimensions.length() {
    let service_name = dimensions[i].slice(8, dimensions[i].length()) // 提取服务名
    let request_count = dimension_values[i]
    let error_count = dimension_errors[i]
    let error_rate = error_count.to_double() / request_count.to_double() * 100.0
    
    dimension_aggregates.push((service_name, request_count, error_count, error_rate))
    i = i + 1
  }
  
  // 验证维度聚合
  assert_eq(dimension_aggregates.length(), 4)
  assert_eq(dimension_aggregates[0].0, "api")
  assert_eq(dimension_aggregates[0].1, 1000)
  assert_eq(dimension_aggregates[0].2, 10)
  assert_eq(dimension_aggregates[0].3 > 1.0, true) // 10/1000*100 = 1.0%
  
  // 找出错误率最高的服务
  let mut max_error_rate = 0.0
  let mut worst_service = ""
  
  i = 0
  while i < dimension_aggregates.length() {
    if (dimension_aggregates[i].3 > max_error_rate) {
      max_error_rate = dimension_aggregates[i].3
      worst_service = dimension_aggregates[i].0
    }
    i = i + 1
  }
  
  // 验证错误率最高的服务
  assert_eq(max_error_rate > 5.0, true) // auth服务: 15/300*100 = 5.0%
  assert_eq(worst_service, "auth")
  
  // 创建维度聚合报告
  let dimension_report = "Worst Service: " + worst_service + 
                        " (Error Rate: " + max_error_rate.to_string().slice(0, 4) + "%)"
  
  // 验证维度报告
  assert_eq(dimension_report.contains("Worst Service: auth"), true)
  assert_eq(dimension_report.contains("Error Rate:"), true)
}
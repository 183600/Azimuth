// 遥测数据聚合测试用例

test "telemetry_metric_sum_aggregation" {
  // 测试遥测指标求和聚合
  
  let metric_name = "http_requests_total"
  let metric_values = [10.5, 20.3, 15.7, 30.2, 25.1, 18.9, 22.4, 12.6]
  
  // 验证指标名称
  assert_eq(metric_name, "http_requests_total")
  assert_eq(metric_name.has_suffix("_total"), true)
  
  // 验证指标值数组
  assert_eq(metric_values.length(), 8)
  assert_eq(metric_values[0], 10.5)
  assert_eq(metric_values[7], 12.6)
  
  // 计算总和
  let mut sum = 0.0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  
  // 验证总和计算
  assert_eq(sum, 155.7) // 10.5 + 20.3 + 15.7 + 30.2 + 25.1 + 18.9 + 22.4 + 12.6 = 155.7
  assert_eq(sum > 100.0, true)
  assert_eq(sum < 200.0, true)
  
  // 按时间窗口聚合
  let time_windows = [
    ("2023-01-01T00:00:00Z", [10.5, 20.3]),
    ("2023-01-01T01:00:00Z", [15.7, 30.2]),
    ("2023-01-01T02:00:00Z", [25.1, 18.9]),
    ("2023-01-01T03:00:00Z", [22.4, 12.6])
  ]
  
  // 验证时间窗口
  assert_eq(time_windows.length(), 4)
  assert_eq(time_windows[0].0, "2023-01-01T00:00:00Z")
  assert_eq(time_windows[0].1.length(), 2)
  
  // 计算每个时间窗口的总和
  let window_sums = []
  i = 0
  while i < time_windows.length() {
    let window_values = time_windows[i].1
    let mut window_sum = 0.0
    let mut j = 0
    
    while j < window_values.length() {
      window_sum = window_sum + window_values[j]
      j = j + 1
    }
    
    window_sums.push(window_sum)
    i = i + 1
  }
  
  // 验证时间窗口总和
  assert_eq(window_sums.length(), 4)
  assert_eq(window_sums[0], 30.8) // 10.5 + 20.3
  assert_eq(window_sums[1], 45.9) // 15.7 + 30.2
  assert_eq(window_sums[2], 44.0) // 25.1 + 18.9
  assert_eq(window_sums[3], 35.0) // 22.4 + 12.6
}

test "telemetry_metric_average_aggregation" {
  // 测试遥测指标平均值聚合
  
  let metric_name = "response_time_ms"
  let metric_values = [120.5, 85.3, 95.7, 110.2, 75.1, 130.9, 98.4, 105.6]
  
  // 验证指标名称
  assert_eq(metric_name, "response_time_ms")
  assert_eq(metric_name.has_suffix("_ms"), true)
  
  // 验证指标值数组
  assert_eq(metric_values.length(), 8)
  assert_eq(metric_values[0], 120.5)
  assert_eq(metric_values[7], 105.6)
  
  // 计算平均值
  let mut sum = 0.0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  
  let average = sum / metric_values.length().to_double()
  
  // 验证平均值计算
  assert_eq(average, 102.7125) // (120.5 + 85.3 + 95.7 + 110.2 + 75.1 + 130.9 + 98.4 + 105.6) / 8
  assert_eq(average > 80.0, true)
  assert_eq(average < 120.0, true)
  
  // 按服务分组计算平均值
  let service_metrics = [
    ("api-gateway", [120.5, 85.3, 95.7]),
    ("user-service", [110.2, 75.1]),
    ("order-service", [130.9, 98.4, 105.6])
  ]
  
  // 验证服务分组
  assert_eq(service_metrics.length(), 3)
  assert_eq(service_metrics[0].0, "api-gateway")
  assert_eq(service_metrics[0].1.length(), 3)
  
  // 计算每个服务的平均值
  let service_averages = []
  i = 0
  while i < service_metrics.length() {
    let service_name = service_metrics[i].0
    let values = service_metrics[i].1
    
    let mut service_sum = 0.0
    let mut j = 0
    while j < values.length() {
      service_sum = service_sum + values[j]
      j = j + 1
    }
    
    let service_average = service_sum / values.length().to_double()
    service_averages.push((service_name, service_average))
    i = i + 1
  }
  
  // 验证服务平均值
  assert_eq(service_averages.length(), 3)
  assert_eq(service_averages[0].0, "api-gateway")
  assert_eq(service_averages[0].1, 100.5) // (120.5 + 85.3 + 95.7) / 3
  assert_eq(service_averages[1].0, "user-service")
  assert_eq(service_averages[1].1, 92.65) // (110.2 + 75.1) / 2
  assert_eq(service_averages[2].0, "order-service")
  assert_eq(service_averages[2].1, 111.63333333333333) // (130.9 + 98.4 + 105.6) / 3
}

test "telemetry_metric_min_max_aggregation" {
  // 测试遥测指标最小值最大值聚合
  
  let metric_name = "cpu_usage_percent"
  let metric_values = [25.3, 45.7, 15.2, 67.8, 35.1, 55.9, 28.4, 72.6, 18.9, 42.3]
  
  // 验证指标名称
  assert_eq(metric_name, "cpu_usage_percent")
  assert_eq(metric_name.has_suffix("_percent"), true)
  
  // 验证指标值数组
  assert_eq(metric_values.length(), 10)
  assert_eq(metric_values[0], 25.3)
  assert_eq(metric_values[9], 42.3)
  
  // 计算最小值
  let mut min_value = metric_values[0]
  let mut i = 1
  while i < metric_values.length() {
    if metric_values[i] < min_value {
      min_value = metric_values[i]
    }
    i = i + 1
  }
  
  // 计算最大值
  let mut max_value = metric_values[0]
  i = 1
  while i < metric_values.length() {
    if metric_values[i] > max_value {
      max_value = metric_values[i]
    }
    i = i + 1
  }
  
  // 验证最小值和最大值
  assert_eq(min_value, 15.2) // 第3个值是最小的
  assert_eq(max_value, 72.6) // 第8个值是最大的
  assert_eq(max_value > min_value, true)
  
  // 计算范围
  let range = max_value - min_value
  assert_eq(range, 57.4) // 72.6 - 15.2 = 57.4
  
  // 按时间段分组计算最小值和最大值
  let time_series = [
    ("2023-01-01T00:00:00Z", [25.3, 45.7, 15.2]),
    ("2023-01-01T01:00:00Z", [67.8, 35.1, 55.9]),
    ("2023-01-01T02:00:00Z", [28.4, 72.6, 18.9]),
    ("2023-01-01T03:00:00Z", [42.3])
  ]
  
  // 计算每个时间段的最小值和最大值
  let time_min_max = []
  i = 0
  while i < time_series.length() {
    let timestamp = time_series[i].0
    let values = time_series[i].1
    
    let mut time_min = values[0]
    let mut time_max = values[0]
    let mut j = 1
    
    while j < values.length() {
      if values[j] < time_min {
        time_min = values[j]
      }
      if values[j] > time_max {
        time_max = values[j]
      }
      j = j + 1
    }
    
    time_min_max.push((timestamp, time_min, time_max))
    i = i + 1
  }
  
  // 验证时间段最小值最大值
  assert_eq(time_min_max.length(), 4)
  assert_eq(time_min_max[0].0, "2023-01-01T00:00:00Z")
  assert_eq(time_min_max[0].1, 15.2) // 最小值
  assert_eq(time_min_max[0].2, 45.7) // 最大值
  assert_eq(time_min_max[1].0, "2023-01-01T01:00:00Z")
  assert_eq(time_min_max[1].1, 35.1) // 最小值
  assert_eq(time_min_max[1].2, 67.8) // 最大值
}

test "telemetry_metric_percentile_aggregation" {
  // 测试遥测指标百分位数聚合
  
  let metric_name = "request_duration_ms"
  let metric_values = [10.5, 20.3, 15.7, 30.2, 25.1, 18.9, 22.4, 12.6, 35.8, 28.9, 17.3, 21.5]
  
  // 验证指标名称
  assert_eq(metric_name, "request_duration_ms")
  assert_eq(metric_name.has_suffix("_ms"), true)
  
  // 验证指标值数组
  assert_eq(metric_values.length(), 12)
  
  // 排序数组（模拟排序）
  let sorted_values = [10.5, 12.6, 15.7, 17.3, 18.9, 20.3, 21.5, 22.4, 25.1, 28.9, 30.2, 35.8]
  
  // 验证排序结果
  assert_eq(sorted_values.length(), 12)
  assert_eq(sorted_values[0], 10.5) // 最小值
  assert_eq(sorted_values[11], 35.8) // 最大值
  
  // 计算百分位数
  let p50_index = (sorted_values.length() * 50) / 100
  let p95_index = (sorted_values.length() * 95) / 100
  let p99_index = (sorted_values.length() * 99) / 100
  
  let p50_value = sorted_values[p50_index]
  let p95_value = sorted_values[p95_index]
  let p99_value = sorted_values[p99_index]
  
  // 验证百分位数计算
  assert_eq(p50_index, 6) // 12 * 50 / 100 = 6
  assert_eq(p95_index, 11) // 12 * 95 / 100 = 11.4 -> 11
  assert_eq(p99_index, 11) // 12 * 99 / 100 = 11.88 -> 11
  
  assert_eq(p50_value, 21.5) // 第50百分位数
  assert_eq(p95_value, 35.8) // 第95百分位数
  assert_eq(p99_value, 35.8) // 第99百分位数
  
  // 创建百分位数结果
  let percentiles = [
    ("p50", p50_value),
    ("p90", sorted_values[(sorted_values.length() * 90) / 100]),
    ("p95", p95_value),
    ("p99", p99_value)
  ]
  
  // 验证百分位数结果
  assert_eq(percentiles.length(), 4)
  assert_eq(percentiles[0].0, "p50")
  assert_eq(percentiles[0].1, 21.5)
  assert_eq(percentiles[1].0, "p90")
  assert_eq(percentiles[1].1, 30.2) // 第90百分位数
  assert_eq(percentiles[2].0, "p95")
  assert_eq(percentiles[2].1, 35.8)
  assert_eq(percentiles[3].0, "p99")
  assert_eq(percentiles[3].1, 35.8)
}

test "telemetry_metric_histogram_aggregation" {
  // 测试遥测指标直方图聚合
  
  let metric_name = "response_size_bytes"
  let metric_values = [1024, 2048, 512, 4096, 1536, 3072, 2560, 1792, 1280, 3584]
  
  // 验证指标名称
  assert_eq(metric_name, "response_size_bytes")
  assert_eq(metric_name.has_suffix("_bytes"), true)
  
  // 验证指标值数组
  assert_eq(metric_values.length(), 10)
  
  // 定义直方图桶边界
  let bucket_boundaries = [0.0, 1024.0, 2048.0, 3072.0, 4096.0, 8192.0]
  
  // 验证桶边界
  assert_eq(bucket_boundaries.length(), 6)
  assert_eq(bucket_boundaries[0], 0.0)
  assert_eq(bucket_boundaries[5], 8192.0)
  
  // 计算每个桶的计数
  let bucket_counts = [0, 0, 0, 0, 0, 0] // 6个桶
  let mut i = 0
  
  while i < metric_values.length() {
    let value = metric_values[i].to_double()
    let mut j = 0
    
    while j < bucket_boundaries.length() {
      if value <= bucket_boundaries[j] {
        let mut k = j
        while k < bucket_counts.length() {
          bucket_counts[k] = bucket_counts[k] + 1
          k = k + 1
        }
        break
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证桶计数
  assert_eq(bucket_counts[0], 10) // 所有值都 > 0
  assert_eq(bucket_counts[1], 2) // <= 1024的值: 512, 1024
  assert_eq(bucket_counts[2], 5) // <= 2048的值: 512, 1024, 1536, 1792, 2048
  assert_eq(bucket_counts[3], 7) // <= 3072的值: + 2560, 3072
  assert_eq(bucket_counts[4], 9) // <= 4096的值: + 3584, 4096
  assert_eq(bucket_counts[5], 10) // <= 8192的值: 所有值
  
  // 创建直方图数据
  let histogram_buckets = []
  i = 0
  while i < bucket_boundaries.length() - 1 {
    histogram_buckets.push((
      bucket_boundaries[i],
      bucket_boundaries[i + 1],
      bucket_counts[i + 1] - bucket_counts[i]
    ))
    i = i + 1
  }
  
  // 验证直方图桶
  assert_eq(histogram_buckets.length(), 5)
  assert_eq(histogram_buckets[0].0, 0.0) // 下界
  assert_eq(histogram_buckets[0].1, 1024.0) // 上界
  assert_eq(histogram_buckets[0].2, 2) // 计数
  assert_eq(histogram_buckets[1].0, 1024.0)
  assert_eq(histogram_buckets[1].1, 2048.0)
  assert_eq(histogram_buckets[1].2, 3) // 5 - 2 = 3
}

test "telemetry_log_aggregation_by_level" {
  // 测试遥测日志按级别聚合
  
  let log_entries = [
    ("INFO", "User login successful", "2023-01-01T10:00:00Z"),
    ("ERROR", "Database connection failed", "2023-01-01T10:01:00Z"),
    ("WARN", "High memory usage detected", "2023-01-01T10:02:00Z"),
    ("INFO", "Request processed", "2023-01-01T10:03:00Z"),
    ("ERROR", "Timeout occurred", "2023-01-01T10:04:00Z"),
    ("DEBUG", "Cache hit", "2023-01-01T10:05:00Z"),
    ("INFO", "User logout", "2023-01-01T10:06:00Z"),
    ("WARN", "Rate limit approaching", "2023-01-01T10:07:00Z"),
    ("ERROR", "Service unavailable", "2023-01-01T10:08:00Z"),
    ("INFO", "Health check passed", "2023-01-01T10:09:00Z")
  ]
  
  // 验证日志条目
  assert_eq(log_entries.length(), 10)
  assert_eq(log_entries[0].0, "INFO")
  assert_eq(log_entries[1].0, "ERROR")
  
  // 按级别聚合日志
  let log_levels = ["DEBUG", "INFO", "WARN", "ERROR", "FATAL"]
  let level_counts = [0, 0, 0, 0, 0] // 对应每个级别的计数
  
  let mut i = 0
  while i < log_entries.length() {
    let level = log_entries[i].0
    let mut j = 0
    
    while j < log_levels.length() {
      if level == log_levels[j] {
        level_counts[j] = level_counts[j] + 1
        break
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证级别计数
  assert_eq(level_counts[0], 1) // DEBUG: 1
  assert_eq(level_counts[1], 4) // INFO: 4
  assert_eq(level_counts[2], 2) // WARN: 2
  assert_eq(level_counts[3], 3) // ERROR: 3
  assert_eq(level_counts[4], 0) // FATAL: 0
  
  // 创建级别聚合结果
  let level_aggregation = []
  i = 0
  while i < log_levels.length() {
    level_aggregation.push((log_levels[i], level_counts[i]))
    i = i + 1
  }
  
  // 验证级别聚合结果
  assert_eq(level_aggregation.length(), 5)
  assert_eq(level_aggregation[0].0, "DEBUG")
  assert_eq(level_aggregation[0].1, 1)
  assert_eq(level_aggregation[1].0, "INFO")
  assert_eq(level_aggregation[1].1, 4)
  assert_eq(level_aggregation[2].0, "WARN")
  assert_eq(level_aggregation[2].1, 2)
  assert_eq(level_aggregation[3].0, "ERROR")
  assert_eq(level_aggregation[3].1, 3)
  assert_eq(level_aggregation[4].0, "FATAL")
  assert_eq(level_aggregation[4].1, 0)
}

test "telemetry_trace_aggregation_by_status" {
  // 测试遥测追踪按状态聚合
  
  let trace_spans = [
    ("GET /api/users", "SUCCESS", 120.5),
    ("POST /api/orders", "SUCCESS", 250.3),
    ("DatabaseQuery", "ERROR", 5000.0),
    ("CacheLookup", "SUCCESS", 5.2),
    ("PUT /api/users/123", "TIMEOUT", 30000.0),
    ("ExternalServiceCall", "ERROR", 8000.0),
    ("GET /api/products", "SUCCESS", 95.7),
    ("AuthValidation", "SUCCESS", 15.3),
    ("GET /api/orders", "SUCCESS", 180.9),
    ("MessageQueuePublish", "TIMEOUT", 25000.0)
  ]
  
  // 验证追踪span
  assert_eq(trace_spans.length(), 10)
  assert_eq(trace_spans[0].0, "GET /api/users")
  assert_eq(trace_spans[0].1, "SUCCESS")
  
  // 按状态聚合追踪
  let status_types = ["SUCCESS", "ERROR", "TIMEOUT", "CANCELLED"]
  let status_counts = [0, 0, 0, 0] // 对应每个状态的计数
  let status_durations = [0.0, 0.0, 0.0, 0.0] // 对应每个状态的总持续时间
  
  let mut i = 0
  while i < trace_spans.length() {
    let operation = trace_spans[i].0
    let status = trace_spans[i].1
    let duration = trace_spans[i].2
    
    let mut j = 0
    while j < status_types.length() {
      if status == status_types[j] {
        status_counts[j] = status_counts[j] + 1
        status_durations[j] = status_durations[j] + duration
        break
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证状态计数
  assert_eq(status_counts[0], 5) // SUCCESS: 5
  assert_eq(status_counts[1], 2) // ERROR: 2
  assert_eq(status_counts[2], 2) // TIMEOUT: 2
  assert_eq(status_counts[3], 0) // CANCELLED: 0
  
  // 验证状态持续时间
  assert_eq(status_durations[0], 667.6) // SUCCESS总持续时间
  assert_eq(status_durations[1], 13000.0) // ERROR总持续时间
  assert_eq(status_durations[2], 55000.0) // TIMEOUT总持续时间
  
  // 计算每个状态的平均持续时间
  let status_avg_durations = []
  i = 0
  while i < status_types.length() {
    let avg_duration = 0.0
    if status_counts[i] > 0 {
      avg_duration = status_durations[i] / status_counts[i].to_double()
    }
    status_avg_durations.push((status_types[i], status_counts[i], avg_duration))
    i = i + 1
  }
  
  // 验证状态平均持续时间
  assert_eq(status_avg_durations.length(), 4)
  assert_eq(status_avg_durations[0].0, "SUCCESS")
  assert_eq(status_avg_durations[0].1, 5)
  assert_eq(status_avg_durations[0].2, 133.52) // 667.6 / 5
  assert_eq(status_avg_durations[1].0, "ERROR")
  assert_eq(status_avg_durations[1].1, 2)
  assert_eq(status_avg_durations[1].2, 6500.0) // 13000.0 / 2
  assert_eq(status_avg_durations[2].0, "TIMEOUT")
  assert_eq(status_avg_durations[2].1, 2)
  assert_eq(status_avg_durations[2].2, 27500.0) // 55000.0 / 2
}

test "telemetry_time_series_aggregation" {
  // 测试遥测时间序列聚合
  
  let time_series_data = [
    ("2023-01-01T00:00:00Z", 100.5),
    ("2023-01-01T01:00:00Z", 120.3),
    ("2023-01-01T02:00:00Z", 95.7),
    ("2023-01-01T03:00:00Z", 110.2),
    ("2023-01-01T04:00:00Z", 85.1),
    ("2023-01-01T05:00:00Z", 130.9),
    ("2023-01-01T06:00:00Z", 98.4),
    ("2023-01-01T07:00:00Z", 105.6)
  ]
  
  // 验证时间序列数据
  assert_eq(time_series_data.length(), 8)
  assert_eq(time_series_data[0].0, "2023-01-01T00:00:00Z")
  assert_eq(time_series_data[0].1, 100.5)
  
  // 按时间段聚合（4小时为一个窗口）
  let time_windows = [
    ("2023-01-01T00:00:00Z", "2023-01-01T04:00:00Z"),
    ("2023-01-01T04:00:00Z", "2023-01-01T08:00:00Z")
  ]
  
  // 验证时间窗口
  assert_eq(time_windows.length(), 2)
  assert_eq(time_windows[0].0, "2023-01-01T00:00:00Z")
  assert_eq(time_windows[0].1, "2023-01-01T04:00:00Z")
  
  // 聚合每个时间窗口的数据
  let window_aggregations = []
  let mut i = 0
  
  while i < time_windows.length() {
    let window_start = time_windows[i].0
    let window_end = time_windows[i].1
    
    let window_values = []
    let mut j = 0
    
    while j < time_series_data.length() {
      let timestamp = time_series_data[j].0
      let value = time_series_data[j].1
      
      if timestamp >= window_start && timestamp < window_end {
        window_values.push(value)
      }
      j = j + 1
    }
    
    // 计算窗口统计信息
    let mut sum = 0.0
    let mut min_val = window_values[0]
    let mut max_val = window_values[0]
    
    j = 0
    while j < window_values.length() {
      let val = window_values[j]
      sum = sum + val
      
      if val < min_val {
        min_val = val
      }
      if val > max_val {
        max_val = val
      }
      j = j + 1
    }
    
    let avg = sum / window_values.length().to_double()
    
    window_aggregations.push((
      window_start + "-" + window_end,
      window_values.length(),
      min_val,
      max_val,
      avg
    ))
    
    i = i + 1
  }
  
  // 验证窗口聚合结果
  assert_eq(window_aggregations.length(), 2)
  assert_eq(window_aggregations[0].0, "2023-01-01T00:00:00Z-2023-01-01T04:00:00Z")
  assert_eq(window_aggregations[0].1, 4) // 4个数据点
  assert_eq(window_aggregations[0].2, 85.1) // 最小值
  assert_eq(window_aggregations[0].3, 130.9) // 最大值
  assert_eq(window_aggregations[0].4, 106.675) // 平均值
  
  assert_eq(window_aggregations[1].0, "2023-01-01T04:00:00Z-2023-01-01T08:00:00Z")
  assert_eq(window_aggregations[1].1, 4) // 4个数据点
  assert_eq(window_aggregations[1].2, 85.1) // 最小值
  assert_eq(window_aggregations[1].3, 130.9) // 最大值
  assert_eq(window_aggregations[1].4, 105.0) // 平均值
}

test "telemetry_cross_dimension_aggregation" {
  // 测试遥测跨维度聚合
  
  let multi_dimensional_data = [
    ("api-gateway", "production", "GET", 200, 120.5),
    ("api-gateway", "production", "POST", 201, 250.3),
    ("user-service", "staging", "GET", 200, 95.7),
    ("order-service", "production", "PUT", 400, 5000.0),
    ("api-gateway", "development", "GET", 200, 85.1),
    ("user-service", "production", "GET", 500, 8000.0),
    ("order-service", "staging", "POST", 201, 180.9),
    ("api-gateway", "production", "DELETE", 204, 130.9)
  ]
  
  // 验证多维数据
  assert_eq(multi_dimensional_data.length(), 8)
  assert_eq(multi_dimensional_data[0].0, "api-gateway")
  assert_eq(multi_dimensional_data[0].1, "production")
  
  // 按服务维度聚合
  let services = ["api-gateway", "user-service", "order-service"]
  let service_counts = [0, 0, 0]
  let service_avg_durations = [0.0, 0.0, 0.0]
  let service_total_durations = [0.0, 0.0, 0.0]
  
  let mut i = 0
  while i < multi_dimensional_data.length() {
    let service = multi_dimensional_data[i].0
    let duration = multi_dimensional_data[i].4
    
    let mut j = 0
    while j < services.length() {
      if service == services[j] {
        service_counts[j] = service_counts[j] + 1
        service_total_durations[j] = service_total_durations[j] + duration
        break
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 计算服务平均持续时间
  i = 0
  while i < services.length() {
    if service_counts[i] > 0 {
      service_avg_durations[i] = service_total_durations[i] / service_counts[i].to_double()
    }
    i = i + 1
  }
  
  // 验证服务聚合
  assert_eq(service_counts[0], 4) // api-gateway: 4个请求
  assert_eq(service_counts[1], 2) // user-service: 2个请求
  assert_eq(service_counts[2], 2) // order-service: 2个请求
  
  assert_eq(service_avg_durations[0], 146.7) // api-gateway平均持续时间
  assert_eq(service_avg_durations[1], 4047.85) // user-service平均持续时间
  assert_eq(service_avg_durations[2], 2590.45) // order-service平均持续时间
  
  // 按环境维度聚合
  let environments = ["production", "staging", "development"]
  let env_counts = [0, 0, 0]
  
  i = 0
  while i < multi_dimensional_data.length() {
    let env = multi_dimensional_data[i].1
    
    let mut j = 0
    while j < environments.length() {
      if env == environments[j] {
        env_counts[j] = env_counts[j] + 1
        break
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证环境聚合
  assert_eq(env_counts[0], 4) // production: 4个请求
  assert_eq(env_counts[1], 2) // staging: 2个请求
  assert_eq(env_counts[2], 1) // development: 1个请求
  
  // 创建跨维度聚合结果
  let cross_dimension_results = [
    ("service", "api-gateway", service_counts[0], service_avg_durations[0]),
    ("service", "user-service", service_counts[1], service_avg_durations[1]),
    ("service", "order-service", service_counts[2], service_avg_durations[2]),
    ("environment", "production", env_counts[0], 0.0),
    ("environment", "staging", env_counts[1], 0.0),
    ("environment", "development", env_counts[2], 0.0)
  ]
  
  // 验证跨维度聚合结果
  assert_eq(cross_dimension_results.length(), 6)
  assert_eq(cross_dimension_results[0].0, "service")
  assert_eq(cross_dimension_results[0].1, "api-gateway")
  assert_eq(cross_dimension_results[0].2, 4)
  assert_eq(cross_dimension_results[0].3, 146.7)
  assert_eq(cross_dimension_results[3].0, "environment")
  assert_eq(cross_dimension_results[3].1, "production")
  assert_eq(cross_dimension_results[3].2, 4)
}
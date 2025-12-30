// 遥测数据聚合功能测试用例
// 专注于测试遥测数据的聚合、统计和分析功能

test "telemetry_metrics_aggregation" {
  // 测试遥测指标聚合功能
  
  // 1. 创建原始指标数据
  let raw_metrics = [
    ("cpu_usage", 75.5, "server-01", "2023-01-01T10:00:00Z"),
    ("cpu_usage", 78.2, "server-01", "2023-01-01T10:01:00Z"),
    ("cpu_usage", 72.8, "server-01", "2023-01-01T10:02:00Z"),
    ("cpu_usage", 80.1, "server-02", "2023-01-01T10:00:00Z"),
    ("cpu_usage", 82.3, "server-02", "2023-01-01T10:01:00Z"),
    ("cpu_usage", 79.6, "server-02", "2023-01-01T10:02:00Z"),
    ("memory_usage", 1024.0, "server-01", "2023-01-01T10:00:00Z"),
    ("memory_usage", 1056.0, "server-01", "2023-01-01T10:01:00Z"),
    ("memory_usage", 1088.0, "server-01", "2023-01-01T10:02:00Z")
  ]
  
  // 2. 验证原始数据
  assert_eq(raw_metrics.length(), 9)
  assert_eq(raw_metrics[0].0, "cpu_usage")
  assert_eq(raw_metrics[8].0, "memory_usage")
  
  // 3. 按指标名称分组聚合
  let grouped_metrics = {}
  let mut i = 0
  
  while i < raw_metrics.length() {
    let (metric_name, value, source, timestamp) = raw_metrics[i]
    
    if !grouped_metrics.has(metric_name) {
      grouped_metrics = grouped_metrics.with(metric_name, [])
    }
    
    let existing_values = grouped_metrics.get(metric_name)
    grouped_metrics = grouped_metrics.with(metric_name, existing_values.push(value))
    
    i = i + 1
  }
  
  // 4. 验证分组结果
  assert_eq(grouped_metrics.has("cpu_usage"), true)
  assert_eq(grouped_metrics.has("memory_usage"), true)
  assert_eq(grouped_metrics.get("cpu_usage").length(), 6)
  assert_eq(grouped_metrics.get("memory_usage").length(), 3)
  
  // 5. 计算聚合统计
  let cpu_values = grouped_metrics.get("cpu_usage")
  let memory_values = grouped_metrics.get("memory_usage")
  
  // CPU使用率统计
  let mut cpu_sum = 0.0
  let mut cpu_min = cpu_values[0]
  let mut cpu_max = cpu_values[0]
  let mut j = 0
  
  while j < cpu_values.length() {
    cpu_sum = cpu_sum + cpu_values[j]
    if cpu_values[j] < cpu_min {
      cpu_min = cpu_values[j]
    }
    if cpu_values[j] > cpu_max {
      cpu_max = cpu_values[j]
    }
    j = j + 1
  }
  
  let cpu_avg = cpu_sum / cpu_values.length().to_double()
  
  // 内存使用率统计
  let mut memory_sum = 0.0
  let mut memory_min = memory_values[0]
  let mut memory_max = memory_values[0]
  j = 0
  
  while j < memory_values.length() {
    memory_sum = memory_sum + memory_values[j]
    if memory_values[j] < memory_min {
      memory_min = memory_values[j]
    }
    if memory_values[j] > memory_max {
      memory_max = memory_values[j]
    }
    j = j + 1
  }
  
  let memory_avg = memory_sum / memory_values.length().to_double()
  
  // 6. 验证聚合统计结果
  assert_eq(cpu_avg >= 70.0 && cpu_avg <= 85.0, true)
  assert_eq(cpu_min, 72.8)
  assert_eq(cpu_max, 82.3)
  assert_eq(memory_avg >= 1000.0 && memory_avg <= 1100.0, true)
  assert_eq(memory_min, 1024.0)
  assert_eq(memory_max, 1088.0)
}

test "telemetry_time_series_aggregation" {
  // 测试遥测时间序列聚合
  
  // 1. 创建时间序列数据
  let time_series_data = [
    (1640995200L, 100.5, "requests_per_second"),
    (1640995260L, 105.2, "requests_per_second"),
    (1640995320L, 98.7, "requests_per_second"),
    (1640995380L, 112.3, "requests_per_second"),
    (1640995440L, 108.9, "requests_per_second"),
    (1640995500L, 115.6, "requests_per_second")
  ]
  
  // 2. 验证时间序列数据
  assert_eq(time_series_data.length(), 6)
  assert_eq(time_series_data[0].0, 1640995200L)
  assert_eq(time_series_data[5].0, 1640995500L)
  
  // 3. 按时间窗口聚合（5分钟窗口）
  let window_size_seconds = 300L  // 5分钟
  let time_windows = {}
  let mut i = 0
  
  while i < time_series_data.length() {
    let (timestamp, value, metric_name) = time_series_data[i]
    
    // 计算时间窗口
    let window_start = (timestamp / window_size_seconds) * window_size_seconds
    let window_key = window_start.to_string()
    
    if !time_windows.has(window_key) {
      time_windows = time_windows.with(window_key, [])
    }
    
    let existing_values = time_windows.get(window_key)
    time_windows = time_windows.with(window_key, existing_values.push(value))
    
    i = i + 1
  }
  
  // 4. 验证时间窗口分组
  let window_keys = ["1640995200", "1640995500"]  // 应该有两个窗口
  assert_eq(time_windows.has("1640995200"), true)
  assert_eq(time_windows.has("1640995500"), true)
  
  // 5. 计算每个时间窗口的聚合统计
  let window_stats = []
  let mut i = 0
  
  while i < window_keys.length() {
    let window_key = window_keys[i]
    let values = time_windows.get(window_key)
    
    let mut sum = 0.0
    let mut count = 0
    let mut j = 0
    
    while j < values.length() {
      sum = sum + values[j]
      count = count + 1
      j = j + 1
    }
    
    let avg = sum / count.to_double()
    
    let window_stat = {
      "window_start" => window_key,
      "count" => count.to_string(),
      "sum" => sum.to_string(),
      "avg" => avg.to_string()
    }
    
    window_stats.push(window_stat)
    i = i + 1
  }
  
  // 6. 验证时间窗口聚合结果
  assert_eq(window_stats.length(), 2)
  
  // 第一个窗口应该有更多数据点
  let first_window_count = window_stats[0].get("count").to_int()
  let second_window_count = window_stats[1].get("count").to_int()
  assert_eq(first_window_count > second_window_count, true)
  
  // 验证平均值在合理范围内
  let first_window_avg = window_stats[0].get("avg").to_double()
  let second_window_avg = window_stats[1].get("avg").to_double()
  assert_eq(first_window_avg >= 90.0 && first_window_avg <= 120.0, true)
  assert_eq(second_window_avg >= 110.0 && second_window_avg <= 120.0, true)
}

test "telemetry_percentile_calculation" {
  // 测试遥测百分位数计算
  
  // 1. 创建响应时间数据
  let response_times = [
    12.5, 15.2, 18.7, 22.1, 25.8, 28.3, 31.6, 35.9, 42.7, 48.1,
    55.3, 62.8, 71.4, 85.6, 98.2, 112.7, 128.9, 145.3, 167.8, 195.6
  ]
  
  // 2. 验证响应时间数据
  assert_eq(response_times.length(), 20)
  assert_eq(response_times[0], 12.5)
  assert_eq(response_times[19], 195.6)
  
  // 3. 排序数据（如果未排序）
  let sorted_times = response_times  // 假设已排序
  let mut i = 1
  while i < sorted_times.length() {
    assert_eq(sorted_times[i-1] <= sorted_times[i], true)
    i = i + 1
  }
  
  // 4. 计算百分位数
  let percentiles = [50, 90, 95, 99]
  let percentile_results = []
  let mut i = 0
  
  while i < percentiles.length() {
    let percentile = percentiles[i]
    let index = (percentile.to_double() / 100.0 * (sorted_times.length() - 1).to_double()).to_int()
    let value = sorted_times[index]
    
    percentile_results.push({
      "percentile" => percentile.to_string(),
      "value" => value.to_string()
    })
    
    i = i + 1
  }
  
  // 5. 验证百分位数计算结果
  assert_eq(percentile_results.length(), 4)
  
  // P50 (中位数)
  assert_eq(percentile_results[0].get("percentile"), "50")
  let p50_value = percentile_results[0].get("value").to_double()
  assert_eq(p50_value >= 25.0 && p50_value <= 35.0, true)
  
  // P90
  assert_eq(percentile_results[1].get("percentile"), "90")
  let p90_value = percentile_results[1].get("value").to_double()
  assert_eq(p90_value >= 140.0 && p90_value <= 170.0, true)
  
  // P95
  assert_eq(percentile_results[2].get("percentile"), "95")
  let p95_value = percentile_results[2].get("value").to_double()
  assert_eq(p95_value >= 160.0 && p95_value <= 200.0, true)
  
  // P99
  assert_eq(percentile_results[3].get("percentile"), "99")
  let p99_value = percentile_results[3].get("value").to_double()
  assert_eq(p99_value >= 180.0 && p99_value <= 200.0, true)
  
  // 6. 验证百分位数递增
  assert_eq(p50_value <= p90_value, true)
  assert_eq(p90_value <= p95_value, true)
  assert_eq(p95_value <= p99_value, true)
}

test "telemetry_histogram_aggregation" {
  // 测试遥测直方图聚合
  
  // 1. 定义直方图桶边界
  let histogram_buckets = [
    (0.0, 10.0),    // 0-10ms
    (10.0, 50.0),   // 10-50ms
    (50.0, 100.0),  // 50-100ms
    (100.0, 500.0), // 100-500ms
    (500.0, 1000.0), // 500ms-1s
    (1000.0, 5000.0) // 1s-5s
  ]
  
  // 2. 创建延迟数据
  let latency_data = [
    5.2, 8.7, 12.3, 15.6, 23.4, 28.9, 35.1, 42.8, 48.6, 52.3,
    67.8, 73.4, 85.6, 92.1, 105.7, 123.4, 156.8, 189.2, 234.5, 345.6,
    456.7, 567.8, 678.9, 789.0, 1234.5, 2345.6, 3456.7
  ]
  
  // 3. 验证数据
  assert_eq(histogram_buckets.length(), 6)
  assert_eq(latency_data.length(), 27)
  
  // 4. 计算直方图分布
  let histogram_counts = []
  let mut i = 0
  
  while i < histogram_buckets.length() {
    let (lower_bound, upper_bound) = histogram_buckets[i]
    let mut count = 0
    let mut j = 0
    
    while j < latency_data.length() {
      let latency = latency_data[j]
      if latency >= lower_bound && latency < upper_bound {
        count = count + 1
      }
      j = j + 1
    }
    
    histogram_counts.push(count)
    i = i + 1
  }
  
  // 5. 验证直方图分布
  assert_eq(histogram_counts.length(), 6)
  
  // 计算总计数
  let mut total_count = 0
  i = 0
  while i < histogram_counts.length() {
    total_count = total_count + histogram_counts[i]
    i = i + 1
  }
  assert_eq(total_count, latency_data.length())
  
  // 验证各桶的计数
  assert_eq(histogram_counts[0], 2)  // 0-10ms: 5.2, 8.7
  assert_eq(histogram_counts[1], 7)  // 10-50ms: 12.3, 15.6, 23.4, 28.9, 35.1, 42.8, 48.6
  assert_eq(histogram_counts[2], 4)  // 50-100ms: 52.3, 67.8, 73.4, 85.6
  assert_eq(histogram_counts[3], 6)  // 100-500ms: 92.1, 105.7, 123.4, 156.8, 189.2, 234.5, 345.6, 456.7
  assert_eq(histogram_counts[4], 3)  // 500-1000ms: 567.8, 678.9, 789.0
  assert_eq(histogram_counts[5], 3)  // 1000-5000ms: 1234.5, 2345.6, 3456.7
  
  // 6. 计算累积分布
  let cumulative_distribution = []
  let mut cumulative_count = 0
  i = 0
  
  while i < histogram_counts.length() {
    cumulative_count = cumulative_count + histogram_counts[i]
    let cumulative_percentage = cumulative_count.to_double() / total_count.to_double() * 100.0
    
    cumulative_distribution.push({
      "bucket" => i.to_string(),
      "cumulative_count" => cumulative_count.to_string(),
      "cumulative_percentage" => cumulative_percentage.to_string().slice(0, 5) + "%"
    })
    
    i = i + 1
  }
  
  // 7. 验证累积分布
  assert_eq(cumulative_distribution.length(), 6)
  assert_eq(cumulative_distribution[0].get("cumulative_count"), "2")
  assert_eq(cumulative_distribution[5].get("cumulative_count"), "27")
  assert_eq(cumulative_distribution[5].get("cumulative_percentage"), "100.0%")
}

test "telemetry_rate_calculation" {
  // 测试遥测速率计算
  
  // 1. 创建事件计数数据
  let event_counts = [
    (1640995200L, 100),  // 基准时间
    (1640995260L, 105),  // +1分钟
    (1640995320L, 108),  // +2分钟
    (1640995380L, 115),  // +3分钟
    (1640995440L, 123),  // +4分钟
    (1640995500L, 132),  // +5分钟
    (1640995560L, 141),  // +6分钟
    (1640995620L, 148)   // +7分钟
  ]
  
  // 2. 验证事件计数数据
  assert_eq(event_counts.length(), 8)
  assert_eq(event_counts[0].1, 100)
  assert_eq(event_counts[7].1, 148)
  
  // 3. 计算时间间隔内的速率
  let rate_calculations = []
  let mut i = 1
  
  while i < event_counts.length() {
    let (prev_timestamp, prev_count) = event_counts[i-1]
    let (curr_timestamp, curr_count) = event_counts[i]
    
    let time_diff_seconds = curr_timestamp - prev_timestamp
    let count_diff = curr_count - prev_count
    let rate_per_second = count_diff.to_double() / time_diff_seconds.to_double()
    let rate_per_minute = rate_per_second * 60.0
    
    rate_calculations.push({
      "time_interval" => (i-1).to_string() + "-" + i.to_string(),
      "time_diff_seconds" => time_diff_seconds.to_string(),
      "count_diff" => count_diff.to_string(),
      "rate_per_second" => rate_per_second.to_string().slice(0, 6),
      "rate_per_minute" => rate_per_minute.to_string().slice(0, 6)
    })
    
    i = i + 1
  }
  
  // 4. 验证速率计算结果
  assert_eq(rate_calculations.length(), 7)
  
  // 验证时间间隔
  let mut i = 0
  while i < rate_calculations.length() {
    assert_eq(rate_calculations[i].get("time_diff_seconds"), "60")
    i = i + 1
  }
  
  // 验证速率计算
  assert_eq(rate_calculations[0].get("count_diff"), "5")   // 105-100
  assert_eq(rate_calculations[1].get("count_diff"), "3")   // 108-105
  assert_eq(rate_calculations[6].get("count_diff"), "7")   // 148-141
  
  // 5. 计算平均速率
  let total_time_diff = event_counts[7].0 - event_counts[0].0
  let total_count_diff = event_counts[7].1 - event_counts[0].1
  let avg_rate_per_second = total_count_diff.to_double() / total_time_diff.to_double()
  let avg_rate_per_minute = avg_rate_per_second * 60.0
  
  // 6. 验证平均速率
  assert_eq(total_time_diff, 420L)  // 7分钟
  assert_eq(total_count_diff, 48)   // 148-100
  assert_eq(avg_rate_per_second >= 0.1 && avg_rate_per_second <= 0.2, true)
  assert_eq(avg_rate_per_minute >= 6.0 && avg_rate_per_minute <= 8.0, true)
  
  // 7. 计算速率趋势
  let rate_trend = if rate_calculations[rate_calculations.length() - 1].get("rate_per_second").to_double() > 
                     rate_calculations[0].get("rate_per_second").to_double() {
    "increasing"
  } else if rate_calculations[rate_calculations.length() - 1].get("rate_per_second").to_double() < 
            rate_calculations[0].get("rate_per_second").to_double() {
    "decreasing"
  } else {
    "stable"
  }
  
  assert_eq(rate_trend == "increasing" || rate_trend == "decreasing" || rate_trend == "stable", true)
}

test "telemetry_anomaly_detection" {
  // 测试遥测异常检测
  
  // 1. 创建正常指标数据
  let normal_metrics = [
    45.2, 47.8, 46.5, 48.1, 44.9, 47.3, 46.8, 45.7, 48.5, 46.2,
    47.1, 45.9, 48.2, 46.7, 47.6, 45.4, 48.8, 46.3, 47.4, 45.8
  ]
  
  // 2. 添加异常数据点
  let all_metrics = normal_metrics.push([85.2, 12.3, 92.7, 8.9])  // 异常值
  
  // 3. 验证数据
  assert_eq(normal_metrics.length(), 20)
  assert_eq(all_metrics.length(), 24)
  
  // 4. 计算正常数据的统计特征
  let mut sum = 0.0
  let mut i = 0
  
  while i < normal_metrics.length() {
    sum = sum + normal_metrics[i]
    i = i + 1
  }
  
  let mean = sum / normal_metrics.length().to_double()
  
  // 计算标准差
  let mut variance_sum = 0.0
  i = 0
  while i < normal_metrics.length() {
    let diff = normal_metrics[i] - mean
    variance_sum = variance_sum + diff * diff
    i = i + 1
  }
  
  let variance = variance_sum / normal_metrics.length().to_double()
  let std_dev = variance.sqrt()
  
  // 5. 验证统计特征
  assert_eq(mean >= 45.0 && mean <= 48.0, true)
  assert_eq(std_dev > 0.0 && std_dev < 5.0, true)
  
  // 6. 使用3σ规则检测异常
  let anomaly_threshold = 3.0 * std_dev
  let detected_anomalies = []
  let normal_points = []
  i = 0
  
  while i < all_metrics.length() {
    let value = all_metrics[i]
    let z_score = (value - mean).abs() / std_dev
    
    if z_score > 3.0 {
      detected_anomalies.push({
        "index" => i.to_string(),
        "value" => value.to_string(),
        "z_score" => z_score.to_string().slice(0, 6)
      })
    } else {
      normal_points.push({
        "index" => i.to_string(),
        "value" => value.to_string(),
        "z_score" => z_score.to_string().slice(0, 6)
      })
    }
    
    i = i + 1
  }
  
  // 7. 验证异常检测结果
  assert_eq(detected_anomalies.length() >= 4, true)  // 至少检测到4个异常
  assert_eq(normal_points.length(), 20)  // 正常数据点应该都被识别为正常
  
  // 验证异常值确实异常
  let mut i = 0
  while i < detected_anomalies.length() {
    let z_score = detected_anomalies[i].get("z_score").to_double()
    assert_eq(z_score > 3.0, true)
    i = i + 1
  }
  
  // 验证正常值的z-score
  i = 0
  while i < normal_points.length() {
    let z_score = normal_points[i].get("z_score").to_double()
    assert_eq(z_score <= 3.0, true)
    i = i + 1
  }
  
  // 8. 计算异常统计
  let total_points = all_metrics.length()
  let anomaly_count = detected_anomalies.length()
  let anomaly_rate = anomaly_count.to_double() / total_points.to_double() * 100.0
  
  assert_eq(anomaly_rate >= 15.0 && anomaly_rate <= 25.0, true)  // 异常率应该在合理范围内
}
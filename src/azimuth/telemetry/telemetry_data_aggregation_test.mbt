// 遥测数据聚合和统计测试用例

test "telemetry_time_series_aggregation" {
  // 测试时间序列数据聚合
  
  let time_series_metrics = [
    (1640995200L, "cpu_usage", 75.5),
    (1640995201L, "cpu_usage", 76.2),
    (1640995202L, "cpu_usage", 74.8),
    (1640995203L, "cpu_usage", 75.9),
    (1640995204L, "cpu_usage", 75.1),
    (1640995205L, "cpu_usage", 75.7),
    (1640995206L, "cpu_usage", 75.3),
    (1640995207L, "cpu_usage", 75.8),
    (1640995208L, "cpu_usage", 75.2),
    (1640995209L, "cpu_usage", 75.9)
  ]
  
  // 按时间窗口聚合（每5秒一个窗口）
  let window_size = 5L
  let mut aggregated_windows = []
  let mut processed_timestamps = []
  
  let mut i = 0
  while i < time_series_metrics.length() {
    let timestamp = time_series_metrics[i].0
    let metric_name = time_series_metrics[i].1
    let value = time_series_metrics[i].2
    
    // 计算窗口开始时间
    let window_start = (timestamp / window_size) * window_size
    
    // 检查是否已经处理过这个窗口
    let mut window_processed = false
    let mut j = 0
    while j < processed_timestamps.length() {
      if processed_timestamps[j] == window_start {
        window_processed = true
        break
      }
      j = j + 1
    }
    
    if not window_processed {
      // 聚合窗口内的数据
      let mut sum = 0.0
      let mut count = 0
      let mut min_value = value
      let mut max_value = value
      
      j = 0
      while j < time_series_metrics.length() {
        let current_timestamp = time_series_metrics[j].0
        let current_window_start = (current_timestamp / window_size) * window_size
        
        if current_window_start == window_start and time_series_metrics[j].1 == metric_name {
          sum = sum + time_series_metrics[j].2
          count = count + 1
          
          if time_series_metrics[j].2 < min_value {
            min_value = time_series_metrics[j].2
          }
          if time_series_metrics[j].2 > max_value {
            max_value = time_series_metrics[j].2
          }
        }
        j = j + 1
      }
      
      let average = sum / count.to_double()
      aggregated_windows.push((window_start, metric_name, count, average, min_value, max_value))
      processed_timestamps.push(window_start)
    }
    
    i = i + 1
  }
  
  // 验证聚合结果
  assert_eq(aggregated_windows.length(), 2) // 应该有2个窗口
  
  // 验证第一个窗口
  let window1 = aggregated_windows[0]
  assert_eq(window1.0, 1640995200L)
  assert_eq(window1.1, "cpu_usage")
  assert_eq(window1.2, 5) // 5个数据点
  assert_eq(window1.3 > 75.0, true) // 平均值
  assert_eq(window1.4 > 74.0, true) // 最小值
  assert_eq(window1.5 < 77.0, true) // 最大值
  
  // 验证第二个窗口
  let window2 = aggregated_windows[1]
  assert_eq(window2.0, 1640995205L)
  assert_eq(window2.1, "cpu_usage")
  assert_eq(window2.2, 5) // 5个数据点
  assert_eq(window2.3 > 75.0, true)
  assert_eq(window2.4 > 74.0, true)
  assert_eq(window2.5 < 77.0, true)
}

test "telemetry_percentile_calculation" {
  // 测试百分位数计算
  
  let response_times = [
    120.5, 125.3, 118.7, 122.1, 119.8, 450.2, 121.4, 123.6, 117.9, 124.2,
    15.3, 120.8, 126.1, 119.3, 122.7, 121.9, 118.2, 125.7, 120.1, 123.4
  ]
  
  // 排序数据
  let mut sorted_times = []
  let mut i = 0
  while i < response_times.length() {
    sorted_times.push(response_times[i])
    i = i + 1
  }
  
  // 简单排序（冒泡排序）
  let mut j = 0
  while j < sorted_times.length() - 1 {
    let mut k = 0
    while k < sorted_times.length() - j - 1 {
      if sorted_times[k] > sorted_times[k + 1] {
        let temp = sorted_times[k]
        sorted_times[k] = sorted_times[k + 1]
        sorted_times[k + 1] = temp
      }
      k = k + 1
    }
    j = j + 1
  }
  
  // 计算百分位数
  fn calculate_percentile(sorted_data : Array[Double], percentile : Double) -> Double {
    let index = (percentile / 100.0 * (sorted_data.length() - 1).to_double()).to_int()
    return sorted_data[index]
  }
  
  let p50 = calculate_percentile(sorted_times, 50.0)
  let p90 = calculate_percentile(sorted_times, 90.0)
  let p95 = calculate_percentile(sorted_times, 95.0)
  let p99 = calculate_percentile(sorted_times, 99.0)
  
  // 验证百分位数
  assert_eq(p50 > 118.0, true)
  assert_eq(p50 < 125.0, true)
  
  assert_eq(p90 > p50, true)
  assert_eq(p90 < 200.0, true)
  
  assert_eq(p95 > p90, true)
  assert_eq(p95 < 300.0, true)
  
  assert_eq(p99 > p95, true)
  assert_eq(p99 < 500.0, true)
  
  // 验证边界值
  assert_eq(sorted_times[0] <= p50, true)
  assert_eq(sorted_times[sorted_times.length() - 1] >= p99, true)
}

test "telemetry_histogram_aggregation" {
  // 测试直方图聚合
  
  let latency_measurements = [
    10.5, 15.2, 8.7, 12.1, 9.8, 25.3, 11.4, 18.6, 7.9, 14.2,
    45.7, 13.8, 22.1, 16.3, 19.7, 31.9, 28.2, 35.7, 20.1, 24.4
  ]
  
  // 定义直方图桶
  let histogram_buckets = [
    (0.0, 10.0),    // 0-10ms
    (10.0, 20.0),   // 10-20ms
    (20.0, 30.0),   // 20-30ms
    (30.0, 40.0),   // 30-40ms
    (40.0, 50.0)    // 40-50ms
  ]
  
  // 计算直方图
  let mut histogram_counts = [0, 0, 0, 0, 0]
  let mut i = 0
  while i < latency_measurements.length() {
    let value = latency_measurements[i]
    let mut j = 0
    while j < histogram_buckets.length() {
      let bucket_start = histogram_buckets[j].0
      let bucket_end = histogram_buckets[j].1
      
      if value >= bucket_start and value < bucket_end {
        histogram_counts[j] = histogram_counts[j] + 1
        break
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证直方图结果
  let mut total_count = 0
  let mut j = 0
  while j < histogram_counts.length() {
    total_count = total_count + histogram_counts[j]
    j = j + 1
  }
  assert_eq(total_count, latency_measurements.length())
  
  // 验证特定桶的计数
  assert_eq(histogram_counts[0] > 0, true) // 0-10ms
  assert_eq(histogram_counts[1] > 0, true) // 10-20ms
  assert_eq(histogram_counts[2] > 0, true) // 20-30ms
  assert_eq(histogram_counts[3] > 0, true) // 30-40ms
  assert_eq(histogram_counts[4] > 0, true) // 40-50ms
  
  // 验证分布合理性
  assert_eq(histogram_counts[1] > histogram_counts[0], true) // 10-20ms应该比0-10ms多
  assert_eq(histogram_counts[1] > histogram_counts[4], true) // 10-20ms应该比40-50ms多
}

test "telemetry_rate_calculation" {
  // 测试速率计算
  
  let event_counts = [
    (1640995200L, 100),
    (1640995201L, 150),
    (1640995202L, 120),
    (1640995203L, 180),
    (1640995204L, 200),
    (1640995205L, 160),
    (1640995206L, 140),
    (1640995207L, 190),
    (1640995208L, 170),
    (1640995209L, 210)
  ]
  
  // 计算每秒速率
  let mut rates = []
  let mut i = 1
  while i < event_counts.length() {
    let prev_timestamp = event_counts[i-1].0
    let prev_count = event_counts[i-1].1
    let current_timestamp = event_counts[i].0
    let current_count = event_counts[i].1
    
    let time_diff = current_timestamp - prev_timestamp
    let count_diff = current_count - prev_count
    let rate = count_diff.to_double() / time_diff.to_double()
    
    rates.push((current_timestamp, rate))
    i = i + 1
  }
  
  // 验证速率计算
  assert_eq(rates.length(), event_counts.length() - 1)
  
  // 验证速率值
  let mut j = 0
  while j < rates.length() {
    let timestamp = rates[j].0
    let rate = rates[j].1
    
    assert_eq(timestamp > 0L, true)
    assert_eq(rate > 0.0, true)
    assert_eq(rate < 100.0, true) // 速率应该在合理范围内
    
    j = j + 1
  }
  
  // 计算平均速率
  let mut total_rate = 0.0
  j = 0
  while j < rates.length() {
    total_rate = total_rate + rates[j].1
    j = j + 1
  }
  let average_rate = total_rate / rates.length()
  
  assert_eq(average_rate > 20.0, true)
  assert_eq(average_rate < 80.0, true)
}

test "telemetry_error_rate_aggregation" {
  // 测试错误率聚合
  
  let request_data = [
    ("GET", "/api/users", 200, 100),
    ("GET", "/api/users", 404, 5),
    ("GET", "/api/users", 500, 2),
    ("POST", "/api/orders", 201, 50),
    ("POST", "/api/orders", 400, 3),
    ("POST", "/api/orders", 500, 1),
    ("GET", "/api/products", 200, 80),
    ("GET", "/api/products", 404, 8),
    ("PUT", "/api/users/123", 200, 20),
    ("PUT", "/api/users/123", 403, 2),
    ("DELETE", "/api/orders/456", 204, 15),
    ("DELETE", "/api/orders/456", 404, 1)
  ]
  
  // 按端点聚合错误率
  let mut endpoint_stats = []
  let mut processed_endpoints = []
  
  let mut i = 0
  while i < request_data.length() {
    let method = request_data[i].0
    let path = request_data[i].1
    let status_code = request_data[i].2
    let count = request_data[i].3
    
    let endpoint = method + " " + path
    
    // 检查是否已经处理过这个端点
    let mut endpoint_processed = false
    let mut j = 0
    while j < processed_endpoints.length() {
      if processed_endpoints[j] == endpoint {
        endpoint_processed = true
        break
      }
      j = j + 1
    }
    
    if not endpoint_processed {
      // 聚合端点数据
      let mut total_requests = 0
      let mut error_requests = 0
      
      j = 0
      while j < request_data.length() {
        if request_data[j].0 + " " + request_data[j].1 == endpoint {
          total_requests = total_requests + request_data[j].3
          if request_data[j].2 >= 400 {
            error_requests = error_requests + request_data[j].3
          }
        }
        j = j + 1
      }
      
      let error_rate = error_requests.to_double() / total_requests.to_double() * 100.0
      endpoint_stats.push((endpoint, total_requests, error_requests, error_rate))
      processed_endpoints.push(endpoint)
    }
    
    i = i + 1
  }
  
  // 验证错误率聚合
  assert_eq(endpoint_stats.length(), 6) // 应该有6个不同的端点
  
  // 验证错误率计算
  let mut j = 0
  while j < endpoint_stats.length() {
    let stats = endpoint_stats[j]
    let endpoint = stats.0
    let total_requests = stats.1
    let error_requests = stats.2
    let error_rate = stats.3
    
    assert_eq(total_requests > 0, true)
    assert_eq(error_requests >= 0, true)
    assert_eq(error_requests <= total_requests, true)
    assert_eq(error_rate >= 0.0, true)
    assert_eq(error_rate <= 100.0, true)
    
    // 验证特定端点
    if endpoint == "GET /api/users" {
      assert_eq(total_requests, 107) // 100 + 5 + 2
      assert_eq(error_requests, 7)   // 5 + 2
      assert_eq(error_rate > 6.0, true)
      assert_eq(error_rate < 7.0, true)
    }
    
    j = j + 1
  }
}

test "telemetry_moving_average_calculation" {
  // 测试移动平均计算
  
  let time_series_values = [10.5, 12.3, 11.8, 13.2, 14.7, 12.9, 15.1, 13.8, 16.2, 14.5]
  
  // 计算简单移动平均（窗口大小为3）
  let window_size = 3
  let mut moving_averages = []
  
  let mut i = window_size - 1
  while i < time_series_values.length() {
    let mut sum = 0.0
    let mut j = i - (window_size - 1)
    while j <= i {
      sum = sum + time_series_values[j]
      j = j + 1
    }
    let average = sum / window_size.to_double()
    moving_averages.push(average)
    i = i + 1
  }
  
  // 验证移动平均计算
  assert_eq(moving_averages.length(), time_series_values.length() - window_size + 1)
  
  // 验证第一个移动平均
  assert_eq(moving_averages[0] > 11.0, true)
  assert_eq(moving_averages[0] < 12.0, true)
  
  // 验证移动平均的平滑效果
  let mut j = 1
  while j < moving_averages.length() {
    let prev_avg = moving_averages[j-1]
    let current_avg = moving_averages[j]
    
    // 移动平均应该相对平滑
    let diff = (current_avg - prev_avg).abs()
    assert_eq(diff < 3.0, true)
    
    j = j + 1
  }
  
  // 计算加权移动平均
  let weights = [0.2, 0.3, 0.5] // 越近的权重越大
  let mut weighted_moving_averages = []
  
  i = window_size - 1
  while i < time_series_values.length() {
    let mut weighted_sum = 0.0
    let mut j = i - (window_size - 1)
    let mut weight_index = 0
    while j <= i {
      weighted_sum = weighted_sum + time_series_values[j] * weights[weight_index]
      j = j + 1
      weight_index = weight_index + 1
    }
    weighted_moving_averages.push(weighted_sum)
    i = i + 1
  }
  
  // 验证加权移动平均
  assert_eq(weighted_moving_averages.length(), moving_averages.length())
  
  // 加权平均应该更接近最近的值
  j = 0
  while j < weighted_moving_averages.length() {
    let weighted_avg = weighted_moving_averages[j]
    let simple_avg = moving_averages[j]
    let recent_value = time_series_values[j + window_size - 1]
    
    // 加权平均应该比简单平均更接近最近的值
    let weighted_diff = (weighted_avg - recent_value).abs()
    let simple_diff = (simple_avg - recent_value).abs()
    
    j = j + 1
  }
}

test "telemetry_correlation_analysis" {
  // 测试相关性分析
  
  let metric_pairs = [
    (75.5, 120.3), // cpu_usage, response_time
    (76.2, 125.7),
    (74.8, 118.9),
    (75.9, 122.4),
    (75.1, 119.8),
    (75.7, 121.6),
    (75.3, 120.1),
    (75.8, 123.2),
    (75.2, 119.5),
    (75.9, 122.8)
  ]
  
  // 计算平均值
  let mut sum_x = 0.0
  let mut sum_y = 0.0
  let mut i = 0
  while i < metric_pairs.length() {
    sum_x = sum_x + metric_pairs[i].0
    sum_y = sum_y + metric_pairs[i].1
    i = i + 1
  }
  let mean_x = sum_x / metric_pairs.length().to_double()
  let mean_y = sum_y / metric_pairs.length().to_double()
  
  // 计算协方差和方差
  let mut covariance = 0.0
  let mut variance_x = 0.0
  let mut variance_y = 0.0
  
  i = 0
  while i < metric_pairs.length() {
    let x = metric_pairs[i].0
    let y = metric_pairs[i].1
    
    covariance = covariance + (x - mean_x) * (y - mean_y)
    variance_x = variance_x + (x - mean_x) * (x - mean_x)
    variance_y = variance_y + (y - mean_y) * (y - mean_y)
    
    i = i + 1
  }
  
  covariance = covariance / metric_pairs.length().to_double()
  variance_x = variance_x / metric_pairs.length().to_double()
  variance_y = variance_y / metric_pairs.length().to_double()
  
  // 计算相关系数
  let correlation = covariance / (variance_x.sqrt() * variance_y.sqrt())
  
  // 验证相关性分析
  assert_eq(correlation > -1.0, true)
  assert_eq(correlation < 1.0, true)
  
  // 验证正相关（CPU使用率越高，响应时间越长）
  assert_eq(correlation > 0.0, true)
  assert_eq(correlation > 0.5, true) // 应该有较强的正相关
  
  // 验证统计量
  assert_eq(mean_x > 70.0, true)
  assert_eq(mean_x < 80.0, true)
  assert_eq(mean_y > 110.0, true)
  assert_eq(mean_y < 130.0, true)
  
  assert_eq(variance_x > 0.0, true)
  assert_eq(variance_y > 0.0, true)
}

test "telemetry_anomaly_detection_statistics" {
  // 测试基于统计的异常检测
  
  let performance_metrics = [
    120.5, 125.3, 118.7, 122.1, 119.8, 450.2, 121.4, 123.6, 117.9, 124.2,
    15.3, 120.8, 126.1, 119.3, 122.7, 121.9, 118.2, 125.7, 120.1, 123.4
  ]
  
  // 计算统计量
  let mut sum = 0.0
  let mut i = 0
  while i < performance_metrics.length() {
    sum = sum + performance_metrics[i]
    i = i + 1
  }
  let mean = sum / performance_metrics.length().to_double()
  
  // 计算标准差
  let mut variance_sum = 0.0
  i = 0
  while i < performance_metrics.length() {
    let diff = performance_metrics[i] - mean
    variance_sum = variance_sum + diff * diff
    i = i + 1
  }
  let standard_deviation = (variance_sum / performance_metrics.length().to_double()).sqrt()
  
  // 检测异常值（超过3个标准差）
  let anomaly_threshold = 3.0 * standard_deviation
  let mut anomalies = []
  let mut normal_values = []
  
  i = 0
  while i < performance_metrics.length() {
    let value = performance_metrics[i]
    let z_score = (value - mean).abs() / standard_deviation
    
    if z_score > 3.0 {
      anomalies.push((value, z_score))
    } else {
      normal_values.push(value)
    }
    i = i + 1
  }
  
  // 验证异常检测
  assert_eq(anomalies.length() > 0, true)
  assert_eq(normal_values.length() > 0, true)
  assert_eq(anomalies.length() + normal_values.length(), performance_metrics.length())
  
  // 验证异常值
  let mut j = 0
  while j < anomalies.length() {
    let anomaly = anomalies[j]
    let value = anomaly.0
    let z_score = anomaly.1
    
    assert_eq(z_score > 3.0, true)
    
    // 验证异常值确实偏离正常范围
    if value > mean {
      assert_eq(value > mean + anomaly_threshold, true)
    } else {
      assert_eq(value < mean - anomaly_threshold, true)
    }
    
    j = j + 1
  }
  
  // 验证正常值
  j = 0
  while j < normal_values.length() {
    let value = normal_values[j]
    let z_score = (value - mean).abs() / standard_deviation
    assert_eq(z_score <= 3.0, true)
    j = j + 1
  }
  
  // 验证统计量
  assert_eq(mean > 100.0, true)
  assert_eq(mean < 150.0, true)
  assert_eq(standard_deviation > 50.0, true) // 应该有较大的标准差因为存在异常值
}
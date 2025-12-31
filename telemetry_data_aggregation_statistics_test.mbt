// 遥测数据聚合和统计测试
// 测试遥测数据的聚合算法和统计计算

test "telemetry_data_time_series_aggregation" {
  // 测试遥测时间序列数据聚合
  
  // 创建时间序列数据
  let time_series = [
    (1640995200L, "cpu_usage", 25.5),
    (1640995201L, "cpu_usage", 30.2),
    (1640995202L, "cpu_usage", 28.7),
    (1640995203L, "cpu_usage", 35.1),
    (1640995204L, "cpu_usage", 32.9),
    (1640995205L, "cpu_usage", 29.8),
    (1640995206L, "cpu_usage", 31.4),
    (1640995207L, "cpu_usage", 27.6),
    (1640995208L, "cpu_usage", 33.2),
    (1640995209L, "cpu_usage", 30.5)
  ]
  
  // 验证时间序列数据
  assert_eq(time_series.length(), 10)
  
  // 计算基本统计量
  let mut sum = 0.0
  let mut min_value = 1000.0
  let mut max_value = 0.0
  
  let mut i = 0
  while i < time_series.length() {
    let (_, _, value) = time_series[i]
    sum = sum + value
    
    if value < min_value {
      min_value = value
    }
    if value > max_value {
      max_value = value
    }
    
    i = i + 1
  }
  
  let average = sum / time_series.length().to_double()
  
  // 验证基本统计量
  assert_eq(average > 25.0, true)
  assert_eq(average < 35.0, true)
  assert_eq(min_value, 25.5)
  assert_eq(max_value, 35.1)
  
  // 计算方差和标准差
  let mut variance_sum = 0.0
  i = 0
  while i < time_series.length() {
    let (_, _, value) = time_series[i]
    let deviation = value - average
    variance_sum = variance_sum + deviation * deviation
    i = i + 1
  }
  
  let variance = variance_sum / time_series.length().to_double()
  let standard_deviation = variance.sqrt()
  
  // 验证方差和标准差
  assert_eq(variance > 0.0, true)
  assert_eq(standard_deviation > 0.0, true)
  assert_eq(standard_deviation < 10.0, true)
  
  // 时间窗口聚合（每5个数据点一个窗口）
  let window_size = 5
  let window_count = time_series.length() / window_size
  
  // 验证窗口数量
  assert_eq(window_count, 2)
  
  // 计算每个窗口的统计量
  let window_1_start = 0
  let window_1_end = window_size - 1
  let window_2_start = window_size
  let window_2_end = window_size * 2 - 1
  
  // 窗口1统计
  let mut window_1_sum = 0.0
  let mut window_1_min = 1000.0
  let mut window_1_max = 0.0
  
  let mut j = window_1_start
  while j <= window_1_end {
    let (_, _, value) = time_series[j]
    window_1_sum = window_1_sum + value
    
    if value < window_1_min {
      window_1_min = value
    }
    if value > window_1_max {
      window_1_max = value
    }
    
    j = j + 1
  }
  
  let window_1_avg = window_1_sum / window_size.to_double()
  
  // 窗口2统计
  let mut window_2_sum = 0.0
  let mut window_2_min = 1000.0
  let mut window_2_max = 0.0
  
  j = window_2_start
  while j <= window_2_end {
    let (_, _, value) = time_series[j]
    window_2_sum = window_2_sum + value
    
    if value < window_2_min {
      window_2_min = value
    }
    if value > window_2_max {
      window_2_max = value
    }
    
    j = j + 1
  }
  
  let window_2_avg = window_2_sum / window_size.to_double()
  
  // 验证窗口统计
  assert_eq(window_1_avg > 25.0, true)
  assert_eq(window_2_avg > 25.0, true)
  assert_eq(window_1_min, 25.5)
  assert_eq(window_1_max, 35.1)
  assert_eq(window_2_min, 27.6)
  assert_eq(window_2_max, 33.2)
}

test "telemetry_data_percentile_aggregation" {
  // 测试遥测数据百分位数聚合
  
  // 创建延迟数据
  let latency_data = [
    10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0,
    60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0, 95.0, 100.0, 150.0,
    200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0
  ]
  
  // 验证延迟数据
  assert_eq(latency_data.length(), 27)
  
  // 计算百分位数（简化算法）
  let p50_index = (latency_data.length() * 50) / 100
  let p95_index = (latency_data.length() * 95) / 100
  let p99_index = (latency_data.length() * 99) / 100
  
  let p50_latency = latency_data[p50_index]
  let p95_latency = latency_data[p95_index]
  let p99_latency = latency_data[p99_index]
  
  // 验证百分位数
  assert_eq(p50_latency, 90.0)
  assert_eq(p95_latency, 450.0)
  assert_eq(p99_latency, 500.0)
  
  // 验证百分位数递增性
  assert_eq(p95_latency >= p50_latency, true)
  assert_eq(p99_latency >= p95_latency, true)
  
  // 创建直方图数据
  let histogram_buckets = [
    (0.0, 50.0, 5),    // 0-50ms: 5个请求
    (50.0, 100.0, 10), // 50-100ms: 10个请求
    (100.0, 200.0, 5), // 100-200ms: 5个请求
    (200.0, 500.0, 6), // 200-500ms: 6个请求
    (500.0, 1000.0, 1) // 500-1000ms: 1个请求
  ]
  
  // 验证直方图数据
  assert_eq(histogram_buckets.length(), 5)
  
  // 计算总请求数
  let mut total_requests = 0
  let mut i = 0
  
  while i < histogram_buckets.length() {
    let (_, _, count) = histogram_buckets[i]
    total_requests = total_requests + count
    i = i + 1
  }
  
  assert_eq(total_requests, 27)
  
  // 计算每个桶的百分比
  let mut bucket_percentages = []
  i = 0
  while i < histogram_buckets.length() {
    let (lower_bound, upper_bound, count) = histogram_buckets[i]
    let percentage = count.to_double() / total_requests.to_double() * 100.0
    bucket_percentages.push((lower_bound, upper_bound, percentage))
    i = i + 1
  }
  
  // 验证桶百分比
  assert_eq(bucket_percentages.length(), 5)
  assert_eq(bucket_percentages[0].2, 18.52) // 5/27 ≈ 18.52%
  assert_eq(bucket_percentages[1].2, 37.04) // 10/27 ≈ 37.04%
  
  // 验证百分比总和
  let mut total_percentage = 0.0
  i = 0
  while i < bucket_percentages.length() {
    total_percentage = total_percentage + bucket_percentages[i].2
    i = i + 1
  }
  
  assert_eq(total_percentage > 99.0, true) // 允许小的浮点误差
  assert_eq(total_percentage <= 101.0, true)
}

test "telemetry_data_group_by_aggregation" {
  // 测试遥测数据分组聚合
  
  // 创建多维度数据
  let multi_dimensional_data = [
    ("service_a", "endpoint_1", "GET", 200, 50.0),
    ("service_a", "endpoint_1", "GET", 200, 55.0),
    ("service_a", "endpoint_2", "POST", 201, 150.0),
    ("service_b", "endpoint_3", "GET", 200, 75.0),
    ("service_b", "endpoint_3", "GET", 500, 25.0),
    ("service_a", "endpoint_1", "GET", 404, 10.0),
    ("service_c", "endpoint_4", "POST", 200, 200.0),
    ("service_b", "endpoint_5", "PUT", 200, 100.0),
    ("service_a", "endpoint_2", "POST", 400, 20.0),
    ("service_c", "endpoint_4", "POST", 200, 180.0)
  ]
  
  // 验证多维度数据
  assert_eq(multi_dimensional_data.length(), 10)
  
  // 按服务分组聚合
  let mut service_groups = []
  
  // 收集所有服务名称
  let mut services = []
  let mut i = 0
  while i < multi_dimensional_data.length() {
    let (service_name, _, _, _, _) = multi_dimensional_data[i]
    
    // 检查服务是否已存在
    let mut service_exists = false
    let mut j = 0
    while j < services.length() {
      if services[j] == service_name {
        service_exists = true
      }
      j = j + 1
    }
    
    if !service_exists {
      services.push(service_name)
    }
    
    i = i + 1
  }
  
  // 验证服务列表
  assert_eq(services.length(), 3)
  assert_eq(services.contains("service_a"), true)
  assert_eq(services.contains("service_b"), true)
  assert_eq(services.contains("service_c"), true)
  
  // 按服务聚合统计
  i = 0
  while i < services.length() {
    let service_name = services[i]
    let mut request_count = 0
    let mut total_latency = 0.0
    let mut success_count = 0
    let mut error_count = 0
    
    // 统计该服务的数据
    let mut j = 0
    while j < multi_dimensional_data.length() {
      let (s_name, _, _, status_code, latency) = multi_dimensional_data[j]
      
      if s_name == service_name {
        request_count = request_count + 1
        total_latency = total_latency + latency
        
        if status_code >= 200 && status_code < 300 {
          success_count = success_count + 1
        } else {
          error_count = error_count + 1
        }
      }
      
      j = j + 1
    }
    
    let average_latency = total_latency / request_count.to_double()
    let success_rate = success_count.to_double() / request_count.to_double() * 100.0
    
    service_groups.push((service_name, request_count, average_latency, success_rate))
    
    i = i + 1
  }
  
  // 验证服务分组聚合结果
  assert_eq(service_groups.length(), 3)
  
  // 验证service_a的统计
  let mut service_a_stats = ("", 0, 0.0, 0.0)
  i = 0
  while i < service_groups.length() {
    if service_groups[i].0 == "service_a" {
      service_a_stats = service_groups[i]
    }
    i = i + 1
  }
  
  assert_eq(service_a_stats.0, "service_a")
  assert_eq(service_a_stats.1, 4) // service_a有4个请求
  assert_eq(service_a_stats.2 > 0.0, true)
  assert_eq(service_a_stats.3 > 0.0, true)
  
  // 按端点和HTTP方法分组聚合
  let mut endpoint_groups = []
  
  i = 0
  while i < multi_dimensional_data.length() {
    let (_, endpoint, method, _, _) = multi_dimensional_data[i]
    let group_key = endpoint + ":" + method
    
    // 检查分组是否已存在
    let mut group_exists = false
    let mut j = 0
    while j < endpoint_groups.length() {
      if endpoint_groups[j].0 == group_key {
        group_exists = true
      }
      j = j + 1
    }
    
    if !group_exists {
      // 统计该分组的请求
      let mut count = 0
      let mut j = 0
      while j < multi_dimensional_data.length() {
        let (_, e, m, _, _) = multi_dimensional_data[j]
        if (e + ":" + m) == group_key {
          count = count + 1
        }
        j = j + 1
      }
      
      endpoint_groups.push((group_key, count))
    }
    
    i = i + 1
  }
  
  // 验证端点分组聚合结果
  assert_eq(endpoint_groups.length() > 0, true)
  
  // 验证特定端点的统计
  let mut endpoint_1_get_count = 0
  i = 0
  while i < endpoint_groups.length() {
    if endpoint_groups[i].0 == "endpoint_1:GET" {
      endpoint_1_get_count = endpoint_groups[i].1
    }
    i = i + 1
  }
  
  assert_eq(endpoint_1_get_count, 3) // endpoint_1:GET有3个请求
}

test "telemetry_data_rate_aggregation" {
  // 测试遥测数据速率聚合
  
  // 创建时间戳计数数据
  let timestamp_counts = [
    (1640995200L, 100), // 每秒请求数
    (1640995201L, 120),
    (1640995202L, 95),
    (1640995203L, 110),
    (1640995204L, 105),
    (1640995205L, 130),
    (1640995206L, 115),
    (1640995207L, 125),
    (1640995208L, 90),
    (1640995209L, 100)
  ]
  
  // 验证时间戳计数数据
  assert_eq(timestamp_counts.length(), 10)
  
  // 计算平均速率
  let mut total_requests = 0
  let mut i = 0
  
  while i < timestamp_counts.length() {
    let (_, count) = timestamp_counts[i]
    total_requests = total_requests + count
    i = i + 1
  }
  
  let time_span = 10 // 10秒
  let average_rate = total_requests.to_double() / time_span.to_double()
  
  // 验证平均速率
  assert_eq(average_rate > 100.0, true)
  assert_eq(average_rate < 120.0, true)
  
  // 计算最大和最小速率
  let mut max_rate = 0
  let mut min_rate = 1000
  
  i = 0
  while i < timestamp_counts.length() {
    let (_, count) = timestamp_counts[i]
    
    if count > max_rate {
      max_rate = count
    }
    if count < min_rate {
      min_rate = count
    }
    
    i = i + 1
  }
  
  // 验证极值速率
  assert_eq(max_rate, 130)
  assert_eq(min_rate, 90)
  
  // 计算速率方差
  let mut variance_sum = 0.0
  i = 0
  while i < timestamp_counts.length() {
    let (_, count) = timestamp_counts[i]
    let deviation = count.to_double() - average_rate
    variance_sum = variance_sum + deviation * deviation
    i = i + 1
  }
  
  let rate_variance = variance_sum / timestamp_counts.length().to_double()
  let rate_std_dev = rate_variance.sqrt()
  
  // 验证速率方差
  assert_eq(rate_variance > 0.0, true)
  assert_eq(rate_std_dev > 0.0, true)
  
  // 滑动窗口速率计算
  let window_size = 3
  let mut sliding_rates = []
  
  i = 0
  while i <= timestamp_counts.length() - window_size {
    let mut window_sum = 0
    let mut j = 0
    
    while j < window_size {
      window_sum = window_sum + timestamp_counts[i + j].1
      j = j + 1
    }
    
    let window_rate = window_sum.to_double() / window_size.to_double()
    sliding_rates.push(window_rate)
    
    i = i + 1
  }
  
  // 验证滑动窗口速率
  assert_eq(sliding_rates.length(), 8) // 10-3+1 = 8个窗口
  
  // 验证第一个窗口的速率
  let first_window_rate = (100 + 120 + 95).to_double() / 3.0
  assert_eq(sliding_rates[0], first_window_rate)
  
  // 验证最后一个窗口的速率
  let last_window_rate = (125 + 90 + 100).to_double() / 3.0
  assert_eq(sliding_rates[7], last_window_rate)
  
  // 创建错误率数据
  let error_rate_data = [
    (1640995200L, 5, 100),   // (timestamp, error_count, total_count)
    (1640995201L, 8, 120),
    (1640995202L, 3, 95),
    (1640995203L, 12, 110),
    (1640995204L, 7, 105)
  ]
  
  // 验证错误率数据
  assert_eq(error_rate_data.length(), 5)
  
  // 计算错误率
  let mut error_rates = []
  i = 0
  while i < error_rate_data.length() {
    let (_, error_count, total_count) = error_rate_data[i]
    let error_rate = error_count.to_double() / total_count.to_double() * 100.0
    error_rates.push(error_rate)
    i = i + 1
  }
  
  // 验证错误率计算
  assert_eq(error_rates.length(), 5)
  assert_eq(error_rates[0], 5.0) // 5/100 = 5%
  assert_eq(error_rates[1], 6.67) // 8/120 ≈ 6.67%
  
  // 计算平均错误率
  let mut total_error_rate = 0.0
  i = 0
  while i < error_rates.length() {
    total_error_rate = total_error_rate + error_rates[i]
    i = i + 1
  }
  
  let average_error_rate = total_error_rate / error_rates.length().to_double()
  
  // 验证平均错误率
  assert_eq(average_error_rate > 5.0, true)
  assert_eq(average_error_rate < 10.0, true)
}

test "telemetry_data_histogram_aggregation" {
  // 测试遥测数据直方图聚合
  
  // 创建原始数值数据
  let raw_values = [
    1.2, 2.5, 3.7, 4.1, 5.8, 6.3, 7.9, 8.4, 9.6, 10.2,
    11.5, 12.8, 13.1, 14.7, 15.3, 16.9, 17.4, 18.6, 19.2, 20.5,
    21.8, 22.3, 23.7, 24.1, 25.9, 26.4, 27.6, 28.2, 29.5, 30.1
  ]
  
  // 验证原始数据
  assert_eq(raw_values.length(), 30)
  
  // 创建等宽直方图
  let bin_width = 5.0
  let min_value = 0.0
  let max_value = 30.0
  let bin_count = ((max_value - min_value) / bin_width).to_int()
  
  // 验证直方图参数
  assert_eq(bin_count, 6)
  
  // 初始化直方图桶
  let mut histogram_bins = []
  let mut i = 0
  
  while i < bin_count {
    let bin_start = min_value + i.to_double() * bin_width
    let bin_end = bin_start + bin_width
    histogram_bins.push((bin_start, bin_end, 0))
    i = i + 1
  }
  
  // 验证直方图桶初始化
  assert_eq(histogram_bins.length(), 6)
  
  // 将数据分配到桶中
  i = 0
  while i < raw_values.length() {
    let value = raw_values[i]
    let bin_index = (value / bin_width).to_int()
    
    if bin_index >= 0 && bin_index < histogram_bins.length() {
      let (start, end, count) = histogram_bins[bin_index]
      histogram_bins[bin_index] = (start, end, count + 1)
    }
    
    i = i + 1
  }
  
  // 验证直方图分布
  let mut total_count = 0
  i = 0
  while i < histogram_bins.length() {
    let (_, _, count) = histogram_bins[i]
    total_count = total_count + count
    i = i + 1
  }
  
  assert_eq(total_count, raw_values.length())
  
  // 验证第一个桶 (0-5)
  assert_eq(histogram_bins[0].2, 5) // 1.2, 2.5, 3.7, 4.1, 5.8中的前4个
  
  // 验证最后一个桶 (25-30)
  assert_eq(histogram_bins[5].2, 5) // 25.9, 26.4, 27.6, 28.2, 29.5, 30.1中的前5个
  
  // 创建等频直方图（每个桶包含相同数量的数据点）
  let items_per_bin = raw_values.length() / 6 // 6个桶
  let mut equal_freq_bins = []
  
  i = 0
  while i < 6 {
    let start_index = i * items_per_bin
    let end_index = start_index + items_per_bin - 1
    
    if start_index < raw_values.length() && end_index < raw_values.length() {
      let bin_start = raw_values[start_index]
      let bin_end = raw_values[end_index]
      equal_freq_bins.push((bin_start, bin_end, items_per_bin))
    }
    
    i = i + 1
  }
  
  // 验证等频直方图
  assert_eq(equal_freq_bins.length(), 6)
  
  // 验证每个桶的数据点数量
  i = 0
  while i < equal_freq_bins.length() {
    let (_, _, count) = equal_freq_bins[i]
    assert_eq(count, items_per_bin)
    i = i + 1
  }
  
  // 创建自定义直方图桶
  let custom_bins = [
    (0.0, 10.0),    // 桶1: 0-10
    (10.0, 20.0),   // 桶2: 10-20
    (20.0, 30.0),   // 桶3: 20-30
    (30.0, 100.0)   // 桶4: 30-100
  ]
  
  // 初始化自定义桶计数
  let mut custom_bin_counts = []
  i = 0
  while i < custom_bins.length() {
    custom_bin_counts.push(0)
    i = i + 1
  }
  
  // 分配数据到自定义桶
  i = 0
  while i < raw_values.length() {
    let value = raw_values[i]
    let mut j = 0
    
    while j < custom_bins.length() {
      let (bin_start, bin_end) = custom_bins[j]
      
      if value >= bin_start && value < bin_end {
        custom_bin_counts[j] = custom_bin_counts[j] + 1
      }
      
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证自定义直方图
  assert_eq(custom_bin_counts.length(), 4)
  assert_eq(custom_bin_counts[0], 9)  // 0-10范围有9个值
  assert_eq(custom_bin_counts[1], 10) // 10-20范围有10个值
  assert_eq(custom_bin_counts[2], 10) // 20-30范围有10个值
  assert_eq(custom_bin_counts[3], 1)  // 30-100范围有1个值
  
  // 验证总计数
  let mut custom_total = 0
  i = 0
  while i < custom_bin_counts.length() {
    custom_total = custom_total + custom_bin_counts[i]
    i = i + 1
  }
  
  assert_eq(custom_total, raw_values.length())
}
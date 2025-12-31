// 遥测数据聚合测试用例
// 测试telemetry数据的聚合功能

test "telemetry_data_aggregation_basic" {
  // 测试基本的遥测数据聚合功能
  
  let metric_values = [10.5, 15.2, 8.7, 12.3, 9.8, 11.1, 14.6, 7.9]
  let metric_names = ["cpu_usage", "memory_usage", "disk_io", "network_latency"]
  
  // 验证数据长度
  assert_eq(metric_values.length(), 8)
  assert_eq(metric_names.length(), 4)
  
  // 计算总和
  let mut sum = 0.0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  assert_eq(sum > 80.0, true)
  assert_eq(sum < 100.0, true)
  
  // 计算平均值
  let average = sum / metric_values.length().to_double()
  assert_eq(average > 10.0, true)
  assert_eq(average < 13.0, true)
  
  // 查找最大值和最小值
  let mut max_value = metric_values[0]
  let mut min_value = metric_values[0]
  i = 1
  while i < metric_values.length() {
    if metric_values[i] > max_value {
      max_value = metric_values[i]
    }
    if metric_values[i] < min_value {
      min_value = metric_values[i]
    }
    i = i + 1
  }
  assert_eq(max_value, 15.2)
  assert_eq(min_value, 7.9)
  
  // 计算范围
  let range = max_value - min_value
  assert_eq(range, 7.3)
}

test "telemetry_data_aggregation_time_window" {
  // 测试时间窗口内的数据聚合
  
  let timestamps = [1640995200L, 1640995260L, 1640995320L, 1640995380L, 1640995440L]
  let values = [100.0, 120.0, 90.0, 110.0, 105.0]
  
  // 验证时间戳递增
  let mut i = 1
  while i < timestamps.length() {
    assert_eq(timestamps[i] > timestamps[i-1], true)
    i = i + 1
  }
  
  // 计算时间窗口内的平均值
  let window_start = 1640995200L
  let window_end = 1640995400L
  
  let mut window_sum = 0.0
  let mut window_count = 0
  i = 0
  while i < timestamps.length() {
    if timestamps[i] >= window_start && timestamps[i] <= window_end {
      window_sum = window_sum + values[i]
      window_count = window_count + 1
    }
    i = i + 1
  }
  
  assert_eq(window_count, 4)
  let window_average = window_sum / window_count.to_double()
  assert_eq(window_average > 100.0, true)
  assert_eq(window_average < 110.0, true)
}

test "telemetry_data_aggregation_group_by" {
  // 测试按属性分组的聚合
  
  let service_names = ["api-service", "db-service", "api-service", "cache-service", "db-service"]
  let response_times = [50.0, 120.0, 60.0, 15.0, 130.0]
  
  // 按服务名称分组聚合
  let api_times = []
  let db_times = []
  let cache_times = []
  
  let mut i = 0
  while i < service_names.length() {
    if service_names[i] == "api-service" {
      api_times.push(response_times[i])
    } else if service_names[i] == "db-service" {
      db_times.push(response_times[i])
    } else if service_names[i] == "cache-service" {
      cache_times.push(response_times[i])
    }
    i = i + 1
  }
  
  // 验证分组结果
  assert_eq(api_times.length(), 2)
  assert_eq(db_times.length(), 2)
  assert_eq(cache_times.length(), 1)
  
  // 计算每组的平均值
  let api_avg = (api_times[0] + api_times[1]) / api_times.length().to_double()
  let db_avg = (db_times[0] + db_times[1]) / db_times.length().to_double()
  let cache_avg = cache_times[0]
  
  assert_eq(api_avg, 55.0)
  assert_eq(db_avg, 125.0)
  assert_eq(cache_avg, 15.0)
}

test "telemetry_data_aggregation_percentiles" {
  // 测试百分位数计算
  
  let sorted_values = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
  
  // 验证数组已排序
  let mut i = 1
  while i < sorted_values.length() {
    assert_eq(sorted_values[i] >= sorted_values[i-1], true)
    i = i + 1
  }
  
  // 计算中位数 (50th percentile)
  let middle_index = sorted_values.length() / 2
  let median = if sorted_values.length() % 2 == 0 {
    (sorted_values[middle_index - 1] + sorted_values[middle_index]) / 2.0
  } else {
    sorted_values[middle_index]
  }
  assert_eq(median, 5.5)
  
  // 计算90th百分位数
  let p90_index = (sorted_values.length() * 90) / 100
  let p90_value = sorted_values[p90_index]
  assert_eq(p90_value, 9.0)
  
  // 计算95th百分位数
  let p95_index = (sorted_values.length() * 95) / 100
  let p95_value = sorted_values[p95_index]
  assert_eq(p95_value, 10.0)
}

test "telemetry_data_aggregation_rate_calculation" {
  // 测试速率计算
  
  let event_counts = [100L, 150L, 125L, 175L, 140L]
  let time_intervals = [60L, 60L, 60L, 60L, 60L] // 秒
  
  // 计算每个时间间隔的速率
  let rates = []
  let mut i = 0
  while i < event_counts.length() {
    let rate = event_counts[i].to_double() / time_intervals[i].to_double()
    rates.push(rate)
    i = i + 1
  }
  
  // 验证速率计算
  assert_eq(rates.length(), 5)
  assert_eq(rates[0], 100.0 / 60.0)
  assert_eq(rates[2], 125.0 / 60.0)
  assert_eq(rates[4], 140.0 / 60.0)
  
  // 计算平均速率
  let mut total_rate = 0.0
  i = 0
  while i < rates.length() {
    total_rate = total_rate + rates[i]
    i = i + 1
  }
  let average_rate = total_rate / rates.length().to_double()
  assert_eq(average_rate > 2.0, true)
  assert_eq(average_rate < 3.0, true)
}

test "telemetry_data_aggregation_histogram" {
  // 测试直方图聚合
  
  let response_times = [0.1, 0.5, 1.2, 0.8, 2.5, 0.3, 1.8, 0.6, 1.1, 0.9]
  let bucket_boundaries = [0.5, 1.0, 1.5, 2.0, 2.5]
  
  // 初始化桶计数
  let bucket_counts = [0L, 0L, 0L, 0L, 0L, 0L] // 包括一个overflow桶
  
  // 将响应时间分配到桶中
  let mut i = 0
  while i < response_times.length() {
    let mut bucket_index = bucket_boundaries.length() // 默认到overflow桶
    let mut j = 0
    while j < bucket_boundaries.length() {
      if response_times[i] <= bucket_boundaries[j] {
        bucket_index = j
        break
      }
      j = j + 1
    }
    bucket_counts[bucket_index] = bucket_counts[bucket_index] + 1L
    i = i + 1
  }
  
  // 验证桶计数
  assert_eq(bucket_counts[0], 3) // <= 0.5
  assert_eq(bucket_counts[1], 2) // > 0.5, <= 1.0
  assert_eq(bucket_counts[2], 2) // > 1.0, <= 1.5
  assert_eq(bucket_counts[3], 1) // > 1.5, <= 2.0
  assert_eq(bucket_counts[4], 1) // > 2.0, <= 2.5
  assert_eq(bucket_counts[5], 1) // > 2.5 (overflow)
  
  // 验证总计数
  let mut total_count = 0L
  i = 0
  while i < bucket_counts.length() {
    total_count = total_count + bucket_counts[i]
    i = i + 1
  }
  assert_eq(total_count, response_times.length().to_long())
}

test "telemetry_data_aggregation_downsampling" {
  // 测试数据下采样
  
  let high_frequency_data = [1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9]
  let downsample_factor = 3
  
  // 计算下采样后的数据点数
  let original_length = high_frequency_data.length()
  let downsampled_length = (original_length + downsample_factor - 1) / downsample_factor
  assert_eq(downsampled_length, 4)
  
  // 执行下采样（取每个窗口的平均值）
  let downsampled_data = []
  let mut i = 0
  while i < original_length {
    let window_start = i
    let window_end = if i + downsample_factor < original_length {
      i + downsample_factor
    } else {
      original_length
    }
    
    // 计算窗口平均值
    let mut window_sum = 0.0
    let mut window_count = 0
    let mut j = window_start
    while j < window_end {
      window_sum = window_sum + high_frequency_data[j]
      window_count = window_count + 1
      j = j + 1
    }
    
    let window_average = window_sum / window_count.to_double()
    downsampled_data.push(window_average)
    
    i = i + downsample_factor
  }
  
  // 验证下采样结果
  assert_eq(downsampled_data.length(), 4)
  assert_eq(downsampled_data[0] > 1.0 && downsampled_data[0] < 1.2, true)
  assert_eq(downsampled_data[1] > 1.3 && downsampled_data[1] < 1.5, true)
  assert_eq(downsampled_data[2] > 1.5 && downsampled_data[2] < 1.7, true)
  assert_eq(downsampled_data[3] > 1.8 && downsampled_data[3] < 2.0, true)
}
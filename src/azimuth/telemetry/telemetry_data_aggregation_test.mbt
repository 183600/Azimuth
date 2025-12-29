// 遥测数据聚合测试用例

test "aggregation_sum_operations" {
  // 测试求和聚合操作
  
  let metric_values = [10.5, 15.2, 8.7, 12.3, 9.8, 14.6, 11.1, 13.4]
  let mut sum_value = 0.0
  
  // 计算总和
  let mut i = 0
  while i < metric_values.length() {
    sum_value = sum_value + metric_values[i]
    i = i + 1
  }
  
  // 验证求和结果
  assert_eq(metric_values.length(), 8)
  assert_eq(sum_value, 95.6) // 手动计算验证
  
  // 测试分组求和
  let grouped_values = [
    ([1.0, 2.0, 3.0], "group-a"),
    ([4.0, 5.0, 6.0], "group-b"),
    ([7.0, 8.0, 9.0], "group-c")
  ]
  
  let group_sums = []
  i = 0
  while i < grouped_values.length() {
    let (values, group_name) = grouped_values[i]
    let mut group_sum = 0.0
    let mut j = 0
    while j < values.length() {
      group_sum = group_sum + values[j]
      j = j + 1
    }
    group_sums.push((group_name, group_sum))
    i = i + 1
  }
  
  // 验证分组求和
  assert_eq(grouped_values.length(), 3)
  assert_eq(group_sums.length(), 3)
  assert_eq(group_sums[0].1, 6.0) // 1+2+3
  assert_eq(group_sums[1].1, 15.0) // 4+5+6
  assert_eq(group_sums[2].1, 24.0) // 7+8+9
}

test "aggregation_average_operations" {
  // 测试平均值聚合操作
  
  let temperature_readings = [22.5, 23.1, 21.8, 24.2, 22.9, 23.6, 21.4, 22.7]
  let mut sum = 0.0
  
  // 计算平均值
  let mut i = 0
  while i < temperature_readings.length() {
    sum = sum + temperature_readings[i]
    i = i + 1
  }
  let average = sum / temperature_readings.length().to_double()
  
  // 验证平均值计算
  assert_eq(temperature_readings.length(), 8)
  assert_eq(average > 22.0 && average < 24.0, true)
  
  // 测试移动平均
  let window_size = 3
  let moving_averages = []
  i = 0
  while i <= temperature_readings.length() - window_size {
    let mut window_sum = 0.0
    let mut j = 0
    while j < window_size {
      window_sum = window_sum + temperature_readings[i + j]
      j = j + 1
    }
    let window_avg = window_sum / window_size.to_double()
    moving_averages.push(window_avg)
    i = i + 1
  }
  
  // 验证移动平均
  assert_eq(moving_averages.length(), 6) // 8-3+1
  assert_eq(moving_averages[0], (22.5 + 23.1 + 21.8) / 3.0)
  assert_eq(moving_averages[5], (22.9 + 23.6 + 21.4) / 3.0)
}

test "aggregation_min_max_operations" {
  // 测试最小值最大值聚合操作
  
  let response_times = [120.5, 85.3, 200.1, 95.7, 150.8, 78.2, 180.4, 110.6]
  
  // 查找最小值和最大值
  let mut min_time = response_times[0]
  let mut max_time = response_times[0]
  let mut i = 0
  while i < response_times.length() {
    if response_times[i] < min_time {
      min_time = response_times[i]
    }
    if response_times[i] > max_time {
      max_time = response_times[i]
    }
    i = i + 1
  }
  
  // 验证最值
  assert_eq(response_times.length(), 8)
  assert_eq(min_time, 78.2)
  assert_eq(max_time, 200.1)
  
  // 计算范围
  let range = max_time - min_time
  assert_eq(range, 121.9)
  
  // 测试分组最值
  let service_metrics = [
    (["api-service"], [50.0, 75.0, 25.0, 100.0]),
    (["db-service"], [150.0, 200.0, 120.0, 180.0]),
    (["cache-service"], [5.0, 8.0, 3.0, 12.0])
  ]
  
  let group_min_max = []
  i = 0
  while i < service_metrics.length() {
    let (services, times) = service_metrics[i]
    let mut group_min = times[0]
    let mut group_max = times[0]
    let mut j = 0
    while j < times.length() {
      if times[j] < group_min {
        group_min = times[j]
      }
      if times[j] > group_max {
        group_max = times[j]
      }
      j = j + 1
    }
    group_min_max.push((services[0], group_min, group_max))
    i = i + 1
  }
  
  // 验证分组最值
  assert_eq(group_min_max.length(), 3)
  assert_eq(group_min_max[0].1, 25.0) // api-service最小值
  assert_eq(group_min_max[0].2, 100.0) // api-service最大值
  assert_eq(group_min_max[1].1, 120.0) // db-service最小值
  assert_eq(group_min_max[1].2, 200.0) // db-service最大值
}

test "aggregation_percentile_operations" {
  // 测试百分位数聚合操作
  
  let sorted_latencies = [10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0]
  
  // 计算P50 (中位数)
  let p50_index = sorted_latencies.length() / 2
  let p50_value = if sorted_latencies.length() % 2 == 0 {
    (sorted_latencies[p50_index - 1] + sorted_latencies[p50_index]) / 2.0
  } else {
    sorted_latencies[p50_index]
  }
  
  // 计算P95
  let p95_index = (sorted_latencies.length() * 95) / 100
  let p95_value = sorted_latencies[p95_index]
  
  // 计算P99
  let p99_index = (sorted_latencies.length() * 99) / 100
  let p99_value = sorted_latencies[p99_index]
  
  // 验证百分位数
  assert_eq(sorted_latencies.length(), 10)
  assert_eq(p50_value, 27.5) // (25 + 30) / 2
  assert_eq(p95_value, 50.0) // 第95个百分位
  assert_eq(p99_value, 55.0) // 第99个百分位
  
  // 验证百分位数关系
  assert_eq(p50_value < p95_value, true)
  assert_eq(p95_value < p99_value, true)
  assert_eq(p99_value <= sorted_latencies[sorted_latencies.length() - 1], true)
}

test "aggregation_count_operations" {
  // 测试计数聚合操作
  
  let event_types = ["error", "success", "warning", "success", "error", "info", "success", "warning", "error", "info"]
  let event_counts = []
  
  // 统计事件类型计数
  let unique_types = ["error", "success", "warning", "info"]
  let mut i = 0
  while i < unique_types.length() {
    let event_type = unique_types[i]
    let mut count = 0
    let mut j = 0
    while j < event_types.length() {
      if event_types[j] == event_type {
        count = count + 1
      }
      j = j + 1
    }
    event_counts.push((event_type, count))
    i = i + 1
  }
  
  // 验证计数结果
  assert_eq(event_types.length(), 10)
  assert_eq(event_counts.length(), 4)
  
  // 验证具体计数
  let mut error_count = 0
  let mut success_count = 0
  let mut warning_count = 0
  let mut info_count = 0
  i = 0
  while i < event_counts.length() {
    match event_counts[i].0 {
      "error" => error_count = event_counts[i].1,
      "success" => success_count = event_counts[i].1,
      "warning" => warning_count = event_counts[i].1,
      "info" => info_count = event_counts[i].1,
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(error_count, 3)
  assert_eq(success_count, 3)
  assert_eq(warning_count, 2)
  assert_eq(info_count, 2)
  assert_eq(error_count + success_count + warning_count + info_count, 10)
}

test "aggregation_rate_operations" {
  // 测试速率聚合操作
  
  let request_counts = [100, 150, 120, 180, 200, 160, 140, 190]
  let time_intervals = [60, 60, 60, 60, 60, 60, 60, 60] // 每个间隔60秒
  let request_rates = []
  
  // 计算每个时间间隔的请求速率
  let mut i = 0
  while i < request_counts.length() {
    let rate = request_counts[i].to_double() / time_intervals[i].to_double()
    request_rates.push(rate)
    i = i + 1
  }
  
  // 验证速率计算
  assert_eq(request_counts.length(), 8)
  assert_eq(request_rates.length(), 8)
  assert_eq(request_rates[0], 100.0 / 60.0) // 约1.67请求/秒
  assert_eq(request_rates[4], 200.0 / 60.0) // 约3.33请求/秒
  
  // 计算平均速率
  let mut total_requests = 0
  let mut total_time = 0
  i = 0
  while i < request_counts.length() {
    total_requests = total_requests + request_counts[i]
    total_time = total_time + time_intervals[i]
    i = i + 1
  }
  let average_rate = total_requests.to_double() / total_time.to_double()
  
  // 验证平均速率
  assert_eq(total_requests, 1240)
  assert_eq(total_time, 480) // 8 * 60
  assert_eq(average_rate, 1240.0 / 480.0) // 约2.58请求/秒
}

test "aggregation_histogram_operations" {
  // 测试直方图聚合操作
  
  let response_times = [45.2, 78.5, 120.3, 23.7, 156.8, 89.4, 34.1, 201.2, 67.9, 143.6]
  let histogram_bins = [
    (0.0, 50.0),   // 0-50ms
    (50.0, 100.0), // 50-100ms
    (100.0, 150.0), // 100-150ms
    (150.0, 200.0), // 150-200ms
    (200.0, 250.0)  // 200-250ms
  ]
  let histogram_counts = []
  
  // 统计每个桶的计数
  let mut i = 0
  while i < histogram_bins.length() {
    let (lower_bound, upper_bound) = histogram_bins[i]
    let mut count = 0
    let mut j = 0
    while j < response_times.length() {
      if response_times[j] >= lower_bound && response_times[j] < upper_bound {
        count = count + 1
      }
      j = j + 1
    }
    histogram_counts.push((lower_bound, upper_bound, count))
    i = i + 1
  }
  
  // 验证直方图计数
  assert_eq(response_times.length(), 10)
  assert_eq(histogram_bins.length(), 5)
  assert_eq(histogram_counts.length(), 5)
  
  // 验证具体桶计数
  assert_eq(histogram_counts[0].2, 3) // 0-50ms: 45.2, 23.7, 34.1
  assert_eq(histogram_counts[1].2, 3) // 50-100ms: 78.5, 89.4, 67.9
  assert_eq(histogram_counts[2].2, 2) // 100-150ms: 120.3, 143.6
  assert_eq(histogram_counts[3].2, 1) // 150-200ms: 156.8
  assert_eq(histogram_counts[4].2, 1) // 200-250ms: 201.2
  
  // 验证总计数
  let mut total_count = 0
  i = 0
  while i < histogram_counts.length() {
    total_count = total_count + histogram_counts[i].2
    i = i + 1
  }
  assert_eq(total_count, 10)
}

test "aggregation_time_bucket_operations" {
  // 测试时间桶聚合操作
  
  let timestamped_metrics = [
    (1640995200L, 10.0), // 2022-01-01 00:00:00
    (1640995260L, 15.0), // 2022-01-01 00:01:00
    (1640995320L, 12.0), // 2022-01-01 00:02:00
    (1640995380L, 18.0), // 2022-01-01 00:03:00
    (1640995440L, 20.0), // 2022-01-01 00:04:00
    (1640995500L, 14.0), // 2022-01-01 00:05:00
    (1640995560L, 16.0), // 2022-01-01 00:06:00
    (1640995620L, 22.0)  // 2022-01-01 00:07:00
  ]
  let bucket_size_seconds = 300L // 5分钟桶
  let time_buckets = []
  
  // 按时间桶聚合
  let mut i = 0
  while i < timestamped_metrics.length() {
    let (timestamp, value) = timestamped_metrics[i]
    let bucket_start = (timestamp / bucket_size_seconds) * bucket_size_seconds
    let bucket_end = bucket_start + bucket_size_seconds
    
    // 查找或创建桶
    let mut bucket_found = false
    let mut j = 0
    while j < time_buckets.length() {
      if time_buckets[j].0 == bucket_start {
        // 更新现有桶
        let (start, end, values) = time_buckets[j]
        let new_values = []
        let mut k = 0
        while k < values.length() {
          new_values.push(values[k])
          k = k + 1
        }
        new_values.push(value)
        time_buckets[j] = (start, end, new_values)
        bucket_found = true
        break
      }
      j = j + 1
    }
    
    // 创建新桶
    if !bucket_found {
      time_buckets.push((bucket_start, bucket_end, [value]))
    }
    i = i + 1
  }
  
  // 验证时间桶
  assert_eq(timestamped_metrics.length(), 8)
  assert_eq(time_buckets.length(), 2) // 8个点分成2个5分钟桶
  
  // 验证第一个桶 (00:00-00:05)
  assert_eq(time_buckets[0].0, 1640995200L)
  assert_eq(time_buckets[0].1, 1640995500L)
  assert_eq(time_buckets[0].2.length(), 5) // 前5个数据点
  
  // 验证第二个桶 (00:05-00:10)
  assert_eq(time_buckets[1].0, 1640995500L)
  assert_eq(time_buckets[1].1, 1640995800L)
  assert_eq(time_buckets[1].2.length(), 3) // 后3个数据点
}
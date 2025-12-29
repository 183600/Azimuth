// 遥测数据聚合测试用例
// 测试各种数据聚合场景和算法

test "telemetry_counter_aggregation" {
  // 测试计数器聚合
  
  let counter_values = [10, 15, 20, 25, 30]
  let mut sum = 0
  
  // 计算总和
  let mut i = 0
  while i < counter_values.length() {
    sum = sum + counter_values[i]
    i = i + 1
  }
  
  // 验证聚合结果
  assert_eq(sum, 100)
  assert_eq(counter_values.length(), 5)
  
  // 计算平均值
  let average = sum / counter_values.length()
  assert_eq(average, 20)
  
  // 验证最大值和最小值
  let mut max = counter_values[0]
  let mut min = counter_values[0]
  i = 1
  while i < counter_values.length() {
    if counter_values[i] > max {
      max = counter_values[i]
    }
    if counter_values[i] < min {
      min = counter_values[i]
    }
    i = i + 1
  }
  
  assert_eq(max, 30)
  assert_eq(min, 10)
}

test "telemetry_histogram_aggregation" {
  // 测试直方图聚合
  
  let bucket_bounds = [10.0, 50.0, 100.0, 500.0, 1000.0]
  let bucket_counts = [100, 80, 60, 40, 20, 10]
  let mut total_count = 0
  
  // 计算总计数
  let mut i = 0
  while i < bucket_counts.length() {
    total_count = total_count + bucket_counts[i]
    i = i + 1
  }
  
  // 验证直方图数据
  assert_eq(bucket_bounds.length(), 5)
  assert_eq(bucket_counts.length(), 6) // 包含+∞桶
  assert_eq(total_count, 310)
  
  // 验证桶边界
  assert_eq(bucket_bounds[0], 10.0)
  assert_eq(bucket_bounds[4], 1000.0)
  
  // 计算累积计数
  let mut cumulative_counts = []
  let mut cumulative = 0
  i = 0
  while i < bucket_counts.length() {
    cumulative = cumulative + bucket_counts[i]
    cumulative_counts.push(cumulative)
    i = i + 1
  }
  
  assert_eq(cumulative_counts[0], 100)
  assert_eq(cumulative_counts[5], 310)
}

test "telemetry_percentile_calculation" {
  // 测试百分位数计算
  
  let response_times = [10, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250]
  
  // 验证数据集
  assert_eq(response_times.length(), 11)
  assert_eq(response_times[0], 10)
  assert_eq(response_times[10], 250)
  
  // 计算中位数 (50th percentile)
  let median_index = response_times.length() / 2
  let median = response_times[median_index]
  assert_eq(median, 125)
  
  // 计算95th百分位数
  let p95_index = (response_times.length() * 95) / 100
  let p95_value = response_times[p95_index]
  assert_eq(p95_value, 250)
  
  // 计算99th百分位数
  let p99_index = (response_times.length() * 99) / 100
  let p99_value = response_times[p99_index]
  assert_eq(p99_value, 250)
}

test "telemetry_time_window_aggregation" {
  // 测试时间窗口聚合
  
  let time_windows = [
    ("2023-01-01T00:00:00Z", 100),
    ("2023-01-01T01:00:00Z", 150),
    ("2023-01-01T02:00:00Z", 120),
    ("2023-01-01T03:00:00Z", 180),
    ("2023-01-01T04:00:00Z", 90)
  ]
  
  // 验证时间窗口数据
  assert_eq(time_windows.length(), 5)
  assert_eq(time_windows[0].0, "2023-01-01T00:00:00Z")
  assert_eq(time_windows[0].1, 100)
  
  // 计算总计数
  let mut total_count = 0
  let mut i = 0
  while i < time_windows.length() {
    total_count = total_count + time_windows[i].1
    i = i + 1
  }
  
  assert_eq(total_count, 640)
  
  // 计算平均每小时计数
  let avg_per_hour = total_count / time_windows.length()
  assert_eq(avg_per_hour, 128)
  
  // 找出最大值和最小值的时间窗口
  let mut max_count = time_windows[0].1
  let mut min_count = time_windows[0].1
  let mut max_time = time_windows[0].0
  let mut min_time = time_windows[0].0
  
  i = 1
  while i < time_windows.length() {
    if time_windows[i].1 > max_count {
      max_count = time_windows[i].1
      max_time = time_windows[i].0
    }
    if time_windows[i].1 < min_count {
      min_count = time_windows[i].1
      min_time = time_windows[i].0
    }
    i = i + 1
  }
  
  assert_eq(max_count, 180)
  assert_eq(max_time, "2023-01-01T03:00:00Z")
  assert_eq(min_count, 90)
  assert_eq(min_time, "2023-01-01T04:00:00Z")
}

test "telemetry_dimension_grouping" {
  // 测试维度分组聚合
  
  let metric_data = [
    ("http_requests", "GET", "/api/users", 100),
    ("http_requests", "POST", "/api/users", 50),
    ("http_requests", "GET", "/api/products", 200),
    ("http_requests", "POST", "/api/products", 75),
    ("http_requests", "PUT", "/api/users", 25),
    ("http_requests", "DELETE", "/api/users", 10)
  ]
  
  // 按HTTP方法分组
  let mut get_count = 0
  let mut post_count = 0
  let mut put_count = 0
  let mut delete_count = 0
  
  let mut i = 0
  while i < metric_data.length() {
    let method = metric_data[i].1
    let count = metric_data[i].3
    
    if method == "GET" {
      get_count = get_count + count
    } else if method == "POST" {
      post_count = post_count + count
    } else if method == "PUT" {
      put_count = put_count + count
    } else if method == "DELETE" {
      delete_count = delete_count + count
    }
    
    i = i + 1
  }
  
  // 验证分组结果
  assert_eq(get_count, 300)
  assert_eq(post_count, 125)
  assert_eq(put_count, 25)
  assert_eq(delete_count, 10)
  
  // 按路径分组
  let mut users_count = 0
  let mut products_count = 0
  
  i = 0
  while i < metric_data.length() {
    let path = metric_data[i].2
    let count = metric_data[i].3
    
    if path == "/api/users" {
      users_count = users_count + count
    } else if path == "/api/products" {
      products_count = products_count + count
    }
    
    i = i + 1
  }
  
  // 验证路径分组结果
  assert_eq(users_count, 185)
  assert_eq(products_count, 275)
}

test "telemetry_rate_calculation" {
  // 测试速率计算
  
  let time_series_data = [
    (1000L, 100),    // 时间戳, 计数
    (2000L, 150),
    (3000L, 180),
    (4000L, 220),
    (5000L, 280)
  ]
  
  // 计算时间窗口内的速率
  let start_time = time_series_data[0].0
  let end_time = time_series_data[time_series_data.length() - 1].0
  let start_count = time_series_data[0].1
  let end_count = time_series_data[time_series_data.length() - 1].1
  
  let time_duration = end_time - start_time
  let count_diff = end_count - start_count
  
  // 计算每秒速率
  let rate_per_second = count_diff.to_double() / time_duration.to_double()
  
  // 验证速率计算
  assert_eq(time_duration, 4000L)
  assert_eq(count_diff, 180)
  assert_eq(rate_per_second > 0.04, true)
  assert_eq(rate_per_second < 0.05, true)
  
  // 计算每分钟速率
  let rate_per_minute = rate_per_second * 60.0
  assert_eq(rate_per_minute > 2.5, true)
  assert_eq(rate_per_minute < 3.0, true)
  
  // 计算增量速率
  let mut incremental_rates = []
  let mut i = 1
  while i < time_series_data.length() {
    let time_diff = time_series_data[i].0 - time_series_data[i-1].0
    let count_diff = time_series_data[i].1 - time_series_data[i-1].1
    let incremental_rate = count_diff.to_double() / time_diff.to_double()
    incremental_rates.push(incremental_rate)
    i = i + 1
  }
  
  // 验证增量速率
  assert_eq(incremental_rates.length(), 4)
  assert_eq(incremental_rates[0] > 0.04, true)  // 50/1000
  assert_eq(incremental_rates[1] > 0.02, true)  // 30/1000
  assert_eq(incremental_rates[2] > 0.03, true)  // 40/1000
  assert_eq(incremental_rates[3] > 0.05, true)  // 60/1000
}
// 遥测数据聚合测试用例
// 测试遥测数据的聚合、统计和汇总功能

test "telemetry_data_aggregation_counters" {
  // 测试计数器数据聚合
  
  let request_counts = [
    ("GET", 1500),
    ("POST", 800),
    ("PUT", 300),
    ("DELETE", 100),
    ("PATCH", 50)
  ]
  
  // 验证计数器数量
  assert_eq(request_counts.length(), 5)
  
  // 验证GET请求最多
  assert_eq(request_counts[0].0, "GET")
  assert_eq(request_counts[0].1, 1500)
  
  // 验证PATCH请求最少
  assert_eq(request_counts[4].0, "PATCH")
  assert_eq(request_counts[4].1, 50)
  
  // 计算总请求数
  let mut total_requests = 0
  let mut i = 0
  while i < request_counts.length() {
    total_requests = total_requests + request_counts[i].1
    i = i + 1
  }
  
  assert_eq(total_requests, 2750)
  
  // 计算平均请求数
  let average_requests = total_requests / request_counts.length()
  assert_eq(average_requests, 550)
  
  // 计算GET请求占比
  let get_percentage = (request_counts[0].1.to_double() / total_requests.to_double()) * 100.0
  assert_eq(get_percentage > 50.0, true)
  assert_eq(get_percentage < 60.0, true)
}

test "telemetry_data_aggregation_averages" {
  // 测试平均值数据聚合
  
  let response_times = [
    ("endpoint1", 120.5),
    ("endpoint2", 250.3),
    ("endpoint3", 80.2),
    ("endpoint4", 500.1),
    ("endpoint5", 95.7)
  ]
  
  // 验证响应时间数据数量
  assert_eq(response_times.length(), 5)
  
  // 验证最快响应时间
  let mut fastest_time = response_times[0].1
  let mut fastest_endpoint = response_times[0].0
  let mut i = 1
  while i < response_times.length() {
    if response_times[i].1 < fastest_time {
      fastest_time = response_times[i].1
      fastest_endpoint = response_times[i].0
    }
    i = i + 1
  }
  
  assert_eq(fastest_time, 80.2)
  assert_eq(fastest_endpoint, "endpoint3")
  
  // 验证最慢响应时间
  let mut slowest_time = response_times[0].1
  let mut slowest_endpoint = response_times[0].0
  i = 1
  while i < response_times.length() {
    if response_times[i].1 > slowest_time {
      slowest_time = response_times[i].1
      slowest_endpoint = response_times[i].0
    }
    i = i + 1
  }
  
  assert_eq(slowest_time, 500.1)
  assert_eq(slowest_endpoint, "endpoint4")
  
  // 计算平均响应时间
  let mut total_time = 0.0
  i = 0
  while i < response_times.length() {
    total_time = total_time + response_times[i].1
    i = i + 1
  }
  
  let average_time = total_time / response_times.length().to_double()
  assert_eq(average_time > 200.0, true)
  assert_eq(average_time < 250.0, true)
}

test "telemetry_data_aggregation_percentiles" {
  // 测试百分位数数据聚合
  
  let latencies = [
    10.5, 15.2, 20.1, 25.3, 30.7, 35.9, 40.2, 45.8, 50.1, 55.6,
    60.3, 65.7, 70.9, 75.2, 80.4, 85.8, 90.1, 95.5, 100.2, 150.7
  ]
  
  // 验证延迟数据数量
  assert_eq(latencies.length(), 20)
  
  // 验证最小延迟
  assert_eq(latencies[0], 10.5)
  
  // 验证最大延迟
  assert_eq(latencies[19], 150.7)
  
  // 计算中位数 (P50)
  let p50_index = latencies.length() / 2
  let p50 = latencies[p50_index]
  assert_eq(p50 > 50.0, true)
  assert_eq(p50 < 60.0, true)
  
  // 计算P95百分位数
  let p95_index = (latencies.length() * 95) / 100
  let p95 = latencies[p95_index]
  assert_eq(p95 > 90.0, true)
  assert_eq(p95 < 100.0, true)
  
  // 计算P99百分位数
  let p99_index = (latencies.length() * 99) / 100
  let p99 = latencies[p99_index]
  assert_eq(p99 > 95.0, true)
  assert_eq(p99 < 110.0, true)
}

test "telemetry_data_aggregation_time_series" {
  // 测试时间序列数据聚合
  
  let time_series_data = [
    (1640995200L, 100),  // 2022-01-01 00:00:00
    (1640995260L, 150),  // 2022-01-01 00:01:00
    (1640995320L, 120),  // 2022-01-01 00:02:00
    (1640995380L, 180),  // 2022-01-01 00:03:00
    (1640995440L, 200)   // 2022-01-01 00:04:00
  ]
  
  // 验证时间序列数据数量
  assert_eq(time_series_data.length(), 5)
  
  // 验证时间戳递增
  let mut i = 1
  while i < time_series_data.length() {
    assert_eq(time_series_data[i].0 > time_series_data[i-1].0, true)
    i = i + 1
  }
  
  // 验证时间间隔为60秒
  assert_eq(time_series_data[1].0 - time_series_data[0].0, 60L)
  assert_eq(time_series_data[4].0 - time_series_data[3].0, 60L)
  
  // 计算总请求数
  let mut total_requests = 0
  i = 0
  while i < time_series_data.length() {
    total_requests = total_requests + time_series_data[i].1
    i = i + 1
  }
  
  assert_eq(total_requests, 750)
  
  // 计算平均每分钟请求数
  let average_per_minute = total_requests / time_series_data.length()
  assert_eq(average_per_minute, 150)
  
  // 找出最大请求数的时间点
  let mut max_requests = time_series_data[0].1
  let mut max_timestamp = time_series_data[0].0
  i = 1
  while i < time_series_data.length() {
    if time_series_data[i].1 > max_requests {
      max_requests = time_series_data[i].1
      max_timestamp = time_series_data[i].0
    }
    i = i + 1
  }
  
  assert_eq(max_requests, 200)
  assert_eq(max_timestamp, 1640995440L)
}

test "telemetry_data_aggregation_histogram" {
  // 测试直方图数据聚合
  
  let response_time_buckets = [
    ("0-100ms", 450),
    ("100-200ms", 280),
    ("200-500ms", 150),
    ("500-1000ms", 80),
    ("1000ms+", 40)
  ]
  
  // 验证桶数量
  assert_eq(response_time_buckets.length(), 5)
  
  // 验证最快响应时间桶最多
  assert_eq(response_time_buckets[0].0, "0-100ms")
  assert_eq(response_time_buckets[0].1, 450)
  
  // 验证最慢响应时间桶最少
  assert_eq(response_time_buckets[4].0, "1000ms+")
  assert_eq(response_time_buckets[4].1, 40)
  
  // 计算总请求数
  let mut total_requests = 0
  let mut i = 0
  while i < response_time_buckets.length() {
    total_requests = total_requests + response_time_buckets[i].1
    i = i + 1
  }
  
  assert_eq(total_requests, 1000)
  
  // 计算小于100ms的请求百分比
  let under_100ms_percentage = (response_time_buckets[0].1.to_double() / total_requests.to_double()) * 100.0
  assert_eq(under_100ms_percentage > 40.0, true)
  assert_eq(under_100ms_percentage < 50.0, true)
  
  // 计算小于500ms的请求百分比
  let mut under_500ms_count = 0
  i = 0
  while i < 3 {  // 前三个桶都是小于500ms的
    under_500ms_count = under_500ms_count + response_time_buckets[i].1
    i = i + 1
  }
  
  let under_500ms_percentage = (under_500ms_count.to_double() / total_requests.to_double()) * 100.0
  assert_eq(under_500ms_percentage > 80.0, true)
  assert_eq(under_500ms_percentage < 90.0, true)
}

test "telemetry_data_aggregation_rate_calculations" {
  // 测试速率计算聚合
  
  let metrics_over_time = [
    (1640995200L, 1000),  // 时间戳，累计值
    (1640995260L, 1150),  // 1分钟后，增加了150
    (1640995320L, 1350),  // 2分钟后，增加了200
    (1640995380L, 1550),  // 3分钟后，增加了200
    (1640995440L, 1800)   // 4分钟后，增加了250
  ]
  
  // 验证数据点数量
  assert_eq(metrics_over_time.length(), 5)
  
  // 计算各时间段的速率
  let rates = []
  let mut i = 1
  while i < metrics_over_time.length() {
    let time_diff = metrics_over_time[i].0 - metrics_over_time[i-1].0
    let value_diff = metrics_over_time[i].1 - metrics_over_time[i-1].1
    let rate = value_diff.to_double() / time_diff.to_double()
    rates.push(rate)
    i = i + 1
  }
  
  // 验证速率数量
  assert_eq(rates.length(), 4)
  
  // 验证第一个时间段的速率 (150/60 = 2.5)
  assert_eq(rates[0] > 2.0, true)
  assert_eq(rates[0] < 3.0, true)
  
  // 验证最后一个时间段的速率 (250/60 ≈ 4.17)
  assert_eq(rates[3] > 4.0, true)
  assert_eq(rates[3] < 5.0, true)
  
  // 计算平均速率
  let mut total_rate = 0.0
  i = 0
  while i < rates.length() {
    total_rate = total_rate + rates[i]
    i = i + 1
  }
  
  let average_rate = total_rate / rates.length().to_double()
  assert_eq(average_rate > 2.5, true)
  assert_eq(average_rate < 3.5, true)
}

test "telemetry_data_aggregation_error_rates" {
  // 测试错误率聚合
  
  let status_code_counts = [
    ("200", 850),
    ("201", 100),
    ("400", 30),
    ("401", 15),
    ("404", 25),
    ("500", 10),
    ("502", 5)
  ]
  
  // 验证状态码数量
  assert_eq(status_code_counts.length(), 7)
  
  // 计算总请求数
  let mut total_requests = 0
  let mut i = 0
  while i < status_code_counts.length() {
    total_requests = total_requests + status_code_counts[i].1
    i = i + 1
  }
  
  assert_eq(total_requests, 1035)
  
  // 计算成功请求数 (2xx)
  let mut success_count = 0
  i = 0
  while i < 2 {  // 前两个是成功状态码
    success_count = success_count + status_code_counts[i].1
    i = i + 1
  }
  
  let success_rate = (success_count.to_double() / total_requests.to_double()) * 100.0
  assert_eq(success_rate > 90.0, true)
  assert_eq(success_rate < 95.0, true)
  
  // 计算客户端错误数 (4xx)
  let mut client_error_count = 0
  i = 2
  while i < 5 {  // 4xx状态码
    client_error_count = client_error_count + status_code_counts[i].1
    i = i + 1
  }
  
  let client_error_rate = (client_error_count.to_double() / total_requests.to_double()) * 100.0
  assert_eq(client_error_rate > 5.0, true)
  assert_eq(client_error_rate < 10.0, true)
  
  // 计算服务器错误数 (5xx)
  let mut server_error_count = 0
  i = 5
  while i < status_code_counts.length() {  // 5xx状态码
    server_error_count = server_error_count + status_code_counts[i].1
    i = i + 1
  }
  
  let server_error_rate = (server_error_count.to_double() / total_requests.to_double()) * 100.0
  assert_eq(server_error_rate > 1.0, true)
  assert_eq(server_error_rate < 2.0, true)
}

test "telemetry_data_aggregation_top_n" {
  // 测试Top N数据聚合
  
  let endpoint_metrics = [
    ("/api/v1/users", 1500),
    ("/api/v1/orders", 1200),
    ("/api/v1/products", 800),
    ("/api/v1/payments", 600),
    ("/api/v1/auth", 400),
    ("/api/v1/notifications", 300),
    ("/api/v1/reviews", 200),
    ("/api/v1/categories", 100)
  ]
  
  // 验证端点数量
  assert_eq(endpoint_metrics.length(), 8)
  
  // 找出Top 3端点
  let top_3 = []
  let mut i = 0
  while i < 3 {
    let mut max_index = 0
    let mut j = 1
    while j < endpoint_metrics.length() {
      if endpoint_metrics[j].1 > endpoint_metrics[max_index].1 {
        max_index = j
      }
      j = j + 1
    }
    top_3.push(endpoint_metrics[max_index])
    i = i + 1
  }
  
  // 验证Top 3
  assert_eq(top_3.length(), 3)
  assert_eq(top_3[0].0, "/api/v1/users")
  assert_eq(top_3[0].1, 1500)
  assert_eq(top_3[1].0, "/api/v1/orders")
  assert_eq(top_3[1].1, 1200)
  assert_eq(top_3[2].0, "/api/v1/products")
  assert_eq(top_3[2].1, 800)
  
  // 计算Top 3端点的总请求数
  let mut top_3_total = 0
  i = 0
  while i < top_3.length() {
    top_3_total = top_3_total + top_3[i].1
    i = i + 1
  }
  
  // 计算总请求数
  let mut total_requests = 0
  i = 0
  while i < endpoint_metrics.length() {
    total_requests = total_requests + endpoint_metrics[i].1
    i = i + 1
  }
  
  // 计算Top 3端点的请求占比
  let top_3_percentage = (top_3_total.to_double() / total_requests.to_double()) * 100.0
  assert_eq(top_3_percentage > 60.0, true)
  assert_eq(top_3_percentage < 70.0, true)
}

test "telemetry_data_aggregation_group_by" {
  // 测试分组聚合
  
  let request_data = [
    ("/api/v1/users", "GET", 800),
    ("/api/v1/users", "POST", 400),
    ("/api/v1/users", "PUT", 200),
    ("/api/v1/orders", "GET", 600),
    ("/api/v1/orders", "POST", 500),
    ("/api/v1/orders", "PUT", 100),
    ("/api/v1/products", "GET", 900),
    ("/api/v1/products", "POST", 100)
  ]
  
  // 验证请求数据数量
  assert_eq(request_data.length(), 8)
  
  // 按端点分组
  let endpoint_groups = [
    ("/api/v1/users", 1400),  // 800 + 400 + 200
    ("/api/v1/orders", 1200),  // 600 + 500 + 100
    ("/api/v1/products", 1000) // 900 + 100
  ]
  
  // 验证分组结果
  assert_eq(endpoint_groups.length(), 3)
  assert_eq(endpoint_groups[0].1, 1400)
  assert_eq(endpoint_groups[1].1, 1200)
  assert_eq(endpoint_groups[2].1, 1000)
  
  // 按HTTP方法分组
  let method_groups = [
    ("GET", 2300),  // 800 + 600 + 900
    ("POST", 1000), // 400 + 500 + 100
    ("PUT", 300)    // 200 + 100
  ]
  
  // 验证分组结果
  assert_eq(method_groups.length(), 3)
  assert_eq(method_groups[0].1, 2300)
  assert_eq(method_groups[1].1, 1000)
  assert_eq(method_groups[2].1, 300)
  
  // 验证GET请求最多
  assert_eq(method_groups[0].0, "GET")
  assert_eq(method_groups[0].1 > 2000, true)
  
  // 验证PUT请求最少
  assert_eq(method_groups[2].0, "PUT")
  assert_eq(method_groups[2].1 < 500, true)
}

test "telemetry_data_aggregation_sliding_window" {
  // 测试滑动窗口聚合
  
  let time_window_data = [
    (1640995200L, 100),  // t0
    (1640995260L, 150),  // t1
    (1640995320L, 120),  // t2
    (1640995380L, 180),  // t3
    (1640995440L, 200),  // t4
    (1640995500L, 160),  // t5
    (1640995560L, 140),  // t6
    (1640995620L, 170)   // t7
  ]
  
  // 验证时间窗口数据数量
  assert_eq(time_window_data.length(), 8)
  
  // 计算3分钟滑动窗口总和 (窗口大小为3个数据点)
  let sliding_sums = []
  let mut i = 2
  while i < time_window_data.length() {
    let window_sum = time_window_data[i].1 + time_window_data[i-1].1 + time_window_data[i-2].1
    sliding_sums.push(window_sum)
    i = i + 1
  }
  
  // 验证滑动窗口数量
  assert_eq(sliding_sums.length(), 6)
  
  // 验证第一个窗口 (t0 + t1 + t2)
  assert_eq(sliding_sums[0], 100 + 150 + 120)
  assert_eq(sliding_sums[0], 370)
  
  // 验证最后一个窗口 (t5 + t6 + t7)
  assert_eq(sliding_sums[5], 160 + 140 + 170)
  assert_eq(sliding_sums[5], 470)
  
  // 找出最大窗口总和
  let mut max_sum = sliding_sums[0]
  let mut max_index = 0
  i = 1
  while i < sliding_sums.length() {
    if sliding_sums[i] > max_sum {
      max_sum = sliding_sums[i]
      max_index = i
    }
    i = i + 1
  }
  
  assert_eq(max_sum, 470)
  assert_eq(max_index, 5)
  
  // 找出最小窗口总和
  let mut min_sum = sliding_sums[0]
  let mut min_index = 0
  i = 1
  while i < sliding_sums.length() {
    if sliding_sums[i] < min_sum {
      min_sum = sliding_sums[i]
      min_index = i
    }
    i = i + 1
  }
  
  assert_eq(min_sum, 370)
  assert_eq(min_index, 0)
}
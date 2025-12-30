// 遥测数据聚合测试用例

test "telemetry_counter_aggregation" {
  // 测试遥测计数器聚合
  
  let metric_values = [10, 15, 20, 25, 30, 35, 40, 45, 50]
  let metric_name = "http_requests_total"
  
  // 验证输入数据
  assert_eq(metric_values.length(), 9)
  assert_eq(metric_values[0], 10)
  assert_eq(metric_values[8], 50)
  
  // 计算总和
  let mut sum = 0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  
  // 验证总和
  assert_eq(sum, 270) // 10+15+20+25+30+35+40+45+50 = 270
  
  // 计算平均值
  let average = sum.to_double() / metric_values.length().to_double()
  assert_eq(average, 30.0)
  
  // 创建聚合结果
  let aggregation_result = metric_name + ":count=" + metric_values.length().to_string() + 
                          ",sum=" + sum.to_string() + 
                          ",avg=" + average.to_string()
  
  // 验证聚合结果
  assert_eq(aggregation_result.contains("http_requests_total:count=9"), true)
  assert_eq(aggregation_result.contains("sum=270"), true)
  assert_eq(aggregation_result.contains("avg=30"), true)
}

test "telemetry_histogram_aggregation" {
  // 测试遥测直方图聚合
  
  let response_times = [10.5, 25.3, 45.8, 78.2, 120.5, 200.1, 350.7, 500.2, 750.9, 1000.0]
  let buckets = [10.0, 50.0, 100.0, 500.0, 1000.0, 5000.0]
  
  // 验证输入数据
  assert_eq(response_times.length(), 10)
  assert_eq(buckets.length(), 6)
  assert_eq(response_times[0] < response_times[9], true)
  
  // 计算每个bucket的计数
  let bucket_counts = []
  let mut i = 0
  while i < buckets.length() {
    let mut count = 0
    let mut j = 0
    while j < response_times.length() {
      if response_times[j] <= buckets[i] {
        count = count + 1
      }
      j = j + 1
    }
    bucket_counts.push(count)
    i = i + 1
  }
  
  // 验证bucket计数
  assert_eq(bucket_counts.length(), 6)
  assert_eq(bucket_counts[0], 0) // <=10.0的响应时间
  assert_eq(bucket_counts[1], 3) // <=50.0的响应时间 (10.5, 25.3, 45.8)
  assert_eq(bucket_counts[2], 5) // <=100.0的响应时间
  assert_eq(bucket_counts[3], 8) // <=500.0的响应时间
  assert_eq(bucket_counts[4], 9) // <=1000.0的响应时间
  assert_eq(bucket_counts[5], 10) // <=5000.0的响应时间
  
  // 计算百分位数
  let sorted_times = response_times // 假设已排序
  let p50_index = (sorted_times.length() * 50) / 100
  let p95_index = (sorted_times.length() * 95) / 100
  let p99_index = (sorted_times.length() * 99) / 100
  
  // 验证百分位数索引
  assert_eq(p50_index, 5) // 第50百分位数
  assert_eq(p95_index, 9) // 第95百分位数
  assert_eq(p99_index, 9) // 第99百分位数（取整）
  
  // 创建直方图聚合结果
  let histogram_result = "response_time_histogram:"
  i = 0
  while i < buckets.length() {
    histogram_result = histogram_result + "le_" + buckets[i].to_string() + "=" + bucket_counts[i].to_string()
    if i < buckets.length() - 1 {
      histogram_result = histogram_result + ","
    }
    i = i + 1
  }
  
  // 验证直方图结果
  assert_eq(histogram_result.contains("response_time_histogram:"), true)
  assert_eq(histogram_result.contains("le_10.0=0"), true)
  assert_eq(histogram_result.contains("le_50.0=3"), true)
  assert_eq(histogram_result.contains("le_100.0=5"), true)
  assert_eq(histogram_result.contains("le_5000.0=10"), true)
}

test "telemetry_time_series_aggregation" {
  // 测试时间序列聚合
  
  let time_series_data = [
    (1640995200L, 100.5), // 2022-01-01 00:00:00
    (1640995260L, 105.2), // 2022-01-01 00:01:00
    (1640995320L, 98.7),  // 2022-01-01 00:02:00
    (1640995380L, 110.3), // 2022-01-01 00:03:00
    (1640995440L, 115.8)  // 2022-01-01 00:04:00
  ]
  
  // 验证时间序列数据
  assert_eq(time_series_data.length(), 5)
  assert_eq(time_series_data[0].0, 1640995200L)
  assert_eq(time_series_data[4].1, 115.8)
  
  // 验证时间间隔（60秒）
  let mut i = 1
  while i < time_series_data.length() {
    let time_diff = time_series_data[i].0 - time_series_data[i-1].0
    assert_eq(time_diff, 60L)
    i = i + 1
  }
  
  // 计算移动平均（窗口大小为3）
  let window_size = 3
  let moving_averages = []
  
  i = window_size - 1
  while i < time_series_data.length() {
    let mut sum = 0.0
    let mut j = i - window_size + 1
    while j <= i {
      sum = sum + time_series_data[j].1
      j = j + 1
    }
    let avg = sum / window_size.to_double()
    moving_averages.push((time_series_data[i].0, avg))
    i = i + 1
  }
  
  // 验证移动平均
  assert_eq(moving_averages.length(), 3) // 5-3+1 = 3
  assert_eq(moving_averages[0].0, 1640995320L) // 第3个点的时间戳
  assert_eq(moving_averages[0].1, (100.5 + 105.2 + 98.7) / 3.0) // 前3个点的平均
  
  // 计算趋势（简单线性趋势）
  let first_value = time_series_data[0].1
  let last_value = time_series_data[time_series_data.length() - 1].1
  let trend = last_value - first_value
  let trend_percentage = (trend / first_value) * 100.0
  
  // 验证趋势计算
  assert_eq(trend > 0.0, true) // 上升趋势
  assert_eq(trend_percentage > 10.0, true) // 增长超过10%
  
  // 创建时间序列聚合报告
  let aggregation_report = "time_series:points=" + time_series_data.length().to_string() +
                          ",trend=" + trend.to_string() + 
                          ",trend_percent=" + trend_percentage.to_string() + "%" +
                          ",moving_avg_count=" + moving_averages.length().to_string()
  
  // 验证聚合报告
  assert_eq(aggregation_report.contains("time_series:points=5"), true)
  assert_eq(aggregation_report.contains("trend="), true)
  assert_eq(aggregation_report.contains("trend_percent="), true)
  assert_eq(aggregation_report.contains("moving_avg_count=3"), true)
}
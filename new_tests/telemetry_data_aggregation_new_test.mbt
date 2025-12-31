// 遥测数据聚合测试用例

test "metric_sum_aggregation" {
  // 测试指标求和聚合
  
  let metric_values = [10, 20, 30, 40, 50]
  let mut sum_result = 0
  
  // 计算总和
  let mut i = 0
  while i < metric_values.length() {
    sum_result = sum_result + metric_values[i]
    i = i + 1
  }
  
  // 验证聚合结果
  assert_eq(sum_result, 150)
  assert_eq(metric_values.length(), 5)
  assert_eq(metric_values[0], 10)
  assert_eq(metric_values[4], 50)
  
  // 验证平均值
  let avg_result = sum_result / metric_values.length()
  assert_eq(avg_result, 30)
}

test "metric_avg_aggregation" {
  // 测试指标平均值聚合
  
  let response_times = [100, 150, 200, 120, 180, 160, 140]
  let mut total_time = 0
  
  // 计算总时间
  let mut i = 0
  while i < response_times.length() {
    total_time = total_time + response_times[i]
    i = i + 1
  }
  
  // 计算平均响应时间
  let avg_response_time = total_time / response_times.length()
  assert_eq(avg_response_time, 150)
  
  // 验证响应时间范围
  assert_eq(response_times[0], 100)
  assert_eq(response_times[2], 200)
  assert_eq(avg_response_time > response_times[0], true)
  assert_eq(avg_response_time < response_times[2], true)
}

test "histogram_aggregation" {
  // 测试直方图聚合
  
  let histogram_buckets = [
    (0.0, 10.0, 5),
    (10.0, 50.0, 15),
    (50.0, 100.0, 25),
    (100.0, 500.0, 35),
    (500.0, 1000.0, 20)
  ]
  
  // 验证桶数量
  assert_eq(histogram_buckets.length(), 5)
  
  // 验证桶边界
  assert_eq(histogram_buckets[0].0, 0.0)
  assert_eq(histogram_buckets[0].1, 10.0)
  assert_eq(histogram_buckets[4].0, 500.0)
  assert_eq(histogram_buckets[4].1, 1000.0)
  
  // 验证桶计数
  let mut total_count = 0
  let mut i = 0
  while i < histogram_buckets.length() {
    total_count = total_count + histogram_buckets[i].2
    i = i + 1
  }
  assert_eq(total_count, 100)
  
  // 验证最大值桶
  assert_eq(histogram_buckets[3].2, 35)  // 100-500范围的计数最多
}

test "percentile_calculation" {
  // 测试百分位数计算
  
  let latency_values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
  
  // 验证数据集
  assert_eq(latency_values.length(), 10)
  assert_eq(latency_values[0], 10)
  assert_eq(latency_values[9], 100)
  
  // 计算50百分位数（中位数）
  let p50_index = latency_values.length() / 2
  let p50_value = latency_values[p50_index]
  assert_eq(p50_value, 60)
  
  // 计算95百分位数
  let p95_index = (latency_values.length() * 95) / 100 - 1
  let p95_value = latency_values[p95_index]
  assert_eq(p95_value, 90)
  
  // 计算99百分位数
  let p99_index = (latency_values.length() * 99) / 100 - 1
  let p99_value = latency_values[p99_index]
  assert_eq(p99_value, 100)
}

test "time_window_aggregation" {
  // 测试时间窗口聚合
  
  let window_size_ms = 60000  // 1分钟窗口
  let current_time = 1640995200000  // 2022-01-01 00:00:00 UTC
  let window_start = current_time - window_size_ms
  
  // 验证时间窗口
  assert_eq(window_size_ms, 60000)
  assert_eq(current_time > window_start, true)
  assert_eq(current_time - window_start, window_size_ms)
  
  // 模拟窗口内的数据点
  let data_points = [
    (window_start + 1000, 10),
    (window_start + 15000, 20),
    (window_start + 30000, 15),
    (window_start + 45000, 25),
    (window_start + 59000, 30)
  ]
  
  // 验证数据点都在窗口内
  let mut i = 0
  while i < data_points.length() {
    let point_time = data_points[i].0
    assert_eq(point_time >= window_start, true)
    assert_eq(point_time <= current_time, true)
    i = i + 1
  }
  
  // 验证数据点数量
  assert_eq(data_points.length(), 5)
  
  // 计算窗口内的总和
  let mut window_sum = 0
  i = 0
  while i < data_points.length() {
    window_sum = window_sum + data_points[i].1
    i = i + 1
  }
  assert_eq(window_sum, 100)
}
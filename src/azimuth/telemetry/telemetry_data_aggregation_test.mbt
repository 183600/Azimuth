// 遥测数据聚合测试用例
// 测试各种遥测数据的聚合功能

test "telemetry_counter_aggregation" {
  // 测试计数器聚合
  
  let counter_values = [100L, 150L, 200L, 175L, 120L, 180L, 160L, 140L, 190L, 165L]
  
  // 计算总和
  let mut sum = 0L
  let mut i = 0
  while i < counter_values.length() {
    sum = sum + counter_values[i]
    i = i + 1
  }
  
  // 计算平均值
  let average = sum / counter_values.length().to_int64()
  
  // 计算最大值和最小值
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
  
  // 验证聚合结果
  assert_eq(counter_values.length(), 10)
  assert_eq(sum, 1680L)
  assert_eq(average, 168L)
  assert_eq(max, 200L)
  assert_eq(min, 100L)
}

test "telemetry_gauge_aggregation" {
  // 测试仪表盘数据聚合
  
  let gauge_readings = [45.2, 48.7, 42.1, 50.3, 46.8, 44.5, 49.2, 47.6, 43.9, 48.1]
  
  // 计算总和
  let mut sum = 0.0
  let mut i = 0
  while i < gauge_readings.length() {
    sum = sum + gauge_readings[i]
    i = i + 1
  }
  
  // 计算平均值
  let average = sum / gauge_readings.length().to_double()
  
  // 计算最大值和最小值
  let mut max = gauge_readings[0]
  let mut min = gauge_readings[0]
  i = 1
  while i < gauge_readings.length() {
    if gauge_readings[i] > max {
      max = gauge_readings[i]
    }
    if gauge_readings[i] < min {
      min = gauge_readings[i]
    }
    i = i + 1
  }
  
  // 验证聚合结果
  assert_eq(gauge_readings.length(), 10)
  assert_eq(sum > 460.0, true)
  assert_eq(sum < 470.0, true)
  assert_eq(average > 46.0, true)
  assert_eq(average < 47.0, true)
  assert_eq(max, 50.3)
  assert_eq(min, 42.1)
}

test "telemetry_histogram_aggregation" {
  // 测试直方图聚合
  
  let histogram_buckets = [
    (0.0, 10),
    (10.0, 25),
    (25.0, 45),
    (50.0, 60),
    (75.0, 70),
    (100.0, 75)
  ]
  
  let sample_values = [5.2, 12.8, 28.3, 52.1, 78.9, 105.6, 15.4, 35.7, 65.2, 88.3]
  
  // 计算每个桶的计数
  let bucket_counts = [0, 0, 0, 0, 0, 0]
  let mut i = 0
  while i < sample_values.length() {
    let value = sample_values[i]
    let mut j = 0
    while j < histogram_buckets.length() {
      if value <= histogram_buckets[j].0 {
        bucket_counts[j] = bucket_counts[j] + 1
        break
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证直方图聚合结果
  assert_eq(bucket_counts[0], 1)  // <= 0.0
  assert_eq(bucket_counts[1], 2)  // <= 10.0
  assert_eq(bucket_counts[2], 2)  // <= 25.0
  assert_eq(bucket_counts[3], 1)  // <= 50.0
  assert_eq(bucket_counts[4], 2)  // <= 75.0
  assert_eq(bucket_counts[5], 2)  // <= 100.0
}

test "telemetry_percentile_calculation" {
  // 测试百分位数计算
  
  let response_times = [12, 15, 18, 20, 22, 25, 28, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90]
  
  // 验证数据集
  assert_eq(response_times.length(), 20)
  
  // 计算P50 (中位数)
  let p50_index = response_times.length() / 2
  let p50 = response_times[p50_index]
  
  // 计算P90
  let p90_index = (response_times.length() * 90) / 100
  let p90 = response_times[p90_index]
  
  // 计算P95
  let p95_index = (response_times.length() * 95) / 100
  let p95 = response_times[p95_index]
  
  // 计算P99
  let p99_index = (response_times.length() * 99) / 100
  let p99 = response_times[p99_index]
  
  // 验证百分位数
  assert_eq(p50, 30)
  assert_eq(p90, 80)
  assert_eq(p95, 85)
  assert_eq(p99, 90)
}

test "telemetry_rate_calculation" {
  // 测试速率计算
  
  let timestamps = [1000L, 1050L, 1100L, 1150L, 1200L, 1250L, 1300L, 1350L, 1400L, 1450L]
  let event_counts = [10, 15, 12, 18, 20, 14, 16, 22, 19, 25]
  
  // 计算时间窗口
  let time_window = timestamps[timestamps.length() - 1] - timestamps[0]
  
  // 计算总事件数
  let mut total_events = 0
  let mut i = 0
  while i < event_counts.length() {
    total_events = total_events + event_counts[i]
    i = i + 1
  }
  
  // 计算事件速率
  let event_rate = total_events.to_double() / time_window.to_double() * 1000.0
  
  // 验证速率计算
  assert_eq(timestamps.length(), 10)
  assert_eq(event_counts.length(), 10)
  assert_eq(time_window, 450L)
  assert_eq(total_events, 171)
  assert_eq(event_rate > 350.0, true)
  assert_eq(event_rate < 400.0, true)
}

test "telemetry_time_bucket_aggregation" {
  // 测试时间桶聚合
  
  let events_with_timestamps = [
    (1000L, "request"),
    (1015L, "request"),
    (1030L, "response"),
    (1045L, "request"),
    (1060L, "error"),
    (1075L, "request"),
    (1090L, "response"),
    (1105L, "request"),
    (1120L, "response"),
    (1135L, "error")
  ]
  
  // 按时间桶分组 (50ms桶)
  let bucket_size = 50L
  let bucket_counts = [0, 0, 0, 0]  // 4个桶
  
  let mut i = 0
  while i < events_with_timestamps.length() {
    let timestamp = events_with_timestamps[i].0
    let bucket_index = (timestamp - 1000L) / bucket_size
    if bucket_index >= 0 && bucket_index < bucket_counts.length().to_int64() {
      bucket_counts[bucket_index.to_int()] = bucket_counts[bucket_index.to_int()] + 1
    }
    i = i + 1
  }
  
  // 验证时间桶聚合
  assert_eq(bucket_counts[0], 3)  // 1000-1050ms
  assert_eq(bucket_counts[1], 3)  // 1050-1100ms
  assert_eq(bucket_counts[2], 3)  // 1100-1150ms
  assert_eq(bucket_counts[3], 1)  // 1150-1200ms
}

test "telemetry_attribute_aggregation" {
  // 测试属性聚合
  
  let events_with_attributes = [
    ("GET", 200, 120),
    ("POST", 201, 250),
    ("GET", 404, 80),
    ("PUT", 200, 180),
    ("GET", 500, 300),
    ("POST", 400, 150),
    ("GET", 200, 110),
    ("DELETE", 204, 90),
    ("GET", 200, 130),
    ("POST", 200, 220)
  ]
  
  // 按HTTP方法分组
  let method_counts = [("GET", 0), ("POST", 0), ("PUT", 0), ("DELETE", 0)]
  let mut i = 0
  while i < events_with_attributes.length() {
    let method = events_with_attributes[i].0
    let mut j = 0
    while j < method_counts.length() {
      if method == method_counts[j].0 {
        method_counts[j] = (method_counts[j].0, method_counts[j].1 + 1)
        break
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 按状态码分组
  let status_counts = [(200, 0), (201, 0), (204, 0), (400, 0), (404, 0), (500, 0)]
  i = 0
  while i < events_with_attributes.length() {
    let status = events_with_attributes[i].1
    let mut j = 0
    while j < status_counts.length() {
      if status == status_counts[j].0 {
        status_counts[j] = (status_counts[j].0, status_counts[j].1 + 1)
        break
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证属性聚合
  assert_eq(method_counts[0].1, 5)  // GET: 5
  assert_eq(method_counts[1].1, 3)  // POST: 3
  assert_eq(method_counts[2].1, 1)  // PUT: 1
  assert_eq(method_counts[3].1, 1)  // DELETE: 1
  
  assert_eq(status_counts[0].1, 4)  // 200: 4
  assert_eq(status_counts[1].1, 1)  // 201: 1
  assert_eq(status_counts[2].1, 1)  // 204: 1
  assert_eq(status_counts[3].1, 1)  // 400: 1
  assert_eq(status_counts[4].1, 1)  // 404: 1
  assert_eq(status_counts[5].1, 1)  // 500: 1
}
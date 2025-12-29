// 遥测数据时间窗口分析测试用例

test "telemetry_sliding_window_average" {
  // 测试滑动窗口平均值计算
  
  let metrics = [
    (1000, 10.5),   // (timestamp, value)
    (2000, 15.2),
    (3000, 12.8),
    (4000, 18.3),
    (5000, 14.7),
    (6000, 20.1),
    (7000, 16.9),
    (8000, 22.4)
  ]
  
  let window_size = 3000  // 3秒窗口
  
  // 计算滑动窗口平均值
  let mut window_averages = []
  let mut i = 0
  while i < metrics.length() {
    let current_time = metrics[i].0
    let window_start = current_time - window_size
    
    let mut sum = 0.0
    let mut count = 0
    
    let mut j = 0
    while j < metrics.length() {
      if metrics[j].0 >= window_start && metrics[j].0 <= current_time {
        sum = sum + metrics[j].1
        count = count + 1
      }
      j = j + 1
    }
    
    if count > 0 {
      window_averages = window_averages.push(sum / count.to_double())
    }
    i = i + 1
  }
  
  // 验证窗口平均值
  assert_eq(window_averages.length() > 0, true)
  assert_eq(window_averages[0] > 10.0 && window_averages[0] < 11.0, true)  // 第一个窗口
  assert_eq(window_averages[window_averages.length() - 1] > 18.0 && window_averages[window_averages.length() - 1] < 23.0, true)  // 最后一个窗口
}

test "telemetry_time_bucket_aggregation" {
  // 测试时间桶聚合
  
  let events = [
    (100, "request_start"),
    (250, "request_end"),
    (350, "request_start"),
    (450, "request_end"),
    (650, "request_start"),
    (750, "request_end"),
    (850, "request_start"),
    (950, "request_end")
  ]
  
  let bucket_size = 200  // 200ms桶
  
  // 创建时间桶
  let mut buckets = []
  let mut bucket_time = 0
  while bucket_time < 1000 {
    buckets = buckets.push((bucket_time, 0, 0))  // (start_time, start_count, end_count)
    bucket_time = bucket_time + bucket_size
  }
  
  // 填充桶数据
  let mut i = 0
  while i < events.length() {
    let event_time = events[i].0
    let event_type = events[i].1
    
    // 找到对应的桶
    let mut bucket_index = event_time / bucket_size
    bucket_index = bucket_index * bucket_size  // 对齐到桶开始时间
    
    let mut j = 0
    while j < buckets.length() {
      if buckets[j].0 == bucket_index {
        let (start_time, start_count, end_count) = buckets[j]
        if event_type == "request_start" {
          buckets[j] = (start_time, start_count + 1, end_count)
        } else if event_type == "request_end" {
          buckets[j] = (start_time, start_count, end_count + 1)
        }
        break
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证桶聚合结果
  let mut total_starts = 0
  let mut total_ends = 0
  let mut i = 0
  while i < buckets.length() {
    total_starts = total_starts + buckets[i].1
    total_ends = total_ends + buckets[i].2
    i = i + 1
  }
  
  assert_eq(total_starts, 4)  // 应该有4个开始事件
  assert_eq(total_ends, 4)    // 应该有4个结束事件
}

test "telemetry_time_series_gap_detection" {
  // 测试时间序列间隙检测
  
  let time_series = [
    (1000, 10.5),
    (2000, 12.3),
    (4000, 11.8),  // 间隙：缺少3000
    (5000, 13.2),
    (8000, 14.1),  // 间隙：缺少6000, 7000
    (9000, 12.9),
    (12000, 15.3)  // 间隙：缺少10000, 11000
  ]
  
  let expected_interval = 1000  // 预期间隔1秒
  let gap_threshold = 1.5      // 间隙阈值：1.5倍预期间隔
  
  // 检测间隙
  let mut gaps = []
  let mut i = 1
  while i < time_series.length() {
    let prev_time = time_series[i - 1].0
    let curr_time = time_series[i].0
    let interval = curr_time - prev_time
    
    if interval > expected_interval * gap_threshold.to_int() {
      gaps = gaps.push((prev_time, curr_time, interval))
    }
    i = i + 1
  }
  
  // 验证间隙检测结果
  assert_eq(gaps.length(), 3)  // 应该检测到3个间隙
  
  // 验证第一个间隙 (3000缺失)
  assert_eq(gaps[0].0, 2000)
  assert_eq(gaps[0].1, 4000)
  assert_eq(gaps[0].2, 2000)
  
  // 验证第二个间隙 (6000, 7000缺失)
  assert_eq(gaps[1].0, 5000)
  assert_eq(gaps[1].1, 8000)
  assert_eq(gaps[1].2, 3000)
  
  // 验证第三个间隙 (10000, 11000缺失)
  assert_eq(gaps[2].0, 9000)
  assert_eq(gaps[2].1, 12000)
  assert_eq(gaps[2].2, 3000)
}

test "telemetry_time_weighted_average" {
  // 测试时间加权平均值
  
  let samples = [
    (0, 10.0),    // 开始时刻
    (100, 15.0),  // 100ms时
    (200, 12.0),  // 200ms时
    (300, 18.0),  // 300ms时
    (400, 14.0)   // 400ms时结束
  ]
  
  let total_duration = 400  // 总时长400ms
  
  // 计算时间加权平均值
  let mut weighted_sum = 0.0
  let mut i = 1
  while i < samples.length() {
    let prev_time = samples[i - 1].0
    let curr_time = samples[i].0
    let prev_value = samples[i - 1].1
    let duration = curr_time - prev_time
    
    weighted_sum = weighted_sum + prev_value.to_double() * duration.to_double()
    i = i + 1
  }
  
  let time_weighted_avg = weighted_sum / total_duration.to_double()
  
  // 验证时间加权平均值
  assert_eq(time_weighted_avg > 13.0 && time_weighted_avg < 14.0, true)
  
  // 与简单平均值比较
  let mut simple_sum = 0.0
  let mut i = 0
  while i < samples.length() {
    simple_sum = simple_sum + samples[i].1
    i = i + 1
  }
  let simple_avg = simple_sum / samples.length().to_double()
  
  // 时间加权平均值应该考虑时间因素
  assert_eq(time_weighted_avg != simple_avg, true)
}
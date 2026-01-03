// 遥测指标聚合测试用例
// 测试遥测系统的指标聚合功能

test "计数器指标聚合" {
  // 测试多个计数器值的累加
  let counter_values = [10, 15, 20, 25, 30]
  let mut sum = 0
  for value in counter_values {
    sum = sum + value
  }
  assert_eq(sum, 100)
  
  // 测试计数器增量聚合
  let mut counter = 0
  let increments = [1, 2, 3, 4, 5]
  for inc in increments {
    counter = counter + inc
  }
  assert_eq(counter, 15)
  
  // 测试计数器重置和增量
  let mut reset_counter = 50
  reset_counter = 0 // 重置
  let new_increments = [10, 20, 30]
  for inc in new_increments {
    reset_counter = reset_counter + inc
  }
  assert_eq(reset_counter, 60)
}

test "仪表盘指标聚合" {
  // 测试最新值聚合
  let gauge_values = [10.5, 15.2, 8.7, 12.3, 9.8]
  let latest_value = gauge_values[gauge_values.length() - 1]
  assert_eq(latest_value, 9.8)
  
  // 测试最大值聚合
  let mut max_value = gauge_values[0]
  for value in gauge_values {
    if value > max_value {
      max_value = value
    }
  }
  assert_eq(max_value, 15.2)
  
  // 测试最小值聚合
  let mut min_value = gauge_values[0]
  for value in gauge_values {
    if value < min_value {
      min_value = value
    }
  }
  assert_eq(min_value, 8.7)
  
  // 测试平均值聚合
  let mut sum = 0.0
  for value in gauge_values {
    sum = sum + value
  }
  let average = sum / gauge_values.length()
  assert_eq(average > 11.0, true) // 56.5 / 5 = 11.3
}

test "直方图指标聚合" {
  // 测试直方图桶计数
  let histogram_buckets = [0.1, 0.5, 1.0, 2.5, 5.0, 10.0]
  let observations = [0.05, 0.3, 0.7, 1.5, 3.0, 8.0]
  let mut bucket_counts = [0, 0, 0, 0, 0, 0, 0] // 包括+Inf桶
  
  for obs in observations {
    let mut bucket_index = 0
    for i in 0..histogram_buckets.length() {
      if obs <= histogram_buckets[i] {
        bucket_index = i + 1
        break
      }
    }
    if bucket_index == 0 {
      bucket_index = histogram_buckets.length() // +Inf桶
    }
    bucket_counts[bucket_index] = bucket_counts[bucket_index] + 1
  }
  
  assert_eq(bucket_counts[1], 1) // <= 0.1
  assert_eq(bucket_counts[2], 1) // <= 0.5
  assert_eq(bucket_counts[3], 1) // <= 1.0
  assert_eq(bucket_counts[4], 1) // <= 2.5
  assert_eq(bucket_counts[5], 1) // <= 5.0
  assert_eq(bucket_counts[6], 1) // <= 10.0
  
  // 测试总计数
  let mut total_count = 0
  for count in bucket_counts {
    total_count = total_count + count
  }
  assert_eq(total_count, observations.length())
  
  // 测试总和
  let mut sum = 0.0
  for obs in observations {
    sum = sum + obs
  }
  assert_eq(sum > 13.0, true) // 0.05 + 0.3 + 0.7 + 1.5 + 3.0 + 8.0 = 13.55
}

test "时间序列数据聚合" {
  // 测试按时间窗口聚合
  let time_series_data = [
    (1640995200, 10), // 2022-01-01 00:00:00
    (1640995260, 15), // 2022-01-01 00:01:00
    (1640995320, 12), // 2022-01-01 00:02:00
    (1640995380, 18), // 2022-01-01 00:03:00
    (1640995440, 20)  // 2022-01-01 00:04:00
  ]
  
  // 按分钟聚合
  let window_size = 300 // 5分钟窗口
  let start_time = 1640995200
  let end_time = 1640995500
  
  let mut window_sum = 0
  let mut window_count = 0
  for (timestamp, value) in time_series_data {
    if timestamp >= start_time && timestamp < end_time {
      window_sum = window_sum + value
      window_count = window_count + 1
    }
  }
  
  assert_eq(window_count, 5)
  assert_eq(window_sum, 75)
  
  // 测试滑动窗口聚合
  let sliding_window_size = 2
  for i in 0..(time_series_data.length() - sliding_window_size + 1) {
    let mut window_sum = 0
    for j in i..(i + sliding_window_size) {
      window_sum = window_sum + time_series_data[j].1
    }
    assert_eq(window_sum > 0, true)
  }
}

test "多维指标聚合" {
  // 测试按标签分组聚合
  let labeled_metrics = [
    ("serviceA", "endpoint1", 10),
    ("serviceA", "endpoint2", 15),
    ("serviceB", "endpoint1", 20),
    ("serviceB", "endpoint2", 25),
    ("serviceA", "endpoint1", 12) // 重复标签
  ]
  
  // 按service分组聚合
  let mut service_a_total = 0
  let mut service_b_total = 0
  for (service, _, value) in labeled_metrics {
    if service == "serviceA" {
      service_a_total = service_a_total + value
    } else if service == "serviceB" {
      service_b_total = service_b_total + value
    }
  }
  
  assert_eq(service_a_total, 37) // 10 + 15 + 12
  assert_eq(service_b_total, 45) // 20 + 25
  
  // 按endpoint分组聚合
  let mut endpoint1_total = 0
  let mut endpoint2_total = 0
  for (_, endpoint, value) in labeled_metrics {
    if endpoint == "endpoint1" {
      endpoint1_total = endpoint1_total + value
    } else if endpoint == "endpoint2" {
      endpoint2_total = endpoint2_total + value
    }
  }
  
  assert_eq(endpoint1_total, 42) // 10 + 20 + 12
  assert_eq(endpoint2_total, 40) // 15 + 25
}

test "分布式聚合" {
  // 测试多个数据源的聚合
  let node_metrics = [
    ("node1", 100),
    ("node2", 150),
    ("node3", 120),
    ("node4", 80)
  ]
  
  // 计算集群总和
  let mut cluster_total = 0
  for (_, value) in node_metrics {
    cluster_total = cluster_total + value
  }
  assert_eq(cluster_total, 450)
  
  // 计算集群平均值
  let cluster_average = cluster_total / node_metrics.length()
  assert_eq(cluster_average, 112)
  
  // 找出最大和最小值节点
  let mut max_value = node_metrics[0].1
  let mut min_value = node_metrics[0].1
  let mut max_node = node_metrics[0].0
  let mut min_node = node_metrics[0].0
  
  for (node, value) in node_metrics {
    if value > max_value {
      max_value = value
      max_node = node
    }
    if value < min_value {
      min_value = value
      min_node = node
    }
  }
  
  assert_eq(max_node, "node2")
  assert_eq(max_value, 150)
  assert_eq(min_node, "node4")
  assert_eq(min_value, 80)
}
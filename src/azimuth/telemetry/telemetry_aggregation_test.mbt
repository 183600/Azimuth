// 遥测数据聚合测试用例

test "telemetry_metric_aggregation_sum" {
  // 测试指标求和聚合
  
  let metric_values = [10.5, 20.3, 15.7, 8.2, 12.1]
  let metric_name = "request_duration"
  let mut sum_value = 0.0
  
  // 计算总和
  let mut i = 0
  while i < metric_values.length() {
    sum_value = sum_value + metric_values[i]
    i = i + 1
  }
  
  // 验证总和
  assert_eq(sum_value > 60.0, true)
  assert_eq(sum_value < 70.0, true)
  assert_eq(sum_value, 66.8)
  
  // 创建聚合结果
  let aggregation_result = metric_name + "_sum=" + sum_value.to_string()
  assert_eq(aggregation_result.contains("request_duration_sum"), true)
  assert_eq(aggregation_result.contains("66.8"), true)
}

test "telemetry_metric_aggregation_average" {
  // 测试指标平均值聚合
  
  let metric_values = [100.0, 200.0, 300.0, 400.0, 500.0]
  let metric_name = "cpu_usage"
  let mut sum_value = 0.0
  
  // 计算总和
  let mut i = 0
  while i < metric_values.length() {
    sum_value = sum_value + metric_values[i]
    i = i + 1
  }
  
  // 计算平均值
  let average_value = sum_value / metric_values.length().to_double()
  
  // 验证平均值
  assert_eq(average_value, 300.0)
  
  // 创建聚合结果
  let aggregation_result = metric_name + "_avg=" + average_value.to_string()
  assert_eq(aggregation_result, "cpu_usage_avg=300.0")
}

test "telemetry_metric_aggregation_min_max" {
  // 测试指标最小值和最大值聚合
  
  let metric_values = [45.2, 78.9, 23.1, 89.5, 56.7, 12.3, 67.8]
  let metric_name = "response_time"
  
  // 初始化最小值和最大值
  let mut min_value = metric_values[0]
  let mut max_value = metric_values[0]
  
  // 查找最小值和最大值
  let mut i = 1
  while i < metric_values.length() {
    if metric_values[i] < min_value {
      min_value = metric_values[i]
    }
    if metric_values[i] > max_value {
      max_value = metric_values[i]
    }
    i = i + 1
  }
  
  // 验证最小值和最大值
  assert_eq(min_value, 12.3)
  assert_eq(max_value, 89.5)
  
  // 创建聚合结果
  let min_result = metric_name + "_min=" + min_value.to_string()
  let max_result = metric_name + "_max=" + max_value.to_string()
  
  assert_eq(min_result, "response_time_min=12.3")
  assert_eq(max_result, "response_time_max=89.5")
}

test "telemetry_histogram_aggregation" {
  // 测试直方图聚合
  
  let bucket_bounds = [10.0, 50.0, 100.0, 500.0, 1000.0]
  let measurements = [5.0, 25.0, 75.0, 150.0, 750.0, 25.0, 60.0, 5.0]
  let mut bucket_counts = [0, 0, 0, 0, 0, 0] // 包括+Inf桶
  
  // 将测量值分配到桶中
  let mut i = 0
  while i < measurements.length() {
    let value = measurements[i]
    let mut bucket_index = bucket_bounds.length() // 默认+Inf桶
    
    let mut j = 0
    while j < bucket_bounds.length() {
      if value <= bucket_bounds[j] {
        bucket_index = j
        break
      }
      j = j + 1
    }
    
    bucket_counts[bucket_index] = bucket_counts[bucket_index] + 1
    i = i + 1
  }
  
  // 验证桶计数
  assert_eq(bucket_counts[0], 2) // <=10.0: [5.0, 5.0]
  assert_eq(bucket_counts[1], 3) // <=50.0: [25.0, 25.0, 5.0(已计数), 5.0(已计数)]
  assert_eq(bucket_counts[2], 2) // <=100.0: [75.0, 60.0]
  assert_eq(bucket_counts[3], 1) // <=500.0: [150.0]
  assert_eq(bucket_counts[4], 1) // <=1000.0: [750.0]
  assert_eq(bucket_counts[5], 0) // +Inf: 无
  
  // 验证总计数
  let mut total_count = 0
  i = 0
  while i < bucket_counts.length() {
    total_count = total_count + bucket_counts[i]
    i = i + 1
  }
  assert_eq(total_count, measurements.length())
}

test "telemetry_percentile_aggregation" {
  // 测试百分位数聚合
  
  let response_times = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0]
  let percentiles = [0.5, 0.9, 0.95, 0.99] // 50th, 90th, 95th, 99th percentile
  
  // 排序数据（已排序）
  let sorted_data = response_times
  
  // 计算百分位数
  let mut i = 0
  while i < percentiles.length() {
    let percentile = percentiles[i]
    let index = (percentile * (sorted_data.length() - 1).to_double()).to_int()
    let percentile_value = sorted_data[index]
    
    // 验证特定百分位数
    match percentile {
      0.5 => assert_eq(percentile_value, 50.0), // 中位数
      0.9 => assert_eq(percentile_value, 90.0), // 90th percentile
      0.95 => assert_eq(percentile_value, 95.0), // 95th percentile (插值)
      0.99 => assert_eq(percentile_value, 99.0), // 99th percentile (插值)
      _ => ()
    }
    
    i = i + 1
  }
}

test "telemetry_time_window_aggregation" {
  // 测试时间窗口聚合
  
  let time_series_data = [
    (1000L, 10.0),   // timestamp, value
    (2000L, 20.0),
    (3000L, 15.0),
    (4000L, 25.0),
    (5000L, 30.0),
    (6000L, 18.0),
    (7000L, 22.0),
    (8000L, 28.0)
  ]
  
  let window_start = 3000L
  let window_end = 6000L
  let mut window_sum = 0.0
  let mut window_count = 0
  
  // 聚合时间窗口内的数据
  let mut i = 0
  while i < time_series_data.length() {
    let timestamp = time_series_data[i].0
    let value = time_series_data[i].1
    
    if timestamp >= window_start && timestamp <= window_end {
      window_sum = window_sum + value
      window_count = window_count + 1
    }
    
    i = i + 1
  }
  
  // 验证窗口聚合结果
  assert_eq(window_count, 4) // 3000, 4000, 5000, 6000
  assert_eq(window_sum, 88.0) // 15 + 25 + 30 + 18
  
  let window_average = window_sum / window_count.to_double()
  assert_eq(window_average, 22.0)
}

test "telemetry_cardinality_aggregation" {
  // 测试基数聚合（唯一值计数）
  
  let user_ids = ["user1", "user2", "user1", "user3", "user2", "user4", "user1", "user5"]
  let mut unique_users = []
  
  // 计算唯一用户数
  let mut i = 0
  while i < user_ids.length() {
    let user_id = user_ids[i]
    
    // 检查是否已存在
    let mut found = false
    let mut j = 0
    while j < unique_users.length() {
      if unique_users[j] == user_id {
        found = true
        break
      }
      j = j + 1
    }
    
    // 如果不存在，添加到唯一列表
    if !found {
      unique_users.push(user_id)
    }
    
    i = i + 1
  }
  
  // 验证基数
  assert_eq(unique_users.length(), 5) // user1, user2, user3, user4, user5
  assert_eq(user_ids.length(), 8) // 总记录数
  
  // 验证唯一用户列表
  let mut user1_found = false
  let mut user3_found = false
  let mut user5_found = false
  
  i = 0
  while i < unique_users.length() {
    match unique_users[i] {
      "user1" => user1_found = true,
      "user3" => user3_found = true,
      "user5" => user5_found = true,
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(user1_found, true)
  assert_eq(user3_found, true)
  assert_eq(user5_found, true)
}
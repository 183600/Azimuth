// 遥测数据聚合测试用例

test "telemetry_metrics_aggregation_sum" {
  // 测试遥测指标求和聚合
  
  let metric_values = [10.5, 20.3, 15.7, 8.2, 12.9, 18.4, 22.1, 9.6]
  let mut sum = 0.0
  
  // 计算总和
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  
  // 验证总和计算
  assert_eq(sum > 100.0, true)
  assert_eq(sum < 120.0, true)
  // 使用简单的比较代替abs函数
  let diff = sum - 117.7
  assert_eq(diff > -0.1 && diff < 0.1, true)  // 允许0.1的误差
  
  // 验证聚合统计
  let count = metric_values.length()
  let average = sum / count.to_double()
  
  assert_eq(count, 8)
  assert_eq(average > 10.0, true)
  assert_eq(average < 20.0, true)
  let avg_diff = average - 14.7125
  assert_eq(avg_diff > -0.01 && avg_diff < 0.01, true)
}

test "telemetry_metrics_aggregation_average" {
  // 测试遥测指标平均值聚合
  
  let response_times = [125.5, 150.2, 98.7, 210.3, 175.8, 142.1, 189.4, 167.9]
  let mut sum = 0.0
  
  // 计算总和
  let mut i = 0
  while i < response_times.length() {
    sum = sum + response_times[i]
    i = i + 1
  }
  
  // 计算平均值
  let average = sum / response_times.length().to_double()
  
  // 验证平均值计算
  assert_eq(average > 100.0, true)
  assert_eq(average < 200.0, true)
  assert_eq(abs(average - 157.4875) < 0.01, true)
  
  // 计算移动平均值（窗口大小为3）
  let mut moving_averages = []
  let window_size = 3
  
  i = 0
  while i <= response_times.length() - window_size {
    let window_sum = 0.0
    let mut j = 0
    while j < window_size {
      window_sum = window_sum + response_times[i + j]
      j = j + 1
    }
    let window_avg = window_sum / window_size.to_double()
    moving_averages.push(window_avg)
    i = i + 1
  }
  
  // 验证移动平均值
  assert_eq(moving_averages.length(), 6)
  assert_eq(abs(moving_averages[0] - 124.8) < 0.1, true)
  assert_eq(abs(moving_averages[1] - 153.07) < 0.1, true)
  assert_eq(abs(moving_averages[5] - 165.8) < 0.1, true)
}

test "telemetry_metrics_aggregation_min_max" {
  // 测试遥测指标最小值和最大值聚合
  
  let cpu_usage_values = [45.2, 67.8, 23.1, 89.5, 34.6, 78.9, 56.3, 41.7, 72.4, 38.9]
  
  // 查找最小值
  let mut min_value = cpu_usage_values[0]
  let mut i = 1
  while i < cpu_usage_values.length() {
    if cpu_usage_values[i] < min_value {
      min_value = cpu_usage_values[i]
    }
    i = i + 1
  }
  
  // 查找最大值
  let mut max_value = cpu_usage_values[0]
  i = 1
  while i < cpu_usage_values.length() {
    if cpu_usage_values[i] > max_value {
      max_value = cpu_usage_values[i]
    }
    i = i + 1
  }
  
  // 验证最小值和最大值
  assert_eq(abs(min_value - 23.1) < 0.1, true)
  assert_eq(abs(max_value - 89.5) < 0.1, true)
  
  // 计算范围
  let range = max_value - min_value
  assert_eq(abs(range - 66.4) < 0.1, true)
  
  // 计算中位数（先排序）
  let sorted_values = [23.1, 34.6, 38.9, 41.7, 45.2, 56.3, 67.8, 72.4, 78.9, 89.5]
  let median = if sorted_values.length() % 2 == 0 {
    (sorted_values[sorted_values.length() / 2 - 1] + sorted_values[sorted_values.length() / 2]) / 2.0
  } else {
    sorted_values[sorted_values.length() / 2]
  }
  
  assert_eq(abs(median - 50.75) < 0.1, true)
}

test "telemetry_metrics_aggregation_percentiles" {
  // 测试遥测指标百分位数聚合
  
  let latency_values = [
    12.5, 18.3, 25.7, 8.9, 42.1, 35.6, 28.4, 15.2,
    22.8, 31.9, 19.7, 38.3, 45.6, 11.4, 27.1, 33.8
  ]
  
  // 对值进行排序
  let sorted_values = [
    8.9, 11.4, 12.5, 15.2, 18.3, 19.7, 22.8, 25.7,
    27.1, 28.4, 31.9, 33.8, 35.6, 38.3, 42.1, 45.6
  ]
  
  // 计算P50（中位数）
  let p50_index = (sorted_values.length() * 50) / 100
  let p50 = if p50_index < sorted_values.length() {
    sorted_values[p50_index]
  } else {
    sorted_values[sorted_values.length() - 1]
  }
  
  // 计算P95
  let p95_index = (sorted_values.length() * 95) / 100
  let p95 = if p95_index < sorted_values.length() {
    sorted_values[p95_index]
  } else {
    sorted_values[sorted_values.length() - 1]
  }
  
  // 计算P99
  let p99_index = (sorted_values.length() * 99) / 100
  let p99 = if p99_index < sorted_values.length() {
    sorted_values[p99_index]
  } else {
    sorted_values[sorted_values.length() - 1]
  }
  
  // 验证百分位数
  assert_eq(abs(p50 - 25.7) < 0.1, true)
  assert_eq(abs(p95 - 45.6) < 0.1, true)
  assert_eq(abs(p99 - 45.6) < 0.1, true)
  
  // 验证百分位数关系
  assert_eq(p50 <= p95, true)
  assert_eq(p95 <= p99, true)
}

test "telemetry_histogram_aggregation" {
  // 测试遥测直方图聚合
  
  let request_durations = [0.1, 0.5, 1.2, 2.8, 0.3, 1.7, 3.5, 0.8, 2.1, 1.5, 0.6, 4.2]
  let buckets = [0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5]
  
  // 初始化直方图计数
  let mut bucket_counts = []
  let mut i = 0
  while i < buckets.length() {
    bucket_counts.push(0)
    i = i + 1
  }
  
  // 统计每个桶的计数
  i = 0
  while i < request_durations.length() {
    let duration = request_durations[i]
    let mut bucket_index = 0
    
    // 找到合适的桶
    let mut j = 0
    while j < buckets.length() {
      if duration <= buckets[j] {
        bucket_index = j
        break
      }
      j = j + 1
    }
    
    // 如果没有找到合适的桶，使用最后一个桶
    if bucket_index == 0 && duration > buckets[buckets.length() - 1] {
      bucket_index = buckets.length() - 1
    }
    
    // 增加桶计数
    bucket_counts[bucket_index] = bucket_counts[bucket_index] + 1
    i = i + 1
  }
  
  // 验证直方图统计
  let total_count = 0
  i = 0
  while i < bucket_counts.length() {
    total_count = total_count + bucket_counts[i]
    i = i + 1
  }
  
  assert_eq(total_count, request_durations.length())
  
  // 验证桶计数分布
  assert_eq(bucket_counts[0] >= 1, true)  // <= 0.0s
  assert_eq(bucket_counts[1] >= 1, true)  // <= 0.5s
  assert_eq(bucket_counts[2] >= 1, true)  // <= 1.0s
  assert_eq(bucket_counts[3] >= 1, true)  // <= 1.5s
  assert_eq(bucket_counts[4] >= 1, true)  // <= 2.0s
  assert_eq(bucket_counts[5] >= 1, true)  // <= 2.5s
  assert_eq(bucket_counts[6] >= 1, true)  // <= 3.0s
  assert_eq(bucket_counts[7] >= 1, true)  // <= 3.5s
  assert_eq(bucket_counts[8] >= 1, true)  // <= 4.0s
  assert_eq(bucket_counts[9] >= 1, true)  // <= 4.5s
}

test "telemetry_time_series_aggregation" {
  // 测试遥测时间序列聚合
  
  let time_series_data = [
    (1672531200, 100.5),  // 2023-01-01 00:00:00
    (1672531260, 105.2),  // 2023-01-01 00:01:00
    (1672531320, 98.7),   // 2023-01-01 00:02:00
    (1672531380, 112.3),  // 2023-01-01 00:03:00
    (1672531440, 108.9),  // 2023-01-01 00:04:00
    (1672531500, 115.6),  // 2023-01-01 00:05:00
    (1672531560, 102.4),  // 2023-01-01 00:06:00
    (1672531620, 118.1),  // 2023-01-01 00:07:00
    (1672531680, 107.8),  // 2023-01-01 00:08:00
    (1672531740, 110.3)   // 2023-01-01 00:09:00
  ]
  
  // 按时间窗口聚合（5分钟窗口）
  let window_size = 300  // 5分钟
  let start_time = time_series_data[0].0
  let end_time = time_series_data[time_series_data.length() - 1].0
  
  let mut window_aggregates = []
  let mut current_window_start = start_time
  
  while current_window_start < end_time {
    let window_end = current_window_start + window_size
    let mut window_values = []
    
    // 收集窗口内的数据点
    let mut i = 0
    while i < time_series_data.length() {
      let timestamp = time_series_data[i].0
      let value = time_series_data[i].1
      
      if timestamp >= current_window_start && timestamp < window_end {
        window_values.push(value)
      }
      i = i + 1
    }
    
    // 计算窗口统计
    if window_values.length() > 0 {
      let mut sum = 0.0
      let mut min_val = window_values[0]
      let mut max_val = window_values[0]
      
      let mut j = 0
      while j < window_values.length() {
        let val = window_values[j]
        sum = sum + val
        if val < min_val {
          min_val = val
        }
        if val > max_val {
          max_val = val
        }
        j = j + 1
      }
      
      let avg_val = sum / window_values.length().to_double()
      
      window_aggregates.push({
        "window_start": current_window_start,
        "window_end": window_end,
        "count": window_values.length(),
        "sum": sum,
        "avg": avg_val,
        "min": min_val,
        "max": max_val
      })
    }
    
    current_window_start = current_window_start + window_size
  }
  
  // 验证时间窗口聚合结果
  assert_eq(window_aggregates.length(), 2)  // 应该有2个5分钟窗口
  
  // 验证第一个窗口（前5分钟）
  let first_window = window_aggregates[0]
  assert_eq(first_window.get("count", 0), 5)
  assert_eq(first_window.get("sum", 0.0) > 500.0, true)
  assert_eq(first_window.get("avg", 0.0) > 100.0, true)
  
  // 验证第二个窗口（后5分钟）
  let second_window = window_aggregates[1]
  assert_eq(second_window.get("count", 0), 5)
  assert_eq(second_window.get("sum", 0.0) > 500.0, true)
  assert_eq(second_window.get("avg", 0.0) > 100.0, true)
}

test "telemetry_group_by_aggregation" {
  // 测试遥测分组聚合
  
  let request_data = [
    ("GET", "/api/users", 200, 120.5),
    ("POST", "/api/orders", 201, 250.8),
    ("GET", "/api/products", 200, 95.3),
    ("GET", "/api/users", 404, 45.2),
    ("PUT", "/api/users/123", 200, 180.7),
    ("POST", "/api/orders", 400, 125.4),
    ("GET", "/api/products", 500, 210.6),
    ("DELETE", "/api/users/456", 204, 85.9),
    ("GET", "/api/users", 200, 110.3),
    ("POST", "/api/orders", 201, 275.2)
  ]
  
  // 按HTTP方法分组聚合
  let mut method_groups = {}
  
  let mut i = 0
  while i < request_data.length() {
    let method = request_data[i].0
    let status = request_data[i].2
    let duration = request_data[i].3
    
    // 初始化分组数据
    if not method_groups.contains(method) {
      method_groups.set(method, {
        "count": 0,
        "total_duration": 0.0,
        "success_count": 0,
        "error_count": 0
      })
    }
    
    // 更新分组统计
    let group_data = method_groups.get(method, {})
    let updated_count = group_data.get("count", 0) + 1
    let updated_total = group_data.get("total_duration", 0.0) + duration
    let updated_success = group_data.get("success_count", 0) + (if status >= 200 && status < 300 { 1 } else { 0 })
    let updated_error = group_data.get("error_count", 0) + (if status >= 400 { 1 } else { 0 })
    
    method_groups.set(method, {
      "count": updated_count,
      "total_duration": updated_total,
      "success_count": updated_success,
      "error_count": updated_error
    })
    
    i = i + 1
  }
  
  // 验证分组聚合结果
  assert_eq(method_groups.contains("GET"), true)
  assert_eq(method_groups.contains("POST"), true)
  assert_eq(method_groups.contains("PUT"), true)
  assert_eq(method_groups.contains("DELETE"), true)
  
  // 验证GET方法统计
  let get_stats = method_groups.get("GET", {})
  assert_eq(get_stats.get("count", 0), 4)
  assert_eq(get_stats.get("success_count", 0), 3)
  assert_eq(get_stats.get("error_count", 0), 1)
  assert_eq(get_stats.get("total_duration", 0.0) > 300.0, true)
  
  // 验证POST方法统计
  let post_stats = method_groups.get("POST", {})
  assert_eq(post_stats.get("count", 0), 3)
  assert_eq(post_stats.get("success_count", 0), 2)
  assert_eq(post_stats.get("error_count", 0), 1)
  assert_eq(post_stats.get("total_duration", 0.0) > 600.0, true)
  
  // 验证PUT方法统计
  let put_stats = method_groups.get("PUT", {})
  assert_eq(put_stats.get("count", 0), 1)
  assert_eq(put_stats.get("success_count", 0), 1)
  assert_eq(put_stats.get("error_count", 0), 0)
  
  // 验证DELETE方法统计
  let delete_stats = method_groups.get("DELETE", {})
  assert_eq(delete_stats.get("count", 0), 1)
  assert_eq(delete_stats.get("success_count", 0), 1)
  assert_eq(delete_stats.get("error_count", 0), 0)
}
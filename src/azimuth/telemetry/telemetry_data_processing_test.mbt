// 遥测数据处理测试用例，专注于数据转换、聚合和分析功能

test "telemetry_data_aggregation" {
  // 测试遥测数据聚合功能
  
  let metric_names = ["cpu_usage", "memory_usage", "disk_io", "network_throughput"]
  let metric_values = [75.2, 68.5, 45.8, 120.3]
  let time_stamps = [1640995200L, 1640995260L, 1640995320L, 1640995380L]
  
  // 验证数据完整性
  assert_eq(metric_names.length(), metric_values.length())
  assert_eq(metric_names.length(), time_stamps.length())
  
  // 计算平均值
  let mut sum = 0.0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  let average = sum / metric_values.length().to_double()
  assert_eq(average > 50.0, true)
  assert_eq(average < 100.0, true)
  
  // 找出最大值和最小值
  let mut max_value = metric_values[0]
  let mut min_value = metric_values[0]
  i = 1
  while i < metric_values.length() {
    if metric_values[i] > max_value {
      max_value = metric_values[i]
    }
    if metric_values[i] < min_value {
      min_value = metric_values[i]
    }
    i = i + 1
  }
  
  assert_eq(max_value, 120.3)
  assert_eq(min_value, 45.8)
  
  // 创建聚合结果
  let aggregation_result = "avg:" + average.to_string() + ",max:" + max_value.to_string() + ",min:" + min_value.to_string()
  assert_eq(aggregation_result.contains("avg:"), true)
  assert_eq(aggregation_result.contains("max:"), true)
  assert_eq(aggregation_result.contains("min:"), true)
}

test "telemetry_data_transformation" {
  // 测试遥测数据转换功能
  
  let raw_data = [
    ("timestamp", "1640995200"),
    ("metric_name", "response_time"),
    ("metric_value", "250.5"),
    ("unit", "milliseconds"),
    ("tags", "endpoint:/api/users,method:GET")
  ]
  
  // 验证原始数据
  assert_eq(raw_data.length(), 5)
  assert_eq(raw_data[0].0, "timestamp")
  assert_eq(raw_data[0].1, "1640995200")
  
  // 数据类型转换
  let mut transformed_data = []
  let mut i = 0
  while i < raw_data.length() {
    let key = raw_data[i].0
    let value = raw_data[i].1
    
    if key == "timestamp" || key == "metric_value" {
      // 转换为数值
      let num_value = value.to_int()
      transformed_data.push((key, num_value.to_string()))
    } else {
      // 保持字符串类型
      transformed_data.push((key, value))
    }
    i = i + 1
  }
  
  // 验证转换结果
  assert_eq(transformed_data.length(), 5)
  assert_eq(transformed_data[0].0, "timestamp")
  assert_eq(transformed_data[2].0, "metric_value")
  
  // 创建JSON格式的转换数据
  let json_data = "{"
  i = 0
  while i < transformed_data.length() {
    json_data = json_data + "\"" + transformed_data[i].0 + "\":\"" + transformed_data[i].1 + "\""
    if i < transformed_data.length() - 1 {
      json_data = json_data + ","
    }
    i = i + 1
  }
  json_data = json_data + "}"
  
  // 验证JSON格式
  assert_eq(json_data.has_prefix("{"), true)
  assert_eq(json_data.has_suffix("}"), true)
  assert_eq(json_data.contains("\"timestamp\":\"1640995200\""), true)
  assert_eq(json_data.contains("\"metric_value\":\"250\""), true)
}

test "telemetry_data_filtering" {
  // 测试遥测数据过滤功能
  
  let all_metrics = [
    ("cpu_usage", 75.5, "production"),
    ("memory_usage", 68.2, "production"),
    ("cpu_usage", 45.3, "staging"),
    ("memory_usage", 52.1, "staging"),
    ("cpu_usage", 82.7, "production"),
    ("network_io", 120.4, "production")
  ]
  
  // 过滤条件：只保留production环境的CPU使用率
  let mut filtered_metrics = []
  let mut i = 0
  while i < all_metrics.length() {
    if all_metrics[i].0 == "cpu_usage" && all_metrics[i].2 == "production" {
      filtered_metrics.push(all_metrics[i])
    }
    i = i + 1
  }
  
  // 验证过滤结果
  assert_eq(filtered_metrics.length(), 2)
  assert_eq(filtered_metrics[0].0, "cpu_usage")
  assert_eq(filtered_metrics[0].2, "production")
  assert_eq(filtered_metrics[1].0, "cpu_usage")
  assert_eq(filtered_metrics[1].2, "production")
  
  // 过滤条件：值大于70的指标
  let mut high_value_metrics = []
  i = 0
  while i < all_metrics.length() {
    if all_metrics[i].1 > 70.0 {
      high_value_metrics.push(all_metrics[i])
    }
    i = i + 1
  }
  
  // 验证高值过滤结果
  assert_eq(high_value_metrics.length(), 3)
  assert_eq(high_value_metrics[0].1, 75.5)
  assert_eq(high_value_metrics[1].1, 82.7)
  assert_eq(high_value_metrics[2].1, 120.4)
}

test "telemetry_data_time_series_analysis" {
  // 测试遥测时间序列数据分析
  
  let time_series_data = [
    (1640995200L, 100.0),
    (1640995260L, 105.0),
    (1640995320L, 98.0),
    (1640995380L, 110.0),
    (1640995440L, 115.0)
  ]
  
  // 验证时间序列数据
  assert_eq(time_series_data.length(), 5)
  assert_eq(time_series_data[0].0, 1640995200L)
  assert_eq(time_series_data[4].0, 1640995440L)
  
  // 计算时间差（秒）
  let time_diff = time_series_data[4].0 - time_series_data[0].0
  assert_eq(time_diff, 240L) // 4分钟 = 240秒
  
  // 计算变化率
  let value_change = time_series_data[4].1 - time_series_data[0].1
  assert_eq(value_change, 15.0)
  
  let change_rate = value_change / time_diff.to_double()
  assert_eq(change_rate > 0.06, true) // 15/240 ≈ 0.0625
  assert_eq(change_rate < 0.07, true)
  
  // 计算移动平均（窗口大小为3）
  let mut moving_averages = []
  let mut i = 2
  while i < time_series_data.length() {
    let window_avg = (time_series_data[i-2].1 + time_series_data[i-1].1 + time_series_data[i].1) / 3.0
    moving_averages.push(window_avg)
    i = i + 1
  }
  
  // 验证移动平均
  assert_eq(moving_averages.length(), 3)
  assert_eq(moving_averages[0], (100.0 + 105.0 + 98.0) / 3.0)
  assert_eq(moving_averages[2], (98.0 + 110.0 + 115.0) / 3.0)
}

test "telemetry_data_compression" {
  // 测试遥测数据压缩功能
  
  let raw_log_entries = [
    "INFO: User login successful",
    "INFO: User login successful", 
    "INFO: User login successful",
    "ERROR: Database connection failed",
    "WARN: High memory usage detected",
    "INFO: User logout successful",
    "INFO: User login successful",
    "INFO: User login successful"
  ]
  
  // 验证原始日志条目
  assert_eq(raw_log_entries.length(), 8)
  
  // 统计重复条目
  let mut log_counts = []
  let mut i = 0
  while i < raw_log_entries.length() {
    let log_entry = raw_log_entries[i]
    let mut found = false
    let mut j = 0
    
    while j < log_counts.length() {
      if log_counts[j].0 == log_entry {
        log_counts[j] = (log_counts[j].0, log_counts[j].1 + 1)
        found = true
        break
      }
      j = j + 1
    }
    
    if not found {
      log_counts.push((log_entry, 1))
    }
    i = i + 1
  }
  
  // 验证压缩结果
  assert_eq(log_counts.length(), 4) // 4种不同的日志类型
  
  // 查找最常见的日志
  let mut max_count = 0
  let mut most_common_log = ""
  i = 0
  while i < log_counts.length() {
    if log_counts[i].1 > max_count {
      max_count = log_counts[i].1
      most_common_log = log_counts[i].0
    }
    i = i + 1
  }
  
  assert_eq(max_count, 5)
  assert_eq(most_common_log, "INFO: User login successful")
  
  // 计算压缩率
  let compression_ratio = raw_log_entries.length().to_double() / log_counts.length().to_double()
  assert_eq(compression_ratio, 2.0)
}
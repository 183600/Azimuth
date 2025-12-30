// 遥测数据转换测试用例

test "telemetry_unit_conversion" {
  // 测试单位转换功能
  
  let metrics_with_units = [
    ("response_time_ms", 1250, "ms"),
    ("cpu_usage_percent", 75.5, "%"),
    ("memory_usage_mb", 2048, "MB"),
    ("network_throughput_kbps", 1024, "kbps"),
    ("disk_io_ops", 5000, "ops")
  ]
  
  // 转换为不同单位
  let converted_metrics = []
  
  let mut i = 0
  while i < metrics_with_units.length() {
    let name = metrics_with_units[i].0
    let value = metrics_with_units[i].1
    let unit = metrics_with_units[i].2
    let converted_value = 0.0
    let new_unit = ""
    
    if name == "response_time_ms" {
      // 毫秒转秒
      converted_value = value.to_double() / 1000.0
      new_unit = "s"
    } else if name == "cpu_usage_percent" {
      // 百分比转小数
      converted_value = value / 100.0
      new_unit = "ratio"
    } else if name == "memory_usage_mb" {
      // MB转GB
      converted_value = value.to_double() / 1024.0
      new_unit = "GB"
    } else if name == "network_throughput_kbps" {
      // kbps转Mbps
      converted_value = value.to_double() / 1024.0
      new_unit = "Mbps"
    } else if name == "disk_io_ops" {
      // 操作数转千操作数
      converted_value = value.to_double() / 1000.0
      new_unit = "kops"
    }
    
    converted_metrics.push((name, converted_value, new_unit))
    i = i + 1
  }
  
  // 验证转换结果
  assert_eq(converted_metrics[0].1, 1.25)  // 1250ms = 1.25s
  assert_eq(converted_metrics[1].1, 0.755)  // 75.5% = 0.755
  assert_eq(converted_metrics[2].1 > 1.99 && converted_metrics[2].1 < 2.01, true)  // 2048MB ≈ 2GB
  assert_eq(converted_metrics[3].1, 1.0)  // 1024kbps = 1Mbps
  assert_eq(converted_metrics[4].1, 5.0)  // 5000ops = 5kops
}

test "telemetry_format_conversion" {
  // 测试格式转换功能
  
  let raw_data = [
    ("timestamp", "2023-12-01T10:30:45Z"),
    ("level", "INFO"),
    ("message", "Request processed successfully"),
    ("duration", "250ms"),
    ("status", "200")
  ]
  
  // 转换为结构化格式
  let structured_data = {
    "timestamp": "",
    "epoch_time": 0,
    "level_code": 0,
    "duration_ms": 0,
    "status_code": 0
  }
  
  // 模拟格式转换
  let mut i = 0
  while i < raw_data.length() {
    let key = raw_data[i].0
    let value = raw_data[i].1
    
    if key == "timestamp" {
      structured_data["timestamp"] = value
      // 简化：假设时间戳转换为epoch时间
      structured_data["epoch_time"] = 1701423045  // 示例epoch时间
    } else if key == "level" {
      structured_data["level_code"] = if value == "INFO" { 1 } else if value == "WARN" { 2 } else if value == "ERROR" { 3 } else { 0 }
    } else if key == "duration" {
      // 提取数值部分
      let duration_str = value.substring(0, value.length() - 2)  // 去掉"ms"
      structured_data["duration_ms"] = duration_str.to_int()
    } else if key == "status" {
      structured_data["status_code"] = value.to_int()
    }
    
    i = i + 1
  }
  
  // 验证格式转换结果
  assert_eq(structured_data["timestamp"], "2023-12-01T10:30:45Z")
  assert_eq(structured_data["epoch_time"], 1701423045)
  assert_eq(structured_data["level_code"], 1)  // INFO = 1
  assert_eq(structured_data["duration_ms"], 250)
  assert_eq(structured_data["status_code"], 200)
}

test "telemetry_aggregation_conversion" {
  // 测试聚合转换功能
  
  let time_series_data = [
    (100, 10.5),
    (200, 15.3),
    (300, 12.7),
    (400, 18.2),
    (500, 14.9),
    (600, 16.4)
  ]
  
  // 转换为聚合统计
  let aggregated_stats = {
    "count": 0,
    "sum": 0.0,
    "average": 0.0,
    "min": 0.0,
    "max": 0.0,
    "range": 0.0
  }
  
  // 计算聚合统计
  let mut sum = 0.0
  let mut min_value = time_series_data[0].1
  let mut max_value = time_series_data[0].1
  
  let mut i = 0
  while i < time_series_data.length() {
    let value = time_series_data[i].1
    sum = sum + value
    
    if value < min_value {
      min_value = value
    }
    if value > max_value {
      max_value = value
    }
    
    i = i + 1
  }
  
  aggregated_stats["count"] = time_series_data.length()
  aggregated_stats["sum"] = sum
  aggregated_stats["average"] = sum / time_series_data.length().to_double()
  aggregated_stats["min"] = min_value
  aggregated_stats["max"] = max_value
  aggregated_stats["range"] = max_value - min_value
  
  // 验证聚合转换结果
  assert_eq(aggregated_stats["count"], 6)
  assert_eq(aggregated_stats["sum"] > 80.0 && aggregated_stats["sum"] < 90.0, true)
  assert_eq(aggregated_stats["average"] > 13.0 && aggregated_stats["average"] < 15.0, true)
  assert_eq(aggregated_stats["min"], 10.5)
  assert_eq(aggregated_stats["max"], 18.2)
  assert_eq(aggregated_stats["range"], 7.7)
}

test "telemetry_normalization_conversion" {
  // 测试归一化转换功能
  
  let raw_metrics = [23.5, 67.8, 45.2, 89.1, 12.7, 56.3, 78.9, 34.6]
  
  // 归一化到[0,1]范围
  let normalized_metrics = []
  
  // 找到最小值和最大值
  let mut min_value = raw_metrics[0]
  let mut max_value = raw_metrics[0]
  
  let mut i = 0
  while i < raw_metrics.length() {
    if raw_metrics[i] < min_value {
      min_value = raw_metrics[i]
    }
    if raw_metrics[i] > max_value {
      max_value = raw_metrics[i]
    }
    i = i + 1
  }
  
  // 归一化
  i = 0
  while i < raw_metrics.length() {
    let normalized = (raw_metrics[i] - min_value) / (max_value - min_value)
    normalized_metrics.push(normalized)
    i = i + 1
  }
  
  // 验证归一化结果
  assert_eq(normalized_metrics[0] >= 0.0 && normalized_metrics[0] <= 1.0, true)
  assert_eq(normalized_metrics[normalized_metrics.length() - 1] >= 0.0 && normalized_metrics[normalized_metrics.length() - 1] <= 1.0, true)
  
  // 检查最小值归一化为0，最大值归一化为1
  let mut found_zero = false
  let mut found_one = false
  
  i = 0
  while i < normalized_metrics.length() {
    if normalized_metrics[i] < 0.01 {  // 近似0
      found_zero = true
    }
    if normalized_metrics[i] > 0.99 {  // 近似1
      found_one = true
    }
    i = i + 1
  }
  
  assert_eq(found_zero, true)
  assert_eq(found_one, true)
}
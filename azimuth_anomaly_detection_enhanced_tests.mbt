// Azimuth 遥测数据异常检测测试用例
// 专注于遥测系统中的异常检测和告警功能

// 测试1: 基于统计阈值的异常检测
test "基于统计阈值的异常检测" {
  // 模拟正常CPU使用率数据
  let normal_cpu_data = [45.2, 47.8, 46.5, 48.1, 44.9, 46.3, 47.2, 45.8, 46.9, 47.5]
  
  // 模拟包含异常的CPU数据
  let cpu_data_with_anomalies = [45.2, 47.8, 95.5, 48.1, 44.9, 3.2, 47.2, 45.8, 99.1, 47.5]
  
  // 计算正常数据的统计特征
  let mut sum = 0.0
  for value in normal_cpu_data {
    sum = sum + value
  }
  let mean = sum / normal_cpu_data.length().to_float()
  
  // 计算标准差
  let mut variance_sum = 0.0
  for value in normal_cpu_data {
    let diff = value - mean
    variance_sum = variance_sum + diff * diff
  }
  let variance = variance_sum / normal_cpu_data.length().to_float()
  let std_dev = variance.sqrt()
  
  // 设置异常阈值（均值±3倍标准差）
  let upper_threshold = mean + 3.0 * std_dev
  let lower_threshold = mean - 3.0 * std_dev
  
  // 检测异常
  let mut anomalies = []
  for value in cpu_data_with_anomalies {
    if value > upper_threshold || value < lower_threshold {
      anomalies = anomalies.push({
        value: value,
        anomaly_type: if value > upper_threshold { "spike" } else { "drop" }
      })
    }
  }
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 3)
  assert_eq(anomalies[0].value, 95.5)
  assert_eq(anomalies[0].anomaly_type, "spike")
  assert_eq(anomalies[1].value, 3.2)
  assert_eq(anomalies[1].anomaly_type, "drop")
  assert_eq(anomalies[2].value, 99.1)
  assert_eq(anomalies[2].anomaly_type, "spike")
}

// 测试2: 基于时间序列模式的异常检测
test "基于时间序列模式的异常检测" {
  // 模拟具有周期性模式的时间序列数据
  let time_series_data = [
    { timestamp: 1640995200, value: 20.0 },  // 00:00
    { timestamp: 1640995260, value: 25.0 },  // 00:01
    { timestamp: 1640995320, value: 35.0 },  // 00:02
    { timestamp: 1640995380, value: 45.0 },  // 00:03
    { timestamp: 1640995440, value: 35.0 },  // 00:04
    { timestamp: 1640995500, value: 25.0 },  // 00:05
    { timestamp: 1640995560, value: 20.0 },  // 00:06
    { timestamp: 1640995620, value: 80.0 },  // 00:07 - 异常
    { timestamp: 1640995680, value: 25.0 },  // 00:08
    { timestamp: 1640995740, value: 35.0 },  // 00:09
    { timestamp: 1640995800, value: 45.0 },  // 00:10
    { timestamp: 1640995860, value: 35.0 },  // 00:11
    { timestamp: 1640995920, value: 25.0 }   // 00:12
  ]
  
  // 计算移动平均值（窗口大小为3）
  let window_size = 3
  let mut moving_averages = []
  
  let mut i = 0
  while i <= time_series_data.length() - window_size {
    let mut window_sum = 0.0
    let mut j = 0
    while j < window_size {
      window_sum = window_sum + time_series_data[i + j].value
      j = j + 1
    }
    let avg = window_sum / window_size.to_float()
    moving_averages = moving_averages.push({
      timestamp: time_series_data[i + 1].timestamp, // 中心点时间戳
      expected: avg,
      actual: time_series_data[i + 1].value
    })
    i = i + 1
  }
  
  // 检测异常（偏差超过50%）
  let mut pattern_anomalies = []
  for point in moving_averages {
    let deviation_percentage = (point.actual - point.expected).abs() / point.expected * 100.0
    if deviation_percentage > 50.0 {
      pattern_anomalies = pattern_anomalies.push({
        timestamp: point.timestamp,
        expected: point.expected,
        actual: point.actual,
        deviation: deviation_percentage
      })
    }
  }
  
  // 验证模式异常检测结果
  assert_eq(pattern_anomalies.length(), 1)
  assert_eq(pattern_anomalies[0].timestamp, 1640995620)
  assert_true(pattern_anomalies[0].deviation > 200.0) // 偏差应该超过200%
}

// 测试3: 多维度关联异常检测
test "多维度关联异常检测" {
  // 模拟多维度遥测数据
  let multi_dimensional_data = [
    { timestamp: 1640995200, cpu: 30.0, memory: 60.0, disk_io: 50.0, network: 20.0 },
    { timestamp: 1640995260, cpu: 32.0, memory: 62.0, disk_io: 52.0, network: 22.0 },
    { timestamp: 1640995320, cpu: 95.0, memory: 98.0, disk_io: 15.0, network: 85.0 }, // 异常点1
    { timestamp: 1640995380, cpu: 31.0, memory: 61.0, disk_io: 51.0, network: 21.0 },
    { timestamp: 1640995440, cpu: 33.0, memory: 63.0, disk_io: 53.0, network: 23.0 },
    { timestamp: 1640995500, cpu: 25.0, memory: 40.0, disk_io: 90.0, network: 15.0 }, // 异常点2
    { timestamp: 1640995560, cpu: 34.0, memory: 64.0, disk_io: 54.0, network: 24.0 },
    { timestamp: 1640995620, cpu: 35.0, memory: 65.0, disk_io: 55.0, network: 25.0 }
  ]
  
  // 计算各维度正常范围
  let mut cpu_sum = 0.0, memory_sum = 0.0, disk_io_sum = 0.0, network_sum = 0.0
  let count = multi_dimensional_data.length().to_float()
  
  for data in multi_dimensional_data {
    cpu_sum = cpu_sum + data.cpu
    memory_sum = memory_sum + data.memory
    disk_io_sum = disk_io_sum + data.disk_io
    network_sum = network_sum + data.network
  }
  
  let cpu_avg = cpu_sum / count
  let memory_avg = memory_sum / count
  let disk_io_avg = disk_io_sum / count
  let network_avg = network_sum / count
  
  // 定义异常阈值
  let anomaly_threshold = 50.0 // 偏差百分比
  
  // 多维度关联异常检测
  let mut correlation_anomalies = []
  for data in multi_dimensional_data {
    let cpu_deviation = (data.cpu - cpu_avg).abs() / cpu_avg * 100.0
    let memory_deviation = (data.memory - memory_avg).abs() / memory_avg * 100.0
    let disk_io_deviation = (data.disk_io - disk_io_avg).abs() / disk_io_avg * 100.0
    let network_deviation = (data.network - network_avg).abs() / network_avg * 100.0
    
    // 检测多维度异常（多个维度同时异常）
    let anomalous_dimensions = []
    if cpu_deviation > anomaly_threshold {
      anomalous_dimensions = anomalous_dimensions.push("cpu")
    }
    if memory_deviation > anomaly_threshold {
      anomalous_dimensions = anomalous_dimensions.push("memory")
    }
    if disk_io_deviation > anomaly_threshold {
      anomalous_dimensions = anomalous_dimensions.push("disk_io")
    }
    if network_deviation > anomaly_threshold {
      anomalous_dimensions = anomalous_dimensions.push("network")
    }
    
    // 如果有2个或更多维度同时异常，则认为是关联异常
    if anomalous_dimensions.length() >= 2 {
      correlation_anomalies = correlation_anomalies.push({
        timestamp: data.timestamp,
        anomalous_dimensions: anomalous_dimensions,
        cpu: data.cpu,
        memory: data.memory,
        disk_io: data.disk_io,
        network: data.network
      })
    }
  }
  
  // 验证多维度关联异常检测结果
  assert_eq(correlation_anomalies.length(), 2)
  assert_eq(correlation_anomalies[0].timestamp, 1640995320)
  assert_true(correlation_anomalies[0].anomalous_dimensions.length() >= 3) // CPU、内存、网络异常
  assert_eq(correlation_anomalies[1].timestamp, 1640995500)
  assert_true(correlation_anomalies[1].anomalous_dimensions.length() >= 2) // 内存、磁盘IO异常
}

// 测试4: 异常检测的自适应阈值调整
test "异常检测的自适应阈值调整" {
  // 模拟系统负载随时间变化的数据
  let historical_data = [
    { period: "night", values: [20.0, 22.0, 21.0, 23.0, 19.0] },      // 夜间低负载
    { period: "morning", values: [40.0, 42.0, 45.0, 43.0, 41.0] },    // 早晨中等负载
    { period: "afternoon", values: [70.0, 72.0, 75.0, 73.0, 71.0] },  // 下午高负载
    { period: "evening", values: [50.0, 52.0, 55.0, 53.0, 51.0] }     // 晚上中等负载
  ]
  
  // 当前数据（不同时间段的测试数据）
  let current_data = [
    { period: "night", value: 45.0 },      // 夜间异常高
    { period: "morning", value: 65.0 },    // 早晨轻微高
    { period: "afternoon", value: 95.0 },  // 下午异常高
    { period: "evening", value: 48.0 }     // 晚上正常
  ]
  
  // 计算各时间段的统计特征和自适应阈值
  let mut adaptive_thresholds = []
  for period_data in historical_data {
    let mut sum = 0.0
    for value in period_data.values {
      sum = sum + value
    }
    let mean = sum / period_data.values.length().to_float()
    
    // 计算标准差
    let mut variance_sum = 0.0
    for value in period_data.values {
      let diff = value - mean
      variance_sum = variance_sum + diff * diff
    }
    let variance = variance_sum / period_data.values.length().to_float()
    let std_dev = variance.sqrt()
    
    // 自适应阈值（均值±2倍标准差）
    adaptive_thresholds = adaptive_thresholds.push({
      period: period_data.period,
      mean: mean,
      std_dev: std_dev,
      upper_threshold: mean + 2.0 * std_dev,
      lower_threshold: mean - 2.0 * std_dev
    })
  }
  
  // 使用自适应阈值检测异常
  let mut adaptive_anomalies = []
  for data_point in current_data {
    for threshold in adaptive_thresholds {
      if data_point.period == threshold.period {
        if data_point.value > threshold.upper_threshold || data_point.value < threshold.lower_threshold {
          adaptive_anomalies = adaptive_anomalies.push({
            period: data_point.period,
            value: data_point.value,
            upper_threshold: threshold.upper_threshold,
            lower_threshold: threshold.lower_threshold,
            anomaly_type: if data_point.value > threshold.upper_threshold { "high" } else { "low" }
          })
        }
        break
      }
    }
  }
  
  // 验证自适应阈值异常检测结果
  assert_eq(adaptive_anomalies.length(), 2)
  assert_eq(adaptive_anomalies[0].period, "night")
  assert_eq(adaptive_anomalies[0].anomaly_type, "high")
  assert_eq(adaptive_anomalies[1].period, "afternoon")
  assert_eq(adaptive_anomalies[1].anomaly_type, "high")
  
  // 验证阈值确实根据时间段自适应调整
  let night_threshold = adaptive_thresholds.filter(fn(t) { t.period == "night" })[0]
  let afternoon_threshold = adaptive_thresholds.filter(fn(t) { t.period == "afternoon" })[0]
  assert_true(afternoon_threshold.upper_threshold > night_threshold.upper_threshold)
}

// 测试5: 异常告警的优先级和去重
test "异常告警的优先级和去重" {
  // 模拟异常事件流
  let anomaly_events = [
    { timestamp: 1640995200, type: "cpu_spike", severity: "high", service: "auth-service" },
    { timestamp: 1640995210, type: "memory_drop", severity: "medium", service: "auth-service" },
    { timestamp: 1640995220, type: "cpu_spike", severity: "high", service: "auth-service" }, // 重复
    { timestamp: 1640995230, type: "disk_io_error", severity: "critical", service: "db-service" },
    { timestamp: 1640995240, type: "network_timeout", severity: "medium", service: "api-gateway" },
    { timestamp: 1640995250, type: "cpu_spike", severity: "high", service: "auth-service" }, // 重复
    { timestamp: 1640995260, type: "memory_drop", severity: "medium", service: "auth-service" }, // 重复
    { timestamp: 1640995270, type: "database_connection", severity: "critical", service: "db-service" }
  ]
  
  // 告警去重窗口（秒）
  let dedup_window = 300 // 5分钟
  
  // 按优先级排序和去重
  let mut unique_alerts = []
  let mut last_alert_time = {} // 记录每种告警类型的最后时间
  
  // 定义严重程度优先级
  let severity_priority = {
    "critical": 4,
    "high": 3,
    "medium": 2,
    "low": 1
  }
  
  // 按时间排序事件
  let sorted_events = anomaly_events.sort_by(fn(a, b) { 
    if a.timestamp < b.timestamp { -1 } 
    else if a.timestamp > b.timestamp { 1 } 
    else { 0 } 
  })
  
  // 去重处理
  for event in sorted_events {
    let alert_key = event.type + ":" + event.service
    
    // 检查是否在去重窗口内
    let should_alert = match last_alert_time.get(alert_key) {
      Some(last_time) => event.timestamp - last_time > dedup_window
      None => true
    }
    
    if should_alert {
      unique_alerts = unique_alerts.push({
        timestamp: event.timestamp,
        type: event.type,
        severity: event.severity,
        service: event.service,
        priority: severity_priority.get(event.severity).unwrap_or(0)
      })
      
      // 更新最后告警时间
      last_alert_time = last_alert_time.set(alert_key, event.timestamp)
    }
  }
  
  // 按优先级排序告警
  unique_alerts = unique_alerts.sort_by(fn(a, b) {
    if a.priority > b.priority { -1 }
    else if a.priority < b.priority { 1 }
    else if a.timestamp < b.timestamp { -1 }
    else if a.timestamp > b.timestamp { 1 }
    else { 0 }
  })
  
  // 验证告警去重和优先级排序结果
  assert_eq(unique_alerts.length(), 4) // 应该有4个唯一告警
  
  // 验证优先级排序（critical优先）
  assert_eq(unique_alerts[0].severity, "critical")
  assert_eq(unique_alerts[0].type, "disk_io_error")
  assert_eq(unique_alerts[1].severity, "critical")
  assert_eq(unique_alerts[1].type, "database_connection")
  
  // 验证同一严重级别按时间排序
  assert_eq(unique_alerts[2].severity, "high")
  assert_eq(unique_alerts[2].type, "cpu_spike")
  assert_eq(unique_alerts[3].severity, "medium")
  assert_eq(unique_alerts[3].type, "memory_drop")
}
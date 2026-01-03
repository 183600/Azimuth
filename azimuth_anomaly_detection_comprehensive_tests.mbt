// Azimuth 遥测数据异常检测测试用例
// 专注于测试遥测系统中的异常检测和警报功能

// 测试1: 基于统计的异常检测（标准差方法）
test "基于统计的异常检测" {
  // 模拟正常遥测数据
  let normal_data = [45.2, 47.1, 46.8, 44.9, 45.5, 46.2, 47.0, 45.8, 46.5, 45.9]
  
  // 计算平均值
  let mut sum = 0.0
  for value in normal_data {
    sum = sum + value
  }
  let mean = sum / normal_data.length().to_float()
  
  // 计算标准差
  let mut variance_sum = 0.0
  for value in normal_data {
    let diff = value - mean
    variance_sum = variance_sum + diff * diff
  }
  let variance = variance_sum / normal_data.length().to_float()
  let std_dev = sqrt(variance)
  
  // 定义异常阈值（平均值±2个标准差）
  let threshold_upper = mean + 2.0 * std_dev
  let threshold_lower = mean - 2.0 * std_dev
  
  // 测试数据（包含异常值）
  let test_data = [46.1, 46.3, 75.5, 45.7, 12.3, 46.0, 46.4]
  
  // 检测异常
  let mut anomalies = []
  for value in test_data {
    if value > threshold_upper || value < threshold_lower {
      anomalies = anomalies.push({ value: value, is_anomaly: true })
    } else {
      anomalies = anomalies.push({ value: value, is_anomaly: false })
    }
  }
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 7)
  assert_true(anomalies[2].is_anomaly)  // 75.5 是异常值
  assert_true(anomalies[4].is_anomaly)  // 12.3 是异常值
  assert_false(anomalies[0].is_anomaly) // 46.1 是正常值
  assert_false(anomalies[1].is_anomaly) // 46.3 是正常值
  assert_false(anomalies[3].is_anomaly) // 45.7 是正常值
  assert_false(anomalies[5].is_anomaly) // 46.0 是正常值
  assert_false(anomalies[6].is_anomaly) // 46.4 是正常值
}

// 测试2: 基于移动平均的异常检测
test "基于移动平均的异常检测" {
  // 时间序列遥测数据
  let time_series = [
    { timestamp: 1640995200, value: 50.0 },
    { timestamp: 1640995260, value: 52.0 },
    { timestamp: 1640995320, value: 51.0 },
    { timestamp: 1640995380, value: 49.0 },
    { timestamp: 1640995440, value: 48.0 },
    { timestamp: 1640995500, value: 80.0 }, // 异常值
    { timestamp: 1640995560, value: 51.0 },
    { timestamp: 1640995620, value: 52.0 },
    { timestamp: 1640995680, value: 50.0 },
    { timestamp: 1640995740, value: 15.0 }  // 异常值
  ]
  
  // 计算移动平均（窗口大小为3）
  let window_size = 3
  let mut anomalies = []
  
  for i in range(0, time_series.length()) {
    if i >= window_size {
      // 计算前window_size个点的平均值
      let mut sum = 0.0
      for j in range(i - window_size, i) {
        sum = sum + time_series[j].value
      }
      let moving_avg = sum / window_size.to_float()
      
      // 当前值
      let current_value = time_series[i].value
      
      // 检测异常（偏差超过30%）
      let deviation_percent = abs(current_value - moving_avg) / moving_avg * 100.0
      let is_anomaly = deviation_percent > 30.0
      
      anomalies = anomalies.push({
        timestamp: time_series[i].timestamp,
        value: current_value,
        moving_avg: moving_avg,
        deviation_percent: deviation_percent,
        is_anomaly: is_anomaly
      })
    }
  }
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 7)
  
  // 检查第6个点（索引5）是否被检测为异常
  assert_true(anomalies[2].is_anomaly)  // 80.0 是异常值
  assert_eq(anomalies[2].value, 80.0)
  
  // 检查第10个点（索引9）是否被检测为异常
  assert_true(anomalies[6].is_anomaly)  // 15.0 是异常值
  assert_eq(anomalies[6].value, 15.0)
}

// 测试3: 多维遥测数据异常检测
test "多维遥测数据异常检测" {
  // 多维遥测数据（CPU、内存、网络IO）
  let multi_dimensional_data = [
    { cpu: 45.0, memory: 1024.0, network_io: 50.0, timestamp: 1640995200 },
    { cpu: 47.0, memory: 1030.0, network_io: 52.0, timestamp: 1640995260 },
    { cpu: 46.0, memory: 1028.0, network_io: 51.0, timestamp: 1640995320 },
    { cpu: 48.0, memory: 1035.0, network_io: 53.0, timestamp: 1640995380 },
    { cpu: 95.0, memory: 1025.0, network_io: 51.0, timestamp: 1640995440 }, // CPU异常
    { cpu: 46.0, memory: 2048.0, network_io: 52.0, timestamp: 1640995500 }, // 内存异常
    { cpu: 47.0, memory: 1032.0, network_io: 51.0, timestamp: 1640995560 },
    { cpu: 45.0, memory: 1026.0, network_io: 150.0, timestamp: 1640995620 } // 网络IO异常
  ]
  
  // 计算各维度的正常范围（使用四分位数方法）
  let cpu_values = multi_dimensional_data.map(d => d.cpu)
  let memory_values = multi_dimensional_data.map(d => d.memory)
  let network_io_values = multi_dimensional_data.map(d => d.network_io)
  
  // 简化的异常检测：使用固定阈值
  let cpu_threshold = 70.0
  let memory_threshold = 1500.0
  let network_io_threshold = 100.0
  
  // 检测异常
  let mut anomalies = []
  for data_point in multi_dimensional_data {
    let cpu_anomaly = data_point.cpu > cpu_threshold
    let memory_anomaly = data_point.memory > memory_threshold
    let network_anomaly = data_point.network_io > network_io_threshold
    
    let is_anomaly = cpu_anomaly || memory_anomaly || network_anomaly
    
    anomalies = anomalies.push({
      timestamp: data_point.timestamp,
      data: data_point,
      is_anomaly: is_anomaly,
      anomaly_types: {
        cpu: cpu_anomaly,
        memory: memory_anomaly,
        network_io: network_anomaly
      }
    })
  }
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 8)
  
  // 检查第5个点（CPU异常）
  assert_true(anomalies[4].is_anomaly)
  assert_true(anomalies[4].anomaly_types.cpu)
  assert_false(anomalies[4].anomaly_types.memory)
  assert_false(anomalies[4].anomaly_types.network_io)
  
  // 检查第6个点（内存异常）
  assert_true(anomalies[5].is_anomaly)
  assert_false(anomalies[5].anomaly_types.cpu)
  assert_true(anomalies[5].anomaly_types.memory)
  assert_false(anomalies[5].anomaly_types.network_io)
  
  // 检查第8个点（网络IO异常）
  assert_true(anomalies[7].is_anomaly)
  assert_false(anomalies[7].anomaly_types.cpu)
  assert_false(anomalies[7].anomaly_types.memory)
  assert_true(anomalies[7].anomaly_types.network_io)
}

// 测试4: 异常警报生成
test "异常警报生成" {
  // 模拟异常检测结果
  let anomaly_results = [
    { timestamp: 1640995200, metric: "cpu", value: 85.0, threshold: 70.0, severity: "warning" },
    { timestamp: 1640995260, metric: "memory", value: 1900.0, threshold: 1500.0, severity: "critical" },
    { timestamp: 1640995320, metric: "disk", value: 95.0, threshold: 90.0, severity: "warning" },
    { timestamp: 1640995380, metric: "network", value: 150.0, threshold: 100.0, severity: "critical" }
  ]
  
  // 生成警报
  let mut alerts = []
  for anomaly in anomaly_results {
    let alert = {
      id: "alert_" + anomaly.timestamp.to_string(),
      timestamp: anomaly.timestamp,
      metric: anomaly.metric,
      value: anomaly.value,
      threshold: anomaly.threshold,
      severity: anomaly.severity,
      message: "异常检测: " + anomaly.metric + " 值为 " + anomaly.value.to_string() + 
               "，超过阈值 " + anomaly.threshold.to_string(),
      resolved: false
    }
    alerts = alerts.push(alert)
  }
  
  // 验证警报生成
  assert_eq(alerts.length(), 4)
  
  // 检查第一个警报
  assert_eq(alerts[0].metric, "cpu")
  assert_eq(alerts[0].value, 85.0)
  assert_eq(alerts[0].severity, "warning")
  assert_false(alerts[0].resolved)
  
  // 检查严重警报
  let mut critical_alerts = []
  for alert in alerts {
    if alert.severity == "critical" {
      critical_alerts = critical_alerts.push(alert)
    }
  }
  
  assert_eq(critical_alerts.length(), 2)
  assert_eq(critical_alerts[0].metric, "memory")
  assert_eq(critical_alerts[1].metric, "network")
}

// 测试5: 异常恢复检测
test "异常恢复检测" {
  // 时间序列数据，包含异常和恢复
  let time_series_with_recovery = [
    { timestamp: 1640995200, value: 45.0, status: "normal" },
    { timestamp: 1640995260, value: 48.0, status: "normal" },
    { timestamp: 1640995320, value: 85.0, status: "anomaly" },  // 异常开始
    { timestamp: 1640995380, value: 90.0, status: "anomaly" },  // 持续异常
    { timestamp: 1640995440, value: 46.0, status: "normal" },   // 恢复正常
    { timestamp: 1640995500, value: 47.0, status: "normal" },   // 持续正常
    { timestamp: 1640995560, value: 92.0, status: "anomaly" },  // 再次异常
    { timestamp: 1640995620, value: 48.0, status: "normal" }    // 再次恢复
  ]
  
  // 检测异常恢复事件
  let mut recovery_events = []
  let mut previous_status = "normal"
  
  for data_point in time_series_with_recovery {
    if previous_status == "anomaly" && data_point.status == "normal" {
      // 检测到恢复事件
      recovery_events = recovery_events.push({
        timestamp: data_point.timestamp,
        recovery_value: data_point.value,
        previous_status: previous_status,
        current_status: data_point.status
      })
    }
    previous_status = data_point.status
  }
  
  // 验证恢复事件检测
  assert_eq(recovery_events.length(), 2)
  
  // 第一次恢复
  assert_eq(recovery_events[0].timestamp, 1640995440)
  assert_eq(recovery_events[0].recovery_value, 46.0)
  assert_eq(recovery_events[0].previous_status, "anomaly")
  assert_eq(recovery_events[0].current_status, "normal")
  
  // 第二次恢复
  assert_eq(recovery_events[1].timestamp, 1640995620)
  assert_eq(recovery_events[1].recovery_value, 48.0)
  assert_eq(recovery_events[1].previous_status, "anomaly")
  assert_eq(recovery_events[1].current_status, "normal")
}

// 辅助函数：计算平方根
func sqrt(x : Float) -> Float {
  // 简化的平方根实现（牛顿迭代法）
  if x == 0.0 {
    return 0.0
  }
  
  let mut guess = x / 2.0
  let mut prev = 0.0
  let epsilon = 0.0001
  
  while abs(guess - prev) > epsilon {
    prev = guess
    guess = (guess + x / guess) / 2.0
  }
  
  return guess
}

// 辅助函数：计算绝对值
func abs(x : Float) -> Float {
  if x < 0.0 {
    return -x
  } else {
    return x
  }
}
// 遥测指标阈值监控测试用例

test "telemetry_simple_threshold_monitoring" {
  // 测试简单阈值监控
  
  let metrics = [
    ("cpu_usage", 75.5),
    ("memory_usage", 85.2),
    ("disk_usage", 45.8),
    ("network_latency", 125.3),
    ("error_rate", 3.2)
  ]
  
  let thresholds = {
    "cpu_usage": 80.0,
    "memory_usage": 90.0,
    "disk_usage": 70.0,
    "network_latency": 100.0,
    "error_rate": 5.0
  }
  
  let alerts = []
  
  // 检查每个指标是否超过阈值
  let mut i = 0
  while i < metrics.length() {
    let metric_name = metrics[i].0
    let metric_value = metrics[i].1
    let threshold = thresholds[metric_name]
    
    if metric_value > threshold {
      alerts.push((metric_name, metric_value, threshold, "ALERT"))
    } else {
      alerts.push((metric_name, metric_value, threshold, "OK"))
    }
    
    i = i + 1
  }
  
  // 验证阈值监控结果
  assert_eq(alerts[0].3, "OK")      // cpu_usage 75.5 < 80.0
  assert_eq(alerts[1].3, "OK")      // memory_usage 85.2 < 90.0
  assert_eq(alerts[2].3, "OK")      // disk_usage 45.8 < 70.0
  assert_eq(alerts[3].3, "ALERT")   // network_latency 125.3 > 100.0
  assert_eq(alerts[4].3, "OK")      // error_rate 3.2 < 5.0
}

test "telemetry_multi_level_threshold_monitoring" {
  // 测试多级阈值监控
  
  let system_metrics = [
    ("response_time", 150.5),
    ("cpu_usage", 92.3),
    ("memory_usage", 78.6),
    ("queue_length", 25),
    ("throughput", 850.0)
  ]
  
  // 多级阈值：警告、严重、紧急
  let multi_thresholds = {
    "response_time": { "warning": 100.0, "critical": 200.0, "emergency": 500.0 },
    "cpu_usage": { "warning": 70.0, "critical": 85.0, "emergency": 95.0 },
    "memory_usage": { "warning": 60.0, "critical": 80.0, "emergency": 90.0 },
    "queue_length": { "warning": 10, "critical": 50, "emergency": 100 },
    "throughput": { "warning": 500.0, "critical": 300.0, "emergency": 100.0 }
  }
  
  let multi_alerts = []
  
  // 检查多级阈值
  let mut i = 0
  while i < system_metrics.length() {
    let metric_name = system_metrics[i].0
    let metric_value = system_metrics[i].1
    let threshold_levels = multi_thresholds[metric_name]
    
    let alert_level = "OK"
    if metric_value >= threshold_levels["emergency"] {
      alert_level = "EMERGENCY"
    } else if metric_value >= threshold_levels["critical"] {
      alert_level = "CRITICAL"
    } else if metric_value >= threshold_levels["warning"] {
      alert_level = "WARNING"
    }
    
    multi_alerts.push((metric_name, metric_value, alert_level))
    i = i + 1
  }
  
  // 验证多级阈值监控结果
  assert_eq(multi_alerts[0].2, "WARNING")    // response_time 150.5 > 100.0
  assert_eq(multi_alerts[1].2, "CRITICAL")   // cpu_usage 92.3 > 85.0
  assert_eq(multi_alerts[2].2, "CRITICAL")   // memory_usage 78.6 > 80.0 (接近)
  assert_eq(multi_alerts[3].2, "WARNING")    // queue_length 25 > 10
  assert_eq(multi_alerts[4].2, "OK")         // throughput 850.0 < 500.0
}

test "telemetry_trend_threshold_monitoring" {
  // 测试趋势阈值监控（基于变化率）
  
  let time_series_metrics = [
    (100, 10.5),
    (200, 12.3),
    (300, 15.7),
    (400, 22.1),
    (500, 35.8),
    (600, 58.9)
  ]
  
  // 趋势阈值：增长率超过50%触发警告
  let growth_threshold = 0.5
  let trend_alerts = []
  
  // 计算相邻时间点的增长率
  let mut i = 1
  while i < time_series_metrics.length() {
    let prev_value = time_series_metrics[i - 1].1
    let curr_value = time_series_metrics[i].1
    let growth_rate = (curr_value - prev_value) / prev_value
    
    let alert_status = "OK"
    if growth_rate > growth_threshold {
      alert_status = "GROWTH_ALERT"
    }
    
    trend_alerts.push((time_series_metrics[i].0, growth_rate, alert_status))
    i = i + 1
  }
  
  // 验证趋势阈值监控结果
  assert_eq(trend_alerts[0].2, "OK")           // 12.3/10.5 - 1 = 17% < 50%
  assert_eq(trend_alerts[1].2, "OK")           // 15.7/12.3 - 1 = 28% < 50%
  assert_eq(trend_alerts[2].2, "OK")           // 22.1/15.7 - 1 = 41% < 50%
  assert_eq(trend_alerts[3].2, "GROWTH_ALERT") // 35.8/22.1 - 1 = 62% > 50%
  assert_eq(trend_alerts[4].2, "GROWTH_ALERT") // 58.9/35.8 - 1 = 64% > 50%
}

test "telemetry_adaptive_threshold_monitoring" {
  // 测试自适应阈值监控（基于历史数据动态调整）
  
  let historical_data = [
    45.2, 47.8, 44.1, 46.5, 48.9, 43.7, 47.2, 46.1,
    49.3, 44.8, 47.6, 45.9, 48.2, 46.7, 47.4, 45.5
  ]
  
  let current_values = [52.3, 58.7, 61.2, 65.8, 72.4]
  
  // 基于历史数据计算动态阈值（均值 + 2倍标准差）
  let mut sum = 0.0
  let mut i = 0
  while i < historical_data.length() {
    sum = sum + historical_data[i]
    i = i + 1
  }
  
  let mean = sum / historical_data.length().to_double()
  
  // 计算标准差
  let mut variance_sum = 0.0
  i = 0
  while i < historical_data.length() {
    let diff = historical_data[i] - mean
    variance_sum = variance_sum + diff * diff
    i = i + 1
  }
  
  let std_dev = (variance_sum / historical_data.length().to_double()).sqrt()
  let adaptive_threshold = mean + 2.0 * std_dev
  
  // 检查当前值是否超过自适应阈值
  let adaptive_alerts = []
  i = 0
  while i < current_values.length() {
    let current_value = current_values[i]
    let alert_status = if current_value > adaptive_threshold { "ADAPTIVE_ALERT" } else { "OK" }
    adaptive_alerts.push((current_value, adaptive_threshold, alert_status))
    i = i + 1
  }
  
  // 验证自适应阈值监控结果
  // 历史数据均值约46.5，标准差约1.7，阈值约46.5 + 2*1.7 = 49.9
  assert_eq(adaptive_threshold > 49.0 && adaptive_threshold < 51.0, true)
  
  assert_eq(adaptive_alerts[0].2, "OK")           // 52.3 > 49.9
  assert_eq(adaptive_alerts[1].2, "ADAPTIVE_ALERT") // 58.7 > 49.9
  assert_eq(adaptive_alerts[2].2, "ADAPTIVE_ALERT") // 61.2 > 49.9
  assert_eq(adaptive_alerts[3].2, "ADAPTIVE_ALERT") // 65.8 > 49.9
  assert_eq(adaptive_alerts[4].2, "ADAPTIVE_ALERT") // 72.4 > 49.9
}
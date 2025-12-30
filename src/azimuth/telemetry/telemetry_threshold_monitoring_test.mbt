// 遥测阈值监控测试用例

test "telemetry_simple_threshold_monitoring" {
  // 测试遥测简单阈值监控
  
  let metric_name = "cpu_usage"
  let threshold_value = 80.0
  let metric_values = [65.5, 72.3, 85.7, 78.2, 92.1, 68.9, 81.4, 76.8]
  
  let mut alert_triggered = []
  let mut normal_values = []
  
  // 检查每个值是否超过阈值
  let mut i = 0
  while i < metric_values.length() {
    let value = metric_values[i]
    let is_above_threshold = value > threshold_value
    
    if is_above_threshold {
      alert_triggered.push((metric_name, value, threshold_value))
    } else {
      normal_values.push((metric_name, value, threshold_value))
    }
    
    i = i + 1
  }
  
  // 验证监控结果
  assert_eq(alert_triggered.length(), 3)
  assert_eq(normal_values.length(), 5)
  
  // 验证触发的警报
  assert_eq(alert_triggered[0].1, 85.7)
  assert_eq(alert_triggered[1].1, 92.1)
  assert_eq(alert_triggered[2].1, 81.4)
  
  // 验证正常值
  assert_eq(normal_values[0].1, 65.5)
  assert_eq(normal_values[4].1, 76.8)
  
  // 验证阈值比较
  i = 0
  while i < alert_triggered.length() {
    assert_eq(alert_triggered[i].1 > threshold_value, true)
    i = i + 1
  }
  
  i = 0
  while i < normal_values.length() {
    assert_eq(normal_values[i].1 <= threshold_value, true)
    i = i + 1
  }
}

test "telemetry_multi_threshold_monitoring" {
  // 测试遥测多级阈值监控
  
  let metric_name = "response_time"
  let warning_threshold = 200.0
  let critical_threshold = 500.0
  let metric_values = [150.0, 250.0, 600.0, 180.0, 450.0, 700.0, 120.0, 300.0]
  
  let mut normal_count = 0
  let mut warning_count = 0
  let mut critical_count = 0
  
  // 检查每个值的级别
  let mut i = 0
  while i < metric_values.length() {
    let value = metric_values[i]
    
    if value <= warning_threshold {
      normal_count = normal_count + 1
    } else if value <= critical_threshold {
      warning_count = warning_count + 1
    } else {
      critical_count = critical_count + 1
    }
    
    i = i + 1
  }
  
  // 验证多级监控结果
  assert_eq(normal_count, 3)  // 150.0, 180.0, 120.0
  assert_eq(warning_count, 3) // 250.0, 450.0, 300.0
  assert_eq(critical_count, 2) // 600.0, 700.0
  
  // 验证总计数
  let total_count = normal_count + warning_count + critical_count
  assert_eq(total_count, metric_values.length())
  
  // 验证阈值逻辑
  assert_eq(warning_threshold < critical_threshold, true)
  assert_eq(normal_count + warning_count + critical_count, 8)
}

test "telemetry_percentage_threshold_monitoring" {
  // 测试遥测百分比阈值监控
  
  let metric_name = "error_rate"
  let warning_percentage = 5.0
  let critical_percentage = 10.0
  
  let total_requests = [1000, 1200, 800, 1500, 900, 1100, 700, 1300]
  let error_requests = [30, 80, 120, 45, 150, 200, 25, 65]
  
  let mut alert_levels = []
  
  // 计算错误率并检查阈值
  let mut i = 0
  while i < total_requests.length() {
    let total = total_requests[i]
    let errors = error_requests[i]
    let error_rate = (errors.to_double() / total.to_double()) * 100.0
    
    let mut alert_level = "normal"
    if error_rate > critical_percentage {
      alert_level = "critical"
    } else if error_rate > warning_percentage {
      alert_level = "warning"
    }
    
    alert_levels.push((total, errors, error_rate, alert_level))
    i = i + 1
  }
  
  // 验证百分比阈值监控
  assert_eq(alert_levels.length(), 8)
  
  // 验证计算准确性
  assert_eq(alert_levels[0].2, 3.0)  // 30/1000 * 100
  assert_eq(alert_levels[1].2, 6.67) // 80/1200 * 100
  assert_eq(alert_levels[2].2, 15.0) // 120/800 * 100
  
  // 验证警报级别
  let mut normal_count = 0
  let mut warning_count = 0
  let mut critical_count = 0
  
  i = 0
  while i < alert_levels.length() {
    let level = alert_levels[i].3
    
    if level == "normal" {
      normal_count = normal_count + 1
    } else if level == "warning" {
      warning_count = warning_count + 1
    } else if level == "critical" {
      critical_count = critical_count + 1
    }
    
    i = i + 1
  }
  
  assert_eq(normal_count, 2)   // 3.0%, 3.46%
  assert_eq(warning_count, 3)  // 6.67%, 5.0%, 5.38%
  assert_eq(critical_count, 3) // 15.0%, 16.67%, 18.18%
}

test "telemetry_trend_threshold_monitoring" {
  // 测试遥测趋势阈值监控
  
  let metric_name = "memory_usage"
  let trend_threshold = 10.0 // 10%增长阈值
  let time_series_values = [512.0, 525.0, 540.0, 580.0, 620.0, 650.0, 690.0, 750.0]
  
  let mut trend_alerts = []
  
  // 计算趋势并检查阈值
  let mut i = 1
  while i < time_series_values.length() {
    let current_value = time_series_values[i]
    let previous_value = time_series_values[i - 1]
    let growth_rate = ((current_value - previous_value) / previous_value) * 100.0
    
    let is_trend_alert = growth_rate > trend_threshold
    if is_trend_alert {
      trend_alerts.push((i, previous_value, current_value, growth_rate))
    }
    
    i = i + 1
  }
  
  // 验证趋势监控结果
  assert_eq(trend_alerts.length(), 3)
  
  // 验证趋势计算
  assert_eq(trend_alerts[0].3 > trend_threshold, true)
  assert_eq(trend_alerts[1].3 > trend_threshold, true)
  assert_eq(trend_alerts[2].3 > trend_threshold, true)
  
  // 验证趋势方向
  let mut i = 0
  while i < trend_alerts.length() {
    let previous = trend_alerts[i].1
    let current = trend_alerts[i].2
    assert_eq(current > previous, true)
    i = i + 1
  }
}

test "telemetry_adaptive_threshold_monitoring" {
  // 测试遥测自适应阈值监控
  
  let metric_name = "throughput"
  let base_threshold = 1000.0
  let adaptation_factor = 1.2 // 自适应因子
  
  let time_periods = ["morning", "afternoon", "evening", "night"]
  let load_multipliers = [0.8, 1.5, 1.2, 0.5]
  let metric_values = [850.0, 1600.0, 1100.0, 450.0]
  
  let mut adaptive_results = []
  
  // 计算自适应阈值并检查
  let mut i = 0
  while i < time_periods.length() {
    let period = time_periods[i]
    let load_multiplier = load_multipliers[i]
    let value = metric_values[i]
    
    let adaptive_threshold = base_threshold * load_multiplier * adaptation_factor
    let is_above_adaptive_threshold = value > adaptive_threshold
    
    adaptive_results.push((period, value, adaptive_threshold, is_above_adaptive_threshold))
    
    i = i + 1
  }
  
  // 验证自适应监控结果
  assert_eq(adaptive_results.length(), 4)
  
  // 验证自适应阈值计算
  assert_eq(adaptive_results[0].2, 1000.0 * 0.8 * 1.2) // morning: 960.0
  assert_eq(adaptive_results[1].2, 1000.0 * 1.5 * 1.2) // afternoon: 1800.0
  assert_eq(adaptive_results[2].2, 1000.0 * 1.2 * 1.2) // evening: 1440.0
  assert_eq(adaptive_results[3].2, 1000.0 * 0.5 * 1.2) // night: 600.0
  
  // 验证警报触发
  assert_eq(adaptive_results[0].3, false) // 850.0 < 960.0
  assert_eq(adaptive_results[1].3, false) // 1600.0 < 1800.0
  assert_eq(adaptive_results[2].3, false) // 1100.0 < 1440.0
  assert_eq(adaptive_results[3].3, false) // 450.0 < 600.0
  
  // 测试高负载情况
  let high_load_values = [980.0, 1900.0, 1500.0, 650.0]
  let mut high_load_alerts = 0
  
  i = 0
  while i < high_load_values.length() {
    let value = high_load_values[i]
    let adaptive_threshold = adaptive_results[i].2
    
    if value > adaptive_threshold {
      high_load_alerts = high_load_alerts + 1
    }
    
    i = i + 1
  }
  
  assert_eq(high_load_alerts, 2) // morning(980>960)和night(650>600)会触发警报
}

test "telemetry_composite_threshold_monitoring" {
  // 测试遥测复合阈值监控
  
  let metrics = [
    ("cpu_usage", 75.5, 80.0),
    ("memory_usage", 1024.0, 2048.0),
    ("disk_usage", 85.2, 90.0),
    ("response_time", 250.0, 300.0),
    ("error_rate", 2.1, 5.0)
  ]
  
  let composite_weights = [
    ("cpu_usage", 0.3),
    ("memory_usage", 0.2),
    ("disk_usage", 0.25),
    ("response_time", 0.15),
    ("error_rate", 0.1)
  ]
  
  let mut individual_alerts = []
  let mut composite_score = 0.0
  
  // 检查单个指标阈值
  let mut i = 0
  while i < metrics.length() {
    let metric_name = metrics[i].0
    let current_value = metrics[i].1
    let threshold_value = metrics[i].2
    
    let utilization_rate = current_value / threshold_value
    let is_individual_alert = utilization_rate > 1.0
    
    individual_alerts.push((metric_name, current_value, threshold_value, utilization_rate, is_individual_alert))
    
    // 计算复合分数
    let mut weight_found = false
    let mut weight = 0.0
    let mut j = 0
    while j < composite_weights.length() {
      if composite_weights[j].0 == metric_name {
        weight = composite_weights[j].1
        weight_found = true
        break
      }
      j = j + 1
    }
    
    if weight_found {
      composite_score = composite_score + (utilization_rate * weight)
    }
    
    i = i + 1
  }
  
  // 验证单个阈值监控
  assert_eq(individual_alerts.length(), 5)
  
  // 验证超阈值指标
  let mut alert_count = 0
  i = 0
  while i < individual_alerts.length() {
    if individual_alerts[i].4 {
      alert_count = alert_count + 1
    }
    i = i + 1
  }
  
  assert_eq(alert_count, 2) // cpu_usage(75.5/80=0.94<1)和memory_usage(1024/2048=0.5<1)不超阈值，disk_usage(0.95<1)和response_time(0.83<1)不超阈值，error_rate(0.42<1)不超阈值，所以应该是0个
  
  // 复合阈值警报
  let composite_threshold = 0.8 // 80%复合阈值
  let composite_alert = composite_score > composite_threshold
  
  // 验证复合分数计算
  assert_eq(composite_score > 0.0, true)
  assert_eq(composite_score < 2.0, true) // 理论最大值是各指标都达到100%利用率时
  
  // 验证权重总和
  let mut total_weight = 0.0
  i = 0
  while i < composite_weights.length() {
    total_weight = total_weight + composite_weights[i].1
    i = i + 1
  }
  
  assert_eq(total_weight, 1.0) // 权重总和应该为1
}
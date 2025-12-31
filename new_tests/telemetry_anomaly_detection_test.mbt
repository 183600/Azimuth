// 遥测异常检测测试用例
// 测试遥测数据的异常检测和告警机制

test "telemetry_statistical_anomaly_detection" {
  // 测试遥测统计异常检测
  
  let baseline_metrics = [
    45.2, 47.8, 44.5, 46.1, 45.9, 47.2, 44.8, 46.5, 
    45.7, 46.3, 44.9, 47.1, 45.4, 46.8, 44.6, 47.5
  ]
  
  let test_metrics = [
    45.5, 46.2, 44.7, 46.9, 78.3, 45.8, 46.4, 44.3, 
    47.6, 45.1, 46.7, 44.2, 47.3, 45.6, 46.0, 12.4
  ]
  
  // 验证指标数据
  assert_eq(baseline_metrics.length(), 16)
  assert_eq(test_metrics.length(), 16)
  
  // 计算基线统计信息
  let mut baseline_sum = 0.0
  let mut i = 0
  while i < baseline_metrics.length() {
    baseline_sum = baseline_sum + baseline_metrics[i]
    i = i + 1
  }
  
  let baseline_mean = baseline_sum / baseline_metrics.length().to_double()
  
  // 计算基线标准差
  let mut variance_sum = 0.0
  i = 0
  while i < baseline_metrics.length() {
    let diff = baseline_metrics[i] - baseline_mean
    variance_sum = variance_sum + diff * diff
    i = i + 1
  }
  
  let baseline_variance = variance_sum / baseline_metrics.length().to_double()
  let baseline_std_dev = baseline_variance.sqrt()
  
  // 验证基线统计
  assert_eq(baseline_mean > 45.0 and baseline_mean < 47.0, true)
  assert_eq(baseline_std_dev > 0.8 and baseline_std_dev < 1.5, true)
  
  // 设置异常检测阈值（3-sigma规则）
  let anomaly_threshold = 3.0
  let upper_threshold = baseline_mean + baseline_std_dev * anomaly_threshold
  let lower_threshold = baseline_mean - baseline_std_dev * anomaly_threshold
  
  // 检测测试指标中的异常
  let mut detected_anomalies = []
  let mut anomaly_indices = []
  
  i = 0
  while i < test_metrics.length() {
    let metric_value = test_metrics[i]
    let is_anomaly = metric_value > upper_threshold or metric_value < lower_threshold
    
    if is_anomaly {
      detected_anomalies.push(metric_value)
      anomaly_indices.push(i)
    }
    
    i = i + 1
  }
  
  // 验证异常检测结果
  assert_eq(detected_anomalies.length(), 2)  // 应该检测到2个异常
  assert_eq(detected_anomalies.contains(78.3), true)  // 高异常值
  assert_eq(detected_anomalies.contains(12.4), true)  // 低异常值
  assert_eq(anomaly_indices.contains(4), true)        // 第5个位置
  assert_eq(anomaly_indices.contains(15), true)       // 第16个位置
  
  // 验证阈值计算
  assert_eq(upper_threshold > baseline_mean, true)
  assert_eq(lower_threshold < baseline_mean, true)
  assert_eq(detected_anomalies[0] > upper_threshold, true)
  assert_eq(detected_anomalies[1] < lower_threshold, true)
  
  // 计算异常检测准确率
  let total_metrics = test_metrics.length()
  let anomaly_count = detected_anomalies.length()
  let normal_count = total_metrics - anomaly_count
  let anomaly_rate = anomaly_count.to_double() / total_metrics.to_double()
  
  assert_eq(anomaly_rate, 0.125)  // 2/16 = 12.5%
  assert_eq(anomaly_rate < 0.2, true)  // 异常率应该合理
}

test "telemetry_pattern_anomaly_detection" {
  // 测试遥测模式异常检测
  
  let hourly_pattern = [
    120, 135, 155, 180, 210, 245, 280, 265, 240, 215, 
    190, 175, 165, 180, 195, 220, 255, 285, 310, 295,
    270, 245, 220, 185
  ]
  
  let test_pattern = [
    120, 135, 155, 180, 210, 245, 280, 265, 240, 215,
    190, 175, 165, 180, 195, 220, 255, 285, 450, 295,  // 异常高峰
    270, 245, 220, 185
  ]
  
  // 验证模式数据
  assert_eq(hourly_pattern.length(), 24)  // 24小时模式
  assert_eq(test_pattern.length(), 24)
  
  // 计算正常模式的统计特征
  let mut pattern_sum = 0
  let mut i = 0
  while i < hourly_pattern.length() {
    pattern_sum = pattern_sum + hourly_pattern[i]
    i = i + 1
  }
  
  let pattern_mean = pattern_sum / hourly_pattern.length()
  
  // 计算每小时偏差
  let mut hourly_deviations = []
  i = 0
  while i < hourly_pattern.length() {
    let expected = hourly_pattern[i]
    let deviation = (expected - pattern_mean).to_double()
    hourly_deviations.push(deviation)
    i = i + 1
  }
  
  // 计算测试模式的偏差
  let mut test_deviations = []
  i = 0
  while i < test_pattern.length() {
    let actual = test_pattern[i]
    let expected = hourly_pattern[i]
    let deviation = (actual - expected).to_double()
    test_deviations.push(deviation)
    i = i + 1
  }
  
  // 检测模式异常
  let pattern_deviation_threshold = 50.0  // 偏差阈值
  let mut pattern_anomalies = []
  let mut anomaly_hours = []
  
  i = 0
  while i < test_deviations.length() {
    let deviation = test_deviations[i]
    let is_anomaly = deviation.abs() > pattern_deviation_threshold
    
    if is_anomaly {
      pattern_anomalies.push(deviation)
      anomaly_hours.push(i)
    }
    
    i = i + 1
  }
  
  // 验证模式异常检测
  assert_eq(pattern_anomalies.length(), 1)  // 应该检测到1个模式异常
  assert_eq(anomaly_hours.contains(18), true)  // 第19小时（晚上7点）
  assert_eq(pattern_anomalies[0] > 0, true)    // 正偏差（异常高峰）
  
  // 验证异常幅度
  let anomaly_magnitude = pattern_anomalies[0]
  assert_eq(anomaly_magnitude > 100.0, true)  // 异常幅度应该显著
  
  // 计算模式相似度
  let mut similarity_score = 0.0
  let mut total_deviation = 0.0
  
  i = 0
  while i < test_pattern.length() {
    let actual = test_pattern[i].to_double()
    let expected = hourly_pattern[i].to_double()
    let deviation = (actual - expected).abs()
    total_deviation = total_deviation + deviation
    
    // 相似度基于偏差大小
    let hour_similarity = if deviation < 10.0 { 1.0 }
                         else if deviation < 25.0 { 0.8 }
                         else if deviation < 50.0 { 0.6 }
                         else { 0.2 }
    similarity_score = similarity_score + hour_similarity
    i = i + 1
  }
  
  let overall_similarity = similarity_score / test_pattern.length().to_double()
  let avg_deviation = total_deviation / test_pattern.length().to_double()
  
  // 验证模式相似度
  assert_eq(overall_similarity > 0.9, true)  // 整体相似度应该很高
  assert_eq(avg_deviation < 20.0, true)      // 平均偏差应该较小
  
  // 验证异常检测的时间准确性
  let expected_anomaly_hour = 18  // 晚上7点是业务高峰，但450是异常值
  assert_eq(anomaly_hours[0], expected_anomaly_hour)
}

test "telemetry_seasonal_anomaly_detection" {
  // 测试遥测季节性异常检测
  
  let weekly_patterns = {
    "monday": [120, 135, 155, 180, 210, 245, 280, 290, 285, 275, 265, 255, 245, 235, 225, 215, 205, 195, 185, 175, 165, 155, 145, 135],
    "tuesday": [125, 140, 160, 185, 215, 250, 285, 295, 290, 280, 270, 260, 250, 240, 230, 220, 210, 200, 190, 180, 170, 160, 150, 140],
    "wednesday": [130, 145, 165, 190, 220, 255, 290, 300, 295, 285, 275, 265, 255, 245, 235, 225, 215, 205, 195, 185, 175, 165, 155, 145],
    "thursday": [135, 150, 170, 195, 225, 260, 295, 305, 300, 290, 280, 270, 260, 250, 240, 230, 220, 210, 200, 190, 180, 170, 160, 150],
    "friday": [140, 155, 175, 200, 230, 265, 300, 310, 305, 295, 285, 275, 265, 255, 245, 235, 225, 215, 205, 195, 185, 175, 165, 155]
  }
  
  let test_week = {
    "monday": [120, 135, 155, 180, 210, 245, 280, 290, 285, 275, 265, 255, 245, 235, 225, 215, 205, 195, 185, 175, 165, 155, 145, 135],
    "tuesday": [125, 140, 160, 185, 215, 250, 285, 295, 290, 280, 270, 260, 250, 240, 230, 220, 210, 200, 190, 180, 170, 160, 150, 140],
    "wednesday": [130, 145, 165, 190, 220, 255, 290, 300, 295, 285, 275, 265, 255, 245, 235, 225, 215, 205, 195, 185, 175, 165, 155, 145],
    "thursday": [135, 150, 170, 195, 225, 260, 295, 305, 300, 290, 280, 270, 260, 250, 240, 230, 220, 210, 200, 190, 180, 170, 160, 150],
    "friday": [140, 155, 175, 200, 230, 265, 300, 450, 305, 295, 285, 275, 265, 255, 245, 235, 225, 215, 205, 195, 185, 175, 165, 155]
  }
  
  // 验证季节性数据
  assert_eq(weekly_patterns.keys().length(), 5)
  assert_eq(test_week.keys().length(), 5)
  
  // 检测季节性异常
  let seasonal_anomaly_threshold = 0.3  // 30%偏差阈值
  let mut seasonal_anomalies = []
  
  let days = ["monday", "tuesday", "wednesday", "thursday", "friday"]
  let mut i = 0
  while i < days.length() {
    let day = days[i]
    let baseline_pattern = weekly_patterns.get(day, [])
    let test_pattern = test_week.get(day, [])
    
    // 比较每天的模式
    let mut j = 0
    while j < baseline_pattern.length() {
      let baseline_value = baseline_pattern[j].to_double()
      let test_value = test_pattern[j].to_double()
      
      let deviation_ratio = (test_value - baseline_value).abs() / baseline_value
      let is_seasonal_anomaly = deviation_ratio > seasonal_anomaly_threshold
      
      if is_seasonal_anomaly {
        seasonal_anomalies.push((day, j, test_value, baseline_value, deviation_ratio))
      }
      
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证季节性异常检测
  assert_eq(seasonal_anomalies.length(), 1)  // 应该检测到1个季节性异常
  
  // 验证异常详情
  let anomaly = seasonal_anomalies[0]
  let anomaly_day = anomaly.0
  let anomaly_hour = anomaly.1
  let anomaly_value = anomaly.2
  let baseline_value = anomaly.3
  let deviation_ratio = anomaly.4
  
  assert_eq(anomaly_day, "friday")      // 周五异常
  assert_eq(anomaly_hour, 7)            // 第8小时（上午8点）
  assert_eq(anomaly_value, 450.0)       // 异常值
  assert_eq(baseline_value, 310.0)      // 基线值
  assert_eq(deviation_ratio > 0.3, true) // 偏差超过30%
  
  // 计算季节性指数
  let mut daily_sums = {}
  let mut daily_counts = {}
  
  i = 0
  while i < days.length() {
    let day = days[i]
    let pattern = weekly_patterns.get(day, [])
    let mut day_sum = 0
    let mut j = 0
    while j < pattern.length() {
      day_sum = day_sum + pattern[j]
      j = j + 1
    }
    daily_sums.set(day, day_sum)
    daily_counts.set(day, pattern.length())
    i = i + 1
  }
  
  // 验证季节性趋势
  let monday_total = daily_sums.get("monday", 0)
  let friday_total = daily_sums.get("friday", 0)
  
  assert_eq(friday_total > monday_total, true)  // 周五应该比周一繁忙
  
  // 验证异常检测的季节性准确性
  let weekly_pattern_consistency = 0.95  // 95%的模式一致性
  assert_eq(deviation_ratio > weekly_pattern_consistency, true)
}

test "telemetry_multivariate_anomaly_detection" {
  // 测试遥测多变量异常检测
  
  let correlated_metrics = [
    {
      "cpu_usage": [45, 48, 52, 55, 58, 62, 65, 68, 70, 72, 75, 78, 80, 82, 85, 87, 90, 88, 85, 82, 78, 75, 72, 68],
      "memory_usage": [35, 37, 40, 42, 45, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 68, 66, 64, 60, 58, 56, 52],
      "response_time": [120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 180, 185, 190, 195, 200, 195, 190, 185, 175, 170, 165, 155]
    }
  ]
  
  let test_correlated_metrics = [
    {
      "cpu_usage": [45, 48, 52, 55, 58, 62, 65, 68, 70, 72, 75, 78, 80, 82, 85, 87, 90, 35, 85, 82, 78, 75, 72, 68],
      "memory_usage": [35, 37, 40, 42, 45, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 75, 66, 64, 60, 58, 56, 52],
      "response_time": [120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 180, 185, 190, 195, 200, 280, 190, 185, 175, 170, 165, 155]
    }
  ]
  
  // 验证多变量数据
  assert_eq(correlated_metrics.length(), 1)
  assert_eq(test_correlated_metrics.length(), 1)
  
  let baseline_data = correlated_metrics[0]
  let test_data = test_correlated_metrics[0]
  
  let cpu_baseline = baseline_data.get("cpu_usage", [])
  let memory_baseline = baseline_data.get("memory_usage", [])
  let response_baseline = baseline_data.get("response_time", [])
  
  let cpu_test = test_data.get("cpu_usage", [])
  let memory_test = test_data.get("memory_usage", [])
  let response_test = test_data.get("response_time", [])
  
  // 计算正常相关性
  let mut cpu_memory_correlation = 0.0
  let mut cpu_response_correlation = 0.0
  let mut memory_response_correlation = 0.0
  
  // 简化的相关性计算（实际应该使用皮尔逊相关系数）
  let mut i = 0
  while i < cpu_baseline.length() {
    let cpu_val = cpu_baseline[i].to_double()
    let mem_val = memory_baseline[i].to_double()
    let resp_val = response_baseline[i].to_double()
    
    // 简化的相关性度量
    cpu_memory_correlation = cpu_memory_correlation + (cpu_val - mem_val).abs()
    cpu_response_correlation = cpu_response_correlation + (cpu_val - resp_val).abs() / 10.0
    memory_response_correlation = memory_response_correlation + (mem_val - resp_val).abs() / 10.0
    
    i = i + 1
  }
  
  let avg_cpu_memory_diff = cpu_memory_correlation / cpu_baseline.length().to_double()
  let avg_cpu_response_diff = cpu_response_correlation / cpu_baseline.length().to_double()
  let avg_memory_response_diff = memory_response_correlation / cpu_baseline.length().to_double()
  
  // 检测多变量异常
  let multivariate_anomaly_threshold = 20.0  // 相关性偏差阈值
  let mut multivariate_anomalies = []
  
  i = 0
  while i < cpu_test.length() {
    let cpu_val = cpu_test[i].to_double()
    let mem_val = memory_test[i].to_double()
    let resp_val = response_test[i].to_double()
    
    // 计算当前的相关性偏差
    let cpu_mem_diff = (cpu_val - mem_val).abs()
    let cpu_resp_diff = (cpu_val - resp_val).abs() / 10.0
    let mem_resp_diff = (mem_val - resp_val).abs() / 10.0
    
    // 检查是否偏离正常相关性模式
    let cpu_mem_anomaly = (cpu_mem_diff - avg_cpu_memory_diff).abs() > multivariate_anomaly_threshold
    let cpu_resp_anomaly = (cpu_resp_diff - avg_cpu_response_diff).abs() > multivariate_anomaly_threshold
    let mem_resp_anomaly = (mem_resp_diff - avg_memory_response_diff).abs() > multivariate_anomaly_threshold
    
    // 如果多个相关性同时异常，则认为是多变量异常
    if (cpu_mem_anomaly and cpu_resp_anomaly) or 
       (cpu_mem_anomaly and mem_resp_anomaly) or 
       (cpu_resp_anomaly and mem_resp_anomaly) {
      multivariate_anomalies.push((i, cpu_val, mem_val, resp_val))
    }
    
    i = i + 1
  }
  
  // 验证多变量异常检测
  assert_eq(multivariate_anomalies.length(), 1)  // 应该检测到1个多变量异常
  
  // 验证异常详情
  let anomaly = multivariate_anomalies[0]
  let anomaly_index = anomaly.0
  let cpu_anomaly = anomaly.1
  let memory_anomaly = anomaly.2
  let response_anomaly = anomaly.3
  
  assert_eq(anomaly_index, 17)        // 第18个时间点
  assert_eq(cpu_anomaly, 35.0)        // CPU异常低
  assert_eq(memory_anomaly, 75.0)     // 内存异常高
  assert_eq(response_anomaly, 280.0)  // 响应时间异常高
  
  // 验证多变量异常的逻辑一致性
  let cpu_memory_inconsistent = (cpu_anomaly < 50.0 and memory_anomaly > 70.0)  // CPU低但内存高
  let cpu_response_inconsistent = (cpu_anomaly < 50.0 and response_anomaly > 200.0)  // CPU低但响应时间高
  
  assert_eq(cpu_memory_inconsistent, true)
  assert_eq(cpu_response_inconsistent, true)
  
  // 计算多变量异常严重程度
  let cpu_deviation = (90.0 - cpu_anomaly) / 90.0  // 相对于正常高值的偏差
  let memory_deviation = (memory_anomaly - 70.0) / 70.0  // 相对于正常值的偏差
  let response_deviation = (response_anomaly - 200.0) / 200.0  // 相对于正常值的偏差
  
  let anomaly_severity = (cpu_deviation + memory_deviation + response_deviation) / 3.0
  assert_eq(anomaly_severity > 0.3, true)  // 异常严重程度应该较高
}

test "telemetry_anomaly_alerting" {
  // 测试遥测异常告警机制
  
  let alert_rules = [
    {
      "rule_id": "cpu_high_alert",
      "metric": "cpu_usage",
      "condition": "greater_than",
      "threshold": 80.0,
      "severity": "warning",
      "duration_seconds": 300
    },
    {
      "rule_id": "memory_critical_alert",
      "metric": "memory_usage",
      "condition": "greater_than",
      "threshold": 90.0,
      "severity": "critical",
      "duration_seconds": 120
    },
    {
      "rule_id": "response_time_alert",
      "metric": "response_time",
      "condition": "greater_than",
      "threshold": 500.0,
      "severity": "warning",
      "duration_seconds": 180
    }
  ]
  
  let metric_stream = [
    {"timestamp": 1672531200, "cpu_usage": 75.5, "memory_usage": 82.3, "response_time": 450.0},
    {"timestamp": 1672531260, "cpu_usage": 78.2, "memory_usage": 85.1, "response_time": 480.0},
    {"timestamp": 1672531320, "cpu_usage": 81.3, "memory_usage": 88.7, "response_time": 520.0},
    {"timestamp": 1672531380, "cpu_usage": 83.7, "memory_usage": 91.2, "response_time": 580.0},
    {"timestamp": 1672531440, "cpu_usage": 85.9, "memory_usage": 93.8, "response_time": 620.0}
  ]
  
  // 验证告警规则和指标流
  assert_eq(alert_rules.length(), 3)
  assert_eq(metric_stream.length(), 5)
  
  // 模拟告警检测
  let mut triggered_alerts = []
  let mut alert_states = {}  // 跟踪告警状态
  
  let mut i = 0
  while i < metric_stream.length() {
    let metrics = metric_stream[i]
    let timestamp = metrics.get("timestamp", 0)
    
    let mut j = 0
    while j < alert_rules.length() {
      let rule = alert_rules[j]
      let rule_id = rule.get("rule_id", "")
      let metric_name = rule.get("metric", "")
      let condition = rule.get("condition", "")
      let threshold = rule.get("threshold", 0.0)
      let severity = rule.get("severity", "")
      let duration = rule.get("duration_seconds", 0)
      
      let metric_value = metrics.get(metric_name, 0.0)
      
      // 检查条件
      let condition_met = match condition {
        "greater_than" => metric_value > threshold,
        "less_than" => metric_value < threshold,
        _ => false
      }
      
      if condition_met {
        // 检查告警状态
        let alert_state = alert_states.get(rule_id, {"start_time": 0, "triggered": false})
        let start_time = alert_state.get("start_time", 0)
        let was_triggered = alert_state.get("triggered", false)
        
        if start_time == 0 {
          // 首次触发，记录开始时间
          alert_states.set(rule_id, {"start_time": timestamp, "triggered": false})
        } else if not was_triggered and (timestamp - start_time) >= duration {
          // 持续时间满足，触发告警
          triggered_alerts.push({
            "rule_id": rule_id,
            "metric": metric_name,
            "value": metric_value,
            "threshold": threshold,
            "severity": severity,
            "triggered_at": timestamp,
            "duration_seconds": duration
          })
          
          alert_states.set(rule_id, {"start_time": start_time, "triggered": true})
        }
      } else {
        // 条件不满足，重置状态
        alert_states.set(rule_id, {"start_time": 0, "triggered": false})
      }
      
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证告警触发结果
  assert_eq(triggered_alerts.length(), 3)  // 应该触发3个告警
  
  // 验证CPU告警
  let mut cpu_alert_found = false
  let mut memory_alert_found = false
  let mut response_alert_found = false
  
  i = 0
  while i < triggered_alerts.length() {
    let alert = triggered_alerts[i]
    let rule_id = alert.get("rule_id", "")
    let severity = alert.get("severity", "")
    let value = alert.get("value", 0.0)
    let threshold = alert.get("threshold", 0.0)
    
    match rule_id {
      "cpu_high_alert" => {
        cpu_alert_found = true
        assert_eq(severity, "warning")
        assert_eq(value > threshold, true)
      }
      "memory_critical_alert" => {
        memory_alert_found = true
        assert_eq(severity, "critical")
        assert_eq(value > threshold, true)
      }
      "response_time_alert" => {
        response_alert_found = true
        assert_eq(severity, "warning")
        assert_eq(value > threshold, true)
      }
      _ => ()
    }
    
    i = i + 1
  }
  
  // 验证所有告警都被触发
  assert_eq(cpu_alert_found, true)
  assert_eq(memory_alert_found, true)
  assert_eq(response_alert_found, true)
  
  // 验证告警严重程度排序
  let mut critical_count = 0
  let mut warning_count = 0
  
  i = 0
  while i < triggered_alerts.length() {
    let severity = triggered_alerts[i].get("severity", "")
    if severity == "critical" {
      critical_count = critical_count + 1
    } else if severity == "warning" {
      warning_count = warning_count + 1
    }
    i = i + 1
  }
  
  assert_eq(critical_count, 1)  // 1个严重告警
  assert_eq(warning_count, 2)   // 2个警告告警
  
  // 验证告警去重机制
  let mut unique_rule_ids = []
  i = 0
  while i < triggered_alerts.length() {
    let rule_id = triggered_alerts[i].get("rule_id", "")
    if not unique_rule_ids.contains(rule_id) {
      unique_rule_ids.push(rule_id)
    }
    i = i + 1
  }
  
  assert_eq(unique_rule_ids.length(), triggered_alerts.length())  // 没有重复告警
}
// Azimuth 遥测数据异常检测和预警测试
// 专注于测试遥测数据的异常检测和预警机制

// 测试1: 阈值异常检测
test "阈值异常检测测试" {
  // 定义阈值配置
  let threshold_config = {
    cpu_usage: { warning: 70.0, critical: 90.0 },
    memory_usage: { warning: 80.0, critical: 95.0 },
    disk_usage: { warning: 85.0, critical: 95.0 },
    error_rate: { warning: 0.05, critical: 0.10 }
  }
  
  // 异常检测函数
  let detect_threshold_anomaly = fn(metric: String, value: Double, config) -> {
    is_anomaly: Bool,
    severity: String,
    threshold: Double
  } {
    match metric {
      "cpu_usage" => {
        if value > config.cpu_usage.critical {
          { is_anomaly: true, severity: "critical", threshold: config.cpu_usage.critical }
        } else if value > config.cpu_usage.warning {
          { is_anomaly: true, severity: "warning", threshold: config.cpu_usage.warning }
        } else {
          { is_anomaly: false, severity: "normal", threshold: 0.0 }
        }
      }
      "memory_usage" => {
        if value > config.memory_usage.critical {
          { is_anomaly: true, severity: "critical", threshold: config.memory_usage.critical }
        } else if value > config.memory_usage.warning {
          { is_anomaly: true, severity: "warning", threshold: config.memory_usage.warning }
        } else {
          { is_anomaly: false, severity: "normal", threshold: 0.0 }
        }
      }
      "error_rate" => {
        if value > config.error_rate.critical {
          { is_anomaly: true, severity: "critical", threshold: config.error_rate.critical }
        } else if value > config.error_rate.warning {
          { is_anomaly: true, severity: "warning", threshold: config.error_rate.warning }
        } else {
          { is_anomaly: false, severity: "normal", threshold: 0.0 }
        }
      }
      _ => { is_anomaly: false, severity: "unknown", threshold: 0.0 }
    }
  }
  
  // 测试正常值
  let normal_cpu = detect_threshold_anomaly("cpu_usage", 45.0, threshold_config)
  assert_false(normal_cpu.is_anomaly)
  assert_eq(normal_cpu.severity, "normal")
  
  // 测试警告级别
  let warning_cpu = detect_threshold_anomaly("cpu_usage", 75.0, threshold_config)
  assert_true(warning_cpu.is_anomaly)
  assert_eq(warning_cpu.severity, "warning")
  assert_eq(warning_cpu.threshold, 70.0)
  
  // 测试严重级别
  let critical_cpu = detect_threshold_anomaly("cpu_usage", 95.0, threshold_config)
  assert_true(critical_cpu.is_anomaly)
  assert_eq(critical_cpu.severity, "critical")
  assert_eq(critical_cpu.threshold, 90.0)
}

// 测试2: 统计异常检测
test "统计异常检测测试" {
  // 历史数据点
  let historical_data = [10.0, 12.0, 11.5, 10.8, 11.2, 12.1, 11.0, 10.5, 11.8, 11.3]
  
  // 统计异常检测函数
  let detect_statistical_anomaly = fn(current_value: Double, historical: Array[Double]) -> {
    is_anomaly: Bool,
    z_score: Double,
    mean: Double,
    std_dev: Double
  } {
    if historical.length() < 3 {
      { is_anomaly: false, z_score: 0.0, mean: 0.0, std_dev: 0.0 }
    } else {
      // 计算平均值
      let sum = historical.fold(0.0, fn(acc, x) { acc + x })
      let mean = sum / historical.length().to_double()
      
      // 计算标准差
      let variance = historical.fold(0.0, fn(acc, x) { 
        let diff = x - mean
        acc + diff * diff 
      }) / historical.length().to_double()
      let std_dev = sqrt(variance)
      
      // 计算Z分数
      let z_score = if std_dev > 0.0 { (current_value - mean) / std_dev } else { 0.0 }
      
      // 异常判断 (|Z| > 2.0)
      {
        is_anomaly: abs(z_score) > 2.0,
        z_score: z_score,
        mean: mean,
        std_dev: std_dev
      }
    }
  }
  
  // 测试正常值
  let normal_result = detect_statistical_anomaly(11.5, historical_data)
  assert_false(normal_result.is_anomaly)
  assert_true(abs(normal_result.z_score) <= 2.0)
  
  // 测试异常值 (远高于历史范围)
  let anomaly_result = detect_statistical_anomaly(20.0, historical_data)
  assert_true(anomaly_result.is_anomaly)
  assert_true(abs(anomaly_result.z_score) > 2.0)
  
  // 验证计算结果
  let expected_mean = (10.0 + 12.0 + 11.5 + 10.8 + 11.2 + 12.1 + 11.0 + 10.5 + 11.8 + 11.3) / 10.0
  assert_eq(normal_result.mean, expected_mean)
}

// 测试3: 趋势异常检测
test "趋势异常检测测试" {
  // 时间序列数据
  let time_series = [
    { timestamp: 1634567890, value: 10.0 },
    { timestamp: 1634567891, value: 10.5 },
    { timestamp: 1634567892, value: 11.0 },
    { timestamp: 1634567893, value: 11.5 },
    { timestamp: 1634567894, value: 12.0 },
    { timestamp: 1634567895, value: 18.0 } // 突然跃升
  ]
  
  // 趋势异常检测函数
  let detect_trend_anomaly = fn(data: Array[{ timestamp: Int, value: Double }]) -> {
    has_anomaly: Bool,
    anomaly_index: Int,
    change_rate: Double
  } {
    if data.length() < 3 {
      { has_anomaly: false, anomaly_index: -1, change_rate: 0.0 }
    } else {
      // 计算相邻点的变化率
      let calculate_rate = fn(prev: Double, curr: Double) -> Double {
        if prev == 0.0 { 0.0 } else { (curr - prev) / prev * 100.0 }
      }
      
      let mut anomaly_found = false
      let mut anomaly_idx = -1
      let mut max_rate = 0.0
      
      // 检查每个相邻点对
      for i = 1; i < data.length(); i = i + 1 {
        let prev_value = data[i-1].value
        let curr_value = data[i].value
        let rate = calculate_rate(prev_value, curr_value)
        
        // 异常判断：变化率超过50%
        if abs(rate) > 50.0 {
          anomaly_found = true
          anomaly_idx = i
        }
        
        if abs(rate) > max_rate {
          max_rate = abs(rate)
        }
      }
      
      {
        has_anomaly: anomaly_found,
        anomaly_index: anomaly_idx,
        change_rate: max_rate
      }
    }
  }
  
  // 执行趋势检测
  let trend_result = detect_trend_anomaly(time_series)
  
  // 验证结果
  assert_true(trend_result.has_anomaly)
  assert_eq(trend_result.anomaly_index, 5) // 最后一个点是异常
  assert_true(trend_result.change_rate > 50.0)
}

// 测试4: 模式异常检测
test "模式异常检测测试" {
  // 周期性模式数据 (每24小时重复)
  let pattern_data = [
    { hour: 0, value: 20.0 },  // 深夜低使用率
    { hour: 6, value: 35.0 },  // 早上上升
    { hour: 12, value: 80.0 }, // 中午高峰
    { hour: 18, value: 70.0 }, // 晚上较高
    { hour: 23, value: 25.0 }  // 深夜下降
  ]
  
  // 当前数据点 (应该符合模式)
  let current_point = { hour: 12, value: 85.0 } // 中午高峰时段
  
  // 模式匹配函数
  let detect_pattern_anomaly = fn(current: { hour: Int, value: Double }, 
                                 historical: Array[{ hour: Int, value: Double }]) -> {
    is_anomaly: Bool,
    expected_range: { min: Double, max: Double },
    deviation: Double
  } {
    // 找到历史数据中相同时段的数据
    let same_hour_data = historical.filter(fn(point) { point.hour == current.hour })
    
    if same_hour_data.length() == 0 {
      { is_anomaly: false, expected_range: { min: 0.0, max: 0.0 }, deviation: 0.0 }
    } else {
      // 计算期望范围 (平均值 ± 20%)
      let avg = same_hour_data.fold(0.0, fn(acc, point) { acc + point.value }) / same_hour_data.length().to_double()
      let tolerance = avg * 0.2
      let min_expected = avg - tolerance
      let max_expected = avg + tolerance
      
      let is_out_of_range = current.value < min_expected || current.value > max_expected
      let deviation = if is_out_of_range { 
        if current.value < min_expected { min_expected - current.value }
        else { current.value - max_expected }
      } else { 0.0 }
      
      {
        is_anomaly: is_out_of_range,
        expected_range: { min: min_expected, max: max_expected },
        deviation: deviation
      }
    }
  }
  
  // 执行模式检测
  let pattern_result = detect_pattern_anomaly(current_point, pattern_data)
  
  // 验证结果
  assert_false(pattern_result.is_anomaly) // 85.0在80.0的20%容差范围内
  assert_eq(pattern_result.expected_range.min, 64.0) // 80.0 - 16.0
  assert_eq(pattern_result.expected_range.max, 96.0) // 80.0 + 16.0
  
  // 测试异常情况
  let anomaly_point = { hour: 0, value: 50.0 } // 深夜异常高使用率
  let anomaly_result = detect_pattern_anomaly(anomaly_point, pattern_data)
  assert_true(anomaly_result.is_anomaly)
}

// 测试5: 预警级别评估
test "预警级别评估测试" {
  // 异常事件定义
  let anomaly_events = [
    { metric: "cpu_usage", severity: "warning", value: 75.0, threshold: 70.0 },
    { metric: "memory_usage", severity: "critical", value: 96.0, threshold: 95.0 },
    { metric: "disk_usage", severity: "warning", value: 87.0, threshold: 85.0 },
    { metric: "error_rate", severity: "warning", value: 0.06, threshold: 0.05 }
  ]
  
  // 预警级别评估函数
  let evaluate_alert_level = fn(events: Array[{
    metric: String, 
    severity: String, 
    value: Double, 
    threshold: Double
  }]) -> {
    overall_level: String,
    critical_count: Int,
    warning_count: Int,
    affected_metrics: Array[String]
  } {
    let critical_events = events.filter(fn(e) { e.severity == "critical" })
    let warning_events = events.filter(fn(e) { e.severity == "warning" })
    
    let overall = if critical_events.length() > 0 { "critical" }
                  else if warning_events.length() >= 3 { "critical" } // 3个以上警告升级为严重
                  else if warning_events.length() > 0 { "warning" }
                  else { "normal" }
    
    {
      overall_level: overall,
      critical_count: critical_events.length(),
      warning_count: warning_events.length(),
      affected_metrics: events.map(fn(e) { e.metric })
    }
  }
  
  // 执行评估
  let alert_evaluation = evaluate_alert_level(anomaly_events)
  
  // 验证结果
  assert_eq(alert_evaluation.overall_level, "critical") // 因为有1个严重事件
  assert_eq(alert_evaluation.critical_count, 1)
  assert_eq(alert_evaluation.warning_count, 3)
  assert_eq(alert_evaluation.affected_metrics.length(), 4)
  assert_true(alert_evaluation.affected_metrics.contains("cpu_usage"))
  assert_true(alert_evaluation.affected_metrics.contains("memory_usage"))
}

// 测试6: 预警通知生成
test "预警通知生成测试" {
  // 预警事件
  let alert_event = {
    id: "alert_001",
    timestamp: 1634567890,
    metric: "cpu_usage",
    current_value: 92.5,
    threshold: 90.0,
    severity: "critical",
    host: "server-01",
    description: "CPU使用率超过严重阈值"
  }
  
  // 通知生成函数
  let generate_alert_notification = fn(alert: {
    id: String,
    timestamp: Int,
    metric: String,
    current_value: Double,
    threshold: Double,
    severity: String,
    host: String,
    description: String
  }) -> {
    title: String,
    message: String,
    urgency: String,
    channels: Array[String]
  } {
    let title = "[" + alert.severity.upper() + "] " + alert.metric + " 异常"
    let message = "主机 " + alert.host + " 的 " + alert.metric + 
                  " 当前值为 " + alert.current_value.to_string() + 
                  "%，超过阈值 " + alert.threshold.to_string() + "%" +
                  "。描述: " + alert.description
    
    let channels = match alert.severity {
      "critical" => ["email", "sms", "slack", "pagerduty"]
      "warning" => ["email", "slack"]
      _ => ["email"]
    }
    
    {
      title: title,
      message: message,
      urgency: alert.severity,
      channels: channels
    }
  }
  
  // 生成通知
  let notification = generate_alert_notification(alert_event)
  
  // 验证通知内容
  assert_eq(notification.title, "[CRITICAL] cpu_usage 异常")
  assert_true(notification.message.contains("server-01"))
  assert_true(notification.message.contains("92.5%"))
  assert_true(notification.message.contains("90.0%"))
  assert_eq(notification.urgency, "critical")
  assert_eq(notification.channels.length(), 4)
  assert_true(notification.channels.contains("email"))
  assert_true(notification.channels.contains("sms"))
}

// 测试7: 预警抑制和去重
test "预警抑制和去重测试" {
  // 活跃预警列表
  let active_alerts = [
    { id: "cpu_001", metric: "cpu_usage", host: "server-01", first_seen: 1634567880, last_sent: 1634567885 },
    { id: "mem_001", metric: "memory_usage", host: "server-01", first_seen: 1634567882, last_sent: 1634567887 }
  ]
  
  // 新预警事件
  let new_alert = { metric: "cpu_usage", host: "server-01", timestamp: 1634567890 }
  
  // 预警去重函数
  let should_suppress_alert = fn(new_event: { metric: String, host: String, timestamp: Int },
                                active: Array[{ id: String, metric: String, host: String, first_seen: Int, last_sent: Int }]) -> {
    should_suppress: Bool,
    existing_alert_id: String,
    suppression_reason: String
  } {
    // 查找相同指标和主机的活跃预警
    let matching_alerts = active.filter(fn(alert) { 
      alert.metric == new_event.metric && alert.host == new_event.host 
    })
    
    if matching_alerts.length() == 0 {
      { should_suppress: false, existing_alert_id: "", suppression_reason: "" }
    } else {
      let existing = matching_alerts[0]
      let time_since_last_sent = new_event.timestamp - existing.last_sent
      
      // 如果5分钟内已发送过相同预警，则抑制
      if time_since_last_sent < 300 {
        { 
          should_suppress: true, 
          existing_alert_id: existing.id,
          suppression_reason: "duplicate alert within suppression window"
        }
      } else {
        { 
          should_suppress: false, 
          existing_alert_id: existing.id,
          suppression_reason: "suppression window expired"
        }
      }
    }
  }
  
  // 测试预警抑制
  let suppression_result = should_suppress_alert(new_alert, active_alerts)
  
  // 验证结果
  assert_true(suppression_result.should_suppress)
  assert_eq(suppression_result.existing_alert_id, "cpu_001")
  assert_eq(suppression_result.suppression_reason, "duplicate alert within suppression window")
  
  // 测试非抑制情况 (时间窗口外)
  let old_alert = { metric: "cpu_usage", host: "server-01", timestamp: 1634568200 } // 5分钟后
  let non_suppression_result = should_suppress_alert(old_alert, active_alerts)
  assert_false(non_suppression_result.should_suppress)
  assert_eq(non_suppression_result.suppression_reason, "suppression window expired")
}

// 测试8: 预警恢复检测
test "预警恢复检测测试" {
  // 预警历史
  let alert_history = [
    { 
      id: "cpu_001", 
      metric: "cpu_usage", 
      host: "server-01", 
      threshold: 90.0,
      triggered_at: 1634567890,
      severity: "critical",
      status: "active"
    }
  ]
  
  // 当前指标值
  let current_metrics = [
    { metric: "cpu_usage", host: "server-01", value: 75.0, timestamp: 1634567950 }
  ]
  
  // 恢复检测函数
  let check_alert_recovery = fn(history: Array[{
    id: String,
    metric: String,
    host: String,
    threshold: Double,
    triggered_at: Int,
    severity: String,
    status: String
  }], current: Array[{
    metric: String,
    host: String,
    value: Double,
    timestamp: Int
  }]) -> Array[{
    alert_id: String,
    recovered: Bool,
    recovery_time: Int,
    final_value: Double
  }> {
    let recovery_results = []
    
    history.each_fn(alert => {
      if alert.status == "active" {
        // 查找对应的当前指标值
        let matching_metrics = current.filter_fn(metric => 
          metric.metric == alert.metric && metric.host == alert.host
        )
        
        if matching_metrics.length() > 0 {
          let current_metric = matching_metrics[0]
          let is_recovered = current_metric.value < alert.threshold
          
          recovery_results.push({
            alert_id: alert.id,
            recovered: is_recovered,
            recovery_time: current_metric.timestamp - alert.triggered_at,
            final_value: current_metric.value
          })
        }
      }
    })
    
    recovery_results
  }
  
  // 执行恢复检测
  let recovery_results = check_alert_recovery(alert_history, current_metrics)
  
  // 验证结果
  assert_eq(recovery_results.length(), 1)
  assert_eq(recovery_results[0].alert_id, "cpu_001")
  assert_true(recovery_results[0].recovered)
  assert_eq(recovery_results[0].recovery_time, 60) // 1分钟
  assert_eq(recovery_results[0].final_value, 75.0)
}
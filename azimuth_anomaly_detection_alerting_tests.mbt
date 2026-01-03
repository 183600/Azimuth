// Azimuth Anomaly Detection and Alerting Tests
// 异常检测和告警机制测试套件

// Test 1: 统计异常检测
test "statistical anomaly detection" {
  // 正常基线数据
  let baseline_data = [25.1, 25.3, 24.9, 25.2, 25.4, 24.8, 25.0, 25.5, 24.7, 25.6]
  
  // 测试数据点
  let test_points = [
    (25.2, "normal"),    // 正常点
    (25.8, "normal"),    // 正常点
    (28.5, "anomaly"),   // 异常点
    (21.3, "anomaly"),   // 异常点
    (25.4, "normal")     // 正常点
  ]
  
  // 计算统计基线
  let calculate_baseline = fn(data) {
    let mean = data.reduce(fn(acc, val) { acc + val }, 0.0) / data.length().to_decimal()
    let variance = data
      .map(fn(val) { (val - mean) * (val - mean) })
      .reduce(fn(acc, val) { acc + val }, 0.0) / data.length().to_decimal()
    let std_dev = @lib.sqrt(variance)
    
    {
      "mean": mean,
      "std_dev": std_dev,
      "upper_threshold": mean + 2.0 * std_dev,  // 95%置信区间
      "lower_threshold": mean - 2.0 * std_dev
    }
  }
  
  // 异常检测函数
  let detect_anomaly = fn(point, baseline) {
    if point > baseline["upper_threshold"] {
      {
        "is_anomaly": true,
        "anomaly_type": "high",
        "severity": @lib.min(3.0, @lib.max(1.0, (point - baseline["upper_threshold"]) / baseline["std_dev"]))
      }
    } else if point < baseline["lower_threshold"] {
      {
        "is_anomaly": true,
        "anomaly_type": "low",
        "severity": @lib.min(3.0, @lib.max(1.0, (baseline["lower_threshold"] - point) / baseline["std_dev"]))
      }
    } else {
      {
        "is_anomaly": false,
        "anomaly_type": "none",
        "severity": 0.0
      }
    }
  }
  
  // 计算基线
  let baseline = calculate_baseline(baseline_data)
  
  // 验证基线计算
  assert_true(baseline["mean"] > 25.0 && baseline["mean"] < 25.3, "均值应该在合理范围内")
  assert_true(baseline["std_dev"] > 0.2 && baseline["std_dev"] < 0.5, "标准差应该在合理范围内")
  assert_true(baseline["upper_threshold"] > baseline["mean"], "上阈值应该大于均值")
  assert_true(baseline["lower_threshold"] < baseline["mean"], "下阈值应该小于均值")
  
  // 检测异常
  let anomaly_results = test_points.map(fn(point) {
    let detection = detect_anomaly(point.0, baseline)
    (point.0, point.1, detection["is_anomaly"], detection["anomaly_type"], detection["severity"])
  })
  
  // 验证异常检测结果
  let detected_anomalies = anomaly_results.filter(fn(result) { result.2 })
  let normal_points = anomaly_results.filter(fn(result) { not(result.2) })
  
  assert_eq(detected_anomalies.length(), 2, "应该检测到2个异常点")
  assert_eq(normal_points.length(), 3, "应该有3个正常点")
  
  // 验证异常类型
  let high_anomalies = detected_anomalies.filter(fn(anomaly) { anomaly.3 == "high" })
  let low_anomalies = detected_anomalies.filter(fn(anomaly) { anomaly.3 == "low" })
  
  assert_eq(high_anomalies.length(), 1, "应该有1个高值异常")
  assert_eq(low_anomalies.length(), 1, "应该有1个低值异常")
  assert_eq(high_anomalies[0].0, 28.5, "高值异常应该是28.5")
  assert_eq(low_anomalies[0].0, 21.3, "低值异常应该是21.3")
}

// Test 2: 时间序列异常检测
test "time series anomaly detection" {
  // 时间序列数据
  let timeseries_data = [
    (1640995200, 45.2),
    (1640995260, 46.1),
    (1640995320, 45.8),
    (1640995380, 46.3),
    (1640995440, 45.9),
    (1640995500, 78.5),  // 异常点
    (1640995560, 46.7),
    (1640995620, 46.2),
    (1640995680, 45.6),
    (1640995740, 46.4)
  ]
  
  // 滑动窗口异常检测
  let sliding_window_anomaly_detection = fn(data, window_size, threshold_multiplier) {
    let anomalies = []
    
    for i in window_size..data.length() {
      let window_start = i - window_size
      let window_data = data.slice(window_start, i)
      
      // 计算窗口统计
      let values = window_data.map(fn(point) { point.1 })
      let mean = values.reduce(fn(acc, val) { acc + val }, 0.0) / values.length().to_decimal()
      let variance = values
        .map(fn(val) { (val - mean) * (val - mean) })
        .reduce(fn(acc, val) { acc + val }, 0.0) / values.length().to_decimal()
      let std_dev = @lib.sqrt(variance)
      
      // 检查当前点
      let current_point = data[i]
      let deviation = @lib.abs(current_point.1 - mean)
      let threshold = threshold_multiplier * std_dev
      
      if deviation > threshold {
        anomalies = anomalies.push({
          "timestamp": current_point.0,
          "value": current_point.1,
          "expected_range": (mean - threshold, mean + threshold),
          "deviation": deviation,
          "severity": @lib.min(3.0, deviation / std_dev)
        })
      }
    }
    
    anomalies
  }
  
  // 执行异常检测
  let detected_anomalies = sliding_window_anomaly_detection(timeseries_data, 5, 2.5)
  
  // 验证异常检测结果
  assert_eq(detected_anomalies.length(), 1, "应该检测到1个异常点")
  assert_eq(detected_anomalies[0]["timestamp"], 1640995500, "异常点时间戳应该正确")
  assert_eq(detected_anomalies[0]["value"], 78.5, "异常点值应该正确")
  assert_true(detected_anomalies[0]["severity"] > 2.0, "异常严重程度应该较高")
  
  // 验证预期范围
  let expected_range = detected_anomalies[0]["expected_range"]
  assert_true(expected_range.0 < 50.0 && expected_range.1 > 40.0, "预期范围应该在正常值附近")
  assert_true(detected_anomalies[0]["value"] > expected_range.1, "异常值应该在预期范围之外")
}

// Test 3: 多维异常检测
test "multi-dimensional anomaly detection" {
  // 多维指标数据
  let multidimensional_data = [
    {"timestamp": 1640995200, "cpu_usage": 45.2, "memory_usage": 68.5, "disk_io": 120.5, "network_io": 1024.3},
    {"timestamp": 1640995260, "cpu_usage": 46.1, "memory_usage": 69.2, "disk_io": 125.8, "network_io": 1089.7},
    {"timestamp": 1640995320, "cpu_usage": 45.8, "memory_usage": 70.1, "disk_io": 118.9, "network_io": 998.4},
    {"timestamp": 1640995380, "cpu_usage": 46.3, "memory_usage": 71.5, "disk_io": 122.3, "network_io": 1102.1},
    {"timestamp": 1640995440, "cpu_usage": 45.9, "memory_usage": 72.3, "disk_io": 119.7, "network_io": 1056.8},
    {"timestamp": 1640995500, "cpu_usage": 85.6, "memory_usage": 95.2, "disk_io": 450.8, "network_io": 3024.5},  // 异常点
    {"timestamp": 1640995560, "cpu_usage": 46.7, "memory_usage": 73.8, "disk_io": 124.1, "network_io": 1078.9}
  ]
  
  // 多维异常检测算法（基于马氏距离）
  let multidimensional_anomaly_detection = fn(data, threshold) {
    let anomalies = []
    
    // 计算每个维度的均值和标准差
    let dimensions = ["cpu_usage", "memory_usage", "disk_io", "network_io"]
    let stats = {}
    
    for dimension in dimensions {
      let values = data.map(fn(point) { point[dimension] })
      let mean = values.reduce(fn(acc, val) { acc + val }, 0.0) / values.length().to_decimal()
      let variance = values
        .map(fn(val) { (val - mean) * (val - mean) })
        .reduce(fn(acc, val) { acc + val }, 0.0) / values.length().to_decimal()
      let std_dev = @lib.sqrt(variance)
      
      stats[dimension] = {"mean": mean, "std_dev": std_dev}
    }
    
    // 计算每个点的多维异常分数
    for point in data {
      let squared_distances = []
      
      for dimension in dimensions {
        let value = point[dimension]
        let mean = stats[dimension]["mean"]
        let std_dev = stats[dimension]["std_dev"]
        
        if std_dev > 0 {
          let normalized_distance = (value - mean) / std_dev
          squared_distances = squared_distances.push(normalized_distance * normalized_distance)
        }
      }
      
      let mahalanobis_distance = @lib.sqrt(
        squared_distances.reduce(fn(acc, val) { acc + val }, 0.0)
      )
      
      if mahalanobis_distance > threshold {
        anomalies = anomalies.push({
          "timestamp": point["timestamp"],
          "mahalanobis_distance": mahalanobis_distance,
          "severity": @lib.min(3.0, mahalanobis_distance / threshold),
          "anomalous_dimensions": dimensions.filter(fn(dimension) {
            let value = point[dimension]
            let mean = stats[dimension]["mean"]
            let std_dev = stats[dimension]["std_dev"]
            std_dev > 0 && @lib.abs((value - mean) / std_dev) > 2.0
          })
        })
      }
    }
    
    anomalies
  }
  
  // 执行多维异常检测
  let detected_anomalies = multidimensional_anomaly_detection(multidimensional_data, 3.0)
  
  // 验证异常检测结果
  assert_eq(detected_anomalies.length(), 1, "应该检测到1个多维异常点")
  assert_eq(detected_anomalies[0]["timestamp"], 1640995500, "异常点时间戳应该正确")
  assert_true(detected_anomalies[0]["mahalanobis_distance"] > 3.0, "马氏距离应该超过阈值")
  assert_true(detected_anomalies[0]["severity"] > 1.0, "异常严重程度应该大于1")
  
  // 验证异常维度
  let anomalous_dimensions = detected_anomalies[0]["anomalous_dimensions"]
  assert_true(anomalous_dimensions.length() >= 3, "应该有多个维度异常")
  assert_true(anomalous_dimensions.contains("cpu_usage"), "CPU使用率应该是异常维度")
  assert_true(anomalous_dimensions.contains("memory_usage"), "内存使用率应该是异常维度")
}

// Test 4: 告警规则引擎
test "alerting rules engine" {
  // 告警规则配置
  let alerting_rules = [
    {
      "name": "high_cpu_usage",
      "condition": "cpu_usage > 80",
      "severity": "warning",
      "duration": 300,  // 5分钟
      "description": "CPU使用率过高"
    },
    {
      "name": "critical_memory_usage",
      "condition": "memory_usage > 90",
      "severity": "critical",
      "duration": 120,  // 2分钟
      "description": "内存使用率严重过高"
    },
    {
      "name": "disk_space_low",
      "condition": "disk_usage > 95",
      "severity": "critical",
      "duration": 60,   // 1分钟
      "description": "磁盘空间不足"
    },
    {
      "name": "high_error_rate",
      "condition": "error_rate > 5",
      "severity": "warning",
      "duration": 180,  // 3分钟
      "description": "错误率过高"
    }
  ]
  
  // 监控指标数据
  let metrics_data = [
    {"timestamp": 1640995200, "cpu_usage": 45.2, "memory_usage": 68.5, "disk_usage": 45.0, "error_rate": 0.1},
    {"timestamp": 1640995260, "cpu_usage": 82.1, "memory_usage": 75.3, "disk_usage": 46.2, "error_rate": 0.2},
    {"timestamp": 1640995320, "cpu_usage": 85.6, "memory_usage": 91.2, "disk_usage": 47.1, "error_rate": 0.3},
    {"timestamp": 1640995380, "cpu_usage": 88.9, "memory_usage": 93.5, "disk_usage": 96.2, "error_rate": 5.5},
    {"timestamp": 1640995440, "cpu_usage": 87.3, "memory_usage": 92.8, "disk_usage": 97.1, "error_rate": 6.2},
    {"timestamp": 1640995500, "cpu_usage": 78.5, "memory_usage": 88.1, "disk_usage": 94.3, "error_rate": 2.1}
  ]
  
  // 简化的条件评估函数
  let evaluate_condition = fn(condition, metrics) {
    // 简化的条件解析和评估
    if condition.contains("cpu_usage > 80") {
      metrics["cpu_usage"] > 80.0
    } else if condition.contains("memory_usage > 90") {
      metrics["memory_usage"] > 90.0
    } else if condition.contains("disk_usage > 95") {
      metrics["disk_usage"] > 95.0
    } else if condition.contains("error_rate > 5") {
      metrics["error_rate"] > 5.0
    } else {
      false
    }
  }
  
  // 告警状态跟踪
  let alert_states = {}
  let triggered_alerts = []
  
  // 处理每个时间点的数据
  for i in 0..metrics_data.length() {
    let metrics = metrics_data[i]
    let timestamp = metrics["timestamp"]
    
    for rule in alerting_rules {
      let rule_name = rule["name"]
      let condition_met = evaluate_condition(rule["condition"], metrics)
      
      // 获取或初始化告警状态
      let alert_state = alert_states[rule_name] ?? {
        "active": false,
        "start_time": nil,
        "triggered_count": 0
      }
      
      if condition_met {
        if not(alert_state["active"]) {
          // 开始新的告警
          alert_state["active"] = true
          alert_state["start_time"] = Some(timestamp)
          alert_state["triggered_count"] = 1
        } else {
          // 告警持续中
          alert_state["triggered_count"] = alert_state["triggered_count"] + 1
        }
        
        // 检查持续时间是否满足触发条件
        match alert_state["start_time"] {
          Some(start_time) => {
            let duration = timestamp - start_time
            if duration >= rule["duration"] && alert_state["triggered_count"] == 1 {
              // 首次满足持续条件，触发告警
              triggered_alerts = triggered_alerts.push({
                "rule_name": rule_name,
                "timestamp": timestamp,
                "severity": rule["severity"],
                "description": rule["description"],
                "metrics": metrics,
                "duration": duration
              })
              
              // 标记为已触发，避免重复触发
              alert_state["triggered_count"] = alert_state["triggered_count"] + 1
            }
          }
          None => {}
        }
      } else {
        // 条件不满足，重置告警状态
        if alert_state["active"] {
          alert_state["active"] = false
          alert_state["start_time"] = nil
          alert_state["triggered_count"] = 0
        }
      }
      
      alert_states[rule_name] = alert_state
    }
  }
  
  // 验证告警触发结果
  assert_eq(triggered_alerts.length(), 4, "应该触发4个告警")
  
  // 验证具体告警
  let high_cpu_alert = triggered_alerts.filter(fn(alert) { alert["rule_name"] == "high_cpu_usage" })
  let critical_memory_alert = triggered_alerts.filter(fn(alert) { alert["rule_name"] == "critical_memory_usage" })
  let disk_space_alert = triggered_alerts.filter(fn(alert) { alert["rule_name"] == "disk_space_low" })
  let error_rate_alert = triggered_alerts.filter(fn(alert) { alert["rule_name"] == "high_error_rate" })
  
  assert_eq(high_cpu_alert.length(), 1, "应该有1个CPU使用率告警")
  assert_eq(critical_memory_alert.length(), 1, "应该有1个内存使用率告警")
  assert_eq(disk_space_alert.length(), 1, "应该有1个磁盘空间告警")
  assert_eq(error_rate_alert.length(), 1, "应该有1个错误率告警")
  
  // 验证告警严重程度
  assert_eq(high_cpu_alert[0]["severity"], "warning", "CPU告警应该是warning级别")
  assert_eq(critical_memory_alert[0]["severity"], "critical", "内存告警应该是critical级别")
  assert_eq(disk_space_alert[0]["severity"], "critical", "磁盘告警应该是critical级别")
  assert_eq(error_rate_alert[0]["severity"], "warning", "错误率告警应该是warning级别")
}

// Test 5: 告警通知机制
test "alerting notification mechanism" {
  // 告警通知配置
  let notification_channels = [
    {
      "name": "email",
      "type": "email",
      "enabled": true,
      "recipients": ["admin@example.com", "ops@example.com"],
      "severity_filter": ["warning", "critical"]
    },
    {
      "name": "slack",
      "type": "slack",
      "enabled": true,
      "webhook_url": "https://hooks.slack.com/xxx",
      "channel": "#alerts",
      "severity_filter": ["critical"]
    },
    {
      "name": "sms",
      "type": "sms",
      "enabled": false,
      "recipients": ["+1234567890"],
      "severity_filter": ["critical"]
    },
    {
      "name": "pagerduty",
      "type": "pagerduty",
      "enabled": true,
      "service_key": "xxx",
      "severity_filter": ["critical"]
    }
  ]
  
  // 模拟告警事件
  let alert_events = [
    {
      "id": "alert_001",
      "name": "high_cpu_usage",
      "severity": "warning",
      "timestamp": 1640995320,
      "description": "CPU使用率过高",
      "metrics": {"cpu_usage": 85.6, "memory_usage": 75.3}
    },
    {
      "id": "alert_002",
      "name": "critical_memory_usage",
      "severity": "critical",
      "timestamp": 1640995380,
      "description": "内存使用率严重过高",
      "metrics": {"memory_usage": 93.5, "cpu_usage": 88.9}
    },
    {
      "id": "alert_003",
      "name": "disk_space_low",
      "severity": "critical",
      "timestamp": 1640995440,
      "description": "磁盘空间不足",
      "metrics": {"disk_usage": 97.1, "memory_usage": 92.8}
    }
  ]
  
  // 通知发送记录
  let notification_log = []
  
  // 通知发送函数
  let send_notification = fn(channel, alert) {
    if not(channel["enabled"]) {
      return {
        "success": false,
        "reason": "channel_disabled"
      }
    }
    
    if not(channel["severity_filter"].contains(alert["severity"])) {
      return {
        "success": false,
        "reason": "severity_not_filtered"
      }
    }
    
    // 模拟发送通知
    let notification = {
      "channel": channel["name"],
      "type": channel["type"],
      "alert_id": alert["id"],
      "alert_name": alert["name"],
      "severity": alert["severity"],
      "timestamp": alert["timestamp"],
      "description": alert["description"],
      "sent_at": 1640996000,  // 模拟发送时间
      "status": "sent"
    }
    
    notification_log = notification_log.push(notification)
    
    {
      "success": true,
      "notification_id": "notif_" + @lib.string.random(8)
    }
  }
  
  // 处理告警通知
  for alert in alert_events {
    for channel in notification_channels {
      let result = send_notification(channel, alert)
      
      // 验证通知发送逻辑
      if channel["enabled"] && channel["severity_filter"].contains(alert["severity"]) {
        assert_true(result["success"], "启用的且符合严重程度过滤的通知应该发送成功")
      } else {
        assert_false(result["success"], "禁用的或不符合严重程度过滤的通知应该发送失败")
      }
    }
  }
  
  // 验证通知日志
  assert_eq(notification_log.length(), 7, "应该发送7个通知")
  
  // 验证各渠道通知数量
  let email_notifications = notification_log.filter(fn(notif) { notif["type"] == "email" })
  let slack_notifications = notification_log.filter(fn(notif) { notif["type"] == "slack" })
  let pagerduty_notifications = notification_log.filter(fn(notif) { notif["type"] == "pagerduty" })
  
  assert_eq(email_notifications.length(), 3, "应该发送3个邮件通知（所有告警）")
  assert_eq(slack_notifications.length(), 2, "应该发送2个Slack通知（仅critical告警）")
  assert_eq(pagerduty_notifications.length(), 2, "应该发送2个PagerDuty通知（仅critical告警）")
  
  // 验证SMS通知未发送（渠道禁用）
  let sms_notifications = notification_log.filter(fn(notif) { notif["type"] == "sms" })
  assert_eq(sms_notifications.length(), 0, "不应该发送SMS通知（渠道禁用）")
}

// Test 6: 告警抑制和去重
test "alerting suppression and deduplication" {
  // 告警抑制规则
  let suppression_rules = [
    {
      "name": "high_cpu_suppresses_low_memory",
      "condition": "cpu_usage > 90",
      "suppressed_alerts": ["memory_usage_warning"],
      "duration": 600  // 10分钟
    },
    {
      "name": "critical_suppresses_warning",
      "condition": "severity == 'critical'",
      "suppressed_alerts": ["*"],  // 抑制所有warning级别告警
      "duration": 300  // 5分钟
    }
  ]
  
  // 告警事件序列
  let alert_sequence = [
    {"timestamp": 1640995200, "name": "memory_usage_warning", "severity": "warning", "cpu_usage": 45.2, "memory_usage": 85.1},
    {"timestamp": 1640995260, "name": "high_cpu_usage", "severity": "warning", "cpu_usage": 91.5, "memory_usage": 86.3},
    {"timestamp": 1640995320, "name": "memory_usage_warning", "severity": "warning", "cpu_usage": 92.1, "memory_usage": 87.5},
    {"timestamp": 1640995380, "name": "database_error", "severity": "critical", "cpu_usage": 78.3, "memory_usage": 82.1},
    {"timestamp": 1640995440, "name": "high_latency", "severity": "warning", "cpu_usage": 76.9, "memory_usage": 81.5},
    {"timestamp": 1640995500, "name": "memory_usage_warning", "severity": "warning", "cpu_usage": 65.2, "memory_usage": 84.3}
  ]
  
  // 告警处理结果
  let processed_alerts = []
  let suppressed_alerts = []
  let active_suppressions = []
  
  // 处理告警序列
  for alert in alert_sequence {
    let is_suppressed = false
    let suppression_reason = ""
    
    // 检查抑制规则
    for rule in suppression_rules {
      let rule_matches = if rule["condition"].contains("cpu_usage > 90") {
        alert["cpu_usage"] > 90.0
      } else if rule["condition"].contains("severity == 'critical'") {
        alert["severity"] == "critical"
      } else {
        false
      }
      
      if rule_matches {
        // 检查是否应该抑制当前告警
        let should_suppress = if rule["suppressed_alerts"].contains("*") {
          alert["severity"] == "warning"
        } else if rule["suppressed_alerts"].contains(alert["name"]) {
          true
        } else {
          false
        }
        
        if should_suppress {
          is_suppressed = true
          suppression_reason = rule["name"]
          
          // 添加抑制记录
          active_suppressions = active_suppressions.push({
            "rule": rule["name"],
            "triggering_alert": alert["name"],
            "timestamp": alert["timestamp"],
            "duration": rule["duration"]
          })
          break
        }
      }
    }
    
    // 记录处理结果
    if is_suppressed {
      suppressed_alerts = suppressed_alerts.push({
        "alert": alert,
        "reason": suppression_reason,
        "timestamp": alert["timestamp"]
      })
    } else {
      processed_alerts = processed_alerts.push(alert)
    }
  }
  
  // 验证告警抑制结果
  assert_eq(processed_alerts.length(), 3, "应该有3个告警被处理")
  assert_eq(suppressed_alerts.length(), 3, "应该有3个告警被抑制")
  
  // 验证具体抑制情况
  let memory_warning_suppressed = suppressed_alerts.filter(fn(suppressed) {
    suppressed["alert"]["name"] == "memory_usage_warning"
  })
  
  assert_eq(memory_warning_suppressed.length(), 2, "应该有2个内存警告被抑制")
  assert_eq(memory_warning_suppressed[0]["reason"], "high_cpu_suppresses_low_memory", "第一个内存警告应该被CPU高使用率抑制")
  assert_eq(memory_warning_suppressed[1]["reason"], "critical_suppresses_warning", "第二个内存警告应该被Critical告警抑制")
  
  let high_latency_suppressed = suppressed_alerts.filter(fn(suppressed) {
    suppressed["alert"]["name"] == "high_latency"
  })
  
  assert_eq(high_latency_suppressed.length(), 1, "应该有1个高延迟告警被抑制")
  assert_eq(high_latency_suppressed[0]["reason"], "critical_suppresses_warning", "高延迟告警应该被Critical告警抑制")
  
  // 验证处理的告警
  let processed_names = processed_alerts.map(fn(alert) { alert["name"] })
  assert_true(processed_names.contains("memory_usage_warning"), "第一个内存警告应该被处理")
  assert_true(processed_names.contains("database_error"), "数据库错误告警应该被处理")
  assert_true(processed_names.contains("memory_usage_warning"), "最后一个内存警告应该被处理")
}

// Test 7: 告警升级机制
test "alerting escalation mechanism" {
  // 告警升级策略
  let escalation_policies = [
    {
      "name": "cpu_usage_escalation",
      "alert_pattern": "cpu_usage_*",
      "levels": [
        {
          "level": 1,
          "wait_time": 300,      // 5分钟
          "notification_channels": ["email"],
          "recipients": ["team_lead@example.com"]
        },
        {
          "level": 2,
          "wait_time": 600,      // 10分钟
          "notification_channels": ["email", "slack"],
          "recipients": ["manager@example.com", "ops@example.com"]
        },
        {
          "level": 3,
          "wait_time": 900,      // 15分钟
          "notification_channels": ["email", "slack", "sms"],
          "recipients": ["director@example.com", "oncall@example.com"]
        }
      ]
    },
    {
      "name": "critical_escalation",
      "alert_pattern": "*critical*",
      "levels": [
        {
          "level": 1,
          "wait_time": 60,       // 1分钟
          "notification_channels": ["email", "slack"],
          "recipients": ["oncall@example.com"]
        },
        {
          "level": 2,
          "wait_time": 300,      // 5分钟
          "notification_channels": ["email", "slack", "pagerduty"],
          "recipients": ["manager@example.com", "oncall@example.com"]
        }
      ]
    }
  ]
  
  // 长时间持续的告警事件
  let persistent_alerts = [
    {
      "id": "alert_001",
      "name": "cpu_usage_high",
      "start_time": 1640995200,
      "current_time": 1640995200,
      "severity": "warning"
    },
    {
      "id": "alert_002",
      "name": "database_critical_error",
      "start_time": 1640995200,
      "current_time": 1640995200,
      "severity": "critical"
    }
  ]
  
  // 模拟时间推进和告警升级
  let escalation_events = []
  
  for alert in persistent_alerts {
    let mut escalation_level = 1
    
    // 查找适用的升级策略
    let applicable_policy = escalation_policies.filter(fn(policy) {
      if policy["alert_pattern"] == "cpu_usage_*" {
        alert["name"].contains("cpu_usage")
      } else if policy["alert_pattern"] == "*critical*" {
        alert["severity"] == "critical"
      } else {
        false
      }
    })[0]
    
    // 模拟时间推进和升级检查
    let time_points = [300, 600, 900, 1200]  // 时间点（秒）
    
    for time_offset in time_points {
      alert["current_time"] = alert["start_time"] + time_offset
      
      // 检查是否应该升级
      let current_level_config = applicable_policy["levels"].filter(fn(level) {
        level["level"] == escalation_level
      })[0]
      
      if time_offset >= current_level_config["wait_time"] && escalation_level < applicable_policy["levels"].length() {
        escalation_level = escalation_level + 1
        
        // 记录升级事件
        escalation_events = escalation_events.push({
          "alert_id": alert["id"],
          "alert_name": alert["name"],
          "escalation_level": escalation_level,
          "timestamp": alert["current_time"],
          "notification_channels": current_level_config["notification_channels"],
          "recipients": current_level_config["recipients"]
        })
      }
    }
  }
  
  // 验证告警升级结果
  assert_eq(escalation_events.length(), 5, "应该有5个升级事件")
  
  // 验证CPU告警升级
  let cpu_escalations = escalation_events.filter(fn(event) { event["alert_name"] == "cpu_usage_high" })
  assert_eq(cpu_escalations.length(), 3, "CPU告警应该升级3次")
  assert_eq(cpu_escalations[0]["escalation_level"], 2, "第一次升级到级别2")
  assert_eq(cpu_escalations[1]["escalation_level"], 3, "第二次升级到级别3")
  
  // 验证Critical告警升级
  let critical_escalations = escalation_events.filter(fn(event) { event["alert_name"] == "database_critical_error" })
  assert_eq(critical_escalations.length(), 2, "Critical告警应该升级2次")
  assert_eq(critical_escalations[0]["escalation_level"], 2, "第一次升级到级别2")
  
  // 验证通知渠道随升级级别增加
  let level1_channels = cpu_escalations[0]["notification_channels"]
  let level2_channels = cpu_escalations[1]["notification_channels"]
  
  assert_eq(level1_channels.length(), 1, "级别1应该有1个通知渠道")
  assert_eq(level2_channels.length(), 3, "级别3应该有3个通知渠道")
  assert_true(level2_channels.contains("sms"), "高级别应该包含SMS通知")
}

// Test 8: 告警恢复和自动解决
test "alerting recovery and auto-resolution" {
  // 告警恢复配置
  let recovery_config = {
    "auto_resolve": true,
    "recovery_threshold": 5,  // 连续5个正常数据点后自动解决
    "recovery_window": 300,   // 5分钟恢复窗口
    "notification_on_recovery": true
  }
  
  // 告警状态跟踪
  let alert_states = {
    "high_cpu_usage": {
      "active": true,
      "start_time": 1640995200,
      "last_trigger_time": 1640995320,
      "consecutive_normal_count": 0,
      "severity": "warning"
    },
    "memory_usage_critical": {
      "active": true,
      "start_time": 1640995180,
      "last_trigger_time": 1640995300,
      "consecutive_normal_count": 0,
      "severity": "critical"
    }
  }
  
  // 监控数据序列（包含恢复过程）
  let monitoring_data = [
    {"timestamp": 1640995380, "cpu_usage": 85.2, "memory_usage": 91.5},  // 仍然异常
    {"timestamp": 1640995440, "cpu_usage": 82.1, "memory_usage": 88.3},  // 仍然异常
    {"timestamp": 1640995500, "cpu_usage": 78.5, "memory_usage": 85.2},  // CPU开始恢复
    {"timestamp": 1640995560, "cpu_usage": 72.3, "memory_usage": 82.1},  // CPU继续恢复
    {"timestamp": 1640995620, "cpu_usage": 65.8, "memory_usage": 78.5},  // CPU基本恢复
    {"timestamp": 1640995680, "cpu_usage": 58.2, "memory_usage": 75.3},  // CPU完全恢复
    {"timestamp": 1640995740, "cpu_usage": 52.1, "memory_usage": 71.8},  // CPU正常，内存开始恢复
    {"timestamp": 1640995800, "cpu_usage": 48.5, "memory_usage": 68.2},  // CPU正常，内存继续恢复
    {"timestamp": 1640995860, "cpu_usage": 45.3, "memory_usage": 65.1},  // CPU正常，内存基本恢复
    {"timestamp": 1640995920, "cpu_usage": 42.8, "memory_usage": 62.5}   // 都已恢复
  ]
  
  // 告警阈值
  let thresholds = {
    "cpu_usage_warning": 80.0,
    "memory_usage_critical": 90.0
  }
  
  // 恢复事件记录
  let recovery_events = []
  
  // 处理监控数据和告警恢复
  for data in monitoring_data {
    // 检查CPU告警恢复
    let cpu_alert_state = alert_states["high_cpu_usage"]
    if cpu_alert_state["active"] {
      if data["cpu_usage"] < thresholds["cpu_usage_warning"] {
        alert_states["high_cpu_usage"]["consecutive_normal_count"] = 
          alert_states["high_cpu_usage"]["consecutive_normal_count"] + 1
        
        // 检查是否满足自动解决条件
        if alert_states["high_cpu_usage"]["consecutive_normal_count"] >= recovery_config["recovery_threshold"] {
          // 自动解决告警
          alert_states["high_cpu_usage"]["active"] = false
          alert_states["high_cpu_usage"]["recovery_time"] = data["timestamp"]
          
          recovery_events = recovery_events.push({
            "alert_name": "high_cpu_usage",
            "recovery_time": data["timestamp"],
            "duration": data["timestamp"] - cpu_alert_state["start_time"],
            "auto_resolved": true
          })
        }
      } else {
        // 重置正常计数
        alert_states["high_cpu_usage"]["consecutive_normal_count"] = 0
      }
    }
    
    // 检查内存告警恢复
    let memory_alert_state = alert_states["memory_usage_critical"]
    if memory_alert_state["active"] {
      if data["memory_usage"] < thresholds["memory_usage_critical"] {
        alert_states["memory_usage_critical"]["consecutive_normal_count"] = 
          alert_states["memory_usage_critical"]["consecutive_normal_count"] + 1
        
        // 检查是否满足自动解决条件
        if alert_states["memory_usage_critical"]["consecutive_normal_count"] >= recovery_config["recovery_threshold"] {
          // 自动解决告警
          alert_states["memory_usage_critical"]["active"] = false
          alert_states["memory_usage_critical"]["recovery_time"] = data["timestamp"]
          
          recovery_events = recovery_events.push({
            "alert_name": "memory_usage_critical",
            "recovery_time": data["timestamp"],
            "duration": data["timestamp"] - memory_alert_state["start_time"],
            "auto_resolved": true
          })
        }
      } else {
        // 重置正常计数
        alert_states["memory_usage_critical"]["consecutive_normal_count"] = 0
      }
    }
  }
  
  // 验证告警恢复结果
  assert_eq(recovery_events.length(), 2, "应该有2个告警恢复事件")
  
  // 验证CPU告警恢复
  let cpu_recovery = recovery_events.filter(fn(event) { event["alert_name"] == "high_cpu_usage" })[0]
  assert_true(cpu_recovery["auto_resolved"], "CPU告警应该自动解决")
  assert_true(cpu_recovery["duration"] > 600, "CPU告警持续时间应该超过10分钟")
  assert_eq(cpu_recovery["recovery_time"], 1640995680, "CPU告警恢复时间应该正确")
  
  // 验证内存告警恢复
  let memory_recovery = recovery_events.filter(fn(event) { event["alert_name"] == "memory_usage_critical" })[0]
  assert_true(memory_recovery["auto_resolved"], "内存告警应该自动解决")
  assert_true(memory_recovery["duration"] > 900, "内存告警持续时间应该超过15分钟")
  assert_eq(memory_recovery["recovery_time"], 1640995920, "内存告警恢复时间应该正确")
  
  // 验证告警状态更新
  assert_false(alert_states["high_cpu_usage"]["active"], "CPU告警应该已解决")
  assert_false(alert_states["memory_usage_critical"]["active"], "内存告警应该已解决")
}

// Test 9: 告警聚合和分组
test "alerting aggregation and grouping" {
  // 告警分组规则
  let grouping_rules = [
    {
      "name": "host_resource_grouping",
      "pattern": "host_{host}_*",
      "group_by": ["host"],
      "aggregation_window": 300,  // 5分钟
      "max_alerts_in_group": 10
    },
    {
      "name": "service_dependency_grouping",
      "pattern": "service_{service}_*",
      "group_by": ["service"],
      "aggregation_window": 600,  // 10分钟
      "max_alerts_in_group": 5
    }
  ]
  
  // 大量相关告警
  let related_alerts = [
    {"timestamp": 1640995200, "name": "host_web01_cpu_high", "host": "web01", "service": "web", "severity": "warning"},
    {"timestamp": 1640995210, "name": "host_web01_memory_high", "host": "web01", "service": "web", "severity": "warning"},
    {"timestamp": 1640995220, "name": "host_web01_disk_high", "host": "web01", "service": "web", "severity": "critical"},
    {"timestamp": 1640995230, "name": "host_web02_cpu_high", "host": "web02", "service": "web", "severity": "warning"},
    {"timestamp": 1640995240, "name": "service_web_db_error", "host": "db01", "service": "web", "severity": "critical"},
    {"timestamp": 1640995250, "name": "service_web_cache_error", "host": "cache01", "service": "web", "severity": "warning"},
    {"timestamp": 1640995260, "name": "host_api01_cpu_high", "host": "api01", "service": "api", "severity": "warning"},
    {"timestamp": 1640995270, "name": "host_api01_memory_high", "host": "api01", "service": "api", "severity": "warning"}
  ]
  
  // 告警分组处理
  let alert_groups = {}
  let individual_alerts = []
  
  for alert in related_alerts {
    let grouped = false
    
    // 尝试按主机分组
    if alert["name"].contains("host_") {
      let host = alert["host"]
      let group_key = "host_" + host
      
      if not(alert_groups[group_key]) {
        alert_groups[group_key] = {
          "group_type": "host",
          "group_key": host,
          "alerts": [],
          "highest_severity": "info",
          "start_time": alert["timestamp"],
          "last_update": alert["timestamp"]
        }
      }
      
      let group = alert_groups[group_key]
      group["alerts"] = group["alerts"].push(alert)
      group["last_update"] = alert["timestamp"]
      
      // 更新最高严重程度
      if alert["severity"] == "critical" {
        group["highest_severity"] = "critical"
      } else if alert["severity"] == "warning" && group["highest_severity"] != "critical" {
        group["highest_severity"] = "warning"
      }
      
      grouped = true
    }
    
    // 尝试按服务分组
    if alert["name"].contains("service_") {
      let service = alert["service"]
      let group_key = "service_" + service
      
      if not(alert_groups[group_key]) {
        alert_groups[group_key] = {
          "group_type": "service",
          "group_key": service,
          "alerts": [],
          "highest_severity": "info",
          "start_time": alert["timestamp"],
          "last_update": alert["timestamp"]
        }
      }
      
      let group = alert_groups[group_key]
      group["alerts"] = group["alerts"].push(alert)
      group["last_update"] = alert["timestamp"]
      
      // 更新最高严重程度
      if alert["severity"] == "critical" {
        group["highest_severity"] = "critical"
      } else if alert["severity"] == "warning" && group["highest_severity"] != "critical" {
        group["highest_severity"] = "warning"
      }
      
      grouped = true
    }
    
    // 如果没有分组，保持为独立告警
    if not(grouped) {
      individual_alerts = individual_alerts.push(alert)
    }
  }
  
  // 验证告警分组结果
  assert_eq(@lib.object.keys(alert_groups).length(), 4, "应该有4个告警组")
  assert_eq(individual_alerts.length(), 0, "不应该有独立告警")
  
  // 验证主机分组
  let host_web01_group = alert_groups["host_web01"]
  assert_eq(host_web01_group["group_type"], "host", "web01组应该是主机类型")
  assert_eq(host_web01_group["group_key"], "web01", "web01组的键应该是web01")
  assert_eq(host_web01_group["alerts"].length(), 3, "web01组应该有3个告警")
  assert_eq(host_web01_group["highest_severity"], "critical", "web01组的最高严重程度应该是critical")
  
  let host_web02_group = alert_groups["host_web02"]
  assert_eq(host_web02_group["alerts"].length(), 1, "web02组应该有1个告警")
  
  let host_api01_group = alert_groups["host_api01"]
  assert_eq(host_api01_group["alerts"].length(), 2, "api01组应该有2个告警")
  
  // 验证服务分组
  let service_web_group = alert_groups["service_web"]
  assert_eq(service_web_group["group_type"], "service", "web服务组应该是服务类型")
  assert_eq(service_web_group["group_key"], "web", "web服务组的键应该是web")
  assert_eq(service_web_group["alerts"].length(), 2, "web服务组应该有2个告警")
  assert_eq(service_web_group["highest_severity"], "critical", "web服务组的最高严重程度应该是critical")
}

// Test 10: 告警性能和负载测试
test "alerting performance and load testing" {
  // 模拟大量监控数据和告警规则
  let generate_test_data = fn(metric_count, time_points, anomaly_rate) {
    let data = []
    let anomaly_count = (time_points * anomaly_rate).to_int()
    let anomaly_indices = []
    
    // 随机选择异常点
    for i in 0..anomaly_count {
      anomaly_indices = anomaly_indices.push(@lib.random.int(time_points))
    }
    
    for metric_id in 0..metric_count {
      for time_point in 0..time_points {
        let timestamp = 1640995200 + time_point * 60  // 每分钟一个点
        let base_value = 50.0 + @lib.random.float() * 10.0
        
        let value = if anomaly_indices.contains(time_point) {
          base_value + @lib.random.float() * 40.0 + 20.0  // 异常值
        } else {
          base_value
        }
        
        data = data.push({
          "timestamp": timestamp,
          "metric_id": "metric_" + metric_id.to_string(),
          "value": value,
          "is_anomaly": anomaly_indices.contains(time_point)
        })
      }
    }
    
    data
  }
  
  // 生成测试数据：100个指标，1000个时间点，5%异常率
  let test_data = generate_test_data(100, 1000, 0.05)
  
  // 告警检测性能测试
  let anomaly_detection_performance = fn(data, batch_size) {
    let start_time = @lib.time.now()
    let detected_anomalies = []
    
    // 分批处理数据
    for i in 0..data.length() {
      if i % batch_size == 0 {
        let batch_end = if i + batch_size < data.length() {
          i + batch_size
        } else {
          data.length()
        }
        
        let batch = data.slice(i, batch_end)
        
        // 简化的异常检测
        for point in batch {
          if point["value"] > 80.0 {  // 简单阈值检测
            detected_anomalies = detected_anomalies.push(point)
          }
        }
      }
    }
    
    let end_time = @lib.time.now()
    let processing_time = end_time - start_time
    
    {
      "total_points": data.length(),
      "detected_anomalies": detected_anomalies.length(),
      "processing_time_ms": processing_time,
      "points_per_second": (data.length().to_decimal() / processing_time.to_decimal()) * 1000.0
    }
  }
  
  // 执行性能测试
  let performance_result = anomaly_detection_performance(test_data, 100)
  
  // 验证性能测试结果
  assert_eq(performance_result["total_points"], 100000, "应该处理100,000个数据点")
  assert_true(performance_result["detected_anomalies"] > 0, "应该检测到异常")
  assert_true(performance_result["processing_time_ms"] > 0, "处理时间应该大于0")
  assert_true(performance_result["points_per_second"] > 1000, "每秒应该能处理至少1000个点")
  
  // 告警规则负载测试
  let alert_rules_load_test = fn(alert_count, data_points) {
    let rules = []
    for i in 0..alert_count {
      rules = rules.push({
        "name": "rule_" + i.to_string(),
        "condition": "value > " + (50.0 + i.to_float()).to_string(),
        "severity": if i % 2 == 0 { "warning" } else { "critical" }
      })
    }
    
    let start_time = @lib.time.now()
    let triggered_rules = 0
    
    for point in data_points {
      for rule in rules {
        let threshold = @lib.float.parse(rule["condition"].split(">")[1])
        if point["value"] > threshold {
          triggered_rules = triggered_rules + 1
        }
      }
    }
    
    let end_time = @lib.time.now()
    let processing_time = end_time - start_time
    
    {
      "rule_count": alert_count,
      "data_points": data_points.length(),
      "triggered_rules": triggered_rules,
      "processing_time_ms": processing_time,
      "evaluations_per_second": ((alert_count * data_points.length()).to_decimal() / processing_time.to_decimal()) * 1000.0
    }
  }
  
  // 执行负载测试：1000个规则，10000个数据点
  let sample_data = test_data.slice(0, 10000)
  let load_test_result = alert_rules_load_test(1000, sample_data)
  
  // 验证负载测试结果
  assert_eq(load_test_result["rule_count"], 1000, "应该有1000个告警规则")
  assert_eq(load_test_result["data_points"], 10000, "应该有10000个数据点")
  assert_true(load_test_result["triggered_rules"] > 0, "应该有触发的规则")
  assert_true(load_test_result["evaluations_per_second"] > 10000, "每秒应该能完成至少10000次评估")
}
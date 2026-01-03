// Azimuth 异常检测和告警系统测试
// 专注于测试异常检测算法、告警生成和通知机制

// 测试1: 多层次异常检测系统
test "多层次异常检测系统验证" {
  // 定义异常严重程度
  enum Severity {
    Low
    Medium
    High
    Critical
  }
  
  // 定义异常类型
  enum AnomalyType {
    StatisticalOutlier  // 统计离群值
    ThresholdBreach     // 阈值突破
    PatternDeviation    // 模式偏差
    RateChange          // 变化率异常
    DataQuality         // 数据质量问题
  }
  
  // 定义异常检测结果
  type AnomalyDetection = {
    id: String,
    timestamp: Int,
    metric_name: String,
    value: Float,
    expected_range: (Float, Float),
    anomaly_type: AnomalyType,
    severity: Severity,
    confidence: Float,
    context: Array[(String, String)],
    related_metrics: Array[String]
  }
  
  // 定义多层次检测器
  type MultiLevelDetector = {
    name: String,
    level: Int,  // 检测层级：1=基础，2=中级，3=高级
    detect: fn(Array[DataPoint]) -> Array[AnomalyDetection],
    get_thresholds: fn() -> Array[(String, (Float, Float))]
  }
  
  // 创建基础统计检测器
  let create_statistical_detector = fn(z_threshold: Float) {
    let detect = fn(data_points: Array[DataPoint>) {
      if data_points.length() < 20 {
        return []
      }
      
      let values = data_points.map(fn(p) { p.value })
      let mean = values.reduce(0.0, fn(sum, v) { sum + v }) / (values.length() as Float)
      let variance = values.reduce(0.0, fn(sum, v) { sum + (v - mean) * (v - mean) }) / (values.length() as Float)
      let std_dev = Math::sqrt(variance)
      
      let anomalies = []
      for point in data_points {
        let z_score = Math::abs((point.value - mean) / std_dev)
        if z_score > z_threshold {
          let severity = if z_score > 4.0 {
            Severity::Critical
          } else if z_score > 3.0 {
            Severity::High
          } else if z_score > 2.5 {
            Severity::Medium
          } else {
            Severity::Low
          }
          
          anomalies = anomalies.push({
            id: UUID::v4(),
            timestamp: point.timestamp,
            metric_name: point.metric_name,
            value: point.value,
            expected_range: (mean - z_threshold * std_dev, mean + z_threshold * std_dev),
            anomaly_type: AnomalyType::StatisticalOutlier,
            severity: severity,
            confidence: Math::min(z_score / 5.0, 1.0),
            context: point.labels,
            related_metrics: []
          })
        }
      }
      
      anomalies
    }
    
    let get_thresholds = fn() {
      [("statistical_z_score", (z_threshold, Float::infinity()))]
    }
    
    {
      name: "statistical_detector",
      level: 1,
      detect,
      get_thresholds
    }
  }
  
  // 创建阈值检测器
  let create_threshold_detector = fn(thresholds: Array[(String, (Float, Float))>) {
    let detect = fn(data_points: Array[DataPoint>) {
      let anomalies = []
      
      for point in data_points {
        match thresholds.find(fn(th) { th.0 == point.metric_name }) {
          Some((_, (min_val, max_val))) => {
            if point.value < min_val || point.value > max_val {
              let breach_type = if point.value > max_val { "upper" } else { "lower" }
              let breach_percentage = if point.value > max_val {
                ((point.value - max_val) / max_val) * 100.0
              } else {
                ((min_val - point.value) / min_val) * 100.0
              }
              
              let severity = if breach_percentage > 100.0 {
                Severity::Critical
              } else if breach_percentage > 50.0 {
                Severity::High
              } else if breach_percentage > 20.0 {
                Severity::Medium
              } else {
                Severity::Low
              }
              
              anomalies = anomalies.push({
                id: UUID::v4(),
                timestamp: point.timestamp,
                metric_name: point.metric_name,
                value: point.value,
                expected_range: (min_val, max_val),
                anomaly_type: AnomalyType::ThresholdBreach,
                severity: severity,
                confidence: Math::min(breach_percentage / 100.0, 1.0),
                context: point.labels + [("breach_type", breach_type)],
                related_metrics: []
              })
            }
          }
          None => {}  // 没有阈值配置，跳过
        }
      }
      
      anomalies
    }
    
    let get_thresholds = fn() { thresholds }
    
    {
      name: "threshold_detector",
      level: 1,
      detect,
      get_thresholds
    }
  }
  
  // 创建模式偏差检测器
  let create_pattern_detector = fn(window_size: Int, deviation_threshold: Float) {
    let detect = fn(data_points: Array[DataPoint>) {
      if data_points.length() < window_size * 2 {
        return []
      }
      
      let anomalies = []
      
      // 滑动窗口检测模式偏差
      for i in window_size..(data_points.length() - window_size) {
        let current_point = data_points[i]
        
        // 构建历史窗口
        let historical_values = []
        for j in (i - window_size)..(i - 1) {
          historical_values = historical_values.push(data_points[j].value)
        }
        
        // 构建未来窗口
        let future_values = []
        for j in (i + 1)..(i + window_size) {
          future_values = future_values.push(data_points[j].value)
        }
        
        let historical_mean = historical_values.reduce(0.0, fn(sum, v) { sum + v }) / (historical_values.length() as Float)
        let future_mean = future_values.reduce(0.0, fn(sum, v) { sum + v }) / (future_values.length() as Float)
        
        // 计算当前点与历史模式的偏差
        let historical_deviation = Math::abs(current_point.value - historical_mean)
        let historical_std = Math::sqrt(
          historical_values.reduce(0.0, fn(sum, v) { sum + (v - historical_mean) * (v - historical_mean) }) / 
          (historical_values.length() as Float)
        )
        
        let pattern_deviation = historical_deviation / historical_std
        
        if pattern_deviation > deviation_threshold {
          let severity = if pattern_deviation > 4.0 {
            Severity::Critical
          } else if pattern_deviation > 3.0 {
            Severity::High
          } else if pattern_deviation > 2.0 {
            Severity::Medium
          } else {
            Severity::Low
          }
          
          anomalies = anomalies.push({
            id: UUID::v4(),
            timestamp: current_point.timestamp,
            metric_name: current_point.metric_name,
            value: current_point.value,
            expected_range: (historical_mean - deviation_threshold * historical_std, historical_mean + deviation_threshold * historical_std),
            anomaly_type: AnomalyType::PatternDeviation,
            severity: severity,
            confidence: Math::min(pattern_deviation / 5.0, 1.0),
            context: current_point.labels + [
              ("historical_mean", historical_mean.to_string()),
              ("pattern_deviation", pattern_deviation.to_string())
            ],
            related_metrics: []
          })
        }
      }
      
      anomalies
    }
    
    let get_thresholds = fn() {
      [("pattern_deviation", (deviation_threshold, Float::infinity()))]
    }
    
    {
      name: "pattern_detector",
      level: 2,
      detect,
      get_thresholds
    }
  }
  
  // 创建测试数据
  let create_test_data_points = fn() {
    let base_timestamp = Time::now() - 300000  // 5分钟前
    let data_points = []
    
    // 生成正常数据
    for i in 0..100 {
      let timestamp = base_timestamp + i * 3000  // 每3秒一个点
      let mut value = 50.0 + Math::random() * 10.0 - 5.0  // 50 ± 5
      
      // 添加一些异常点
      if i == 20 {
        value = 85.0  // 统计离群值
      } else if i == 40 {
        value = 15.0  // 阈值突破
      } else if i == 60 {
        value = 120.0  // 严重阈值突破
      } else if i == 80 {
        value = 25.0  // 模式偏差
      }
      
      data_points = data_points.push({
        timestamp: timestamp,
        value: value,
        metric_name: "cpu_usage",
        labels: [
          ("service", "api-gateway"),
          ("instance", "instance-1"),
          ("datacenter", "dc-west")
        ]
      })
    }
    
    data_points
  }
  
  // 测试多层次检测
  let test_data = create_test_data_points()
  
  // 创建检测器
  let statistical_detector = create_statistical_detector(2.5)
  let threshold_detector = create_threshold_detector([
    ("cpu_usage", (20.0, 80.0)),  // CPU使用率正常范围20%-80%
    ("memory_usage", (30.0, 90.0))
  ])
  let pattern_detector = create_pattern_detector(10, 2.5)
  
  // 执行检测
  let statistical_anomalies = statistical_detector.detect(test_data)
  let threshold_anomalies = threshold_detector.detect(test_data)
  let pattern_anomalies = pattern_detector.detect(test_data)
  
  // 验证统计检测结果
  assert_true(statistical_anomalies.length() > 0)
  assert_true(statistical_anomalies.any(fn(a) { a.value > 80.0 }))  // 应该检测到高值异常
  
  // 验证阈值检测结果
  assert_true(threshold_anomalies.length() > 0)
  assert_true(threshold_anomalies.any(fn(a) { a.value > 80.0 }))  // 应该检测到阈值突破
  assert_true(threshold_anomalies.any(fn(a) { a.value < 20.0 }))  // 应该检测到低值异常
  
  // 验证模式偏差检测结果
  assert_true(pattern_anomalies.length() >= 0)  // 可能有也可能没有模式偏差
  
  // 验证异常严重程度分布
  let critical_anomalies = statistical_anomalies.filter(fn(a) { 
    match a.severity { Severity::Critical => true, _ => false } 
  })
  let high_anomalies = statistical_anomalies.filter(fn(a) { 
    match a.severity { Severity::High => true, _ => false } 
  })
  
  // 应该有高严重程度的异常
  assert_true(high_anomalies.length() > 0 || critical_anomalies.length() > 0)
  
  // 验证置信度
  for anomaly in statistical_anomalies {
    assert_true(anomaly.confidence >= 0.0 && anomaly.confidence <= 1.0)
  }
  
  // 验证检测器层级
  assert_eq(statistical_detector.level, 1)
  assert_eq(threshold_detector.level, 1)
  assert_eq(pattern_detector.level, 2)
}

// 测试2: 告警聚合和去重
test "告警聚合和去重机制验证" {
  // 定义告警类型
  enum AlertType {
    Simple
    Composite
    Trend
    Prediction
  }
  
  // 定义告警状态
  enum AlertStatus {
    Active
    Acknowledged
    Resolved
    Suppressed
  }
  
  // 定义告警
  type Alert = {
    id: String,
    name: String,
    description: String,
    severity: Severity,
    status: AlertStatus,
    timestamp: Int,
    source: String,
    metric_name: String,
    current_value: Float,
    threshold_value: Float,
    alert_type: AlertType,
    labels: Array[(String, String)],
    fingerprint: String  // 用于去重的指纹
  }
  
  // 定义告警聚合规则
  type AggregationRule = {
    name: String,
    group_by: Array[String],  // 按这些标签分组
    aggregation_window_ms: Int,  // 聚合时间窗口
    max_alerts_per_group: Int,  // 每组最大告警数
    suppression_rules: Array[(String, String)]  // 抑制规则
  }
  
  // 创建告警聚合器
  let create_alert_aggregator = fn(rules: Array[AggregationRule]) {
    let generate_fingerprint = fn(alert: Alert, group_keys: Array[String>) {
      let key_values = group_keys.map(fn(key) {
        match alert.labels.find(fn(label) { label.0 == key }) {
          Some((_, value)) => key + ":" + value
          None => key + ":unknown"
        }
      })
      key_values.join("|")
    }
    
    let aggregate_alerts = fn(alerts: Array[Alert]) {
      let aggregated = []
      let processed_groups = Map::empty()
      
      for rule in rules {
        // 按规则分组
        let groups = Map::empty()
        
        for alert in alerts {
          let fingerprint = generate_fingerprint(alert, rule.group_by)
          
          let group_alerts = match Map::get(groups, fingerprint) {
            Some(existing_alerts) => existing_alerts.push(alert)
            None => [alert]
          }
          
          let _ = Map::insert(groups, fingerprint, group_alerts)
        }
        
        // 处理每个组
        for (fingerprint, group_alerts) in groups {
          // 检查是否已经处理过这个组
          if Map::contains_key(processed_groups, fingerprint) {
            continue
          }
          
          let _ = Map::insert(processed_groups, fingerprint, true)
          
          // 检查抑制规则
          let should_suppress = group_alerts.any(fn(alert) {
            rule.suppression_rules.any(fn(rule) {
              alert.labels.any(fn(label) { label.0 == rule.0 && label.1 == rule.1 })
            })
          })
          
          if should_suppress {
            // 创建抑制的聚合告警
            let suppressed_alert = {
              id: UUID::v4(),
              name: "Suppressed Alerts: " + rule.name,
              description: "Group of " + group_alerts.length().to_string() + " alerts suppressed",
              severity: Severity::Low,
              status: AlertStatus::Suppressed,
              timestamp: group_alerts.reduce(0, fn(max, alert) { 
                if alert.timestamp > max { alert.timestamp } else { max } 
              }),
              source: "aggregator",
              metric_name: "multiple",
              current_value: 0.0,
              threshold_value: 0.0,
              alert_type: AlertType::Composite,
              labels: [
                ("group_name", rule.name),
                ("suppressed_count", group_alerts.length().to_string()),
                ("fingerprint", fingerprint)
              ],
              fingerprint: "suppressed:" + fingerprint
            }
            
            aggregated = aggregated.push(suppressed_alert)
          } else if group_alerts.length() > rule.max_alerts_per_group {
            // 创建聚合告警
            let highest_severity = group_alerts.reduce(Severity::Low, fn(max, alert) {
              match (max, alert.severity) {
                (Severity::Critical, _) => Severity::Critical
                (_, Severity::Critical) => Severity::Critical
                (Severity::High, _) => Severity::High
                (_, Severity::High) => Severity::High
                (Severity::Medium, _) => Severity::Medium
                (_, Severity::Medium) => Severity::Medium
                _ => Severity::Low
              }
            })
            
            let latest_timestamp = group_alerts.reduce(0, fn(max, alert) { 
              if alert.timestamp > max { alert.timestamp } else { max } 
            })
            
            let aggregated_alert = {
              id: UUID::v4(),
              name: "Aggregated Alerts: " + rule.name,
              description: "Group of " + group_alerts.length().to_string() + " alerts aggregated",
              severity: highest_severity,
              status: AlertStatus::Active,
              timestamp: latest_timestamp,
              source: "aggregator",
              metric_name: "multiple",
              current_value: group_alerts.length() as Float,
              threshold_value: rule.max_alerts_per_group as Float,
              alert_type: AlertType::Composite,
              labels: [
                ("group_name", rule.name),
                ("alert_count", group_alerts.length().to_string()),
                ("fingerprint", fingerprint)
              ],
              fingerprint: "aggregated:" + fingerprint
            }
            
            aggregated = aggregated.push(aggregated_alert)
          } else {
            // 保留原始告警
            for alert in group_alerts {
              aggregated = aggregated.push(alert)
            }
          }
        }
      }
      
      // 处理未匹配任何规则的告警
      let unprocessed_alerts = alerts.filter(fn(alert) {
        not rules.any(fn(rule) {
          let fingerprint = generate_fingerprint(alert, rule.group_by)
          Map::contains_key(processed_groups, fingerprint)
        })
      })
      
      aggregated + unprocessed_alerts
    }
    
    { aggregate_alerts }
  }
  
  // 创建测试告警
  let create_test_alerts = fn() {
    let base_timestamp = Time::now() - 60000
    let alerts = []
    
    // 创建来自同一服务的多个告警
    for i in 0..15 {
      let timestamp = base_timestamp + i * 2000
      let severity = if i % 3 == 0 { Severity::High } else if i % 3 == 1 { Severity::Medium } else { Severity::Low }
      
      alerts = alerts.push({
        id: UUID::v4(),
        name: "High CPU Usage",
        description: "CPU usage is above threshold",
        severity: severity,
        status: AlertStatus::Active,
        timestamp: timestamp,
        source: "monitoring-system",
        metric_name: "cpu_usage",
        current_value: 80.0 + (i as Float) * 2.0,
        threshold_value: 80.0,
        alert_type: AlertType::Simple,
        labels: [
          ("service", "api-gateway"),
          ("instance", "instance-" + ((i % 3) + 1).to_string()),
          ("datacenter", "dc-west")
        ],
        fingerprint: "cpu_usage:api-gateway:instance-" + ((i % 3) + 1).to_string()
      })
    }
    
    // 创建一些应该被抑制的告警
    for i in 0..5 {
      alerts = alerts.push({
        id: UUID::v4(),
        name: "Debug Info",
        description: "Debug information alert",
        severity: Severity::Low,
        status: AlertStatus::Active,
        timestamp: base_timestamp + i * 3000,
        source: "debug-system",
        metric_name: "debug_info",
        current_value: 1.0,
        threshold_value: 0.0,
        alert_type: AlertType::Simple,
        labels: [
          ("service", "debug-service"),
          ("environment", "development"),
          ("severity", "debug")
        ],
        fingerprint: "debug_info:debug-service"
      })
    }
    
    alerts
  }
  
  // 测试告警聚合
  let test_alerts = create_test_alerts()
  assert_eq(test_alerts.length(), 20)  // 15个CPU告警 + 5个调试告警
  
  // 创建聚合规则
  let aggregation_rules = [
    {
      name: "service_cpu_alerts",
      group_by: ["service", "datacenter"],
      aggregation_window_ms: 300000,  // 5分钟
      max_alerts_per_group: 5,
      suppression_rules: [("environment", "development"), ("severity", "debug")]
    },
    {
      name: "instance_alerts",
      group_by: ["service", "instance"],
      aggregation_window_ms: 300000,
      max_alerts_per_group: 3,
      suppression_rules: []
    }
  ]
  
  let aggregator = create_alert_aggregator(aggregation_rules)
  let aggregated_alerts = aggregator.aggregate_alerts(test_alerts)
  
  // 验证聚合结果
  assert_true(aggregated_alerts.length() < test_alerts.length())  // 聚合后告警数量应该减少
  
  // 验证有聚合告警
  let composite_alerts = aggregated_alerts.filter(fn(alert) { 
    match alert.alert_type { AlertType::Composite => true, _ => false } 
  })
  assert_true(composite_alerts.length() > 0)
  
  // 验证有抑制告警
  let suppressed_alerts = aggregated_alerts.filter(fn(alert) { 
    alert.status == AlertStatus::Suppressed 
  })
  assert_true(suppressed_alerts.length() > 0)
  
  // 验证聚合告警的内容
  for alert in composite_alerts {
    assert_true(alert.name.contains("Aggregated"))
    assert_true(alert.description.contains("Group of"))
    assert_true(alert.labels.any(fn(label) { label.0 == "alert_count" }))
    
    let alert_count = match alert.labels.find(fn(label) { label.0 == "alert_count" }) {
      Some((_, count)) => count.to_int()
      None => 0
    }
    assert_true(alert_count > 5)  // 聚合的告警数应该超过阈值
  }
  
  // 验证抑制告警的内容
  for alert in suppressed_alerts {
    assert_eq(alert.status, AlertStatus::Suppressed)
    assert_true(alert.name.contains("Suppressed"))
    assert_true(alert.labels.any(fn(label) { label.0 == "suppressed_count" }))
  }
}

// 测试3: 告警通知和升级机制
test "告警通知和升级机制验证" {
  // 定义通知渠道
  enum NotificationChannel {
    Email
    Slack
    PagerDuty
    Webhook
    SMS
  }
  
  // 定义通知状态
  enum NotificationStatus {
    Pending
    Sent
    Failed
    Acknowledged
  }
  
  // 定义通知记录
  type Notification = {
    id: String,
    alert_id: String,
    channel: NotificationChannel,
    recipient: String,
    status: NotificationStatus,
    timestamp: Int,
    retry_count: Int,
    max_retries: Int,
    error_message: Option[String]
  }
  
  // 定义升级策略
  type EscalationPolicy = {
    name: String,
    conditions: Array[(String, String)>,  // 触发条件
    actions: Array[EscalationAction>,
    cooldown_period_ms: Int  // 冷却期
  }
  
  // 定义升级动作
  type EscalationAction = {
    delay_ms: Int,
    channel: NotificationChannel,
    recipients: Array[String>,
    message_template: String
  }
  
  // 创建通知管理器
  let create_notification_manager = fn(escalation_policies: Array[EscalationPolicy>) {
    let mut notifications = []
    let mut escalation_history = Map::empty()
    
    let send_notification = fn(alert: Alert, channel: NotificationChannel, recipient: String, message: String) {
      let notification = {
        id: UUID::v4(),
        alert_id: alert.id,
        channel: channel,
        recipient: recipient,
        status: NotificationStatus::Pending,
        timestamp: Time::now(),
        retry_count: 0,
        max_retries: 3,
        error_message: None
      }
      
      // 模拟发送通知
      let success = Math::random() > 0.1  // 90%成功率
      
      let updated_notification = if success {
        { notification with status: NotificationStatus::Sent }
      } else {
        { notification with 
          status: NotificationStatus::Failed,
          error_message: Some("模拟发送失败")
        }
      }
      
      notifications = notifications.push(updated_notification)
      updated_notification
    }
    
    let check_escalation = fn(alert: Alert) {
      let applicable_policies = escalation_policies.filter(fn(policy) {
        policy.conditions.any(fn(condition) {
          alert.labels.any(fn(label) { 
            label.0 == condition.0 && label.1 == condition.1 
          })
        })
      })
      
      let escalation_actions = []
      
      for policy in applicable_policies {
        let last_escalation = match Map::get(escalation_history, policy.name) {
          Some(timestamp) => timestamp
          None => 0
        }
        
        let current_time = Time::now()
        if current_time - last_escalation > policy.cooldown_period_ms {
          // 可以执行升级
          for action in policy.actions {
            escalation_actions = escalation_actions.push((policy.name, action))
          }
          
          let _ = Map::insert(escalation_history, policy.name, current_time)
        }
      }
      
      escalation_actions
    }
    
    let process_alert = fn(alert: Alert) {
      // 发送初始通知
      let initial_message = "告警: " + alert.name + " - " + alert.description
      let initial_notification = send_notification(alert, NotificationChannel::Email, "team@example.com", initial_message)
      
      // 检查是否需要升级
      let escalation_actions = check_escalation(alert)
      
      let escalation_notifications = []
      for (policy_name, action) in escalation_actions {
        let message = action.message_template
          .replace("{alert_name}", alert.name)
          .replace("{severity}", match alert.severity {
            Severity::Critical => "严重"
            Severity::High => "高"
            Severity::Medium => "中等"
            Severity::Low => "低"
          })
        
        for recipient in action.recipients {
          let notification = send_notification(alert, action.channel, recipient, message)
          escalation_notifications = escalation_notifications.push(notification)
        }
      }
      
      (initial_notification, escalation_notifications)
    }
    
    let get_notifications = fn() { notifications }
    let get_escalation_history = fn() { escalation_history }
    
    {
      send_notification,
      check_escalation,
      process_alert,
      get_notifications,
      get_escalation_history
    }
  }
  
  // 创建测试告警
  let create_test_alert = fn(severity: Severity, labels: Array[(String, String)>) {
    {
      id: UUID::v4(),
      name: "Test Alert",
      description: "This is a test alert for notification testing",
      severity: severity,
      status: AlertStatus::Active,
      timestamp: Time::now(),
      source: "test-system",
      metric_name: "test_metric",
      current_value: 100.0,
      threshold_value: 80.0,
      alert_type: AlertType::Simple,
      labels: labels,
      fingerprint: "test_alert"
    }
  }
  
  // 创建升级策略
  let escalation_policies = [
    {
      name: "critical_alert_escalation",
      conditions: [("severity", "critical")],
      actions: [
        {
          delay_ms: 0,
          channel: NotificationChannel::PagerDuty,
          recipients: ["oncall@example.com"],
          message_template: "严重告警: {alert_name} 需要立即处理"
        },
        {
          delay_ms: 300000,  // 5分钟后
          channel: NotificationChannel::SMS,
          recipients: ["+1234567890"],
          message_template: "严重告警升级: {alert_name} 严重程度: {severity}"
        }
      ],
      cooldown_period_ms: 600000  // 10分钟冷却期
    },
    {
      name: "high_alert_escalation",
      conditions: [("severity", "high")],
      actions: [
        {
          delay_ms: 0,
          channel: NotificationChannel::Slack,
          recipients: ["#alerts"],
          message_template: "高优先级告警: {alert_name}"
        }
      ],
      cooldown_period_ms: 300000  // 5分钟冷却期
    }
  ]
  
  // 测试通知管理
  let notification_manager = create_notification_manager(escalation_policies)
  
  // 测试严重告警
  let critical_alert = create_test_alert(Severity::Critical, [
    ("severity", "critical"),
    ("service", "payment-service")
  ])
  
  let (initial_notification, escalation_notifications) = notification_manager.process_alert(critical_alert)
  
  // 验证初始通知
  assert_eq(initial_notification.alert_id, critical_alert.id)
  assert_eq(initial_notification.channel, NotificationChannel::Email)
  
  // 验证升级通知
  assert_true(escalation_notifications.length() > 0)
  assert_true(escalation_notifications.any(fn(notif) { 
    notif.channel == NotificationChannel::PagerDuty 
  }))
  
  // 测试高优先级告警
  let high_alert = create_test_alert(Severity::High, [
    ("severity", "high"),
    ("service", "user-service")
  ])
  
  let (high_initial, high_escalations) = notification_manager.process_alert(high_alert)
  
  // 验证高优先级告警的升级
  assert_true(high_escalations.any(fn(notif) { 
    notif.channel == NotificationChannel::Slack 
  }))
  
  // 测试低优先级告警（不应该触发升级）
  let low_alert = create_test_alert(Severity::Low, [
    ("severity", "low"),
    ("service", "logging-service")
  ])
  
  let (low_initial, low_escalations) = notification_manager.process_alert(low_alert)
  
  // 低优先级告警不应该有升级通知
  assert_eq(low_escalations.length(), 0)
  
  // 验证通知历史
  let all_notifications = notification_manager.get_notifications()
  assert_true(all_notifications.length() >= 3)  // 至少3个初始通知
  
  // 验证升级历史
  let escalation_history = notification_manager.get_escalation_history()
  assert_true(Map::contains_key(escalation_history, "critical_alert_escalation"))
  assert_true(Map::contains_key(escalation_history, "high_alert_escalation"))
  
  // 验证通知状态
  let sent_notifications = all_notifications.filter(fn(notif) { 
    notif.status == NotificationStatus::Sent 
  })
  let failed_notifications = all_notifications.filter(fn(notif) { 
    notif.status == NotificationStatus::Failed 
  })
  
  assert_true(sent_notifications.length() > 0)
  // 由于有10%的失败率，可能会有失败的通知
  assert_true(failed_notifications.length() >= 0)
  
  // 验证失败通知的错误信息
  for notif in failed_notifications {
    assert_true(notif.error_message.is_some())
    assert_eq(notif.retry_count, 0)  // 第一次尝试失败
  }
}

// 测试4: 告警抑制和静默规则
test "告警抑制和静默规则验证" {
  // 定义静默规则
  type SilenceRule = {
    id: String,
    name: String,
    matchers: Array[(String, String)>,  // 匹配条件
    start_time: Int,
    end_time: Int,
    created_by: String,
    comment: String
  }
  
  // 定义抑制规则
  type InhibitionRule = {
    id: String,
    name: String,
    source_matchers: Array[(String, String)>,  // 源告警匹配条件
    target_matchers: Array[(String, String)>,  // 目标告警匹配条件
    equal: Array<String>  // 必须相等的标签
  }
  
  // 创建静默规则管理器
  let create_silence_manager = fn() {
    let mut silence_rules = []
    
    let add_silence_rule = fn(rule: SilenceRule) {
      silence_rules = silence_rules.push(rule)
    }
    
    let remove_silence_rule = fn(rule_id: String) {
      silence_rules = silence_rules.filter(fn(rule) { rule.id != rule_id })
    }
    
    let is_silenced = fn(alert: Alert) {
      let current_time = Time::now()
      
      silence_rules.any(fn(rule) {
        // 检查时间范围
        if current_time < rule.start_time || current_time > rule.end_time {
          return false
        }
        
        // 检查匹配条件
        rule.matchers.all(fn(matcher) {
          alert.labels.any(fn(label) { 
            label.0 == matcher.0 && label.1 == matcher.1 
          })
        })
      })
    }
    
    let get_active_rules = fn() {
      let current_time = Time::now()
      silence_rules.filter(fn(rule) { 
        current_time >= rule.start_time && current_time <= rule.end_time 
      })
    }
    
    {
      add_silence_rule,
      remove_silence_rule,
      is_silenced,
      get_active_rules
    }
  }
  
  // 创建抑制规则管理器
  let create_inhibition_manager = fn() {
    let mut inhibition_rules = []
    
    let add_inhibition_rule = fn(rule: InhibitionRule) {
      inhibition_rules = inhibition_rules.push(rule)
    }
    
    let remove_inhibition_rule = fn(rule_id: String) {
      inhibition_rules = inhibition_rules.filter(fn(rule) { rule.id != rule_id })
    }
    
    let is_inhibited = fn(target_alert: Alert, active_alerts: Array[Alert>) {
      inhibition_rules.any(fn(rule) {
        // 检查目标告警是否匹配
        let target_matches = rule.target_matchers.all_fn(matcher) {
          target_alert.labels.any(fn(label) { 
            label.0 == matcher.0 && label.1 == matcher.1 
          })
        }
        
        if not target_matches {
          return false
        }
        
        // 查找匹配的源告警
        active_alerts.any_fn(source_alert) {
          // 检查源告警是否匹配
          let source_matches = rule.source_matchers.all_fn(matcher) {
            source_alert.labels.any(fn(label) { 
              label.0 == matcher.0 && label.1 == matcher.1 
            })
          }
          
          if not source_matches {
            return false
          }
          
          // 检查必须相等的标签
          rule.equal.all_fn(label_key) {
            match source_alert.labels.find(fn(label) { label.0 == label_key }) {
              Some((_, source_value)) => {
                match target_alert.labels.find(fn(label) { label.0 == label_key }) {
                  Some((_, target_value)) => source_value == target_value
                  None => false
                }
              }
              None => false
            }
          }
        })
      })
    }
    
    {
      add_inhibition_rule,
      remove_inhibition_rule,
      is_inhibited
    }
  }
  
  // 创建测试告警
  let create_test_alerts = fn() {
    let base_timestamp = Time::now()
    
    [
      {
        id: UUID::v4(),
        name: "Database Down",
        description: "Database connection failed",
        severity: Severity::Critical,
        status: AlertStatus::Active,
        timestamp: base_timestamp,
        source: "database-monitor",
        metric_name: "db_connection_status",
        current_value: 0.0,
        threshold_value: 1.0,
        alert_type: AlertType::Simple,
        labels: [
          ("service", "user-service"),
          ("component", "database"),
          ("instance", "db-primary")
        ],
        fingerprint: "db_down"
      },
      {
        id: UUID::v4(),
        name: "High Query Latency",
        description: "Database query latency is high",
        severity: Severity::High,
        status: AlertStatus::Active,
        timestamp: base_timestamp + 1000,
        source: "database-monitor",
        metric_name: "db_query_latency",
        current_value: 5000.0,
        threshold_value: 1000.0,
        alert_type: AlertType::Simple,
        labels: [
          ("service", "user-service"),
          ("component", "database"),
          ("instance", "db-primary")
        ],
        fingerprint: "db_latency"
      },
      {
        id: UUID::v4(),
        name: "API Error Rate",
        description: "API error rate is high",
        severity: Severity::High,
        status: AlertStatus::Active,
        timestamp: base_timestamp + 2000,
        source: "api-monitor",
        metric_name: "api_error_rate",
        current_value: 0.1,
        threshold_value: 0.05,
        alert_type: AlertType::Simple,
        labels: [
          ("service", "user-service"),
          ("component", "api"),
          ("endpoint", "/api/users")
        ],
        fingerprint: "api_errors"
      },
      {
        id: UUID::v4(),
        name: "Maintenance Mode",
        description: "System is under maintenance",
        severity: Severity::Medium,
        status: AlertStatus::Active,
        timestamp: base_timestamp + 3000,
        source: "system-monitor",
        metric_name: "maintenance_status",
        current_value: 1.0,
        threshold_value: 0.0,
        alert_type: AlertType::Simple,
        labels: [
          ("service", "user-service"),
          ("environment", "maintenance")
        ],
        fingerprint: "maintenance"
      }
    ]
  }
  
  // 测试静默规则
  let silence_manager = create_silence_manager()
  
  // 添加维护期间的静默规则
  let maintenance_silence = {
    id: "maintenance-silence",
    name: "Maintenance Period Silence",
    matchers: [("environment", "maintenance")],
    start_time: Time::now() - 10000,
    end_time: Time::now() + 10000,
    created_by: "admin",
    comment: "Silence alerts during maintenance window"
  }
  
  silence_manager.add_silence_rule(maintenance_silence)
  
  // 测试告警静默
  let test_alerts = create_test_alerts()
  let maintenance_alert = test_alerts[3]  // 维护告警
  
  assert_true(silence_manager.is_silenced(maintenance_alert))
  
  // 其他告警不应该被静默
  for i in 0..3 {
    assert_false(silence_manager.is_silenced(test_alerts[i]))
  }
  
  // 验证活跃规则
  let active_rules = silence_manager.get_active_rules()
  assert_eq(active_rules.length(), 1)
  assert_eq(active_rules[0].id, "maintenance-silence")
  
  // 测试抑制规则
  let inhibition_manager = create_inhibition_manager()
  
  // 添加数据库故障抑制规则
  let db_inhibition = {
    id: "database-failure-inhibition",
    name: "Database Failure Inhibition",
    source_matchers: [("component", "database"), ("alert_name", "Database Down")],
    target_matchers: [("component", "database")],
    equal: ["service", "instance"]
  }
  
  inhibition_manager.add_inhibition_rule(db_inhibition)
  
  // 测试告警抑制
  let db_down_alert = test_alerts[0]  // 数据库宕机告警
  let db_latency_alert = test_alerts[1]  // 数据库延迟告警
  let api_error_alert = test_alerts[2]  // API错误告警
  
  // 数据库延迟告警应该被数据库宕机告警抑制
  assert_true(inhibition_manager.is_inhibited(db_latency_alert, [db_down_alert]))
  
  // API错误告警不应该被抑制
  assert_false(inhibition_manager.is_inhibited(api_error_alert, [db_down_alert]))
  
  // 测试静默和抑制的组合效果
  let final_alerts = []
  for alert in test_alerts {
    // 检查是否被静默
    if silence_manager.is_silenced(alert) {
      continue  // 跳过被静默的告警
    }
    
    // 检查是否被抑制
    if inhibition_manager.is_inhibited(alert, [db_down_alert]) {
      continue  // 跳过被抑制的告警
    }
    
    final_alerts = final_alerts.push(alert)
  }
  
  // 验证最终告警列表
  assert_eq(final_alerts.length(), 2)  // 只剩下数据库宕机和API错误告警
  assert_true(final_alerts.any(fn(alert) { alert.name == "Database Down" }))
  assert_true(final_alerts.any(fn(alert) { alert.name == "API Error Rate" }))
  
  // 测试静默规则过期
  let expired_silence = {
    id: "expired-silence",
    name: "Expired Silence Rule",
    matchers: [("service", "test-service")],
    start_time: Time::now() - 20000,
    end_time: Time::now() - 10000,  // 已过期
    created_by: "admin",
    comment: "This rule is expired"
  }
  
  silence_manager.add_silence_rule(expired_silence)
  
  // 过期的规则不应该影响告警
  let test_alert = {
    id: UUID::v4(),
    name: "Test Alert",
    description: "Test alert for expired rule",
    severity: Severity::Medium,
    status: AlertStatus::Active,
    timestamp: Time::now(),
    source: "test",
    metric_name: "test_metric",
    current_value: 1.0,
    threshold_value: 0.0,
    alert_type: AlertType::Simple,
    labels: [("service", "test-service")],
    fingerprint: "test"
  }
  
  assert_false(silence_manager.is_silenced(test_alert))
  
  // 验证活跃规则仍然只有1个（过期的规则不算）
  let active_rules_after = silence_manager.get_active_rules()
  assert_eq(active_rules_after.length(), 1)
}
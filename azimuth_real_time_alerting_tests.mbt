// Azimuth 实时告警测试套件
// 专注于遥测数据的实时监控和告警系统功能

// 测试1: 阈值告警系统
test "阈值告警系统" {
  // 模拟实时遥测数据流
  let telemetry_stream = [
    { metric: "cpu_usage", value: 45.0, timestamp: 1640995200, service: "api", host: "server-1" },
    { metric: "cpu_usage", value: 52.0, timestamp: 1640995201, service: "api", host: "server-1" },
    { metric: "cpu_usage", value: 78.0, timestamp: 1640995202, service: "api", host: "server-1" },
    { metric: "cpu_usage", value: 85.0, timestamp: 1640995203, service: "api", host: "server-1" },
    { metric: "cpu_usage", value: 91.0, timestamp: 1640995204, service: "api", host: "server-1" },
    { metric: "cpu_usage", value: 88.0, timestamp: 1640995205, service: "api", host: "server-1" },
    { metric: "cpu_usage", value: 65.0, timestamp: 1640995206, service: "api", host: "server-1" },
    { metric: "cpu_usage", value: 42.0, timestamp: 1640995207, service: "api", host: "server-1" }
  ]
  
  // 定义告警规则
  let alert_rules = [
    {
      name: "high_cpu_usage",
      metric: "cpu_usage",
      condition: "greater_than",
      threshold: 80.0,
      severity: "warning",
      duration: 2  // 持续2秒触发告警
    },
    {
      name: "critical_cpu_usage",
      metric: "cpu_usage",
      condition: "greater_than",
      threshold: 90.0,
      severity: "critical",
      duration: 1  // 持续1秒触发告警
    }
  ]
  
  // 阈值告警检测引擎
  let threshold_alert_engine = fn(stream: Array[Dynamic], rules: Array[Dynamic]) {
    let mut alerts = []
    let mut alert_states = {}  // 跟踪每个规则的状态
    
    for data_point in stream {
      for rule in rules {
        // 检查规则是否适用于当前数据点
        if data_point.metric == rule.metric {
          let condition_met = match rule.condition {
            "greater_than" => data_point.value > rule.threshold,
            "less_than" => data_point.value < rule.threshold,
            "equals" => data_point.value == rule.threshold,
            _ => false
          }
          
          let rule_key = rule.name + ":" + data_point.service + ":" + data_point.host
          
          if condition_met {
            // 条件满足，增加持续时间计数
            let current_count = match alert_states.get(rule_key) {
              Some(count) => count + 1
              None => 1
            }
            alert_states = alert_states.set(rule_key, current_count)
            
            // 检查是否达到触发持续时间
            if current_count >= rule.duration {
              // 检查是否已经发送过此告警
              let alert_key = rule_key + ":" + data_point.timestamp.to_string()
              let already_alerted = alerts.exists(fn(alert) { alert.key == alert_key })
              
              if not already_alerted {
                alerts = alerts.push({
                  key: alert_key,
                  rule_name: rule.name,
                  metric: data_point.metric,
                  value: data_point.value,
                  threshold: rule.threshold,
                  severity: rule.severity,
                  timestamp: data_point.timestamp,
                  service: data_point.service,
                  host: data_point.host,
                  message: rule.name + " triggered: " + data_point.metric + " is " + 
                           data_point.value.to_string() + " (threshold: " + 
                           rule.threshold.to_string() + ") on " + 
                           data_point.service + "/" + data_point.host
                })
              }
            }
          } else {
            // 条件不满足，重置计数器
            alert_states = alert_states.remove(rule_key)
          }
        }
      }
    }
    
    alerts
  }
  
  // 执行阈值告警检测
  let triggered_alerts = threshold_alert_engine(telemetry_stream, alert_rules)
  
  // 验证告警触发结果
  assert_eq(triggered_alerts.length(), 3)  // 应该触发3个告警
  
  // 验证第一个警告级别告警（CPU使用率超过80%持续2秒）
  let warning_alert = triggered_alerts[0]
  assert_eq(warning_alert.rule_name, "high_cpu_usage")
  assert_eq(warning_alert.metric, "cpu_usage")
  assert_eq(warning_alert.value, 85.0)
  assert_eq(warning_alert.threshold, 80.0)
  assert_eq(warning_alert.severity, "warning")
  assert_eq(warning_alert.timestamp, 1640995203)
  assert_eq(warning_alert.service, "api")
  assert_eq(warning_alert.host, "server-1")
  
  // 验证第二个警告级别告警（CPU使用率继续超过80%）
  let warning_alert2 = triggered_alerts[1]
  assert_eq(warning_alert2.rule_name, "high_cpu_usage")
  assert_eq(warning_alert2.value, 91.0)
  assert_eq(warning_alert2.timestamp, 1640995204)
  
  // 验证严重级别告警（CPU使用率超过90%持续1秒）
  let critical_alert = triggered_alerts[2]
  assert_eq(critical_alert.rule_name, "critical_cpu_usage")
  assert_eq(critical_alert.metric, "cpu_usage")
  assert_eq(critical_alert.value, 91.0)
  assert_eq(critical_alert.threshold, 90.0)
  assert_eq(critical_alert.severity, "critical")
  assert_eq(critical_alert.timestamp, 1640995204)
  assert_eq(critical_alert.service, "api")
  assert_eq(critical_alert.host, "server-1")
  
  // 验证告警消息格式
  assert_true(warning_alert.message.contains("high_cpu_usage triggered"))
  assert_true(warning_alert.message.contains("cpu_usage"))
  assert_true(warning_alert.message.contains("85.0"))
  assert_true(warning_alert.message.contains("80.0"))
  assert_true(warning_alert.message.contains("api/server-1"))
}

// 测试2: 趋势告警系统
test "趋势告警系统" {
  // 模拟显示趋势的遥测数据
  let trend_data = []
  let mut i = 0
  while i < 20 {
    let base_value = 50.0
    let trend_value = base_value + i.to_float() * 2.0  // 每次增加2
    let noise = (i % 3).to_float() * 0.5  // 小幅随机噪声
    
    trend_data = trend_data.push({
      metric: "memory_usage",
      value: trend_value + noise,
      timestamp: 1640995200 + i * 60,
      service: "database",
      host: "db-server-1"
    })
    i = i + 1
  }
  
  // 定义趋势告警规则
  let trend_alert_rules = [
    {
      name: "increasing_memory_trend",
      metric: "memory_usage",
      trend_type: "increasing",
      threshold: 1.5,  // 每分钟增长超过1.5%
      window_size: 5,   // 5个数据点的窗口
      min_points: 3     // 至少需要3个点确认趋势
    },
    {
      name: "rapid_memory_growth",
      metric: "memory_usage",
      trend_type: "increasing",
      threshold: 5.0,   // 每分钟增长超过5%
      window_size: 3,   // 3个数据点的窗口
      min_points: 2     // 至少需要2个点确认趋势
    }
  ]
  
  // 趋势检测算法
  let detect_trend = fn(data: Array[Dynamic], window_size: Int) {
    if data.length() < window_size {
      { trend: 0.0, confidence: 0.0 }
    } else {
      // 简单线性回归计算趋势
      let mut sum_x = 0.0
      let mut sum_y = 0.0
      let mut sum_xy = 0.0
      let mut sum_x2 = 0.0
      let n = window_size.to_float()
      
      let mut i = 0
      while i < window_size {
        let x = i.to_float()
        let y = data[data.length() - window_size + i].value
        sum_x = sum_x + x
        sum_y = sum_y + y
        sum_xy = sum_xy + (x * y)
        sum_x2 = sum_x2 + (x * x)
        i = i + 1
      }
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      let intercept = (sum_y - slope * sum_x) / n
      
      // 计算R²作为置信度
      let mut sum_y_mean = 0.0
      let mut i = 0
      while i < window_size {
        sum_y_mean = sum_y_mean + data[data.length() - window_size + i].value
        i = i + 1
      }
      let y_mean = sum_y_mean / n
      
      let mut ss_tot = 0.0
      let mut ss_res = 0.0
      let mut i = 0
      while i < window_size {
        let x = i.to_float()
        let y = data[data.length() - window_size + i].value
        let y_pred = intercept + slope * x
        
        ss_tot = ss_tot + ((y - y_mean) * (y - y_mean))
        ss_res = ss_res + ((y - y_pred) * (y - y_pred))
        i = i + 1
      }
      
      let r_squared = if ss_tot > 0.0 { 1.0 - (ss_res / ss_tot) } else { 0.0 }
      let confidence = if r_squared > 0.0 { r_squared.sqrt() } else { 0.0 }
      
      { trend: slope, confidence }
    }
  }
  
  // 趋势告警检测引擎
  let trend_alert_engine = fn(stream: Array[Dynamic], rules: Array[Dynamic]) {
    let mut alerts = []
    let mut data_by_metric = {}
    
    // 按指标分组数据
    for data_point in stream {
      let metric_data = match data_by_metric.get(data_point.metric) {
        Some(data) => data.push(data_point)
        None => [data_point]
      }
      data_by_metric = data_by_metric.set(data_point.metric, metric_data)
    }
    
    // 对每个指标应用趋势检测
    for (metric, data) in data_by_metric {
      for rule in rules {
        if metric == rule.metric {
          let mut i = rule.min_points - 1
          while i < data.length() {
            let window_data = data.slice(i - rule.min_points + 1, i + 1)
            let trend_result = detect_trend(window_data, rule.window_size)
            
            let trend_detected = match rule.trend_type {
              "increasing" => trend_result.trend > rule.threshold,
              "decreasing" => trend_result.trend < -rule.threshold,
              _ => false
            }
            
            if trend_detected and trend_result.confidence > 0.5 {
              let alert_key = rule.name + ":" + data[i].service + ":" + data[i].host + ":" + data[i].timestamp.to_string()
              let already_alerted = alerts.exists(fn(alert) { alert.key == alert_key })
              
              if not already_alerted {
                alerts = alerts.push({
                  key: alert_key,
                  rule_name: rule.name,
                  metric: data[i].metric,
                  current_value: data[i].value,
                  trend: trend_result.trend,
                  confidence: trend_result.confidence,
                  threshold: rule.threshold,
                  timestamp: data[i].timestamp,
                  service: data[i].service,
                  host: data[i].host,
                  message: rule.name + " detected: " + metric + " is trending " + 
                           rule.trend_type + " at " + trend_result.trend.to_string() + 
                           " per minute (confidence: " + (trend_result.confidence * 100.0).to_string() + 
                           "%) on " + data[i].service + "/" + data[i].host
                })
              }
            }
            
            i = i + 1
          }
        }
      }
    }
    
    alerts
  }
  
  // 执行趋势告警检测
  let trend_alerts = trend_alert_engine(trend_data, trend_alert_rules)
  
  // 验证趋势告警结果
  assert_true(trend_alerts.length() > 0)  // 应该触发至少一个趋势告警
  
  // 验证第一个趋势告警
  let first_trend_alert = trend_alerts[0]
  assert_eq(first_trend_alert.rule_name, "increasing_memory_trend")
  assert_eq(first_trend_alert.metric, "memory_usage")
  assert_true(first_trend_alert.trend > 1.5)  // 趋势应该超过阈值
  assert_true(first_trend_alert.confidence > 0.5)  // 置信度应该超过50%
  assert_eq(first_trend_alert.service, "database")
  assert_eq(first_trend_alert.host, "db-server-1")
  
  // 验证告警消息格式
  assert_true(first_trend_alert.message.contains("increasing_memory_trend detected"))
  assert_true(first_trend_alert.message.contains("memory_usage"))
  assert_true(first_trend_alert.message.contains("trending increasing"))
  assert_true(first_trend_alert.message.contains("confidence"))
  assert_true(first_trend_alert.message.contains("database/db-server-1"))
  
  // 验证快速增长告警
  let rapid_growth_alerts = trend_alerts.filter(fn(alert) { alert.rule_name == "rapid_memory_growth" })
  assert_true(rapid_growth_alerts.length() > 0)  // 应该触发快速增长告警
  
  let rapid_alert = rapid_growth_alerts[0]
  assert_true(rapid_alert.trend > 5.0)  // 趋势应该超过快速增长阈值
  assert_true(rapid_alert.confidence > 0.5)
}

// 测试3: 复合告警系统
test "复合告警系统" {
  // 模拟多指标遥测数据
  let multi_metric_data = [
    { timestamp: 1640995200, service: "payment", metrics: [
      { name: "cpu_usage", value: 45.0 },
      { name: "memory_usage", value: 1024.0 },
      { name: "error_rate", value: 0.01 },
      { name: "response_time", value: 120.0 }
    ]},
    { timestamp: 1640995201, service: "payment", metrics: [
      { name: "cpu_usage", value: 52.0 },
      { name: "memory_usage", value: 1080.0 },
      { name: "error_rate", value: 0.02 },
      { name: "response_time", value: 150.0 }
    ]},
    { timestamp: 1640995202, service: "payment", metrics: [
      { name: "cpu_usage", value: 78.0 },
      { name: "memory_usage", value: 1150.0 },
      { name: "error_rate", value: 0.05 },
      { name: "response_time", value: 200.0 }
    ]},
    { timestamp: 1640995203, service: "payment", metrics: [
      { name: "cpu_usage", value: 85.0 },
      { name: "memory_usage", value: 1200.0 },
      { name: "error_rate", value: 0.08 },
      { name: "response_time", value: 350.0 }
    ]},
    { timestamp: 1640995204, service: "payment", metrics: [
      { name: "cpu_usage", value: 88.0 },
      { name: "memory_usage", value: 1250.0 },
      { name: "error_rate", value: 0.12 },
      { name: "response_time", value: 500.0 }
    ]}
  ]
  
  // 定义复合告警规则
  let composite_alert_rules = [
    {
      name: "service_degradation",
      conditions: [
        { metric: "cpu_usage", operator: ">", threshold: 80.0 },
        { metric: "memory_usage", operator: ">", threshold: 1100.0 },
        { metric: "error_rate", operator: ">", threshold: 0.05 }
      ],
      logic: "AND",  // 所有条件都必须满足
      severity: "warning"
    },
    {
      name: "critical_service_failure",
      conditions: [
        { metric: "cpu_usage", operator: ">", threshold: 85.0 },
        { metric: "error_rate", operator: ">", threshold: 0.1 },
        { metric: "response_time", operator: ">", threshold: 400.0 }
      ],
      logic: "AND",  // 所有条件都必须满足
      severity: "critical"
    },
    {
      name: "performance_or_reliability_issue",
      conditions: [
        { metric: "response_time", operator: ">", threshold: 300.0 },
        { metric: "error_rate", operator: ">", threshold: 0.08 }
      ],
      logic: "OR",  // 任一条件满足即可
      severity: "warning"
    }
  ]
  
  // 复合告警检测引擎
  let composite_alert_engine = fn(stream: Array[Dynamic], rules: Array[Dynamic]) {
    let mut alerts = []
    
    for data_point in stream {
      for rule in rules {
        // 评估每个条件
        let mut condition_results = []
        
        for condition in rule.conditions {
          let metric_value = {
            let mut found = false
            let mut value = 0.0
            for metric in data_point.metrics {
              if metric.name == condition.metric {
                value = metric.value
                found = true
                break
              }
            }
            if found { value } else { 0.0 }
          }
          
          let condition_met = match condition.operator {
            ">" => metric_value > condition.threshold,
            "<" => metric_value < condition.threshold,
            ">=" => metric_value >= condition.threshold,
            "<=" => metric_value <= condition.threshold,
            "==" => metric_value == condition.threshold,
            _ => false
          }
          
          condition_results = condition_results.push({
            metric: condition.metric,
            value: metric_value,
            threshold: condition.threshold,
            operator: condition.operator,
            met: condition_met
          })
        }
        
        // 评估复合条件
        let rule_triggered = match rule.logic {
          "AND" => condition_results.fold(true, fn(acc, result) { acc and result.met }),
          "OR" => condition_results.fold(false, fn(acc, result) { acc or result.met }),
          _ => false
        }
        
        if rule_triggered {
          let alert_key = rule.name + ":" + data_point.service + ":" + data_point.timestamp.to_string()
          let already_alerted = alerts.exists(fn(alert) { alert.key == alert_key })
          
          if not already_alerted {
            alerts = alerts.push({
              key: alert_key,
              rule_name: rule.name,
              severity: rule.severity,
              timestamp: data_point.timestamp,
              service: data_point.service,
              condition_results: condition_results,
              message: rule.name + " triggered on " + data_point.service + 
                       " at " + data_point.timestamp.to_string()
            })
          }
        }
      }
    }
    
    alerts
  }
  
  // 执行复合告警检测
  let composite_alerts = composite_alert_engine(multi_metric_data, composite_alert_rules)
  
  // 验证复合告警结果
  assert_eq(composite_alerts.length(), 3)  // 应该触发3个复合告警
  
  // 验证服务降级告警
  let degradation_alert = composite_alerts[0]
  assert_eq(degradation_alert.rule_name, "service_degradation")
  assert_eq(degradation_alert.severity, "warning")
  assert_eq(degradation_alert.service, "payment")
  assert_eq(degradation_alert.timestamp, 1640995203)
  assert_eq(degradation_alert.condition_results.length(), 3)
  
  // 验证所有条件都满足（AND逻辑）
  for condition in degradation_alert.condition_results {
    assert_true(condition.met)
  }
  
  // 验证严重服务故障告警
  let critical_alert = composite_alerts[1]
  assert_eq(critical_alert.rule_name, "critical_service_failure")
  assert_eq(critical_alert.severity, "critical")
  assert_eq(critical_alert.service, "payment")
  assert_eq(critical_alert.timestamp, 1640995204)
  
  // 验证性能或可靠性问题告警
  let performance_alert = composite_alerts[2]
  assert_eq(performance_alert.rule_name, "performance_or_reliability_issue")
  assert_eq(performance_alert.severity, "warning")
  assert_eq(performance_alert.service, "payment")
  assert_eq(performance_alert.timestamp, 1640995204)
  
  // 验证OR逻辑：至少一个条件满足
  let or_conditions_met = performance_alert.condition_results.fold(false, fn(acc, result) { acc or result.met })
  assert_true(or_conditions_met)
}

// 测试4: 告警抑制和恢复机制
test "告警抑制和恢复机制" {
  // 模拟包含告警触发和恢复的数据流
  let alert_recovery_data = [
    { metric: "error_rate", value: 0.01, timestamp: 1640995200, service: "auth", host: "auth-1" },
    { metric: "error_rate", value: 0.02, timestamp: 1640995201, service: "auth", host: "auth-1" },
    { metric: "error_rate", value: 0.08, timestamp: 1640995202, service: "auth", host: "auth-1" },  // 触发告警
    { metric: "error_rate", value: 0.12, timestamp: 1640995203, service: "auth", host: "auth-1" },  // 持续告警
    { metric: "error_rate", value: 0.10, timestamp: 1640995204, service: "auth", host: "auth-1" },  // 持续告警
    { metric: "error_rate", value: 0.06, timestamp: 1640995205, service: "auth", host: "auth-1" },  // 恢复
    { metric: "error_rate", value: 0.03, timestamp: 1640995206, service: "auth", host: "auth-1" },  // 正常
    { metric: "error_rate", value: 0.02, timestamp: 1640995207, service: "auth", host: "auth-1" },  // 正常
    { metric: "error_rate", value: 0.09, timestamp: 1640995208, service: "auth", host: "auth-1" },  // 再次触发
    { metric: "error_rate", value: 0.04, timestamp: 1640995209, service: "auth", host: "auth-1" }   // 再次恢复
  ]
  
  // 定义告警规则
  let alert_rule = {
    name: "high_error_rate",
    metric: "error_rate",
    threshold: 0.05,
    severity: "warning",
    trigger_duration: 1,    // 持续1秒触发
    recovery_duration: 2,   // 持续2秒恢复
    suppression_window: 3   // 3秒内不重复告警
  }
  
  // 带抑制和恢复的告警引擎
  let suppression_recovery_alert_engine = fn(stream: Array[Dynamic], rule: Dynamic) {
    let mut alerts = []
    let mut alert_states = {}  // 跟踪告警状态
    let mut last_alert_times = {}  // 跟踪最后告警时间
    
    for data_point in stream {
      if data_point.metric == rule.metric {
        let condition_met = data_point.value > rule.threshold
        let alert_key = rule.name + ":" + data_point.service + ":" + data_point.host
        
        if condition_met {
          // 条件满足，增加持续时间计数
          let current_count = match alert_states.get(alert_key) {
            Some(count) => count + 1
            None => 1
          }
          alert_states = alert_states.set(alert_key, current_count)
          
          // 检查是否达到触发持续时间
          if current_count >= rule.trigger_duration {
            // 检查是否在抑制窗口内
            let last_alert_time = match last_alert_times.get(alert_key) {
              Some(time) => time
              None => 0
            }
            
            let time_since_last_alert = data_point.timestamp - last_alert_time
            
            if time_since_last_alert >= rule.suppression_window {
              // 触发告警
              alerts = alerts.push({
                key: alert_key + ":" + data_point.timestamp.to_string(),
                rule_name: rule.name,
                metric: data_point.metric,
                value: data_point.value,
                threshold: rule.threshold,
                severity: rule.severity,
                timestamp: data_point.timestamp,
                service: data_point.service,
                host: data_point.host,
                state: "triggered",
                message: rule.name + " triggered: " + data_point.metric + " is " + 
                         data_point.value.to_string() + " (threshold: " + 
                         rule.threshold.to_string() + ")"
              })
              
              // 更新最后告警时间
              last_alert_times = last_alert_times.set(alert_key, data_point.timestamp)
            }
          }
        } else {
          // 条件不满足，检查恢复条件
          let current_count = match alert_states.get(alert_key) {
            Some(count) => count
            None => 0
          }
          
          if current_count > 0 {
            // 告警曾经触发过，检查恢复
            let recovery_count = match alert_states.get(alert_key + ":recovery") {
              Some(count) => count + 1
              None => 1
            }
            alert_states = alert_states.set(alert_key + ":recovery", recovery_count)
            
            if recovery_count >= rule.recovery_duration {
              // 触发恢复告警
              alerts = alerts.push({
                key: alert_key + ":recovery:" + data_point.timestamp.to_string(),
                rule_name: rule.name,
                metric: data_point.metric,
                value: data_point.value,
                threshold: rule.threshold,
                severity: "info",  // 恢复告警使用info级别
                timestamp: data_point.timestamp,
                service: data_point.service,
                host: data_point.host,
                state: "recovered",
                message: rule.name + " recovered: " + data_point.metric + " is " + 
                         data_point.value.to_string() + " (threshold: " + 
                         rule.threshold.to_string() + ")"
              })
              
              // 重置告警状态
              alert_states = alert_states.remove(alert_key)
              alert_states = alert_states.remove(alert_key + ":recovery")
            }
          }
        }
      }
    }
    
    alerts
  }
  
  // 执行带抑制和恢复的告警检测
  let suppression_recovery_alerts = suppression_recovery_alert_engine(alert_recovery_data, alert_rule)
  
  // 验证告警结果
  assert_eq(suppression_recovery_alerts.length(), 4)  // 应该有4个告警（2次触发，2次恢复）
  
  // 验证第一次触发告警
  let first_trigger = suppression_recovery_alerts[0]
  assert_eq(first_trigger.rule_name, "high_error_rate")
  assert_eq(first_trigger.state, "triggered")
  assert_eq(first_trigger.value, 0.08)
  assert_eq(first_trigger.timestamp, 1640995202)
  assert_eq(first_trigger.severity, "warning")
  
  // 验证第一次恢复告警
  let first_recovery = suppression_recovery_alerts[1]
  assert_eq(first_recovery.rule_name, "high_error_rate")
  assert_eq(first_recovery.state, "recovered")
  assert_eq(first_recovery.value, 0.03)
  assert_eq(first_recovery.timestamp, 1640995206)
  assert_eq(first_recovery.severity, "info")
  
  // 验证第二次触发告警
  let second_trigger = suppression_recovery_alerts[2]
  assert_eq(second_trigger.rule_name, "high_error_rate")
  assert_eq(second_trigger.state, "triggered")
  assert_eq(second_trigger.value, 0.09)
  assert_eq(second_trigger.timestamp, 1640995208)
  assert_eq(second_trigger.severity, "warning")
  
  // 验证第二次恢复告警
  let second_recovery = suppression_recovery_alerts[3]
  assert_eq(second_recovery.rule_name, "high_error_rate")
  assert_eq(second_recovery.state, "recovered")
  assert_eq(second_recovery.value, 0.04)
  assert_eq(second_recovery.timestamp, 1640995209)
  assert_eq(second_recovery.severity, "info")
  
  // 验证抑制机制：在1640995203和1640995204时，条件也满足但不应该重复告警
  let suppressed_timestamps = [1640995203, 1640995204]
  for alert in suppression_recovery_alerts {
    assert_false(suppressed_timestamps.contains(alert.timestamp))
  }
  
  // 验证告警和恢复的时间顺序
  assert_true(first_trigger.timestamp < first_recovery.timestamp)
  assert_true(first_recovery.timestamp < second_trigger.timestamp)
  assert_true(second_trigger.timestamp < second_recovery.timestamp)
}
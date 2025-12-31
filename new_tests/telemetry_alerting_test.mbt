// 告警测试用例

test "alerting_threshold_based" {
  // 测试基于阈值的告警
  
  let metric_name = "cpu_usage_percent"
  let alert_rule = {
    "metric": metric_name,
    "threshold": 80,
    "operator": ">",
    "duration": 300,
    "severity": "warning"
  }
  
  // 验证告警规则配置
  assert_eq(alert_rule["metric"], metric_name)
  assert_eq(alert_rule["threshold"], "80")
  assert_eq(alert_rule["operator"], ">")
  assert_eq(alert_rule["severity"], "warning")
  
  // 模拟指标数据点
  let metric_data = [
    (1000, 45.5),
    (1010, 52.3),
    (1020, 67.8),
    (1030, 78.9),
    (1040, 82.1),  // 超过阈值
    (1050, 85.3),  // 超过阈值
    (1060, 88.7),  // 超过阈值
    (1070, 91.2),  // 超过阈值
    (1080, 87.6),  // 超过阈值
    (1090, 83.4)   // 超过阈值
  ]
  
  assert_eq(metric_data.length(), 10)
  
  // 统计超过阈值的数据点
  let threshold = 80.0
  let mut exceed_count = 0
  let mut exceed_timestamps = []
  let mut i = 0
  
  while i < metric_data.length() {
    let timestamp = metric_data[i].0
    let value = metric_data[i].1
    
    if value > threshold {
      exceed_count = exceed_count + 1
      exceed_timestamps.push(timestamp)
    }
    
    i = i + 1
  }
  
  assert_eq(exceed_count, 6)
  assert_eq(exceed_timestamps.length(), 6)
  
  // 验证连续超过阈值的持续时间
  let duration_threshold = 300  // 5分钟
  let first_exceed = exceed_timestamps[0]
  let last_exceed = exceed_timestamps[exceed_timestamps.length() - 1]
  let exceed_duration = last_exceed - first_exceed
  
  assert_eq(exceed_duration, 50)  // 1090 - 1040 = 50秒
  
  // 判断是否应该触发告警
  let alert_should_fire = exceed_duration >= duration_threshold
  assert_eq(alert_should_fire, false)  // 50秒 < 300秒，不触发
  
  // 模拟更长时间的数据
  let long_term_data = [
    (1000, 45.5),
    (1100, 82.1),  // 超过阈值
    (1200, 85.3),  // 超过阈值
    (1300, 88.7),  // 超过阈值
    (1400, 91.2),  // 超过阈值
    (1500, 87.6),  // 超过阈值
    (1600, 83.4)   // 超过阈值
  ]
  
  let long_first_exceed = 1100
  let long_last_exceed = 1600
  let long_exceed_duration = long_last_exceed - long_first_exceed
  
  assert_eq(long_exceed_duration, 500)  // 1600 - 1100 = 500秒
  assert_eq(long_exceed_duration >= duration_threshold, true)  // 应该触发告警
}

test "alerting_rate_based" {
  // 测试基于速率的告警
  
  let metric_name = "error_rate_per_second"
  let alert_rule = {
    "metric": metric_name,
    "rate_threshold": 10,
    "window": 60,
    "severity": "critical"
  }
  
  // 验证告警规则配置
  assert_eq(alert_rule["metric"], metric_name)
  assert_eq(alert_rule["rate_threshold"], "10")
  assert_eq(alert_rule["window"], "60")
  assert_eq(alert_rule["severity"], "critical")
  
  // 模拟错误事件时间戳
  let error_events = [
    1005, 1012, 1018, 1025, 1032, 1038, 1045, 1052, 1058, 1065,
    1072, 1078, 1085, 1092, 1098, 1105, 1112, 1118, 1125, 1132
  ]
  
  assert_eq(error_events.length(), 20)
  
  // 计算时间窗口内的错误率
  let window_size = 60  // 60秒窗口
  let start_time = 1000
  let end_time = 1150
  
  // 统计每个时间窗口的错误数
  let mut window_counts = []
  let mut window_start = start_time
  
  while window_start < end_time {
    let window_end = window_start + window_size
    let mut count_in_window = 0
    let mut i = 0
    
    while i < error_events.length() {
      let event_time = error_events[i]
      
      if event_time >= window_start and event_time < window_end {
        count_in_window = count_in_window + 1
      }
      
      i = i + 1
    }
    
    let error_rate = count_in_window
    window_counts.push((window_start, window_end, count_in_window, error_rate))
    
    window_start = window_start + 30  // 30秒滑动窗口
  }
  
  assert_eq(window_counts.length(), 5)  // (1150-1000)/30 = 5个窗口
  
  // 检查是否有窗口超过速率阈值
  let rate_threshold = 10
  let mut windows_exceeding_threshold = 0
  let mut i = 0
  
  while i < window_counts.length() {
    let error_rate = window_counts[i].3
    
    if error_rate > rate_threshold {
      windows_exceeding_threshold = windows_exceeding_threshold + 1
    }
    
    i = i + 1
  }
  
  assert_eq(windows_exceeding_threshold, 0)  // 没有窗口超过阈值
  
  // 模拟高错误率场景
  let high_error_events = []
  let mut i = 0
  
  while i < 150 {  // 150个错误事件
    high_error_events.push(1000 + i * 2)  // 每2秒一个错误
    i = i + 1
  }
  
  // 计算高错误率场景下的窗口统计
  let high_window_start = 1000
  let high_window_end = 1000 + window_size
  let mut high_count_in_window = 0
  i = 0
  
  while i < high_error_events.length() {
    let event_time = high_error_events[i]
    
    if event_time >= high_window_start and event_time < high_window_end {
      high_count_in_window = high_count_in_window + 1
    }
    
    i = i + 1
  }
  
  assert_eq(high_count_in_window, 30)  // 60秒窗口内有30个错误
  assert_eq(high_count_in_window > rate_threshold, true)  // 应该触发告警
}

test "alerting_anomaly_detection" {
  // 测试异常检测告警
  
  let metric_name = "response_time_ms"
  let anomaly_config = {
    "metric": metric_name,
    "algorithm": "statistical",
    "sensitivity": 2.0,  // 2个标准差
    "training_period": 3600,
    "severity": "warning"
  }
  
  // 验证异常检测配置
  assert_eq(anomaly_config["metric"], metric_name)
  assert_eq(anomaly_config["algorithm"], "statistical")
  assert_eq(anomaly_config["sensitivity"], "2.0")
  
  // 模拟历史训练数据
  let training_data = [
    120, 125, 118, 122, 127, 123, 119, 126, 121, 124,
    122, 128, 125, 119, 123, 126, 121, 124, 122, 127
  ]
  
  assert_eq(training_data.length(), 20)
  
  // 计算训练数据的统计特征
  let mut sum = 0
  let mut i = 0
  
  while i < training_data.length() {
    sum = sum + training_data[i]
    i = i + 1
  }
  
  let mean = sum / training_data.length()
  assert_eq(mean, 123)  // (120+125+...+127)/20 = 2460/20 = 123
  
  // 计算标准差
  let mut variance_sum = 0
  i = 0
  
  while i < training_data.length() {
    let diff = training_data[i] - mean
    variance_sum = variance_sum + diff * diff
    i = i + 1
  }
  
  let variance = variance_sum / training_data.length()
  let std_dev = sqrt(variance.to_float()).to_int()
  
  assert_eq(variance, 9)  // 近似值
  assert_eq(std_dev, 3)   // sqrt(9) = 3
  
  // 模拟实时数据点
  let realtime_data = [
    (1000, 122),  // 正常
    (1010, 125),  // 正常
    (1020, 131),  // 异常：+2.67个标准差
    (1030, 119),  // 正常
    (1040, 132),  // 异常：+3个标准差
    (1050, 124)   // 正常
  ]
  
  assert_eq(realtime_data.length(), 6)
  
  // 检测异常
  let sensitivity = 2.0
  let mut anomalies_detected = 0
  let mut anomaly_timestamps = []
  let mut i = 0
  
  while i < realtime_data.length() {
    let timestamp = realtime_data[i].0
    let value = realtime_data[i].1
    
    let z_score = (value - mean).to_float() / std_dev.to_float()
    let is_anomaly = z_score > sensitivity or z_score < -sensitivity
    
    if is_anomaly {
      anomalies_detected = anomalies_detected + 1
      anomaly_timestamps.push(timestamp)
    }
    
    i = i + 1
  }
  
  assert_eq(anomalies_detected, 2)
  assert_eq(anomaly_timestamps.length(), 2)
  assert_eq(anomaly_timestamps[0], 1020)
  assert_eq(anomaly_timestamps[1], 1040)
  
  // 验证异常检测准确性
  let expected_anomalies = [1020, 1040]
  let mut detection_accurate = true
  i = 0
  
  while i < expected_anomalies.length() {
    let expected_time = expected_anomalies[i]
    let mut found = false
    let mut j = 0
    
    while j < anomaly_timestamps.length() {
      if anomaly_timestamps[j] == expected_time {
        found = true
        break
      }
      j = j + 1
    }
    
    if not found {
      detection_accurate = false
      break
    }
    
    i = i + 1
  }
  
  assert_eq(detection_accurate, true)
}

test "alerting_multi_metric" {
  // 测试多指标组合告警
  
  let alert_rules = [
    {
      "name": "high_cpu_high_memory",
      "conditions": [
        {"metric": "cpu_usage_percent", "operator": ">", "value": 80},
        {"metric": "memory_usage_percent", "operator": ">", "value": 85}
      ],
      "logic": "AND",
      "severity": "critical"
    },
    {
      "name": "low_request_high_error",
      "conditions": [
        {"metric": "request_rate_per_second", "operator": "<", "value": 10},
        {"metric": "error_rate_percent", "operator": ">", "value": 50}
      ],
      "logic": "AND",
      "severity": "warning"
    }
  ]
  
  // 验证多指标告警规则
  assert_eq(alert_rules.length(), 2)
  assert_eq(alert_rules[0]["name"], "high_cpu_high_memory")
  assert_eq(alert_rules[0]["logic"], "AND")
  
  // 模拟指标数据
  let metrics_data = {
    "cpu_usage_percent": 85,
    "memory_usage_percent": 90,
    "request_rate_per_second": 5,
    "error_rate_percent": 60
  }
  
  // 评估第一个告警规则：高CPU高内存
  let cpu_usage = 85
  let memory_usage = 90
  let cpu_threshold = 80
  let memory_threshold = 85
  
  let cpu_condition_met = cpu_usage > cpu_threshold
  let memory_condition_met = memory_usage > memory_threshold
  let rule1_alert = cpu_condition_met and memory_condition_met  // AND逻辑
  
  assert_eq(cpu_condition_met, true)
  assert_eq(memory_condition_met, true)
  assert_eq(rule1_alert, true)  // 应该触发告警
  
  // 评估第二个告警规则：低请求高错误
  let request_rate = 5
  let error_rate = 60
  let request_threshold = 10
  let error_threshold = 50
  
  let request_condition_met = request_rate < request_threshold
  let error_condition_met = error_rate > error_threshold
  let rule2_alert = request_condition_met and error_condition_met  // AND逻辑
  
  assert_eq(request_condition_met, true)
  assert_eq(error_condition_met, true)
  assert_eq(rule2_alert, true)  // 应该触发告警
  
  // 统计触发的告警数量
  let mut triggered_alerts = 0
  if rule1_alert {
    triggered_alerts = triggered_alerts + 1
  }
  if rule2_alert {
    triggered_alerts = triggered_alerts + 1
  }
  
  assert_eq(triggered_alerts, 2)
  
  // 验证告警严重性
  let rule1_severity = alert_rules[0]["severity"]
  let rule2_severity = alert_rules[1]["severity"]
  
  assert_eq(rule1_severity, "critical")
  assert_eq(rule2_severity, "warning")
  
  // 模拟OR逻辑的告警规则
  let or_rule = {
    "name": "cpu_or_memory_high",
    "conditions": [
      {"metric": "cpu_usage_percent", "operator": ">", "value": 80},
      {"metric": "memory_usage_percent", "operator": ">", "value": 85}
    ],
    "logic": "OR",
    "severity": "warning"
  }
  
  let or_alert = cpu_condition_met or memory_condition_met  // OR逻辑
  assert_eq(or_alert, true)  // 应该触发告警
  
  // 测试部分条件满足的情况
  let partial_metrics = {
    "cpu_usage_percent": 75,  // 不满足CPU条件
    "memory_usage_percent": 90  // 满足内存条件
  }
  
  let partial_cpu = 75
  let partial_memory = 90
  let partial_cpu_condition = partial_cpu > cpu_threshold
  let partial_memory_condition = partial_memory > memory_threshold
  
  let partial_and_alert = partial_cpu_condition and partial_memory_condition
  let partial_or_alert = partial_cpu_condition or partial_memory_condition
  
  assert_eq(partial_cpu_condition, false)
  assert_eq(partial_memory_condition, true)
  assert_eq(partial_and_alert, false)  // AND逻辑下不触发
  assert_eq(partial_or_alert, true)    // OR逻辑下触发
}

test "alerting_suppression" {
  // 测试告警抑制
  
  let alert_name = "database_connection_failure"
  let suppression_rules = [
    {
      "name": "maintenance_window",
      "condition": "maintenance_mode == true",
      "duration": 3600,
      "affected_alerts": ["*"]  // 抑制所有告警
    },
    {
      "name": "dependency_failure",
      "condition": "upstream_service == 'down'",
      "duration": 1800,
      "affected_alerts": ["database_connection_failure", "cache_miss_high"]
    }
  ]
  
  // 验证抑制规则配置
  assert_eq(suppression_rules.length(), 2)
  assert_eq(suppression_rules[0]["name"], "maintenance_window")
  assert_eq(suppression_rules[1]["affected_alerts"][0], "database_connection_failure")
  
  // 模拟告警事件
  let alert_events = [
    ("database_connection_failure", 1000, "critical"),
    ("high_cpu_usage", 1010, "warning"),
    ("cache_miss_high", 1020, "warning"),
    ("database_connection_failure", 1030, "critical"),
    ("high_memory_usage", 1040, "warning")
  ]
  
  assert_eq(alert_events.length(), 5)
  
  // 模拟抑制条件状态
  let suppression_context = {
    "maintenance_mode": true,
    "upstream_service": "down",
    "suppression_start": 950
  }
  
  // 检查每个告警是否被抑制
  let mut suppressed_alerts = 0
  let mut active_alerts = 0
  let mut i = 0
  
  while i < alert_events.length() {
    let alert_name = alert_events[i].0
    let timestamp = alert_events[i].1
    let severity = alert_events[i].2
    
    let mut is_suppressed = false
    
    // 检查维护窗口抑制
    if suppression_context["maintenance_mode"] == "true" {
      let maintenance_start = suppression_context["suppression_start"].to_int()
      let maintenance_duration = 3600
      let maintenance_end = maintenance_start + maintenance_duration
      
      if timestamp >= maintenance_start and timestamp <= maintenance_end {
        is_suppressed = true
      }
    }
    
    // 检查依赖失败抑制
    if not is_suppressed and suppression_context["upstream_service"] == "down" {
      let dependency_start = suppression_context["suppression_start"].to_int()
      let dependency_duration = 1800
      let dependency_end = dependency_start + dependency_duration
      
      if timestamp >= dependency_start and timestamp <= dependency_end {
        // 检查告警是否在受影响列表中
        if alert_name == "database_connection_failure" or alert_name == "cache_miss_high" {
          is_suppressed = true
        }
      }
    }
    
    if is_suppressed {
      suppressed_alerts = suppressed_alerts + 1
    } else {
      active_alerts = active_alerts + 1
    }
    
    i = i + 1
  }
  
  assert_eq(suppressed_alerts, 5)  // 所有告警都被维护窗口抑制
  assert_eq(active_alerts, 0)
  
  // 模拟维护窗口结束后的情况
  let post_maintenance_context = {
    "maintenance_mode": false,
    "upstream_service": "down",
    "suppression_start": 950
  }
  
  // 重新评估告警抑制状态
  suppressed_alerts = 0
  active_alerts = 0
  i = 0
  
  while i < alert_events.length() {
    let alert_name = alert_events[i].0
    let timestamp = alert_events[i].1
    let severity = alert_events[i].2
    
    let mut is_suppressed = false
    
    // 检查维护窗口抑制（现在已关闭）
    if post_maintenance_context["maintenance_mode"] == "true" {
      is_suppressed = true
    }
    
    // 检查依赖失败抑制
    if not is_suppressed and post_maintenance_context["upstream_service"] == "down" {
      let dependency_start = post_maintenance_context["suppression_start"].to_int()
      let dependency_duration = 1800
      let dependency_end = dependency_start + dependency_duration
      
      if timestamp >= dependency_start and timestamp <= dependency_end {
        if alert_name == "database_connection_failure" or alert_name == "cache_miss_high" {
          is_suppressed = true
        }
      }
    }
    
    if is_suppressed {
      suppressed_alerts = suppressed_alerts + 1
    } else {
      active_alerts = active_alerts + 1
    }
    
    i = i + 1
  }
  
  assert_eq(suppressed_alerts, 3)  // 只有database_connection_failure和cache_miss_high被抑制
  assert_eq(active_alerts, 2)       // high_cpu_usage和high_memory_usage活跃
}
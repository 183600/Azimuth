// Azimuth Telemetry System - 实时遥测处理测试用例
// 测试系统实时处理遥测数据流的能力和性能

// 测试1: 实时Span流处理
test "实时Span流处理测试" {
  // 模拟实时Span数据流
  let base_timestamp = 1640995200000L  // 2022-01-01 00:00:00 UTC
  let span_stream = []
  let services = ["api-gateway", "user-service", "auth-service", "payment-service"]
  
  // 生成实时Span流（每秒100个Span，持续10秒）
  for second = 0; second < 10; second = second + 1 {
    for span_in_second = 0; span_in_second < 100; span_in_second = span_in_second + 1 {
      let service_name = services[span_in_second % services.length()]
      let trace_id = "trace" + second.to_string().pad_left(10, '0') + span_in_second.to_string().pad_left(10, '0')
      let span_id = "span" + second.to_string().pad_left(10, '0') + span_in_second.to_string().pad_left(10, '0')
      let timestamp = base_timestamp + (second * 1000L) + (span_in_second * 10L)
      let duration = 10 + (span_in_second % 200)  // 10-209ms
      
      let span_data = {
        "trace_id": trace_id,
        "span_id": span_id,
        "service_name": service_name,
        "operation_name": "http.request",
        "status": if span_in_second % 10 == 0 { "error" } else { "success" },
        "duration_ms": duration.to_string(),
        "timestamp": timestamp.to_string()
      }
      span_stream.push(span_data)
    }
  }
  
  // 验证Span流数据
  assert_eq(span_stream.length(), 1000)  // 10秒 × 100个Span/秒
  
  // 模拟实时处理窗口（1秒窗口）
  let processing_windows = []
  
  for second = 0; second < 10; second = second + 1 {
    let window_start = base_timestamp + (second * 1000L)
    let window_end = window_start + 1000L
    
    let window_spans = span_stream.filter(|span| {
      let timestamp = span["timestamp"].to_long()
      timestamp >= window_start && timestamp < window_end
    })
    
    processing_windows.push((second, window_spans))
  }
  
  // 验证处理窗口
  assert_eq(processing_windows.length(), 10)
  
  for (second, window_spans) in processing_windows {
    assert_eq(window_spans.length(), 100)  // 每个窗口100个Span
    
    // 验证时间戳在窗口范围内
    for span in window_spans {
      let timestamp = span["timestamp"].to_long()
      let window_start = base_timestamp + (second * 1000L)
      let window_end = window_start + 1000L
      
      assert_true(timestamp >= window_start)
      assert_true(timestamp < window_end)
    }
  }
  
  // 实时聚合统计
  let realtime_stats = []
  
  for (second, window_spans) in processing_windows {
    let total_spans = window_spans.length()
    let error_spans = window_spans.filter(|span| span["status"] == "error").length()
    let avg_duration = window_spans.map(|span| span["duration_ms"].to_int()).reduce(|acc, val| acc + val, 0) / total_spans
    
    // 按服务分组统计
    let service_stats = []
    for service in services {
      let service_spans = window_spans.filter(|span| span["service_name"] == service)
      let service_count = service_spans.length()
      let service_avg_duration = if service_count > 0 {
        service_spans.map(|span| span["duration_ms"].to_int()).reduce(|acc, val| acc + val, 0) / service_count
      } else {
        0
      }
      
      service_stats.push((service, service_count, service_avg_duration))
    }
    
    realtime_stats.push((second, total_spans, error_spans, avg_duration, service_stats))
  }
  
  // 验证实时统计
  assert_eq(realtime_stats.length(), 10)
  
  for (second, total_spans, error_spans, avg_duration, service_stats) in realtime_stats {
    assert_eq(total_spans, 100)
    assert_eq(error_spans, 10)  // 10%错误率
    assert_true(avg_duration >= 10)
    assert_true(avg_duration <= 209)
    assert_eq(service_stats.length(), services.length())
    
    // 验证服务统计总和
    let service_total = service_stats.reduce(|acc, (_, count, _)| acc + count, 0)
    assert_eq(service_total, total_spans)
  }
  
  // 测试延迟监控
  let latency_threshold = 100  // 100ms延迟阈值
  let latency_violations = []
  
  for (second, window_spans) in processing_windows {
    let slow_spans = window_spans.filter(|span| {
      span["duration_ms"].to_int() > latency_threshold
    })
    
    let violation_rate = slow_spans.length().to_float() / window_spans.length().to_float()
    latency_violations.push((second, slow_spans.length(), violation_rate))
  }
  
  // 验证延迟监控
  assert_eq(latency_violations.length(), 10)
  
  for (second, slow_count, violation_rate) in latency_violations {
    assert_true(slow_count >= 0)
    assert_true(slow_count <= 100)
    assert_true(violation_rate >= 0.0)
    assert_true(violation_rate <= 1.0)
  }
}

// 测试2: 实时度量流处理
test "实时度量流处理测试" {
  // 模拟实时度量数据流
  let base_timestamp = 1640995200000L
  let metrics_stream = []
  let metric_names = ["http.requests", "response.time", "error.rate", "cpu.usage", "memory.usage"]
  
  // 生成实时度量流（每5秒一个数据点，持续60秒）
  for time_point = 0; time_point < 12; time_point = time_point + 1 {
    let timestamp = base_timestamp + (time_point * 5000L)
    
    for metric_name in metric_names {
      let value = if metric_name == "http.requests" {
        (100 + time_point * 10).to_string()  // 递增的请求计数
      } else if metric_name == "response.time" {
        (50 + time_point * 5 + (time_point % 3) * 10).to_string()  // 波动的响应时间
      } else if metric_name == "error.rate" {
        (0.01 + (time_point % 5) * 0.002).to_string()  // 波动的错误率
      } else if metric_name == "cpu.usage" {
        (20.0 + (time_point % 10) * 5.0).to_string()  // 波动的CPU使用率
      } else if metric_name == "memory.usage" {
        (60.0 + (time_point % 8) * 3.0).to_string()  // 波动的内存使用率
      } else {
        "0.0"
      }
      
      let metric_data = {
        "metric_name": metric_name,
        "value": value,
        "timestamp": timestamp.to_string(),
        "tags": {
          "service": "api-gateway",
          "environment": "production"
        }
      }
      metrics_stream.push(metric_data)
    }
  }
  
  // 验证度量流数据
  assert_eq(metrics_stream.length(), 12 * metric_names.length())  // 12个时间点 × 5个度量
  
  // 模拟滑动窗口处理（10秒窗口，5秒滑动）
  let sliding_windows = []
  let window_size = 10000L  // 10秒
  let slide_interval = 5000L  // 5秒
  
  for window_start = base_timestamp; window_start < base_timestamp + 60000L; window_start = window_start + slide_interval {
    let window_end = window_start + window_size
    
    let window_metrics = metrics_stream.filter(|metric| {
      let timestamp = metric["timestamp"].to_long()
      timestamp >= window_start && timestamp < window_end
    })
    
    sliding_windows.push((window_start, window_end, window_metrics))
  }
  
  // 验证滑动窗口
  assert_eq(sliding_windows.length(), 11)  // 60秒 / 5秒滑动 + 1
  
  // 实时度量聚合
  let realtime_metrics = []
  
  for (window_start, window_end, window_metrics) in sliding_windows {
    let window_stats = []
    
    for metric_name in metric_names {
      let metric_values = window_metrics
        .filter(|metric| metric["metric_name"] == metric_name)
        .map(|metric| metric["value"].to_float())
      
      if metric_values.length() > 0 {
        let avg = metric_values.reduce(|acc, val| acc + val, 0.0) / metric_values.length().to_float()
        let min = metric_values.reduce(|acc, val| if val < acc { val } else { acc }, 999999.0)
        let max = metric_values.reduce(|acc, val| if val > acc { val } else { acc }, 0.0)
        
        window_stats.push((metric_name, avg, min, max, metric_values.length()))
      }
    }
    
    realtime_metrics.push((window_start, window_end, window_stats))
  }
  
  // 验证实时度量聚合
  assert_eq(realtime_metrics.length(), sliding_windows.length())
  
  for (window_start, window_end, window_stats) in realtime_metrics {
    assert_eq(window_stats.length(), metric_names.length())
    
    for (metric_name, avg, min, max, count) in window_stats {
      assert_true(count > 0)
      assert_true(avg >= min)
      assert_true(avg <= max)
      assert_true(min <= max)
      
      // 验证特定度量的合理性
      if metric_name == "http.requests" {
        assert_true(avg >= 100.0)
        assert_true(max >= avg)
      } else if metric_name == "error.rate" {
        assert_true(avg >= 0.0)
        assert_true(avg <= 1.0)
      } else if metric_name == "cpu.usage" || metric_name == "memory.usage" {
        assert_true(avg >= 0.0)
        assert_true(avg <= 100.0)
      }
    }
  }
  
  // 测试异常检测
  let anomaly_thresholds = [
    ("response.time", 100.0),    // 响应时间超过100ms为异常
    ("error.rate", 0.05),        // 错误率超过5%为异常
    ("cpu.usage", 80.0),         // CPU使用率超过80%为异常
    ("memory.usage", 90.0)       // 内存使用率超过90%为异常
  ]
  
  let anomaly_events = []
  
  for (window_start, window_end, window_stats) in realtime_metrics {
    let window_anomalies = []
    
    for (metric_name, avg, _, max, _) in window_stats {
      for (threshold_metric, threshold_value) in anomaly_thresholds {
        if metric_name == threshold_metric {
          if avg > threshold_value {
            window_anomalies.push((metric_name, "avg", avg, threshold_value))
          }
          if max > threshold_value {
            window_anomalies.push((metric_name, "max", max, threshold_value))
          }
        }
      }
    }
    
    if window_anomalies.length() > 0 {
      anomaly_events.push((window_start, window_end, window_anomalies))
    }
  }
  
  // 验证异常检测
  assert_true(anomaly_events.length() >= 0)  // 可能有也可能没有异常
  
  for (window_start, window_end, window_anomalies) in anomaly_events {
    assert_true(window_anomalies.length() > 0)
    
    for (metric_name, metric_type, value, threshold) in window_anomalies {
      assert_true(value > threshold)
      assert_true(metric_type == "avg" || metric_type == "max")
    }
  }
}

// 测试3: 实时日志流处理
test "实时日志流处理测试" {
  // 模拟实时日志数据流
  let base_timestamp = 1640995200000L
  let log_stream = []
  let log_levels = ["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"]
  let services = ["web-server", "api-gateway", "database", "cache"]
  
  // 生成实时日志流（每秒50条日志，持续30秒）
  for second = 0; second < 30; second = second + 1 {
    for log_in_second = 0; log_in_second < 50; log_in_second = log_in_second + 1 {
      let service_name = services[log_in_second % services.length()]
      let level = if log_in_second % 20 == 0 { "ERROR" } else if log_in_second % 10 == 0 { "WARN" } else { "INFO" }
      let timestamp = base_timestamp + (second * 1000L) + (log_in_second * 20L)
      let message = "Log message from " + service_name + " at " + second.to_string() + "s"
      
      let log_data = {
        "timestamp": timestamp.to_string(),
        "level": level,
        "service": service_name,
        "message": message,
        "trace_id": if log_in_second % 5 == 0 { "trace" + log_in_second.to_string() } else { "" }
      }
      log_stream.push(log_data)
    }
  }
  
  // 验证日志流数据
  assert_eq(log_stream.length(), 1500)  // 30秒 × 50条日志/秒
  
  // 模拟实时日志处理（5秒窗口）
  let log_windows = []
  
  for window_start_second = 0; window_start_second < 30; window_start_second = window_start_second + 5 {
    let window_start = base_timestamp + (window_start_second * 1000L)
    let window_end = window_start + 5000L
    
    let window_logs = log_stream.filter(|log| {
      let timestamp = log["timestamp"].to_long()
      timestamp >= window_start && timestamp < window_end
    })
    
    log_windows.push((window_start_second, window_logs))
  }
  
  // 验证日志窗口
  assert_eq(log_windows.length(), 6)  // 30秒 / 5秒窗口
  
  for (window_start_second, window_logs) in log_windows {
    assert_eq(window_logs.length(), 250)  // 5秒 × 50条日志/秒
    
    // 验证时间戳在窗口范围内
    for log in window_logs {
      let timestamp = log["timestamp"].to_long()
      let window_start = base_timestamp + (window_start_second * 1000L)
      let window_end = window_start + 5000L
      
      assert_true(timestamp >= window_start)
      assert_true(timestamp < window_end)
    }
  }
  
  // 实时日志级别统计
  let log_level_stats = []
  
  for (window_start_second, window_logs) in log_windows {
    let level_counts = []
    
    for level in log_levels {
      let count = window_logs.filter(|log| log["level"] == level).length()
      level_counts.push((level, count))
    }
    
    log_level_stats.push((window_start_second, level_counts))
  }
  
  // 验证日志级别统计
  assert_eq(log_level_stats.length(), 6)
  
  for (window_start_second, level_counts) in log_level_stats {
    let total_logs = level_counts.reduce(|acc, (_, count)| acc + count, 0)
    assert_eq(total_logs, 250)
    
    // 验证INFO日志最多
    let info_count = level_counts.filter(|(level, _)| level == "INFO")[0].1
    let warn_count = level_counts.filter(|(level, _)| level == "WARN")[0].1
    let error_count = level_counts.filter(|(level, _)| level == "ERROR")[0].1
    
    assert_true(info_count > warn_count)
    assert_true(info_count > error_count)
    assert_eq(warn_count, 25)  // 250条日志中，每10条有1条WARN
    assert_eq(error_count, 12) // 250条日志中，每20条有1条ERROR
  }
  
  // 实时错误日志分析
  let error_analysis = []
  
  for (window_start_second, window_logs) in log_windows {
    let error_logs = window_logs.filter(|log| {
      log["level"] == "ERROR" || log["level"] == "FATAL"
    })
    
    let service_errors = []
    for service in services {
      let service_error_count = error_logs.filter(|log| log["service"] == service).length()
      service_errors.push((service, service_error_count))
    }
    
    let error_rate = error_logs.length().to_float() / window_logs.length().to_float()
    
    error_analysis.push((window_start_second, error_logs.length(), error_rate, service_errors))
  }
  
  // 验证错误日志分析
  assert_eq(error_analysis.length(), 6)
  
  for (window_start_second, error_count, error_rate, service_errors) in error_analysis {
    assert_true(error_count >= 12)  // 至少有ERROR日志
    assert_true(error_rate > 0.0)
    assert_true(error_rate < 0.1)   // 错误率应该小于10%
    assert_eq(service_errors.length(), services.length())
    
    // 验证服务错误总和
    let total_service_errors = service_errors.reduce(|acc, (_, count)| acc + count, 0)
    assert_eq(total_service_errors, error_count)
  }
  
  // 测试实时日志模式检测
  let pattern_detected = []
  
  for (window_start_second, window_logs) in log_windows {
    // 检测错误激增模式
    let error_logs = window_logs.filter(|log| log["level"] == "ERROR")
    let error_spike_threshold = 15  // 5秒内超过15个错误为激增
    
    if error_logs.length() > error_spike_threshold {
      pattern_detected.push(("error_spike", window_start_second, error_logs.length()))
    }
    
    // 检测特定服务错误模式
    for service in services {
      let service_errors = error_logs.filter(|log| log["service"] == service)
      if service_errors.length() > 5 {  // 单个服务5秒内超过5个错误
        pattern_detected.push(("service_error_pattern", window_start_second, service, service_errors.length()))
      }
    }
  }
  
  // 验证模式检测结果
  assert_true(pattern_detected.length() >= 0)
  
  for detection in pattern_detected {
    match detection {
      ("error_spike", window_start_second, count) => {
        assert_true(count > 15)
        assert_true(window_start_second >= 0 && window_start_second < 30)
      }
      ("service_error_pattern", window_start_second, service, count) => {
        assert_true(count > 5)
        assert_true(window_start_second >= 0 && window_start_second < 30)
        assert_true(services.contains(service))
      }
      _ => assert_true(false, "未知的模式检测类型")
    }
  }
}

// 测试4: 实时告警处理
test "实时告警处理测试" {
  // 模拟实时监控数据
  let base_timestamp = 1640995200000L
  let monitoring_data = []
  
  // 生成监控数据（每10秒一个数据点，持续2分钟）
  for time_point = 0; time_point < 12; time_point = time_point + 1 {
    let timestamp = base_timestamp + (time_point * 10000L)
    
    // 模拟系统指标
    let cpu_usage = 30.0 + (time_point % 5) * 10.0 + (if time_point > 8 { 30.0 } else { 0.0 })  // 后期CPU激增
    let memory_usage = 60.0 + (time_point % 3) * 5.0
    let error_rate = 0.01 + (if time_point > 6 { (time_point - 6) * 0.01 } else { 0.0 })  // 后期错误率增加
    let response_time = 50.0 + (time_point % 4) * 25.0 + (if time_point > 9 { 100.0 } else { 0.0 })  // 后期响应时间激增
    
    let monitoring_point = {
      "timestamp": timestamp.to_string(),
      "cpu_usage": cpu_usage.to_string(),
      "memory_usage": memory_usage.to_string(),
      "error_rate": error_rate.to_string(),
      "response_time": response_time.to_string(),
      "request_count": (1000 + time_point * 100).to_string()
    }
    monitoring_data.push(monitoring_point)
  }
  
  // 验证监控数据
  assert_eq(monitoring_data.length(), 12)
  
  // 定义告警规则
  let alert_rules = [
    ("high_cpu", "cpu_usage", ">", 80.0, "CPU使用率过高"),
    ("high_memory", "memory_usage", ">", 85.0, "内存使用率过高"),
    ("high_error_rate", "error_rate", ">", 0.05, "错误率过高"),
    ("slow_response", "response_time", ">", 200.0, "响应时间过慢")
  ]
  
  // 实时告警检测
  let alert_events = []
  
  for data_point in monitoring_data {
    let timestamp = data_point["timestamp"].to_long()
    let point_alerts = []
    
    for (rule_name, metric, operator, threshold, message) in alert_rules {
      let value = data_point[metric].to_float()
      
      let triggered = if operator == ">" {
        value > threshold
      } else if operator == "<" {
        value < threshold
      } else if operator == ">=" {
        value >= threshold
      } else if operator == "<=" {
        value <= threshold
      } else {
        false
      }
      
      if triggered {
        point_alerts.push((rule_name, metric, value, threshold, message))
      }
    }
    
    if point_alerts.length() > 0 {
      alert_events.push((timestamp, point_alerts))
    }
  }
  
  // 验证告警事件
  assert_true(alert_events.length() > 0)  // 应该有告警事件
  
  // 验证CPU告警
  let cpu_alerts = alert_events.filter(|(_, alerts)| {
    alerts.any(|(rule_name, _, _, _, _)| rule_name == "high_cpu")
  })
  assert_true(cpu_alerts.length() > 0)
  
  // 验证错误率告警
  let error_alerts = alert_events.filter(|(_, alerts)| {
    alerts.any(|(rule_name, _, _, _, _)| rule_name == "high_error_rate")
  })
  assert_true(error_alerts.length() > 0)
  
  // 验证响应时间告警
  let response_alerts = alert_events.filter(|(_, alerts)| {
    alerts.any(|(rule_name, _, _, _, _)| rule_name == "slow_response")
  })
  assert_true(response_alerts.length() > 0)
  
  // 测试告警聚合和去重
  let aggregated_alerts = []
  let seen_alerts = []
  
  for (timestamp, alerts) in alert_events {
    for (rule_name, metric, value, threshold, message) in alerts {
      let alert_key = rule_name + "_" + metric
      
      if not seen_alerts.contains(alert_key) {
        aggregated_alerts.push((timestamp, rule_name, metric, value, threshold, message))
        seen_alerts.push(alert_key)
      }
    }
  }
  
  // 验证告警聚合
  assert_true(aggregated_alerts.length() > 0)
  assert_true(aggregated_alerts.length() <= alert_events.length())
  
  // 验证去重效果
  let unique_rule_names = aggregated_alerts.map(|(_, rule_name, _, _, _, _)| rule_name)
  for rule_name in unique_rule_names {
    let count = unique_rule_names.filter(|name| name == rule_name).length()
    assert_eq(count, 1)  // 每个规则名称只出现一次
  }
  
  // 测试告警恢复检测
  let recovery_events = []
  let alert_states = []
  
  for data_point in monitoring_data {
    let timestamp = data_point["timestamp"].to_long()
    let point_states = []
    
    for (rule_name, metric, operator, threshold, _) in alert_rules {
      let value = data_point[metric].to_float()
      let triggered = if operator == ">" { value > threshold } else { false }
      
      point_states.push((rule_name, metric, triggered))
    }
    
    alert_states.push((timestamp, point_states))
  }
  
  // 检测告警恢复（从触发状态变为非触发状态）
  for i = 1; i < alert_states.length(); i = i + 1 {
    let (prev_timestamp, prev_states) = alert_states[i-1]
    let (curr_timestamp, curr_states) = alert_states[i]
    
    for (rule_name, metric, prev_triggered) in prev_states {
      let curr_triggered = curr_states.filter(|(r, m, _)| r == rule_name && m == metric)[0].2
      
      if prev_triggered && not curr_triggered {
        // 告警恢复
        recovery_events.push((curr_timestamp, rule_name, metric))
      }
    }
  }
  
  // 验证告警恢复检测
  assert_true(recovery_events.length() >= 0)
  
  for (timestamp, rule_name, metric) in recovery_events {
    assert_true(timestamp > base_timestamp)
    assert_true(rule_name.length() > 0)
    assert_true(metric.length() > 0)
  }
}

// 测试5: 实时数据流背压处理
test "实时数据流背压处理测试" {
  // 模拟高频率数据流（正常情况下每秒1000个数据点）
  let base_timestamp = 1640995200000L
  let high_frequency_stream = []
  
  // 生成高频数据流（5秒，每秒1000个数据点）
  for second = 0; second < 5; second = second + 1 {
    for data_point = 0; data_point < 1000; data_point = data_point + 1 {
      let timestamp = base_timestamp + (second * 1000L) + data_point
      let data = {
        "id": (second * 1000 + data_point).to_string(),
        "timestamp": timestamp.to_string(),
        "value": (10 + data_point % 100).to_string(),
        "source": "high-frequency-generator"
      }
      high_frequency_stream.push(data)
    }
  }
  
  // 验证高频数据流
  assert_eq(high_frequency_stream.length(), 5000)  // 5秒 × 1000个数据点/秒
  
  // 模拟处理能力（每秒只能处理500个数据点）
  let processing_capacity = 500  // 每秒500个数据点
  let processed_data = []
  let backlog_data = []
  let dropped_data = []
  
  // 模拟背压处理
  let processing_window_start = base_timestamp
  let processing_window_end = processing_window_start + 5000L  // 5秒处理窗口
  
  // 按时间窗口处理数据
  for second = 0; second < 5; second = second + 1 {
    let window_start = base_timestamp + (second * 1000L)
    let window_end = window_start + 1000L
    
    let window_data = high_frequency_stream.filter(|data| {
      let timestamp = data["timestamp"].to_long()
      timestamp >= window_start && timestamp < window_end
    })
    
    // 处理窗口内的数据
    let processed_in_window = window_data.slice(0, processing_capacity)
    let backlog_in_window = window_data.slice(processing_capacity, window_data.length())
    
    // 添加到已处理和积压数据
    for data in processed_in_window {
      processed_data.push(data)
    }
    
    for data in backlog_in_window {
      backlog_data.push(data)
    }
    
    // 模拟处理积压数据（从上一窗口的积压中处理一部分）
    if second > 0 && backlog_data.length() > 0 {
      let backlog_to_process = backlog_data.slice(0, processing_capacity / 2)  // 用一半容量处理积压
      let remaining_backlog = backlog_data.slice(processing_capacity / 2, backlog_data.length())
      
      for data in backlog_to_process {
        processed_data.push(data)
      }
      
      backlog_data = remaining_backlog
    }
    
    // 如果积压过多，开始丢弃数据
    if backlog_data.length() > processing_capacity * 3 {  // 积压超过3倍处理能力
      let to_drop = backlog_data.length() - (processing_capacity * 2)
      let dropped = backlog_data.slice(0, to_drop)
      backlog_data = backlog_data.slice(to_drop, backlog_data.length())
      
      for data in dropped {
        dropped_data.push(data)
      }
    }
  }
  
  // 验证背压处理结果
  assert_true(processed_data.length() > 0)
  assert_true(processed_data.length() < high_frequency_stream.length())
  
  // 验证处理能力限制
  let expected_processed = 5 * processing_capacity + (processing_capacity / 2) * 4  // 5秒正常处理 + 4秒积压处理
  assert_true(processed_data.length() <= expected_processed)
  
  // 验证积压数据
  assert_true(backlog_data.length() >= 0)
  
  // 验证丢弃数据
  assert_true(dropped_data.length() >= 0)
  
  // 验证数据完整性
  let all_processed_or_dropped = processed_data.length() + backlog_data.length() + dropped_data.length()
  assert_eq(all_processed_or_dropped, high_frequency_stream.length())
  
  // 测试背压指标
  let processing_rate = processed_data.length().to_float() / 5.0  // 每秒处理的数据点数
  let drop_rate = dropped_data.length().to_float() / 5.0  // 每秒丢弃的数据点数
  let backlog_growth_rate = backlog_data.length().to_float() / 5.0  // 每秒积压增长的数据点数
  
  // 验证背压指标
  assert_true(processing_rate <= processing_capacity.to_float())  // 处理率不超过处理能力
  assert_true(drop_rate >= 0.0)  // 丢弃率非负
  assert_true(backlog_growth_rate >= 0.0)  // 积压增长率非负
  
  // 测试自适应背压处理
  let adaptive_processed_data = []
  let adaptive_backlog_data = []
  let adaptive_dropped_data = []
  let mut adaptive_capacity = processing_capacity
  
  for second = 0; second < 5; second = second + 1 {
    let window_start = base_timestamp + (second * 1000L)
    let window_end = window_start + 1000L
    
    let window_data = high_frequency_stream.filter(|data| {
      let timestamp = data["timestamp"].to_long()
      timestamp >= window_start && timestamp < window_end
    })
    
    // 根据积压情况调整处理能力
    if adaptive_backlog_data.length() > adaptive_capacity * 2 {
      adaptive_capacity = (adaptive_capacity * 3) / 4  // 减少处理能力
    } else if adaptive_backlog_data.length() < adaptive_capacity / 2 {
      adaptive_capacity = (adaptive_capacity * 5) / 4  // 增加处理能力
    }
    
    // 处理窗口内的数据
    let processed_in_window = window_data.slice(0, adaptive_capacity)
    let backlog_in_window = window_data.slice(adaptive_capacity, window_data.length())
    
    for data in processed_in_window {
      adaptive_processed_data.push(data)
    }
    
    for data in backlog_in_window {
      adaptive_backlog_data.push(data)
    }
    
    // 处理积压数据
    if adaptive_backlog_data.length() > 0 {
      let backlog_to_process = adaptive_backlog_data.slice(0, adaptive_capacity / 2)
      adaptive_backlog_data = adaptive_backlog_data.slice(adaptive_capacity / 2, adaptive_backlog_data.length())
      
      for data in backlog_to_process {
        adaptive_processed_data.push(data)
      }
    }
  }
  
  // 验证自适应背压处理
  assert_true(adaptive_processed_data.length() > 0)
  assert_true(adaptive_backlog_data.length() >= 0)
  
  // 验证自适应处理能力
  let adaptive_processing_rate = adaptive_processed_data.length().to_float() / 5.0
  assert_true(adaptive_processing_rate > 0.0)
  
  // 自适应处理应该比固定处理更高效
  assert_true(adaptive_processed_data.length() >= processed_data.length() * 9 / 10)  // 至少90%的效率
}
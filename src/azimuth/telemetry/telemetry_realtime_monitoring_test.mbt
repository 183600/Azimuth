// 遥测数据实时监控测试用例

test "telemetry_realtime_metrics_collection" {
  // 测试遥测实时指标收集
  
  let monitoring_window_ms = 5000L // 5秒监控窗口
  let metrics_collection_interval = 1000L // 1秒收集间隔
  let simulation_duration = 10000L // 10秒模拟
  
  let realtime_metrics = {
    "request_count": 0,
    "error_count": 0,
    "response_time_total": 0L,
    "active_spans": 0,
    "throughput_samples": [],
    "error_rate_samples": [],
    "latency_samples": []
  }
  
  // 模拟实时数据流
  let current_time = 1634567890123L
  let end_time = current_time + simulation_duration
  
  while current_time < end_time {
    // 模拟请求到达
    let requests_per_second = 50 + (current_time % 3000) / 100 // 50-80请求/秒
    let error_rate = 0.02 + (current_time % 5000) / 50000.0 // 2%-12%错误率
    
    for i in 0..requests_per_second {
      let response_time = 50L + (current_time % 2000) // 50-2050ms响应时间
      let is_error = (i * 7) % 100 < (error_rate * 100)
      
      realtime_metrics["request_count"] = realtime_metrics["request_count"] + 1
      realtime_metrics["response_time_total"] = realtime_metrics["response_time_total"] + response_time
      
      if is_error {
        realtime_metrics["error_count"] = realtime_metrics["error_count"] + 1
      }
      
      realtime_metrics["latency_samples"].push(response_time)
    }
    
    // 模拟span生命周期
    let new_spans = 10 + (current_time % 2000) / 200 // 10-19新span
    let completed_spans = 8 + (current_time % 1800) / 225 // 8-15完成span
    realtime_metrics["active_spans"] = realtime_metrics["active_spans"] + new_spans - completed_spans
    realtime_metrics["active_spans"] = @max(0, realtime_metrics["active_spans"])
    
    // 每秒收集指标样本
    if current_time % metrics_collection_interval == 0 {
      let current_throughput = requests_per_second
      let current_error_rate = if realtime_metrics["request_count"] > 0 {
        realtime_metrics["error_count"].to_float() / realtime_metrics["request_count"].to_float()
      } else { 0.0 }
      let avg_latency = if realtime_metrics["latency_samples"].length() > 0 {
        realtime_metrics["response_time_total"] / realtime_metrics["latency_samples"].length()
      } else { 0L }
      
      realtime_metrics["throughput_samples"].push(current_throughput)
      realtime_metrics["error_rate_samples"].push(current_error_rate)
      
      // 清理旧样本以节省内存
      if realtime_metrics["latency_samples"].length() > 1000 {
        realtime_metrics["latency_samples"] = realtime_metrics["latency_samples"].slice(-500)
        realtime_metrics["response_time_total"] = 0L
        for sample in realtime_metrics["latency_samples"] {
          realtime_metrics["response_time_total"] = realtime_metrics["response_time_total"] + sample
        }
      }
    }
    
    current_time = current_time + 100L // 100ms时间步进
  }
  
  // 验证实时指标收集结果
  assert_eq(realtime_metrics["request_count"] > 0, true)
  assert_eq(realtime_metrics["error_count"] >= 0, true)
  assert_eq(realtime_metrics["response_time_total"] > 0L, true)
  assert_eq(realtime_metrics["active_spans"] >= 0, true)
  
  // 验证指标样本收集
  assert_eq(realtime_metrics["throughput_samples"].length() > 0, true)
  assert_eq(realtime_metrics["error_rate_samples"].length() > 0, true)
  assert_eq(realtime_metrics["latency_samples"].length() > 0, true)
  
  // 验证指标合理性
  let total_error_rate = realtime_metrics["error_count"].to_float() / realtime_metrics["request_count"].to_float()
  assert_eq(total_error_rate >= 0.0 && total_error_rate <= 1.0, true)
  
  let avg_response_time = realtime_metrics["response_time_total"] / realtime_metrics["latency_samples"].length()
  assert_eq(avg_response_time >= 50L && avg_response_time <= 2100L, true)
}

test "telemetry_realtime_alert_detection" {
  // 测试遥测实时告警检测
  
  let alert_thresholds = {
    "error_rate": 0.05,        // 5%错误率阈值
    "response_time_p95": 1000L, // 95%响应时间阈值
    "throughput_drop": 0.3,    // 30%吞吐量下降阈值
    "active_spans_max": 100    // 最大活跃span数
  }
  
  let alert_conditions = {
    "error_rate_high": false,
    "response_time_high": false,
    "throughput_dropped": false,
    "active_spans_exceeded": false
  }
  
  let alert_history = []
  
  // 模拟实时监控场景
  let monitoring_scenarios = [
    {
      "name": "normal_operation",
      "duration": 3000L,
      "error_rate": 0.02,
      "avg_response_time": 200L,
      "throughput": 100,
      "active_spans": 50
    },
    {
      "name": "error_spike",
      "duration": 2000L,
      "error_rate": 0.08,
      "avg_response_time": 250L,
      "throughput": 95,
      "active_spans": 55
    },
    {
      "name": "latency_increase",
      "duration": 2000L,
      "error_rate": 0.03,
      "avg_response_time": 1200L,
      "throughput": 90,
      "active_spans": 60
    },
    {
      "name": "throughput_drop",
      "duration": 2000L,
      "error_rate": 0.025,
      "avg_response_time": 300L,
      "throughput": 60,
      "active_spans": 45
    },
    {
      "name": "span_overload",
      "duration": 1000L,
      "error_rate": 0.04,
      "avg_response_time": 400L,
      "throughput": 80,
      "active_spans": 120
    }
  ]
  
  let baseline_throughput = 100.0
  let current_time = 1634567890123L
  
  for scenario in monitoring_scenarios {
    let scenario_start = current_time
    let scenario_end = current_time + scenario["duration"]
    
    while current_time < scenario_end {
      // 检查告警条件
      alert_conditions["error_rate_high"] = scenario["error_rate"] > alert_thresholds["error_rate"]
      alert_conditions["response_time_high"] = scenario["avg_response_time"] > alert_thresholds["response_time_p95"]
      alert_conditions["throughput_dropped"] = scenario["throughput"] < (baseline_throughput * (1.0 - alert_thresholds["throughput_drop"]))
      alert_conditions["active_spans_exceeded"] = scenario["active_spans"] > alert_thresholds["active_spans_max"]
      
      // 检查是否有新的告警触发
      let active_alerts = []
      for (condition, is_triggered) in alert_conditions {
        if is_triggered {
          active_alerts.push(condition)
        }
      }
      
      // 记录告警事件
      if active_alerts.length() > 0 {
        let alert_event = {
          "timestamp": current_time,
          "scenario": scenario["name"],
          "triggered_alerts": active_alerts,
          "metrics": {
            "error_rate": scenario["error_rate"],
            "avg_response_time": scenario["avg_response_time"],
            "throughput": scenario["throughput"],
            "active_spans": scenario["active_spans"]
          }
        }
        alert_history.push(alert_event)
      }
      
      current_time = current_time + 500L // 500ms检查间隔
    }
  }
  
  // 验证告警检测结果
  assert_eq(alert_history.length() > 0, true)
  
  // 验证特定场景的告警触发
  let error_spike_alerts = 0
  let latency_alerts = 0
  let throughput_alerts = 0
  let span_overload_alerts = 0
  
  for alert in alert_history {
    if alert["scenario"] == "error_spike" && alert["triggered_alerts"].contains("error_rate_high") {
      error_spike_alerts = error_spike_alerts + 1
    }
    if alert["scenario"] == "latency_increase" && alert["triggered_alerts"].contains("response_time_high") {
      latency_alerts = latency_alerts + 1
    }
    if alert["scenario"] == "throughput_drop" && alert["triggered_alerts"].contains("throughput_dropped") {
      throughput_alerts = throughput_alerts + 1
    }
    if alert["scenario"] == "span_overload" && alert["triggered_alerts"].contains("active_spans_exceeded") {
      span_overload_alerts = span_overload_alerts + 1
    }
  }
  
  assert_eq(error_spike_alerts > 0, true)
  assert_eq(latency_alerts > 0, true)
  assert_eq(throughput_alerts > 0, true)
  assert_eq(span_overload_alerts > 0, true)
  
  // 验证告警指标记录
  for alert in alert_history {
    assert_eq(alert["timestamp"] > 0L, true)
    assert_eq(alert["scenario"].length() > 0, true)
    assert_eq(alert["triggered_alerts"].length() > 0, true)
    assert_eq(alert.contains_key("metrics"), true)
  }
}

test "telemetry_realtime_dashboard_data" {
  // 测试遥测实时仪表板数据
  
  let dashboard_update_interval = 2000L // 2秒更新间隔
  let dashboard_data = {
    "current_metrics": {
      "requests_per_second": 0,
      "error_rate": 0.0,
      "avg_response_time": 0L,
      "p95_response_time": 0L,
      "active_connections": 0,
      "cpu_usage": 0.0,
      "memory_usage": 0.0
    },
    "historical_trends": {
      "throughput_history": [],
      "error_rate_history": [],
      "latency_history": [],
      "resource_usage_history": []
    },
    "service_health": {
      "overall_status": "healthy",
      "service_status_map": {},
      "last_updated": 0L
    },
    "alerts": {
      "active_alerts": [],
      "alert_count": 0,
      "severity_distribution": {"critical": 0, "warning": 0, "info": 0}
    }
  }
  
  let services = ["payment-service", "user-service", "order-service", "inventory-service"]
  let current_time = 1634567890123L
  let simulation_duration = 20000L // 20秒模拟
  
  // 初始化服务状态
  for service in services {
    dashboard_data["service_health"]["service_status_map"][service] = "healthy"
  }
  
  while current_time < 1634567890123L + simulation_duration {
    // 模拟实时指标更新
    let time_factor = (current_time % 10000) / 10000.0 // 0-1的时间因子
    
    dashboard_data["current_metrics"]["requests_per_second"] = 80 + @int(time_factor * 40) // 80-120
    dashboard_data["current_metrics"]["error_rate"] = 0.01 + time_factor * 0.04 // 1%-5%
    dashboard_data["current_metrics"]["avg_response_time"] = 150L + @int(time_factor * 350L) // 150-500ms
    dashboard_data["current_metrics"]["p95_response_time"] = 300L + @int(time_factor * 700L) // 300-1000ms
    dashboard_data["current_metrics"]["active_connections"] = 200 + @int(time_factor * 100) // 200-300
    dashboard_data["current_metrics"]["cpu_usage"] = 30.0 + time_factor * 40.0 // 30%-70%
    dashboard_data["current_metrics"]["memory_usage"] = 40.0 + time_factor * 30.0 // 40%-70%
    
    // 更新历史趋势（保持最近10个数据点）
    dashboard_data["historical_trends"]["throughput_history"].push(dashboard_data["current_metrics"]["requests_per_second"])
    dashboard_data["historical_trends"]["error_rate_history"].push(dashboard_data["current_metrics"]["error_rate"])
    dashboard_data["historical_trends"]["latency_history"].push(dashboard_data["current_metrics"]["avg_response_time"])
    dashboard_data["historical_trends"]["resource_usage_history"].push(dashboard_data["current_metrics"]["cpu_usage"])
    
    // 限制历史数据长度
    let max_history_length = 10
    for trend_key in dashboard_data["historical_trends"].keys() {
      let trend = dashboard_data["historical_trends"][trend_key]
      if trend.length() > max_history_length {
        dashboard_data["historical_trends"][trend_key] = trend.slice(-max_history_length)
      }
    }
    
    // 更新服务健康状态
    let overall_health_score = 0.0
    for service in services {
      let service_health = 100.0 - (dashboard_data["current_metrics"]["error_rate"] * 100) - (dashboard_data["current_metrics"]["cpu_usage"] * 0.5)
      service_health = @max(0.0, @min(100.0, service_health))
      
      let service_status = if service_health > 80 { "healthy" } else if service_health > 60 { "warning" } else { "critical" }
      dashboard_data["service_health"]["service_status_map"][service] = service_status
      
      overall_health_score = overall_health_score + service_health
    }
    
    overall_health_score = overall_health_score / services.length()
    dashboard_data["service_health"]["overall_status"] = if overall_health_score > 80 { "healthy" } else if overall_health_score > 60 { "warning" } else { "critical" }
    
    // 生成告警
    dashboard_data["alerts"]["active_alerts"] = []
    let severity_counts = {"critical": 0, "warning": 0, "info": 0}
    
    if dashboard_data["current_metrics"]["error_rate"] > 0.03 {
      dashboard_data["alerts"]["active_alerts"].push({
        "type": "error_rate_high",
        "severity": "warning",
        "message": "Error rate is " + (dashboard_data["current_metrics"]["error_rate"] * 100).to_string() + "%"
      })
      severity_counts["warning"] = severity_counts["warning"] + 1
    }
    
    if dashboard_data["current_metrics"]["p95_response_time"] > 800L {
      dashboard_data["alerts"]["active_alerts"].push({
        "type": "response_time_high",
        "severity": "critical",
        "message": "P95 response time is " + dashboard_data["current_metrics"]["p95_response_time"].to_string() + "ms"
      })
      severity_counts["critical"] = severity_counts["critical"] + 1
    }
    
    if dashboard_data["current_metrics"]["cpu_usage"] > 60.0 {
      dashboard_data["alerts"]["active_alerts"].push({
        "type": "cpu_usage_high",
        "severity": "warning",
        "message": "CPU usage is " + dashboard_data["current_metrics"]["cpu_usage"].to_string() + "%"
      })
      severity_counts["warning"] = severity_counts["warning"] + 1
    }
    
    dashboard_data["alerts"]["alert_count"] = dashboard_data["alerts"]["active_alerts"].length()
    dashboard_data["alerts"]["severity_distribution"] = severity_counts
    dashboard_data["service_health"]["last_updated"] = current_time
    
    current_time = current_time + dashboard_update_interval
  }
  
  // 验证仪表板数据完整性
  assert_eq(dashboard_data["current_metrics"]["requests_per_second"] > 0, true)
  assert_eq(dashboard_data["current_metrics"]["error_rate"] >= 0.0, true)
  assert_eq(dashboard_data["current_metrics"]["avg_response_time"] > 0L, true)
  assert_eq(dashboard_data["current_metrics"]["p95_response_time"] > 0L, true)
  assert_eq(dashboard_data["current_metrics"]["active_connections"] > 0, true)
  assert_eq(dashboard_data["current_metrics"]["cpu_usage"] > 0.0, true)
  assert_eq(dashboard_data["current_metrics"]["memory_usage"] > 0.0, true)
  
  // 验证历史趋势数据
  assert_eq(dashboard_data["historical_trends"]["throughput_history"].length() > 0, true)
  assert_eq(dashboard_data["historical_trends"]["error_rate_history"].length() > 0, true)
  assert_eq(dashboard_data["historical_trends"]["latency_history"].length() > 0, true)
  assert_eq(dashboard_data["historical_trends"]["resource_usage_history"].length() > 0, true)
  
  // 验证历史数据长度限制
  for trend_key in dashboard_data["historical_trends"].keys() {
    let trend = dashboard_data["historical_trends"][trend_key]
    assert_eq(trend.length() <= 10, true)
  }
  
  // 验证服务健康状态
  assert_eq(dashboard_data["service_health"]["overall_status"].length() > 0, true)
  assert_eq(dashboard_data["service_health"]["service_status_map"].keys().length(), services.length())
  assert_eq(dashboard_data["service_health"]["last_updated"] > 0L, true)
  
  // 验证告警数据
  assert_eq(dashboard_data["alerts"]["alert_count"] >= 0, true)
  assert_eq(dashboard_data["alerts"]["active_alerts"].length() == dashboard_data["alerts"]["alert_count"], true)
  assert_eq(dashboard_data["alerts"]["severity_distribution"].contains_key("critical"), true)
  assert_eq(dashboard_data["alerts"]["severity_distribution"].contains_key("warning"), true)
  assert_eq(dashboard_data["alerts"]["severity_distribution"].contains_key("info"), true)
}

test "telemetry_realtime_anomaly_detection" {
  // 测试遥测实时异常检测
  
  let anomaly_detection_config = {
    "statistical_window": 100,     // 统计窗口大小
    "threshold_std_dev": 2.5,      // 标准差阈值
    "trend_analysis_window": 20,   // 趋势分析窗口
    "pattern_recognition_threshold": 0.8 // 模式识别阈值
  }
  
  let anomaly_detector = {
    "metrics_history": {
      "response_time": [],
      "throughput": [],
      "error_rate": [],
      "cpu_usage": []
    },
    "baseline_statistics": {
      "response_time": {"mean": 0.0, "std_dev": 0.0},
      "throughput": {"mean": 0.0, "std_dev": 0.0},
      "error_rate": {"mean": 0.0, "std_dev": 0.0},
      "cpu_usage": {"mean": 0.0, "std_dev": 0.0}
    },
    "detected_anomalies": [],
    "anomaly_patterns": []
  }
  
  // 生成正常基线数据
  let baseline_duration = 5000L
  let current_time = 1634567890123L
  
  while current_time < 1634567890123L + baseline_duration {
    let normal_response_time = 200.0 + (@sin(current_time / 1000.0) * 50.0) // 150-250ms正常波动
    let normal_throughput = 100.0 + (@cos(current_time / 2000.0) * 20.0) // 80-120正常波动
    let normal_error_rate = 0.02 + (@sin(current_time / 3000.0) * 0.01) // 1%-3%正常波动
    let normal_cpu_usage = 45.0 + (@cos(current_time / 1500.0) * 10.0) // 35%-55%正常波动
    
    anomaly_detector["metrics_history"]["response_time"].push(normal_response_time)
    anomaly_detector["metrics_history"]["throughput"].push(normal_throughput)
    anomaly_detector["metrics_history"]["error_rate"].push(normal_error_rate)
    anomaly_detector["metrics_history"]["cpu_usage"].push(normal_cpu_usage)
    
    current_time = current_time + 100L
  }
  
  // 计算基线统计
  for metric_name in anomaly_detector["metrics_history"].keys() {
    let values = anomaly_detector["metrics_history"][metric_name]
    let sum = 0.0
    for value in values {
      sum = sum + value
    }
    let mean = sum / values.length()
    
    let variance = 0.0
    for value in values {
      variance = variance + (value - mean) * (value - mean)
    }
    variance = variance / values.length()
    let std_dev = @sqrt(variance)
    
    anomaly_detector["baseline_statistics"][metric_name]["mean"] = mean
    anomaly_detector["baseline_statistics"][metric_name]["std_dev"] = std_dev
  }
  
  // 注入异常数据并检测
  let anomaly_scenarios = [
    {
      "name": "response_time_spike",
      "duration": 2000L,
      "response_time_multiplier": 3.0,
      "throughput_multiplier": 1.0,
      "error_rate_multiplier": 1.0,
      "cpu_usage_multiplier": 1.0
    },
    {
      "name": "throughput_drop",
      "duration": 2000L,
      "response_time_multiplier": 1.0,
      "throughput_multiplier": 0.3,
      "error_rate_multiplier": 1.0,
      "cpu_usage_multiplier": 1.0
    },
    {
      "name": "error_rate_spike",
      "duration": 1500L,
      "response_time_multiplier": 1.0,
      "throughput_multiplier": 1.0,
      "error_rate_multiplier": 5.0,
      "cpu_usage_multiplier": 1.0
    },
    {
      "name": "cpu_usage_spike",
      "duration": 2500L,
      "response_time_multiplier": 1.0,
      "throughput_multiplier": 1.0,
      "error_rate_multiplier": 1.0,
      "cpu_usage_multiplier": 2.0
    }
  ]
  
  for scenario in anomaly_scenarios {
    let scenario_start = current_time
    let scenario_end = current_time + scenario["duration"]
    
    while current_time < scenario_end {
      // 生成异常数据
      let base_response_time = 200.0 + (@sin(current_time / 1000.0) * 50.0)
      let base_throughput = 100.0 + (@cos(current_time / 2000.0) * 20.0)
      let base_error_rate = 0.02 + (@sin(current_time / 3000.0) * 0.01)
      let base_cpu_usage = 45.0 + (@cos(current_time / 1500.0) * 10.0)
      
      let anomalous_response_time = base_response_time * scenario["response_time_multiplier"]
      let anomalous_throughput = base_throughput * scenario["throughput_multiplier"]
      let anomalous_error_rate = base_error_rate * scenario["error_rate_multiplier"]
      let anomalous_cpu_usage = base_cpu_usage * scenario["cpu_usage_multiplier"]
      
      // 异常检测逻辑
      let detected_anomalies = []
      
      // 统计异常检测
      let response_time_z = @abs(anomalous_response_time - anomaly_detector["baseline_statistics"]["response_time"]["mean"]) / anomaly_detector["baseline_statistics"]["response_time"]["std_dev"]
      let throughput_z = @abs(anomalous_throughput - anomaly_detector["baseline_statistics"]["throughput"]["mean"]) / anomaly_detector["baseline_statistics"]["throughput"]["std_dev"]
      let error_rate_z = @abs(anomalous_error_rate - anomaly_detector["baseline_statistics"]["error_rate"]["mean"]) / anomaly_detector["baseline_statistics"]["error_rate"]["std_dev"]
      let cpu_usage_z = @abs(anomalous_cpu_usage - anomaly_detector["baseline_statistics"]["cpu_usage"]["mean"]) / anomaly_detector["baseline_statistics"]["cpu_usage"]["std_dev"]
      
      if response_time_z > anomaly_detection_config["threshold_std_dev"] {
        detected_anomalies.push("response_time_anomaly")
      }
      if throughput_z > anomaly_detection_config["threshold_std_dev"] {
        detected_anomalies.push("throughput_anomaly")
      }
      if error_rate_z > anomaly_detection_config["threshold_std_dev"] {
        detected_anomalies.push("error_rate_anomaly")
      }
      if cpu_usage_z > anomaly_detection_config["threshold_std_dev"] {
        detected_anomalies.push("cpu_usage_anomaly")
      }
      
      // 记录检测到的异常
      if detected_anomalies.length() > 0 {
        let anomaly_record = {
          "timestamp": current_time,
          "scenario": scenario["name"],
          "anomaly_types": detected_anomalies,
          "metrics": {
            "response_time": anomalous_response_time,
            "throughput": anomalous_throughput,
            "error_rate": anomalous_error_rate,
            "cpu_usage": anomalous_cpu_usage
          },
          "z_scores": {
            "response_time": response_time_z,
            "throughput": throughput_z,
            "error_rate": error_rate_z,
            "cpu_usage": cpu_usage_z
          }
        }
        anomaly_detector["detected_anomalies"].push(anomaly_record)
      }
      
      // 更新指标历史（用于趋势检测）
      anomaly_detector["metrics_history"]["response_time"].push(anomalous_response_time)
      anomaly_detector["metrics_history"]["throughput"].push(anomalous_throughput)
      anomaly_detector["metrics_history"]["error_rate"].push(anomalous_error_rate)
      anomaly_detector["metrics_history"]["cpu_usage"].push(anomalous_cpu_usage)
      
      // 限制历史数据长度
      let max_history = anomaly_detection_config["statistical_window"]
      for metric_name in anomaly_detector["metrics_history"].keys() {
        let history = anomaly_detector["metrics_history"][metric_name]
        if history.length() > max_history {
          anomaly_detector["metrics_history"][metric_name] = history.slice(-max_history)
        }
      }
      
      current_time = current_time + 100L
    }
  }
  
  // 验证异常检测结果
  assert_eq(anomaly_detector["detected_anomalies"].length() > 0, true)
  
  // 验证特定场景的异常检测
  let scenario_anomaly_counts = {
    "response_time_spike": 0,
    "throughput_drop": 0,
    "error_rate_spike": 0,
    "cpu_usage_spike": 0
  }
  
  for anomaly in anomaly_detector["detected_anomalies"] {
    let scenario = anomaly["scenario"]
    if scenario_anomaly_counts.contains_key(scenario) {
      scenario_anomaly_counts[scenario] = scenario_anomaly_counts[scenario] + 1
    }
  }
  
  assert_eq(scenario_anomaly_counts["response_time_spike"] > 0, true)
  assert_eq(scenario_anomaly_counts["throughput_drop"] > 0, true)
  assert_eq(scenario_anomaly_counts["error_rate_spike"] > 0, true)
  assert_eq(scenario_anomaly_counts["cpu_usage_spike"] > 0, true)
  
  // 验证异常记录完整性
  for anomaly in anomaly_detector["detected_anomalies"] {
    assert_eq(anomaly["timestamp"] > 0L, true)
    assert_eq(anomaly["scenario"].length() > 0, true)
    assert_eq(anomaly["anomaly_types"].length() > 0, true)
    assert_eq(anomaly.contains_key("metrics"), true)
    assert_eq(anomaly.contains_key("z_scores"), true)
  }
  
  // 验证基线统计计算正确
  for metric_name in anomaly_detector["baseline_statistics"].keys() {
    let stats = anomaly_detector["baseline_statistics"][metric_name]
    assert_eq(stats["mean"] > 0.0, true)
    assert_eq(stats["std_dev"] > 0.0, true)
  }
}
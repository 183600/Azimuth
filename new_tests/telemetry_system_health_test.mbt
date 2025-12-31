// 遥测系统健康检查测试用例

test "telemetry_system_self_monitoring" {
  // 测试遥测系统自监控
  
  // 定义系统健康指标
  type HealthMetric = {
    metric_name: String,
    current_value: Double,
    threshold_warning: Double,
    threshold_critical: Double,
    unit: String,
    status: String
  }
  
  // 定义系统组件状态
  type ComponentStatus = {
    component_name: String,
    is_healthy: Bool,
    last_check_time: Int64,
    uptime_seconds: Int64,
    error_count: Int64,
    response_time_ms: Double,
    status_message: String
  }
  
  // 模拟系统健康指标
  let health_metrics = [
    HealthMetric {
      metric_name: "cpu_usage",
      current_value: 65.5,
      threshold_warning: 70.0,
      threshold_critical: 90.0,
      unit: "percent",
      status: "healthy"
    },
    HealthMetric {
      metric_name: "memory_usage",
      current_value: 78.2,
      threshold_warning: 80.0,
      threshold_critical: 95.0,
      unit: "percent",
      status: "warning"
    },
    HealthMetric {
      metric_name: "disk_usage",
      current_value: 45.8,
      threshold_warning: 80.0,
      threshold_critical: 95.0,
      unit: "percent",
      status: "healthy"
    },
    HealthMetric {
      metric_name: "network_latency",
      current_value: 125.3,
      threshold_warning: 200.0,
      threshold_critical: 500.0,
      unit: "ms",
      status: "healthy"
    },
    HealthMetric {
      metric_name: "queue_depth",
      current_value: 850.0,
      threshold_warning: 1000.0,
      threshold_critical: 2000.0,
      unit: "items",
      status: "healthy"
    },
    HealthMetric {
      metric_name: "error_rate",
      current_value: 2.8,
      threshold_warning: 5.0,
      threshold_critical: 10.0,
      unit: "percent",
      status: "healthy"
    }
  ]
  
  // 验证健康指标
  assert_eq(health_metrics.length(), 6)
  assert_eq(health_metrics[0].metric_name, "cpu_usage")
  assert_eq(health_metrics[1].status, "warning")
  assert_eq(health_metrics[2].unit, "percent")
  
  // 模拟系统组件状态
  let component_statuses = [
    ComponentStatus {
      component_name: "metrics_collector",
      is_healthy: true,
      last_check_time: 1640995200,
      uptime_seconds: 86400,
      error_count: 5,
      response_time_ms: 45.2,
      status_message: "Operating normally"
    },
    ComponentStatus {
      component_name: "trace_processor",
      is_healthy: true,
      last_check_time: 1640995200,
      uptime_seconds: 86400,
      error_count: 12,
      response_time_ms: 78.5,
      status_message: "Processing traces successfully"
    },
    ComponentStatus {
      component_name: "log_aggregator",
      is_healthy: false,
      last_check_time: 1640995180,
      uptime_seconds: 86220,
      error_count: 156,
      response_time_ms: 1250.8,
      status_message: "High error rate detected"
    },
    ComponentStatus {
      component_name: "data_exporter",
      is_healthy: true,
      last_check_time: 1640995200,
      uptime_seconds: 72000,
      error_count: 3,
      response_time_ms: 234.7,
      status_message: "Exporting data to backend"
    },
    ComponentStatus {
      component_name: "configuration_manager",
      is_healthy: true,
      last_check_time: 1640995195,
      uptime_seconds: 86400,
      error_count: 0,
      response_time_ms: 12.3,
      status_message: "Configuration loaded successfully"
    }
  ]
  
  // 验证组件状态
  assert_eq(component_statuses.length(), 5)
  assert_eq(component_statuses[0].component_name, "metrics_collector")
  assert_eq(component_statuses[2].is_healthy, false)
  assert_eq(component_statuses[4].error_count, 0)
  
  // 计算整体系统健康状态
  type SystemHealth = {
    overall_status: String,
    healthy_components: Int,
    unhealthy_components: Int,
    warning_metrics: Int,
    critical_metrics: Int,
    health_score: Double,
    last_assessment_time: Int64
  }
  
  // 统计健康状态
  let mut healthy_components = 0
  let mut unhealthy_components = 0
  let mut warning_metrics = 0
  let mut critical_metrics = 0
  
  // 统计组件健康状态
  let mut i = 0
  while i < component_statuses.length() {
    if component_statuses[i].is_healthy {
      healthy_components = healthy_components + 1
    } else {
      unhealthy_components = unhealthy_components + 1
    }
    i = i + 1
  }
  
  // 统计指标健康状态
  i = 0
  while i < health_metrics.length() {
    let metric = health_metrics[i]
    if metric.status == "warning" {
      warning_metrics = warning_metrics + 1
    } else if metric.status == "critical" {
      critical_metrics = critical_metrics + 1
    }
    i = i + 1
  }
  
  // 计算健康评分（0-100）
  let component_score = if component_statuses.length() > 0 {
    healthy_components.to_double() / component_statuses.length().to_double() * 70.0
  } else { 0.0 }
  
  let metric_score = if health_metrics.length() > 0 {
    let healthy_metrics = health_metrics.length() - warning_metrics - critical_metrics
    healthy_metrics.to_double() / health_metrics.length().to_double() * 30.0
  } else { 0.0 }
  
  let health_score = component_score + metric_score
  
  // 确定整体状态
  let overall_status = if critical_metrics > 0 or unhealthy_components > 0 {
    "critical"
  } else if warning_metrics > 0 {
    "warning"
  } else {
    "healthy"
  }
  
  // 创建系统健康状态
  let system_health = SystemHealth {
    overall_status: overall_status,
    healthy_components: healthy_components,
    unhealthy_components: unhealthy_components,
    warning_metrics: warning_metrics,
    critical_metrics: critical_metrics,
    health_score: health_score,
    last_assessment_time: 1640995200
  }
  
  // 验证系统健康状态
  assert_eq(system_health.healthy_components, 4)
  assert_eq(system_health.unhealthy_components, 1)
  assert_eq(system_health.warning_metrics, 1)
  assert_eq(system_health.critical_metrics, 0)
  assert_eq(system_health.overall_status, "warning")  // 有警告指标和不健康组件
  assert_eq(system_health.health_score > 50.0, true)
  assert_eq(system_health.health_score < 90.0, true)
  
  // 生成健康检查报告
  let health_report = "Telemetry System Health Report:\n"
    + "Overall Status: " + system_health.overall_status + "\n"
    + "Health Score: " + system_health.health_score.to_string() + "/100\n"
    + "Component Status:\n"
    + "  - Healthy: " + system_health.healthy_components.to_string() + "\n"
    + "  - Unhealthy: " + system_health.unhealthy_components.to_string() + "\n"
    + "Metric Status:\n"
    + "  - Warning: " + system_health.warning_metrics.to_string() + "\n"
    + "  - Critical: " + system_health.critical_metrics.to_string() + "\n"
    + "Critical Components:\n"
    + "  - log_aggregator: " + component_statuses[2].status_message + "\n"
    + "Warning Metrics:\n"
    + "  - memory_usage: " + health_metrics[1].current_value.to_string() + "% (threshold: " + health_metrics[1].threshold_warning.to_string() + "%)\n"
    + "Last Assessment: " + system_health.last_assessment_time.to_string()
  
  // 验证报告内容
  assert_eq(health_report.contains("Overall Status: warning"), true)
  assert_eq(health_report.contains("Health Score:"), true)
  assert_eq(health_report.contains("Healthy: 4"), true)
  assert_eq(health_report.contains("Unhealthy: 1"), true)
  assert_eq(health_report.contains("Warning: 1"), true)
  assert_eq(health_report.contains("Critical: 0"), true)
  assert_eq(health_report.contains("log_aggregator:"), true)
  assert_eq(health_report.contains("memory_usage:"), true)
}

test "telemetry_performance_monitoring" {
  // 测试遥测性能监控
  
  // 定义性能指标
  type PerformanceMetric = {
    metric_name: String,
    current_value: Double,
    baseline_value: Double,
    unit: String,
    trend: String,
    performance_impact: String
  }
  
  // 定义吞吐量统计
  type ThroughputStats = {
    requests_per_second: Double,
    data_points_per_second: Double,
    bytes_per_second: Double,
    peak_rps: Double,
    average_rps: Double
  }
  
  // 定义延迟统计
  type LatencyStats = {
    p50_latency_ms: Double,
    p95_latency_ms: Double,
    p99_latency_ms: Double,
    average_latency_ms: Double,
    max_latency_ms: Double
  }
  
  // 模拟性能指标
  let performance_metrics = [
    PerformanceMetric {
      metric_name: "processing_latency",
      current_value: 125.5,
      baseline_value: 100.0,
      unit: "ms",
      trend: "increasing",
      performance_impact: "moderate"
    },
    PerformanceMetric {
      metric_name: "throughput",
      current_value: 850.2,
      baseline_value: 1000.0,
      unit: "rps",
      trend: "decreasing",
      performance_impact: "high"
    },
    PerformanceMetric {
      metric_name: "error_rate",
      current_value: 2.1,
      baseline_value: 1.5,
      unit: "percent",
      trend: "increasing",
      performance_impact: "moderate"
    },
    PerformanceMetric {
      metric_name: "memory_efficiency",
      current_value: 78.5,
      baseline_value: 85.0,
      unit: "percent",
      trend: "decreasing",
      performance_impact: "low"
    },
    PerformanceMetric {
      metric_name: "cpu_utilization",
      current_value: 67.3,
      baseline_value: 60.0,
      unit: "percent",
      trend: "increasing",
      performance_impact: "moderate"
    }
  ]
  
  // 验证性能指标
  assert_eq(performance_metrics.length(), 5)
  assert_eq(performance_metrics[0].metric_name, "processing_latency")
  assert_eq(performance_metrics[1].trend, "decreasing")
  assert_eq(performance_metrics[2].performance_impact, "moderate")
  
  // 模拟吞吐量统计
  let throughput_stats = ThroughputStats {
    requests_per_second: 850.2,
    data_points_per_second: 4251.0,
    bytes_per_second: 2048576.0,
    peak_rps: 1200.0,
    average_rps: 925.5
  }
  
  // 验证吞吐量统计
  assert_eq(throughput_stats.requests_per_second, 850.2)
  assert_eq(throughput_stats.data_points_per_second, 4251.0)
  assert_eq(throughput_stats.peak_rps, 1200.0)
  assert_eq(throughput_stats.average_rps, 925.5)
  
  // 模拟延迟统计
  let latency_stats = LatencyStats {
    p50_latency_ms: 85.2,
    p95_latency_ms: 245.8,
    p99_latency_ms: 450.3,
    average_latency_ms: 125.5,
    max_latency_ms: 1250.7
  }
  
  // 验证延迟统计
  assert_eq(latency_stats.p50_latency_ms, 85.2)
  assert_eq(latency_stats.p95_latency_ms, 245.8)
  assert_eq(latency_stats.p99_latency_ms, 450.3)
  assert_eq(latency_stats.max_latency_ms, 1250.7)
  
  // 性能评估
  type PerformanceAssessment = {
    overall_performance: String,
    performance_score: Double,
    bottleneck_metrics: Array[String],
    improvement_opportunities: Array[String],
    performance_trend: String
  }
  
  // 计算性能评分
  let mut performance_score = 100.0
  let mut bottleneck_metrics = []
  let mut improvement_opportunities = []
  
  // 评估每个指标
  let mut i = 0
  while i < performance_metrics.length() {
    let metric = performance_metrics[i]
    let deviation = (metric.current_value - metric.baseline_value) / metric.baseline_value * 100.0
    
    // 根据偏差调整评分
    if metric.performance_impact == "high" {
      performance_score = performance_score - (deviation.abs() * 0.5)
    } else if metric.performance_impact == "moderate" {
      performance_score = performance_score - (deviation.abs() * 0.3)
    } else {
      performance_score = performance_score - (deviation.abs() * 0.1)
    }
    
    // 识别瓶颈
    if deviation.abs() > 20.0 and metric.performance_impact != "low" {
      bottleneck_metrics.push(metric.metric_name)
    }
    
    // 识别改进机会
    if metric.trend == "decreasing" and metric.metric_name.contains("throughput") {
      improvement_opportunities.push("Optimize " + metric.metric_name)
    } else if metric.trend == "increasing" and (metric.metric_name.contains("latency") or metric.metric_name.contains("error")) {
      improvement_opportunities.push("Reduce " + metric.metric_name)
    }
    
    i = i + 1
  }
  
  // 确保评分在合理范围内
  if performance_score < 0.0 { performance_score = 0.0 }
  if performance_score > 100.0 { performance_score = 100.0 }
  
  // 确定整体性能状态
  let overall_performance = if performance_score >= 80.0 {
    "excellent"
  } else if performance_score >= 60.0 {
    "good"
  } else if performance_score >= 40.0 {
    "fair"
  } else {
    "poor"
  }
  
  // 计算整体趋势
  let increasing_trends = 0
  let decreasing_trends = 0
  i = 0
  while i < performance_metrics.length() {
    if performance_metrics[i].trend == "increasing" {
      // increasing_trends = increasing_trends + 1
    } else {
      // decreasing_trends = decreasing_trends + 1
    }
    i = i + 1
  }
  
  let performance_trend = if bottleneck_metrics.length() > 2 {
    "degrading"
  } else if bottleneck_metrics.length() > 0 {
    "stable_with_issues"
  } else {
    "improving"
  }
  
  // 创建性能评估
  let performance_assessment = PerformanceAssessment {
    overall_performance: overall_performance,
    performance_score: performance_score,
    bottleneck_metrics: bottleneck_metrics,
    improvement_opportunities: improvement_opportunities,
    performance_trend: performance_trend
  }
  
  // 验证性能评估
  assert_eq(performance_assessment.performance_score < 100.0, true)
  assert_eq(performance_assessment.performance_score > 0.0, true)
  assert_eq(performance_assessment.bottleneck_metrics.length() > 0, true)
  assert_eq(performance_assessment.improvement_opportunities.length() > 0, true)
  
  // 生成性能监控报告
  let performance_report = "Telemetry Performance Monitoring Report:\n"
    + "Overall Performance: " + performance_assessment.overall_performance + "\n"
    + "Performance Score: " + performance_assessment.performance_score.to_string() + "/100\n"
    + "Performance Trend: " + performance_assessment.performance_trend + "\n"
    + "Throughput Statistics:\n"
    + "  - Current RPS: " + throughput_stats.requests_per_second.to_string() + "\n"
    + "  - Peak RPS: " + throughput_stats.peak_rps.to_string() + "\n"
    + "  - Average RPS: " + throughput_stats.average_rps.to_string() + "\n"
    + "Latency Statistics:\n"
    + "  - P50: " + latency_stats.p50_latency_ms.to_string() + "ms\n"
    + "  - P95: " + latency_stats.p95_latency_ms.to_string() + "ms\n"
    + "  - P99: " + latency_stats.p99_latency_ms.to_string() + "ms\n"
    + "  - Max: " + latency_stats.max_latency_ms.to_string() + "ms\n"
    + "Identified Bottlenecks:\n"
    + "  - " + performance_assessment.bottleneck_metrics.join("\n  - ") + "\n"
    + "Improvement Opportunities:\n"
    + "  - " + performance_assessment.improvement_opportunities.join("\n  - ")
  
  // 验证报告内容
  assert_eq(performance_report.contains("Overall Performance:"), true)
  assert_eq(performance_report.contains("Performance Score:"), true)
  assert_eq(performance_report.contains("Throughput Statistics:"), true)
  assert_eq(performance_report.contains("Latency Statistics:"), true)
  assert_eq(performance_report.contains("Identified Bottlenecks:"), true)
  assert_eq(performance_report.contains("Improvement Opportunities:"), true)
  assert_eq(performance_report.contains("Current RPS: 850.2"), true)
  assert_eq(performance_report.contains("P95: 245.8ms"), true)
}

test "telemetry_anomaly_detection" {
  // 测试遥测异常检测
  
  // 定义异常类型
  type AnomalyType = {
    type_name: String,
    severity: String,
    description: String,
    detection_threshold: Double
  }
  
  // 定义异常事件
  type AnomalyEvent = {
    event_id: String,
    anomaly_type: String,
    detection_time: Int64,
    affected_metric: String,
    observed_value: Double,
    expected_value: Double,
    deviation_percentage: Double,
    confidence_score: Double,
    is_resolved: Bool
  }
  
  // 定义异常模式
  type AnomalyPattern = {
    pattern_name: String,
    pattern_type: String,
    description: String,
    frequency: String,
    impact_assessment: String
  }
  
  // 模拟异常类型
  let anomaly_types = [
    AnomalyType {
      type_name: "spike_detection",
      severity: "medium",
      description: "Sudden increase in metric value",
      detection_threshold: 200.0  // 200% of baseline
    },
    AnomalyType {
      type_name: "drop_detection",
      severity: "high",
      description: "Sudden decrease in metric value",
      detection_threshold: 50.0   // 50% of baseline
    },
    AnomalyType {
      type_name: "trend_anomaly",
      severity: "low",
      description: "Unusual trend pattern detected",
      detection_threshold: 150.0  // 150% deviation over time
    }
  ]
  
  // 验证异常类型
  assert_eq(anomaly_types.length(), 3)
  assert_eq(anomaly_types[0].severity, "medium")
  assert_eq(anomaly_types[1].detection_threshold, 50.0)
  
  // 模拟异常事件
  let anomaly_events = [
    AnomalyEvent {
      event_id: "anom-001",
      anomaly_type: "spike_detection",
      detection_time: 1640995200,
      affected_metric: "error_rate",
      observed_value: 15.5,
      expected_value: 2.0,
      deviation_percentage: 675.0,
      confidence_score: 0.95,
      is_resolved: false
    },
    AnomalyEvent {
      event_id: "anom-002",
      anomaly_type: "drop_detection",
      detection_time: 1640995300,
      affected_metric: "throughput",
      observed_value: 200.0,
      expected_value: 1000.0,
      deviation_percentage: -80.0,
      confidence_score: 0.88,
      is_resolved: false
    },
    AnomalyEvent {
      event_id: "anom-003",
      anomaly_type: "trend_anomaly",
      detection_time: 1640995400,
      affected_metric: "memory_usage",
      observed_value: 95.0,
      expected_value: 70.0,
      deviation_percentage: 35.7,
      confidence_score: 0.72,
      is_resolved: true
    },
    AnomalyEvent {
      event_id: "anom-004",
      anomaly_type: "spike_detection",
      detection_time: 1640995500,
      affected_metric: "latency",
      observed_value: 800.0,
      expected_value: 100.0,
      deviation_percentage: 700.0,
      confidence_score: 0.91,
      is_resolved: false
    }
  ]
  
  // 验证异常事件
  assert_eq(anomaly_events.length(), 4)
  assert_eq(anomaly_events[0].anomaly_type, "spike_detection")
  assert_eq(anomaly_events[1].deviation_percentage, -80.0)
  assert_eq(anomaly_events[2].is_resolved, true)
  
  // 模拟异常模式
  let anomaly_patterns = [
    AnomalyPattern {
      pattern_name: "weekend_load_pattern",
      pattern_type: "seasonal",
      description: "Consistent spike in error rates during weekends",
      frequency: "weekly",
      impact_assessment: "medium"
    },
    AnomalyPattern {
      pattern_name: "memory_leak_pattern",
      pattern_type: "trend",
      description: "Gradual memory increase over time",
      frequency: "continuous",
      impact_assessment: "high"
    },
    AnomalyPattern {
      pattern_name: "batch_job_pattern",
      pattern_type: "periodic",
      description: "Regular performance degradation during batch processing",
      frequency: "daily",
      impact_assessment: "low"
    }
  ]
  
  // 验证异常模式
  assert_eq(anomaly_patterns.length(), 3)
  assert_eq(anomaly_patterns[0].pattern_type, "seasonal")
  assert_eq(anomaly_patterns[1].impact_assessment, "high")
  
  // 异常检测分析
  type AnomalyAnalysis = {
    total_anomalies: Int,
    active_anomalies: Int,
    resolved_anomalies: Int,
    high_severity_anomalies: Int,
    anomalies_by_type: Array[(String, Int)],
    most_affected_metrics: Array[String],
    average_confidence: Double,
    detection_accuracy: Double
  }
  
  // 统计异常
  let mut active_anomalies = 0
  let mut resolved_anomalies = 0
  let mut high_severity_anomalies = 0
  let mut anomalies_by_type = {}
  let mut affected_metrics = {}
  let mut total_confidence = 0.0
  
  let mut i = 0
  while i < anomaly_events.length() {
    let event = anomaly_events[i]
    
    if event.is_resolved {
      resolved_anomalies = resolved_anomalies + 1
    } else {
      active_anomalies = active_anomalies + 1
    }
    
    // 统计异常类型
    let anomaly_type = event.anomaly_type
    let current_count = anomalies_by_type.get(anomaly_type) |> unwrap_or(0)
    anomalies_by_type[anomaly_type] = current_count + 1
    
    // 统计受影响指标
    let metric = event.affected_metric
    let current_metric_count = affected_metrics.get(metric) |> unwrap_or(0)
    affected_metrics[metric] = current_metric_count + 1
    
    // 累积置信度
    total_confidence = total_confidence + event.confidence_score
    
    // 检查高严重性（基于偏差程度）
    if event.deviation_percentage.abs() > 500.0 {
      high_severity_anomalies = high_severity_anomalies + 1
    }
    
    i = i + 1
  }
  
  // 计算平均置信度
  let average_confidence = if anomaly_events.length() > 0 {
    total_confidence / anomaly_events.length().to_double()
  } else { 0.0 }
  
  // 找出最受影响的指标
  let mut most_affected_metrics = []
  let metric_keys = ["error_rate", "throughput", "memory_usage", "latency"]
  
  i = 0
  while i < metric_keys.length() {
    let metric = metric_keys[i]
    let count = affected_metrics.get(metric) |> unwrap_or(0)
    if count > 0 {
      most_affected_metrics.push(metric + " (" + count.to_string() + ")")
    }
    i = i + 1
  }
  
  // 创建异常分析
  let anomaly_analysis = AnomalyAnalysis {
    total_anomalies: anomaly_events.length(),
    active_anomalies: active_anomalies,
    resolved_anomalies: resolved_anomalies,
    high_severity_anomalies: high_severity_anomalies,
    anomalies_by_type: [
      ("spike_detection", anomalies_by_type["spike_detection"]),
      ("drop_detection", anomalies_by_type["drop_detection"]),
      ("trend_anomaly", anomalies_by_type["trend_anomaly"])
    ],
    most_affected_metrics: most_affected_metrics,
    average_confidence: average_confidence,
    detection_accuracy: 85.5  // 模拟检测准确率
  }
  
  // 验证异常分析
  assert_eq(anomaly_analysis.total_anomalies, 4)
  assert_eq(anomaly_analysis.active_anomalies, 3)
  assert_eq(anomaly_analysis.resolved_anomalies, 1)
  assert_eq(anomaly_analysis.high_severity_anomalies, 3)
  assert_eq(anomaly_analysis.average_confidence > 0.8, true)
  assert_eq(anomaly_analysis.most_affected_metrics.length() > 0, true)
  
  // 生成异常检测报告
  let anomaly_report = "Telemetry Anomaly Detection Report:\n"
    + "Total Anomalies Detected: " + anomaly_analysis.total_anomalies.to_string() + "\n"
    + "Active Anomalies: " + anomaly_analysis.active_anomalies.to_string() + "\n"
    + "Resolved Anomalies: " + anomaly_analysis.resolved_anomalies.to_string() + "\n"
    + "High Severity Anomalies: " + anomaly_analysis.high_severity_anomalies.to_string() + "\n"
    + "Average Confidence: " + anomaly_analysis.average_confidence.to_string() + "\n"
    + "Detection Accuracy: " + anomaly_analysis.detection_accuracy.to_string() + "%\n"
    + "Anomalies by Type:\n"
    + "  - Spike Detection: " + anomaly_analysis.anomalies_by_type[0].1.to_string() + "\n"
    + "  - Drop Detection: " + anomaly_analysis.anomalies_by_type[1].1.to_string() + "\n"
    + "  - Trend Anomaly: " + anomaly_analysis.anomalies_by_type[2].1.to_string() + "\n"
    + "Most Affected Metrics:\n"
    + "  - " + anomaly_analysis.most_affected_metrics.join("\n  - ") + "\n"
    + "Identified Patterns:\n"
    + "  - " + anomaly_patterns[0].pattern_name + ": " + anomaly_patterns[0].description + "\n"
    + "  - " + anomaly_patterns[1].pattern_name + ": " + anomaly_patterns[1].description + "\n"
    + "  - " + anomaly_patterns[2].pattern_name + ": " + anomaly_patterns[2].description
  
  // 验证报告内容
  assert_eq(anomaly_report.contains("Total Anomalies Detected: 4"), true)
  assert_eq(anomaly_report.contains("Active Anomalies: 3"), true)
  assert_eq(anomaly_report.contains("Resolved Anomalies: 1"), true)
  assert_eq(anomaly_report.contains("High Severity Anomalies: 3"), true)
  assert_eq(anomaly_report.contains("Spike Detection: 2"), true)
  assert_eq(anomaly_report.contains("Drop Detection: 1"), true)
  assert_eq(anomaly_report.contains("Trend Anomaly: 1"), true)
  assert_eq(anomaly_report.contains("Most Affected Metrics:"), true)
  assert_eq(anomaly_report.contains("Identified Patterns:"), true)
}
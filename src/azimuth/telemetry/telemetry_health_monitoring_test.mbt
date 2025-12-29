// 健康检查和监控测试用例
// 验证遥测系统的健康状态检查、监控指标和系统可用性

test "telemetry_system_health_check" {
  // 测试遥测系统健康检查
  
  let system_components = {
    "collector": {
      "status": "healthy",
      "last_check": 1640995200000L,
      "response_time_ms": 45,
      "uptime_seconds": 86400,
      "version": "1.2.3"
    },
    "exporter": {
      "status": "healthy",
      "last_check": 1640995200000L,
      "response_time_ms": 120,
      "uptime_seconds": 86400,
      "version": "1.2.3"
    },
    "storage": {
      "status": "degraded",
      "last_check": 1640995200000L,
      "response_time_ms": 250,
      "uptime_seconds": 86400,
      "version": "1.2.3"
    },
    "sampling": {
      "status": "healthy",
      "last_check": 1640995200000L,
      "response_time_ms": 5,
      "uptime_seconds": 86400,
      "version": "1.2.3"
    }
  }
  
  // 健康检查评估函数
  let evaluate_system_health = fn(components: Map[String, Map[String, Any]]) -> Map[String, Any] {
    let mut healthy_components = 0
    let mut degraded_components = 0
    let mut unhealthy_components = 0
    let mut total_response_time = 0
    
    let component_names = ["collector", "exporter", "storage", "sampling"]
    
    let mut i = 0
    while i < component_names.length() {
      let component_name = component_names[i]
      let component = components[component_name]
      let status = component["status"]
      
      if status == "healthy" {
        healthy_components = healthy_components + 1
      } else if status == "degraded" {
        degraded_components = degraded_components + 1
      } else {
        unhealthy_components = unhealthy_components + 1
      }
      
      total_response_time = total_response_time + component["response_time_ms"]
      i = i + 1
    }
    
    let total_components = component_names.length()
    let health_percentage = (healthy_components.to_double() / total_components.to_double()) * 100.0
    let average_response_time = total_response_time / total_components
    
    let mut overall_status = "healthy"
    if unhealthy_components > 0 {
      overall_status = "unhealthy"
    } else if degraded_components > 0 {
      overall_status = "degraded"
    }
    
    {
      "overall_status": overall_status,
      "health_percentage": health_percentage,
      "healthy_components": healthy_components,
      "degraded_components": degraded_components,
      "unhealthy_components": unhealthy_components,
      "average_response_time_ms": average_response_time
    }
  }
  
  let health_assessment = evaluate_system_health(system_components)
  
  // 验证健康检查结果
  assert_eq(health_assessment["overall_status"], "degraded") // 因为storage组件状态为degraded
  assert_eq(health_assessment["health_percentage"], 75.0) // 3/4 组件健康
  assert_eq(health_assessment["healthy_components"], 3)
  assert_eq(health_assessment["degraded_components"], 1)
  assert_eq(health_assessment["unhealthy_components"], 0)
  assert_eq(health_assessment["average_response_time_ms"], (45 + 120 + 250 + 5) / 4)
}

test "telemetry_system_metrics_collection" {
  // 测试遥测系统指标收集
  
  let system_metrics = {
    "performance": {
      "cpu_usage_percent": 45.2,
      "memory_usage_mb": 1024,
      "disk_usage_percent": 67.8,
      "network_io_bytes_per_second": 1048576
    },
    "telemetry": {
      "spans_received_per_second": 1500,
      "spans_exported_per_second": 1485,
      "spans_dropped_per_second": 15,
      "metrics_received_per_second": 500,
      "metrics_exported_per_second": 495,
      "logs_received_per_second": 200,
      "logs_exported_per_second": 198
    },
    "errors": {
      "export_failures_per_minute": 2,
      "validation_errors_per_minute": 5,
      "timeouts_per_minute": 1,
      "connection_errors_per_minute": 0
    },
    "latency": {
      "span_processing_p50_ms": 10,
      "span_processing_p95_ms": 25,
      "span_processing_p99_ms": 50,
      "export_latency_p50_ms": 100,
      "export_latency_p95_ms": 250,
      "export_latency_p99_ms": 500
    }
  }
  
  // 计算派生指标
  let calculate_derived_metrics = fn(metrics: Map[String, Any]) -> Map[String, Any] {
    let telemetry_metrics = metrics["telemetry"]
    let error_metrics = metrics["errors"]
    
    // 计算丢失率
    let spans_received = telemetry_metrics["spans_received_per_second"]
    let spans_exported = telemetry_metrics["spans_exported_per_second"]
    let span_drop_rate = if spans_received > 0 {
      ((spans_received - spans_exported).to_double() / spans_received.to_double()) * 100.0
    } else {
      0.0
    }
    
    let metrics_received = telemetry_metrics["metrics_received_per_second"]
    let metrics_exported = telemetry_metrics["metrics_exported_per_second"]
    let metric_drop_rate = if metrics_received > 0 {
      ((metrics_received - metrics_exported).to_double() / metrics_received.to_double()) * 100.0
    } else {
      0.0
    }
    
    let logs_received = telemetry_metrics["logs_received_per_second"]
    let logs_exported = telemetry_metrics["logs_exported_per_second"]
    let log_drop_rate = if logs_received > 0 {
      ((logs_received - logs_exported).to_double() / logs_received.to_double()) * 100.0
    } else {
      0.0
    }
    
    // 计算总错误率
    let total_errors = error_metrics["export_failures_per_minute"] + 
                     error_metrics["validation_errors_per_minute"] + 
                     error_metrics["timeouts_per_minute"] + 
                     error_metrics["connection_errors_per_minute"]
    
    {
      "span_drop_rate_percent": span_drop_rate,
      "metric_drop_rate_percent": metric_drop_rate,
      "log_drop_rate_percent": log_drop_rate,
      "total_errors_per_minute": total_errors
    }
  }
  
  let derived_metrics = calculate_derived_metrics(system_metrics)
  
  // 验证派生指标计算
  assert_eq(derived_metrics["span_drop_rate_percent"], ((1500 - 1485).to_double() / 1500.0) * 100.0)
  assert_eq(derived_metrics["metric_drop_rate_percent"], ((500 - 495).to_double() / 500.0) * 100.0)
  assert_eq(derived_metrics["log_drop_rate_percent"], ((200 - 198).to_double() / 200.0) * 100.0)
  assert_eq(derived_metrics["total_errors_per_minute"], 2 + 5 + 1 + 0)
  
  // 验证原始指标
  assert_eq(system_metrics["performance"]["cpu_usage_percent"], 45.2)
  assert_eq(system_metrics["telemetry"]["spans_received_per_second"], 1500)
  assert_eq(system_metrics["errors"]["export_failures_per_minute"], 2)
  assert_eq(system_metrics["latency"]["span_processing_p50_ms"], 10)
}

test "telemetry_system_alerting" {
  // 测试遥测系统告警
  
  let alert_thresholds = {
    "cpu_usage_percent": 80.0,
    "memory_usage_mb": 2048,
    "disk_usage_percent": 85.0,
    "span_drop_rate_percent": 5.0,
    "export_latency_p95_ms": 1000,
    "error_rate_per_minute": 10
  }
  
  let current_metrics = {
    "cpu_usage_percent": 85.5,      // 超过阈值
    "memory_usage_mb": 1536,        // 正常
    "disk_usage_percent": 90.2,     // 超过阈值
    "span_drop_rate_percent": 7.3,  // 超过阈值
    "export_latency_p95_ms": 800,   // 正常
    "error_rate_per_minute": 12     // 超过阈值
  }
  
  // 告警评估函数
  let evaluate_alerts = fn(metrics: Map[String, Any], thresholds: Map[String, Any]) -> Map[String, Any] {
    let mut alerts = []
    let mut alert_levels = []
    
    // CPU使用率告警
    if metrics["cpu_usage_percent"] > thresholds["cpu_usage_percent"] {
      alerts.push({
        "metric": "cpu_usage_percent",
        "current_value": metrics["cpu_usage_percent"],
        "threshold": thresholds["cpu_usage_percent"],
        "severity": if metrics["cpu_usage_percent"] > 95.0 { "critical" } else { "warning" }
      })
    }
    
    // 内存使用告警
    if metrics["memory_usage_mb"] > thresholds["memory_usage_mb"] {
      alerts.push({
        "metric": "memory_usage_mb",
        "current_value": metrics["memory_usage_mb"],
        "threshold": thresholds["memory_usage_mb"],
        "severity": if metrics["memory_usage_mb"] > 4096 { "critical" } else { "warning" }
      })
    }
    
    // 磁盘使用告警
    if metrics["disk_usage_percent"] > thresholds["disk_usage_percent"] {
      alerts.push({
        "metric": "disk_usage_percent",
        "current_value": metrics["disk_usage_percent"],
        "threshold": thresholds["disk_usage_percent"],
        "severity": if metrics["disk_usage_percent"] > 95.0 { "critical" } else { "warning" }
      })
    }
    
    // Span丢失率告警
    if metrics["span_drop_rate_percent"] > thresholds["span_drop_rate_percent"] {
      alerts.push({
        "metric": "span_drop_rate_percent",
        "current_value": metrics["span_drop_rate_percent"],
        "threshold": thresholds["span_drop_rate_percent"],
        "severity": if metrics["span_drop_rate_percent"] > 10.0 { "critical" } else { "warning" }
      })
    }
    
    // 导出延迟告警
    if metrics["export_latency_p95_ms"] > thresholds["export_latency_p95_ms"] {
      alerts.push({
        "metric": "export_latency_p95_ms",
        "current_value": metrics["export_latency_p95_ms"],
        "threshold": thresholds["export_latency_p95_ms"],
        "severity": if metrics["export_latency_p95_ms"] > 2000 { "critical" } else { "warning" }
      })
    }
    
    // 错误率告警
    if metrics["error_rate_per_minute"] > thresholds["error_rate_per_minute"] {
      alerts.push({
        "metric": "error_rate_per_minute",
        "current_value": metrics["error_rate_per_minute"],
        "threshold": thresholds["error_rate_per_minute"],
        "severity": if metrics["error_rate_per_minute"] > 20 { "critical" } else { "warning" }
      })
    }
    
    // 统计告警级别
    let mut warning_count = 0
    let mut critical_count = 0
    
    let mut i = 0
    while i < alerts.length() {
      let alert = alerts[i]
      if alert["severity"] == "warning" {
        warning_count = warning_count + 1
      } else if alert["severity"] == "critical" {
        critical_count = critical_count + 1
      }
      i = i + 1
    }
    
    {
      "total_alerts": alerts.length(),
      "warning_alerts": warning_count,
      "critical_alerts": critical_count,
      "alerts": alerts
    }
  }
  
  let alert_evaluation = evaluate_alerts(current_metrics, alert_thresholds)
  
  // 验证告警评估结果
  assert_eq(alert_evaluation["total_alerts"], 4) // CPU、磁盘、span丢失率、错误率超过阈值
  assert_eq(alert_evaluation["warning_alerts"], 4) // 所有告警都是warning级别
  assert_eq(alert_evaluation["critical_alerts"], 0)
  
  // 验证具体告警
  let alerts = alert_evaluation["alerts"]
  
  let mut cpu_alert_found = false
  let mut disk_alert_found = false
  let mut span_drop_alert_found = false
  let mut error_rate_alert_found = false
  
  let mut i = 0
  while i < alerts.length() {
    let alert = alerts[i]
    
    if alert["metric"] == "cpu_usage_percent" {
      assert_eq(alert["current_value"], 85.5)
      assert_eq(alert["threshold"], 80.0)
      assert_eq(alert["severity"], "warning")
      cpu_alert_found = true
    }
    
    if alert["metric"] == "disk_usage_percent" {
      assert_eq(alert["current_value"], 90.2)
      assert_eq(alert["threshold"], 85.0)
      assert_eq(alert["severity"], "warning")
      disk_alert_found = true
    }
    
    if alert["metric"] == "span_drop_rate_percent" {
      assert_eq(alert["current_value"], 7.3)
      assert_eq(alert["threshold"], 5.0)
      assert_eq(alert["severity"], "warning")
      span_drop_alert_found = true
    }
    
    if alert["metric"] == "error_rate_per_minute" {
      assert_eq(alert["current_value"], 12)
      assert_eq(alert["threshold"], 10)
      assert_eq(alert["severity"], "warning")
      error_rate_alert_found = true
    }
    
    i = i + 1
  }
  
  assert_eq(cpu_alert_found, true)
  assert_eq(disk_alert_found, true)
  assert_eq(span_drop_alert_found, true)
  assert_eq(error_rate_alert_found, true)
}

test "telemetry_system_availability_monitoring" {
  // 测试遥测系统可用性监控
  
  let availability_data = [
    {"timestamp": 1640995200000L, "status": "available", "response_time_ms": 45},
    {"timestamp": 16409952060000L, "status": "available", "response_time_ms": 52},
    {"timestamp": 1640995212000L, "status": "degraded", "response_time_ms": 150},
    {"timestamp": 1640995218000L, "status": "available", "response_time_ms": 48},
    {"timestamp": 1640995224000L, "status": "unavailable", "response_time_ms": -1},
    {"timestamp": 1640995230000L, "status": "unavailable", "response_time_ms": -1},
    {"timestamp": 1640995236000L, "status": "degraded", "response_time_ms": 200},
    {"timestamp": 1640995242000L, "status": "available", "response_time_ms": 55},
    {"timestamp": 1640995248000L, "status": "available", "response_time_ms": 47},
    {"timestamp": 1640995254000L, "status": "available", "response_time_ms": 50}
  ]
  
  // 计算可用性指标
  let calculate_availability_metrics = fn(data: Array[Map[String, Any]]) -> Map[String, Any] {
    let mut available_count = 0
    let mut degraded_count = 0
    let mut unavailable_count = 0
    let mut total_response_time = 0
    let mut valid_response_count = 0
    
    let mut i = 0
    while i < data.length() {
      let record = data[i]
      let status = record["status"]
      
      if status == "available" {
        available_count = available_count + 1
      } else if status == "degraded" {
        degraded_count = degraded_count + 1
      } else if status == "unavailable" {
        unavailable_count = unavailable_count + 1
      }
      
      // 计算响应时间（只统计有效的响应时间）
      if record["response_time_ms"] > 0 {
        total_response_time = total_response_time + record["response_time_ms"]
        valid_response_count = valid_response_count + 1
      }
      
      i = i + 1
    }
    
    let total_checks = data.length()
    let availability_percentage = (available_count.to_double() / total_checks.to_double()) * 100.0
    let uptime_percentage = ((available_count + degraded_count).to_double() / total_checks.to_double()) * 100.0
    let average_response_time = if valid_response_count > 0 {
      total_response_time / valid_response_count
    } else {
      0
    }
    
    {
      "total_checks": total_checks,
      "available_count": available_count,
      "degraded_count": degraded_count,
      "unavailable_count": unavailable_count,
      "availability_percentage": availability_percentage,
      "uptime_percentage": uptime_percentage,
      "average_response_time_ms": average_response_time
    }
  }
  
  let availability_metrics = calculate_availability_metrics(availability_data)
  
  // 验证可用性指标
  assert_eq(availability_metrics["total_checks"], 10)
  assert_eq(availability_metrics["available_count"], 6)
  assert_eq(availability_metrics["degraded_count"], 2)
  assert_eq(availability_metrics["unavailable_count"], 2)
  assert_eq(availability_metrics["availability_percentage"], 60.0) // 6/10 * 100
  assert_eq(availability_metrics["uptime_percentage"], 80.0) // (6+2)/10 * 100
  assert_eq(availability_metrics["average_response_time_ms"], (45 + 52 + 150 + 48 + 200 + 55 + 47 + 50) / 8)
}

test "telemetry_system_dependency_health" {
  // 测试遥测系统依赖健康状态
  
  let system_dependencies = [
    {
      "name": "database",
      "type": "internal",
      "status": "healthy",
      "connection_pool": {
        "active": 5,
        "idle": 15,
        "max": 20
      },
      "response_time_p95_ms": 25,
      "error_rate_percent": 0.1
    },
    {
      "name": "message_queue",
      "type": "internal",
      "status": "degraded",
      "queue_depth": 1500,
      "max_queue_depth": 1000,
      "consumer_lag": 500,
      "response_time_p95_ms": 150,
      "error_rate_percent": 2.5
    },
    {
      "name": "external_api",
      "type": "external",
      "status": "healthy",
      "endpoint": "https://api.example.com",
      "response_time_p95_ms": 200,
      "error_rate_percent": 1.2
    },
    {
      "name": "storage_bucket",
      "type": "external",
      "status": "unhealthy",
      "endpoint": "s3://telemetry-data",
      "last_successful_upload": 1640995000000L,
      "error_rate_percent": 15.0
    }
  ]
  
  // 评估依赖健康状态
  let evaluate_dependency_health = fn(dependencies: Array[Map[String, Any]]) -> Map[String, Any] {
    let mut healthy_deps = 0
    let mut degraded_deps = 0
    let mut unhealthy_deps = 0
    let mut internal_deps = 0
    let mut external_deps = 0
    
    let mut i = 0
    while i < dependencies.length() {
      let dep = dependencies[i]
      let status = dep["status"]
      let dep_type = dep["type"]
      
      if status == "healthy" {
        healthy_deps = healthy_deps + 1
      } else if status == "degraded" {
        degraded_deps = degraded_deps + 1
      } else {
        unhealthy_deps = unhealthy_deps + 1
      }
      
      if dep_type == "internal" {
        internal_deps = internal_deps + 1
      } else {
        external_deps = external_deps + 1
      }
      
      i = i + 1
    }
    
    let total_deps = dependencies.length()
    let health_score = (healthy_deps.to_double() / total_deps.to_double()) * 100.0
    
    {
      "total_dependencies": total_deps,
      "healthy_dependencies": healthy_deps,
      "degraded_dependencies": degraded_deps,
      "unhealthy_dependencies": unhealthy_deps,
      "internal_dependencies": internal_deps,
      "external_dependencies": external_deps,
      "health_score": health_score
    }
  }
  
  let dependency_health = evaluate_dependency_health(system_dependencies)
  
  // 验证依赖健康评估
  assert_eq(dependency_health["total_dependencies"], 4)
  assert_eq(dependency_health["healthy_dependencies"], 2)
  assert_eq(dependency_health["degraded_dependencies"], 1)
  assert_eq(dependency_health["unhealthy_dependencies"], 1)
  assert_eq(dependency_health["internal_dependencies"], 2)
  assert_eq(dependency_health["external_dependencies"], 2)
  assert_eq(dependency_health["health_score"], 50.0) // 2/4 * 100
  
  // 验证具体依赖状态
  let mut database_healthy = false
  let mut message_queue_degraded = false
  let mut external_api_healthy = false
  let mut storage_unhealthy = false
  
  let mut i = 0
  while i < system_dependencies.length() {
    let dep = system_dependencies[i]
    
    if dep["name"] == "database" && dep["status"] == "healthy" {
      database_healthy = true
    }
    
    if dep["name"] == "message_queue" && dep["status"] == "degraded" {
      message_queue_degraded = true
      assert_eq(dep["queue_depth"], 1500)
      assert_eq(dep["max_queue_depth"], 1000)
    }
    
    if dep["name"] == "external_api" && dep["status"] == "healthy" {
      external_api_healthy = true
    }
    
    if dep["name"] == "storage_bucket" && dep["status"] == "unhealthy" {
      storage_unhealthy = true
      assert_eq(dep["error_rate_percent"], 15.0)
    }
    
    i = i + 1
  }
  
  assert_eq(database_healthy, true)
  assert_eq(message_queue_degraded, true)
  assert_eq(external_api_healthy, true)
  assert_eq(storage_unhealthy, true)
}

test "telemetry_system_performance_trends" {
  // 测试遥测系统性能趋势分析
  
  let performance_trend_data = [
    {"timestamp": 1640995200000L, "cpu": 45.2, "memory": 1024, "response_time": 45},
    {"timestamp": 1640995260000L, "cpu": 47.8, "memory": 1056, "response_time": 48},
    {"timestamp": 1640995320000L, "cpu": 52.1, "memory": 1098, "response_time": 52},
    {"timestamp": 1640995380000L, "cpu": 55.6, "memory": 1142, "response_time": 58},
    {"timestamp": 1640995440000L, "cpu": 58.9, "memory": 1185, "response_time": 63},
    {"timestamp": 1640995500000L, "cpu": 62.3, "memory": 1228, "response_time": 71}
  ]
  
  // 计算性能趋势
  let calculate_performance_trends = fn(data: Array[Map[String, Any]]) -> Map[String, Any] {
    if data.length() < 2 {
      return {
        "trend_direction": "stable",
        "cpu_trend_percent": 0.0,
        "memory_trend_percent": 0.0,
        "response_time_trend_percent": 0.0
      }
    }
    
    let first_data = data[0]
    let last_data = data[data.length() - 1]
    
    // 计算CPU趋势
    let cpu_first = first_data["cpu"]
    let cpu_last = last_data["cpu"]
    let cpu_trend_percent = ((cpu_last - cpu_first) / cpu_first) * 100.0
    
    // 计算内存趋势
    let memory_first = first_data["memory"]
    let memory_last = last_data["memory"]
    let memory_trend_percent = ((memory_last - memory_first).to_double() / memory_first.to_double()) * 100.0
    
    // 计算响应时间趋势
    let response_first = first_data["response_time"]
    let response_last = last_data["response_time"]
    let response_time_trend_percent = ((response_last - response_first).to_double() / response_first.to_double()) * 100.0
    
    // 确定总体趋势方向
    let mut trend_direction = "stable"
    let avg_trend = (cpu_trend_percent + memory_trend_percent + response_time_trend_percent) / 3.0
    
    if avg_trend > 10.0 {
      trend_direction = "degrading"
    } else if avg_trend < -5.0 {
      trend_direction = "improving"
    }
    
    {
      "trend_direction": trend_direction,
      "cpu_trend_percent": cpu_trend_percent,
      "memory_trend_percent": memory_trend_percent,
      "response_time_trend_percent": response_time_trend_percent,
      "average_trend_percent": avg_trend
    }
  }
  
  let performance_trends = calculate_performance_trends(performance_trend_data)
  
  // 验证性能趋势计算
  assert_eq(performance_trends["trend_direction"], "degrading") // 性能在恶化
  
  // 验证具体趋势计算
  let cpu_first = 45.2
  let cpu_last = 62.3
  let expected_cpu_trend = ((cpu_last - cpu_first) / cpu_first) * 100.0
  assert_eq(performance_trends["cpu_trend_percent"], expected_cpu_trend)
  
  let memory_first = 1024
  let memory_last = 1228
  let expected_memory_trend = ((memory_last - memory_first).to_double() / memory_first.to_double()) * 100.0
  assert_eq(performance_trends["memory_trend_percent"], expected_memory_trend)
  
  let response_first = 45
  let response_last = 71
  let expected_response_trend = ((response_last - response_first).to_double() / response_first.to_double()) * 100.0
  assert_eq(performance_trends["response_time_trend_percent"], expected_response_trend)
  
  // 验证所有趋势都是正数（性能恶化）
  assert_eq(performance_trends["cpu_trend_percent"] > 0.0, true)
  assert_eq(performance_trends["memory_trend_percent"] > 0.0, true)
  assert_eq(performance_trends["response_time_trend_percent"] > 0.0, true)
}
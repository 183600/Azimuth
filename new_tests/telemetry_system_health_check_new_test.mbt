// 遥测系统健康检查测试用例

test "telemetry_system_component_health_check" {
  // 测试遥测系统组件健康检查
  
  // 创建系统健康检查器
  let health_checker = azimuth::telemetry::api::health::SystemHealthChecker::new()
    .with_check_interval(30000)  // 30秒检查间隔
    .with_timeout_threshold(5000)  // 5秒超时
    .with_component_checks([
      {
        "name": "telemetry-collector",
        "type": "service",
        "endpoint": "http://localhost:4317/health",
        "expected_status": "healthy",
        "critical": true
      },
      {
        "name": "elasticsearch-cluster",
        "type": "database",
        "endpoint": "http://elasticsearch:9200/_cluster/health",
        "expected_status": "green",
        "critical": true
      },
      {
        "name": "influxdb-timeseries",
        "type": "database",
        "endpoint": "http://influxdb:8086/health",
        "expected_status": "pass",
        "critical": false
      },
      {
        "name": "redis-cache",
        "type": "cache",
        "endpoint": "http://redis:6379/ping",
        "expected_status": "pong",
        "critical": false
      }
    ])
  
  // 模拟健康的组件响应
  let healthy_responses = [
    azimuth::telemetry::api::health::HealthCheckResult::new("telemetry-collector")
      .with_status("healthy")
      .with_response_time(120)
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now())
      .with_details({"version": "1.5.2", "uptime": "86400"}),
    
    azimuth::telemetry::api::health::HealthCheckResult::new("elasticsearch-cluster")
      .with_status("green")
      .with_response_time(85)
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now())
      .with_details({"nodes": 3, "shards": 12, "active_shards": 12}),
    
    azimuth::telemetry::api::health::HealthCheckResult::new("influxdb-timeseries")
      .with_status("pass")
      .with_response_time(45)
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now())
      .with_details({"version": "2.3.0", "databases": 5}),
    
    azimuth::telemetry::api::health::HealthCheckResult::new("redis-cache")
      .with_status("pong")
      .with_response_time(15)
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now())
      .with_details({"connected_clients": 25, "used_memory": "512MB"})
  ]
  
  // 执行健康检查
  let health_report = health_checker.execute_checks(healthy_responses)
  
  // 验证健康报告
  assert_eq(health_report.overall_status, "healthy")
  assert_eq(health_report.checked_components, 4)
  assert_eq(health_report.healthy_components, 4)
  assert_eq(health_report.unhealthy_components, 0)
  assert_eq(health_report.critical_failures, 0)
  
  // 验证组件详情
  let collector_health = health_report.component_health.find(|h| h.component_name == "telemetry-collector").unwrap()
  assert_eq(collector_health.status, "healthy")
  assert_eq(collector_health.response_time_ms, 120)
  assert_eq(collector_health.is_healthy, true)
  assert_eq(collector_health.is_critical, true)
  
  // 模拟部分组件故障
  let degraded_responses = [
    azimuth::telemetry::api::health::HealthCheckResult::new("telemetry-collector")
      .with_status("healthy")
      .with_response_time(120),
    
    azimuth::telemetry::api::health::HealthCheckResult::new("elasticsearch-cluster")
      .with_status("yellow")  // 降级状态
      .with_response_time(850),  // 响应慢
    
    azimuth::telemetry::api::health::HealthCheckResult::new("influxdb-timeseries")
      .with_status("fail")  // 故障
      .with_response_time(5000),  // 超时
    
    azimuth::telemetry::api::health::HealthCheckResult::new("redis-cache")
      .with_status("pong")
      .with_response_time(15)
  ]
  
  let degraded_report = health_checker.execute_checks(degraded_responses)
  assert_eq(degraded_report.overall_status, "degraded")
  assert_eq(degraded_report.healthy_components, 2)
  assert_eq(degraded_report.degraded_components, 1)  // elasticsearch
  assert_eq(degraded_report.unhealthy_components, 1)  // influxdb
  assert_eq(degraded_report.critical_failures, 1)    // elasticsearch是关键组件
}

test "telemetry_system_dependency_health_check" {
  // 测试遥测系统依赖健康检查
  
  // 创建依赖健康检查器
  let dependency_checker = azimuth::telemetry::api::health::DependencyHealthChecker::new()
    .with_external_dependencies([
      {
        "name": "kubernetes-api",
        "type": "api",
        "endpoint": "https://kubernetes.default.svc.cluster.local/api/v1",
        "authentication": "service_account",
        "critical": true
      },
      {
        "name": "prometheus-metrics",
        "type": "metrics_source",
        "endpoint": "http://prometheus:9090/api/v1/query",
        "authentication": "none",
        "critical": false
      },
      {
        "name": "jaeger-tracing",
        "type": "tracing_backend",
        "endpoint": "http://jaeger:16686/api/services",
        "authentication": "none",
        "critical": false
      }
    ])
    .with_internal_dependencies([
      {
        "name": "message_queue",
        "type": "queue",
        "connection_string": "amqp://rabbitmq:5672",
        "critical": true
      },
      {
        "name": "configuration_store",
        "type": "config",
        "connection_string": "consul://consul:8500",
        "critical": true
      }
    ])
  
  // 模拟依赖健康状态
  let dependency_status = [
    azimuth::telemetry::api::health::DependencyStatus::new("kubernetes-api")
      .with_status("healthy")
      .with_response_time(250)
      .with_availability(99.9)
      .with_last_check(azimuth::telemetry::api::time::Timestamp::now()),
    
    azimuth::telemetry::api::health::DependencyStatus::new("prometheus-metrics")
      .with_status("healthy")
      .with_response_time(180)
      .with_availability(98.5),
    
    azimuth::telemetry::api::health::DependencyStatus::new("jaeger-tracing")
      .with_status("degraded")
      .with_response_time(1500)  // 响应慢
      .with_availability(95.0),
    
    azimuth::telemetry::api::health::DependencyStatus::new("message_queue")
      .with_status("healthy")
      .with_response_time(35)
      .with_queue_depth(1250)
      .with_consumer_count(5),
    
    azimuth::telemetry::api::health::DependencyStatus::new("configuration_store")
      .with_status("healthy")
      .with_response_time(45)
      .with_connection_pool_size(10)
      .with_active_connections(3)
  ]
  
  // 检查依赖健康状态
  let dependency_report = dependency_checker.check_dependencies(dependency_status)
  
  // 验证依赖报告
  assert_eq(dependency_report.overall_dependency_health, "healthy")
  assert_eq(dependency_report.total_dependencies, 5)
  assert_eq(dependency_report.healthy_dependencies, 4)
  assert_eq(dependency_report.degraded_dependencies, 1)  // jaeger
  assert_eq(dependency_report.failed_dependencies, 0)
  
  // 验证关键依赖状态
  let critical_deps = dependency_report.critical_dependencies
  assert_eq(critical_deps.length(), 3)  // kubernetes-api, message_queue, configuration_store
  assert_eq(critical_deps.all(|d| d.status == "healthy"), true)
  
  // 验证外部依赖
  let external_deps = dependency_report.external_dependencies
  assert_eq(external_deps.length(), 3)
  let k8s_health = external_deps.find(|d| d.name == "kubernetes-api").unwrap()
  assert_eq(k8s_health.status, "healthy")
  assert_eq(k8s_health.availability_percentage, 99.9)
  
  // 验证内部依赖
  let internal_deps = dependency_report.internal_dependencies
  assert_eq(internal_deps.length(), 2)
  let queue_health = internal_deps.find(|d| d.name == "message_queue").unwrap()
  assert_eq(queue_health.status, "healthy")
  assert_eq(queue_health.queue_depth, 1250)
}

test "telemetry_system_performance_health_check" {
  // 测试遥测系统性能健康检查
  
  // 创建性能健康检查器
  let performance_checker = azimuth::telemetry::api::health::PerformanceHealthChecker::new()
    .with_performance_thresholds({
      "response_time_p50": {"warning": 100, "critical": 500},
      "response_time_p95": {"warning": 200, "critical": 1000},
      "response_time_p99": {"warning": 500, "critical": 2000},
      "throughput": {"warning": 1000, "critical": 500},
      "error_rate": {"warning": 1.0, "critical": 5.0},
      "cpu_usage": {"warning": 70.0, "critical": 90.0},
      "memory_usage": {"warning": 80.0, "critical": 95.0}
    })
    .with_benchmark_duration(300)  // 5分钟基准测试
  
  // 模拟性能指标
  let performance_metrics = azimuth::telemetry::api::health::PerformanceMetrics::new()
    .with_response_time_percentiles({
      "p50": 85.0,    // 85ms
      "p95": 180.0,   // 180ms
      "p99": 450.0    // 450ms
    })
    .with_throughput(2500.0)  // 2500 req/s
    .with_error_rate(0.5)     // 0.5%
    .with_cpu_usage(65.5)     // 65.5%
    .with_memory_usage(75.0)  // 75%
    .with_disk_io_rate(100.0) // 100 MB/s
    .with_network_io_rate(500.0) // 500 Mb/s
    .with_gc_pause_time(25.0) // 25ms GC暂停
    .with_thread_pool_utilization(60.0) // 60%线程池利用率
  
  // 执行性能健康检查
  let performance_report = performance_checker.check_performance(performance_metrics)
  
  // 验证性能报告
  assert_eq(performance_report.overall_performance_health, "healthy")
  assert_eq(performance_report.response_time_health, "healthy")
  assert_eq(performance_report.throughput_health, "healthy")
  assert_eq(performance_report.error_rate_health, "healthy")
  assert_eq(performance_report.resource_usage_health, "healthy")
  
  // 验证具体指标
  assert_eq(performance_report.response_time_p50_ms, 85.0)
  assert_eq(performance_report.response_time_p95_ms, 180.0)
  assert_eq(performance_report.response_time_p99_ms, 450.0)
  assert_eq(performance_report.throughput_rps, 2500.0)
  assert_eq(performance_report.error_rate_percentage, 0.5)
  
  // 模拟性能问题
  let degraded_performance = azimuth::telemetry::api::health::PerformanceMetrics::new()
    .with_response_time_percentiles({
      "p50": 250.0,   // 警告级别
      "p95": 800.0,   // 警告级别
      "p99": 1500.0   // 警告级别
    })
    .with_throughput(400.0)   // 临界级别
    .with_error_rate(3.0)     // 警告级别
    .with_cpu_usage(92.0)     // 临界级别
    .with_memory_usage(85.0)  // 警告级别
  
  let degraded_report = performance_checker.check_performance(degraded_performance)
  assert_eq(degraded_report.overall_performance_health, "critical")
  assert_eq(degraded_report.response_time_health, "warning")
  assert_eq(degraded_report.throughput_health, "critical")
  assert_eq(degraded_report.error_rate_health, "warning")
  assert_eq(degraded_report.resource_usage_health, "critical")
  
  // 性能趋势分析
  let historical_metrics = [
    azimuth::telemetry::api::health::PerformanceMetrics::new()
      .with_response_time_percentiles({"p95": 150.0})
      .with_throughput(3000.0)
      .with_error_rate(0.2),
    azimuth::telemetry::api::health::PerformanceMetrics::new()
      .with_response_time_percentiles({"p95": 165.0})
      .with_throughput(2800.0)
      .with_error_rate(0.3),
    azimuth::telemetry::api::health::PerformanceMetrics::new()
      .with_response_time_percentiles({"p95": 180.0})
      .with_throughput(2500.0)
      .with_error_rate(0.5)
  ]
  
  let trend_analysis = performance_checker.analyze_performance_trends(historical_metrics)
  assert_eq(trend_analysis.response_time_trend, "degrading")    // 响应时间增加
  assert_eq(trend_analysis.throughput_trend, "degrading")       // 吞吐量下降
  assert_eq(trend_analysis.error_rate_trend, "degrading")       // 错误率上升
  assert_eq(trend_analysis.overall_trend, "degrading")
}

test "telemetry_system_end_to_end_health_check" {
  // 测试遥测系统端到端健康检查
  
  // 创建端到端健康检查器
  let e2e_checker = azimuth::telemetry::api::health::EndToEndHealthChecker::new()
    .with_test_scenarios([
      {
        "name": "trace_ingestion_to_storage",
        "description": "测试从追踪数据接收到存储的完整流程",
        "steps": [
          {"service": "telemetry-collector", "action": "receive_trace"},
          {"service": "trace-processor", "action": "process_trace"},
          {"service": "elasticsearch-cluster", "action": "store_trace"}
        ],
        "expected_duration": 5000
      },
      {
        "name": "metrics_collection_to_query",
        "description": "测试从指标收集到查询的完整流程",
        "steps": [
          {"service": "metrics-collector", "action": "receive_metrics"},
          {"service": "metrics-aggregator", "action": "aggregate_metrics"},
          {"service": "influxdb-timeseries", "action": "store_metrics"},
          {"service": "query-service", "action": "query_metrics"}
        ],
        "expected_duration": 3000
      },
      {
        "name": "log_processing_to_indexing",
        "description": "测试从日志处理到索引的完整流程",
        "steps": [
          {"service": "log-collector", "action": "receive_logs"},
          {"service": "log-parser", "action": "parse_logs"},
          {"service": "log-enricher", "action": "enrich_logs"},
          {"service": "elasticsearch-cluster", "action": "index_logs"}
        ],
        "expected_duration": 8000
      }
    ])
  
  // 模拟端到端测试结果
  let e2e_test_results = [
    azimuth::telemetry::api::health::E2ETestResult::new("trace_ingestion_to_storage")
      .with_status("passed")
      .with_duration(3200)  // 3.2秒，小于5秒预期
      .with_step_results([
        {"step": "receive_trace", "status": "passed", "duration": 50},
        {"step": "process_trace", "status": "passed", "duration": 1500},
        {"step": "store_trace", "status": "passed", "duration": 1650}
      ])
      .with_test_data({
        "trace_id": "test-trace-12345",
        "span_count": 5,
        "data_size": "2KB"
      }),
    
    azimuth::telemetry::api::health::E2ETestResult::new("metrics_collection_to_query")
      .with_status("passed")
      .with_duration(2100)  // 2.1秒，小于3秒预期
      .with_step_results([
        {"step": "receive_metrics", "status": "passed", "duration": 30},
        {"step": "aggregate_metrics", "status": "passed", "duration": 800},
        {"step": "store_metrics", "status": "passed", "duration": 950},
        {"step": "query_metrics", "status": "passed", "duration": 320}
      ])
      .with_test_data({
        "metric_count": 100,
        "query_result_count": 95,
        "data_size": "5KB"
      }),
    
    azimuth::telemetry::api::health::E2ETestResult::new("log_processing_to_indexing")
      .with_status("failed")
      .with_duration(12000)  // 12秒，超过8秒预期
      .with_step_results([
        {"step": "receive_logs", "status": "passed", "duration": 100},
        {"step": "parse_logs", "status": "passed", "duration": 3500},
        {"step": "enrich_logs", "status": "failed", "duration": 8000},  // 失败
        {"step": "index_logs", "status": "skipped", "duration": 0}
      ])
      .with_error_message("Log enrichment service timeout")
      .with_test_data({
        "log_count": 1000,
        "processed_count": 1000,
        "indexed_count": 0
      })
  ]
  
  // 执行端到端健康检查
  let e2e_report = e2e_checker.execute_e2e_tests(e2e_test_results)
  
  // 验证端到端报告
  assert_eq(e2e_report.overall_e2e_health, "degraded")
  assert_eq(e2e_report.total_tests, 3)
  assert_eq(e2e_report.passed_tests, 2)
  assert_eq(e2e_report.failed_tests, 1)
  assert_eq(e2e_report.success_rate, 66.67)
  
  // 验证测试详情
  let trace_test = e2e_report.test_results.find(|t| t.test_name == "trace_ingestion_to_storage").unwrap()
  assert_eq(trace_test.status, "passed")
  assert_eq(trace_test.duration_ms, 3200)
  assert_eq(trace_test.within_expected_duration, true)
  
  let log_test = e2e_report.test_results.find(|t| t.test_name == "log_processing_to_indexing").unwrap()
  assert_eq(log_test.status, "failed")
  assert_eq(log_test.duration_ms, 12000)
  assert_eq(log_test.within_expected_duration, false)
  assert_eq(log_test.error_message.contains("timeout"), true)
  
  // 分析失败影响
  let impact_analysis = e2e_checker.analyze_failure_impact(e2e_test_results)
  assert_eq(impact_analysis.affected_services.length() > 0, true)
  assert_eq(impact_analysis.affected_services.contains("log-enricher"), true)
  assert_eq(impact_analysis.affected_functionalities.contains("log_indexing"), true)
  assert_eq(impact_analysis.severity_level, "medium")
}

test "telemetry_system_health_automation" {
  // 测试遥测系统健康自动化
  
  // 创建健康自动化管理器
  let health_automation = azimuth::telemetry::api::health::HealthAutomationManager::new()
    .with_auto_healing_enabled(true)
    .with_alerting_enabled(true)
    .with_automation_rules([
      {
        "name": "restart_unhealthy_service",
        "condition": "component.status == 'unhealthy' AND component.critical == true",
        "action": "restart_service",
        "cooldown_period": 300
      },
      {
        "name": "scale_up_on_high_load",
        "condition": "performance.cpu_usage > 80 OR performance.memory_usage > 85",
        "action": "scale_up",
        "max_executions_per_hour": 3
      },
      {
        "name": "clear_cache_on_degradation",
        "condition": "dependency.name == 'redis-cache' AND dependency.status == 'degraded'",
        "action": "clear_cache",
        "cooldown_period": 600
      }
    ])
    .with_notification_channels([
      {"type": "slack", "endpoint": "https://hooks.slack.com/...", "severity": "warning"},
      {"type": "email", "recipients": ["ops@example.com"], "severity": "critical"},
      {"type": "pagerduty", "service_key": "abc123", "severity": "critical"}
    ])
  
  // 模拟健康事件
  let health_events = [
    azimuth::telemetry::api::health::HealthEvent::new("telemetry-collector", "unhealthy")
      .with_event_type("component_failure")
      .with_severity("critical")
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now())
      .with_description("Telemetry collector not responding to health checks")
      .with_metrics({"response_time": 10000, "error_rate": 100}),
    
    azimuth::telemetry::api::health::HealthEvent::new("system", "performance_degradation")
      .with_event_type("performance_issue")
      .with_severity("warning")
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now())
      .with_description("High CPU usage detected")
      .with_metrics({"cpu_usage": 85.5, "memory_usage": 78.0}),
    
    azimuth::telemetry::api::health::HealthEvent::new("redis-cache", "degraded")
      .with_event_type("dependency_issue")
      .with_severity("warning")
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now())
      .with_description("Redis cache response time increased")
      .with_metrics({"response_time": 1500, "hit_rate": 85.0})
  ]
  
  // 处理健康事件
  let automation_results = health_automation.process_health_events(health_events)
  
  // 验证自动化结果
  assert_eq(automation_results.processed_events, 3)
  assert_eq(automation_results.automated_actions_triggered, 3)
  
  // 验证触发的自动化操作
  let restart_action = automation_results.actions.find(|a| a.rule_name == "restart_unhealthy_service").unwrap()
  assert_eq(restart_action.triggered, true)
  assert_eq(restart_action.target_service, "telemetry-collector")
  assert_eq(restart_action.status, "executed")
  
  let scale_action = automation_results.actions.find(|a| a.rule_name == "scale_up_on_high_load").unwrap()
  assert_eq(scale_action.triggered, true)
  assert_eq(scale_action.status, "executed")
  
  let cache_action = automation_results.actions.find(|a| a.rule_name == "clear_cache_on_degradation").unwrap()
  assert_eq(cache_action.triggered, true)
  assert_eq(cache_action.target_service, "redis-cache")
  assert_eq(cache_action.status, "executed")
  
  // 验证通知发送
  assert_eq(automation_results.notifications_sent, 2)  // critical和warning级别
  
  // 验证恢复操作
  let recovery_actions = health_automation.generate_recovery_plan(health_events)
  assert_eq(recovery_actions.length() > 0, true)
  
  let collector_recovery = recovery_actions.find(|r| r.component == "telemetry-collector").unwrap()
  assert_eq(collector_recovery.priority, "high")
  assert_eq(collector_recovery.actions.contains("restart_service"), true)
  assert_eq(collector_recovery.estimated_recovery_time > 0, true)
  
  // 健康趋势分析
  let historical_events = [
    azimuth::telemetry::api::health::HealthEvent::new("telemetry-collector", "healthy")
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().subtract_hours(2)),
    azimuth::telemetry::api::health::HealthEvent::new("telemetry-collector", "degraded")
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().subtract_hours(1)),
    azimuth::telemetry::api::health::HealthEvent::new("telemetry-collector", "unhealthy")
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now())
  ]
  
  let trend_analysis = health_automation.analyze_health_trends(historical_events)
  assert_eq(trend_analysis.health_trend, "degrading")
  assert_eq(trend_analysis.stability_score < 0.5, true)
  assert_eq(trend_analysis.predicted_next_failure > 0, true)
}
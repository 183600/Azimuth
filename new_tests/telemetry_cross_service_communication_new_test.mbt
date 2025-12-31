// 遥测跨服务通信测试用例

test "telemetry_cross_service_trace_propagation" {
  // 测试遥测跨服务追踪传播
  
  // 创建跨服务追踪传播器
  let trace_propagator = azimuth::telemetry::api::communication::TracePropagator::new()
    .with_propagation_format("w3c_trace_context")
    .with_inject_headers(["traceparent", "tracestate"])
    .with_extract_headers(["traceparent", "tracestate", "x-trace-id"])
    .with_baggage_propagation(true)
  
  // 创建原始追踪上下文
  let original_trace = azimuth::telemetry::api::trace::TraceContext::new()
    .with_trace_id("1234567890abcdef1234567890abcdef")
    .with_span_id("1234567890abcdef")
    .with_trace_flags(1)  // 采样标志
    .with_trace_state("vendor1=value1,vendor2=value2")
    .with_baggage_items({
      "user.id": "user123",
      "request.source": "mobile_app",
      "tenant.id": "tenant_456"
    })
  
  // 模拟服务A调用服务B
  let service_a = azimuth::telemetry::api::communication::ServiceNode::new("service-a")
  let service_b = azimuth::telemetry::api::communication::ServiceNode::new("service-b")
  
  // 服务A注入追踪上下文到HTTP请求头
  let http_request = azimuth::telemetry::api::communication::HttpRequest::new()
    .with_method("POST")
    .with_url("http://service-b/api/process")
    .with_headers({"Content-Type": "application/json"})
  
  let injected_request = trace_propagator.inject_trace_context(original_trace, http_request)
  
  // 验证注入的追踪头
  assert_eq(injected_request.headers.contains("traceparent"), true)
  assert_eq(injected_request.headers.contains("tracestate"), true)
  assert_eq(injected_request.headers.contains("baggage"), true)
  
  // 验证traceparent格式
  let traceparent = injected_request.headers.get("traceparent").unwrap()
  assert_eq(traceparent.contains("1234567890abcdef1234567890abcdef"), true)
  assert_eq(traceparent.contains("1234567890abcdef"), true)
  assert_eq(traceparent.contains("01"), true)  // 采样标志
  
  // 服务B提取追踪上下文
  let extracted_trace = trace_propagator.extract_trace_context(injected_request)
  
  // 验证提取的追踪上下文
  assert_eq(extracted_trace.trace_id, original_trace.trace_id)
  assert_eq(extracted_trace.span_id, original_trace.span_id)
  assert_eq(extracted_trace.trace_flags, original_trace.trace_flags)
  assert_eq(extracted_trace.trace_state, original_trace.trace_state)
  assert_eq(extracted_trace.baggage_items.get("user.id").unwrap(), "user123")
  assert_eq(extracted_trace.baggage_items.get("request.source").unwrap(), "mobile_app")
  assert_eq(extracted_trace.baggage_items.get("tenant.id").unwrap(), "tenant_456")
  
  // 服务B创建子span
  let child_span = azimuth::telemetry::api::trace::SpanBuilder::new()
    .with_parent_context(extracted_trace)
    .with_span_id("fedcba0987654321")
    .with_operation_name("process_data")
    .with_service_name("service-b")
    .build()
  
  // 验证父子关系
  assert_eq(child_span.parent_trace_id, original_trace.trace_id)
  assert_eq(child_span.parent_span_id, original_trace.span_id)
  assert_eq(child_span.trace_id, original_trace.trace_id)  // 同一个追踪ID
  assert_eq(child_span.span_id, "fedcba0987654321")       // 新的span ID
  
  // 验证传播的baggage
  assert_eq(child_span.baggage_items.get("user.id").unwrap(), "user123")
  assert_eq(child_span.baggage_items.get("request.source").unwrap(), "mobile_app")
  assert_eq(child_span.baggage_items.get("tenant.id").unwrap(), "tenant_456")
}

test "telemetry_cross_service_metrics_aggregation" {
  // 测试遥测跨服务指标聚合
  
  // 创建跨服务指标聚合器
  let metrics_aggregator = azimuth::telemetry::api::communication::CrossServiceMetricsAggregator::new()
    .with_aggregation_window(60000)  // 1分钟聚合窗口
    .with_aggregation_functions(["sum", "avg", "min", "max", "p95", "p99"])
    .with_dimension_propagation(["service_name", "operation_name", "tenant_id"])
    .with_global_labels({"environment": "production", "region": "us-west-2"})
  
  // 模拟不同服务的指标数据
  let service_a_metrics = [
    azimuth::telemetry::api::metrics::Metric::new("request_duration", 150.0, "ms")
      .with_labels({
        "service_name": "user-service",
        "operation_name": "authenticate",
        "tenant_id": "tenant_001",
        "status": "success"
      })
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now()),
    
    azimuth::telemetry::api::metrics::Metric::new("request_duration", 200.0, "ms")
      .with_labels({
        "service_name": "user-service",
        "operation_name": "authenticate",
        "tenant_id": "tenant_001",
        "status": "success"
      })
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().add_seconds(10)),
    
    azimuth::telemetry::api::metrics::Metric::new("request_count", 1.0, "count")
      .with_labels({
        "service_name": "user-service",
        "operation_name": "authenticate",
        "tenant_id": "tenant_001"
      })
  ]
  
  let service_b_metrics = [
    azimuth::telemetry::api::metrics::Metric::new("request_duration", 75.0, "ms")
      .with_labels({
        "service_name": "auth-service",
        "operation_name": "validate_credentials",
        "tenant_id": "tenant_001",
        "status": "success"
      })
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().add_seconds(5)),
    
    azimuth::telemetry::api::metrics::Metric::new("request_duration", 100.0, "ms")
      .with_labels({
        "service_name": "auth-service",
        "operation_name": "validate_credentials",
        "tenant_id": "tenant_001",
        "status": "success"
      })
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().add_seconds(15)),
    
    azimuth::telemetry::api::metrics::Metric::new("request_count", 1.0, "count")
      .with_labels({
        "service_name": "auth-service",
        "operation_name": "validate_credentials",
        "tenant_id": "tenant_001"
      })
  ]
  
  let service_c_metrics = [
    azimuth::telemetry::api::metrics::Metric::new("request_duration", 25.0, "ms")
      .with_labels({
        "service_name": "notification-service",
        "operation_name": "send_welcome_email",
        "tenant_id": "tenant_001",
        "status": "success"
      })
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().add_seconds(20)),
    
    azimuth::telemetry::api::metrics::Metric::new("request_count", 1.0, "count")
      .with_labels({
        "service_name": "notification-service",
        "operation_name": "send_welcome_email",
        "tenant_id": "tenant_001"
      })
  ]
  
  // 聚合所有服务的指标
  let all_metrics = service_a_metrics + service_b_metrics + service_c_metrics
  let aggregated_metrics = metrics_aggregator.aggregate_metrics(all_metrics)
  
  // 验证聚合结果
  assert_eq(aggregated_metrics.length() > 0, true)
  
  // 验证请求持续时间聚合
  let duration_aggregations = aggregated_metrics.filter(|m| m.name == "request_duration_aggregated")
  assert_eq(duration_aggregations.length() > 0, true)
  
  // 验证跨服务总请求时间
  let total_duration = duration_aggregations.find(|m| 
    m.labels.get("aggregation").unwrap() == "sum" &&
    m.labels.get("tenant_id").unwrap() == "tenant_001"
  ).unwrap()
  assert_eq(total_duration.value, 550.0)  // 150 + 200 + 75 + 100 + 25
  
  // 验证跨服务平均请求时间
  let avg_duration = duration_aggregations.find(|m| 
    m.labels.get("aggregation").unwrap() == "avg" &&
    m.labels.get("tenant_id").unwrap() == "tenant_001"
  ).unwrap()
  assert_eq(avg_duration.value, 110.0)  // 550 / 5
  
  // 验证请求计数聚合
  let count_aggregations = aggregated_metrics.filter(|m| m.name == "request_count_aggregated")
  let total_count = count_aggregations.find(|m| 
    m.labels.get("aggregation").unwrap() == "sum" &&
    m.labels.get("tenant_id").unwrap() == "tenant_001"
  ).unwrap()
  assert_eq(total_count.value, 3.0)  // 3个请求
  
  // 生成跨服务性能报告
  let performance_report = metrics_aggregator.generate_cross_service_report(aggregated_metrics)
  assert_eq(performance_report.total_services, 3)
  assert_eq(performance_report.total_requests, 3)
  assert_eq(performance_report.overall_avg_duration_ms, 110.0)
  assert_eq(performance_report.overall_total_duration_ms, 550.0)
  
  // 验证服务级别统计
  let user_service_stats = performance_report.service_stats.find(|s| s.service_name == "user-service").unwrap()
  assert_eq(user_service_stats.request_count, 2)
  assert_eq(user_service_stats.avg_duration_ms, 175.0)  // (150 + 200) / 2
  
  let auth_service_stats = performance_report.service_stats.find(|s| s.service_name == "auth-service").unwrap()
  assert_eq(auth_service_stats.request_count, 2)
  assert_eq(auth_service_stats.avg_duration_ms, 87.5)   // (75 + 100) / 2
}

test "telemetry_cross_service_log_correlation" {
  // 测试遥测跨服务日志关联
  
  // 创建跨服务日志关联器
  let log_correlator = azimuth::telemetry::api::communication::LogCorrelator::new()
    .with_correlation_fields(["trace_id", "span_id", "correlation_id"])
    .with_enrichment_fields(["service_name", "operation_name", "user_id"])
    .with_log_level_mapping(["DEBUG", "INFO", "WARN", "ERROR", "FATAL"])
    .with_timeline_reconstruction(true)
  
  // 模拟跨服务日志事件
  let log_events = [
    // 服务A的日志
    azimuth::telemetry::api::logs::LogEvent::new()
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now())
      .with_level("INFO")
      .with_service("user-service")
      .with_message("Authentication request received")
      .with_fields({
        "trace_id": "trace-12345",
        "span_id": "span-001",
        "correlation_id": "corr-abc-123",
        "user_id": "user123",
        "operation_name": "authenticate"
      }),
    
    azimuth::telemetry::api::logs::LogEvent::new()
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().add_millis(50))
      .with_level("DEBUG")
      .with_service("user-service")
      .with_message("Validating user credentials")
      .with_fields({
        "trace_id": "trace-12345",
        "span_id": "span-001",
        "correlation_id": "corr-abc-123",
        "user_id": "user123"
      }),
    
    // 服务B的日志
    azimuth::telemetry::api::logs::LogEvent::new()
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().add_millis(100))
      .with_level("INFO")
      .with_service("auth-service")
      .with_message("Credential validation request")
      .with_fields({
        "trace_id": "trace-12345",
        "span_id": "span-002",
        "parent_span_id": "span-001",
        "correlation_id": "corr-abc-123",
        "user_id": "user123",
        "operation_name": "validate_credentials"
      }),
    
    azimuth::telemetry::api::logs::LogEvent::new()
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().add_millis(150))
      .with_level("WARN")
      .with_service("auth-service")
      .with_message("Slow database query detected")
      .with_fields({
        "trace_id": "trace-12345",
        "span_id": "span-002",
        "correlation_id": "corr-abc-123",
        "query_duration_ms": "850"
      }),
    
    // 服务A的响应日志
    azimuth::telemetry::api::logs::LogEvent::new()
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().add_millis(200))
      .with_level("INFO")
      .with_service("user-service")
      .with_message("Authentication completed successfully")
      .with_fields({
        "trace_id": "trace-12345",
        "span_id": "span-001",
        "correlation_id": "corr-abc-123",
        "user_id": "user123",
        "total_duration_ms": "200"
      }),
    
    // 服务C的日志
    azimuth::telemetry::api::logs::LogEvent::new()
      .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().add_millis(250))
      .with_level("INFO")
      .with_service("notification-service")
      .with_message("Sending welcome email")
      .with_fields({
        "trace_id": "trace-12345",
        "span_id": "span-003",
        "correlation_id": "corr-abc-123",
        "user_id": "user123",
        "operation_name": "send_welcome_email"
      })
  ]
  
  // 按追踪ID关联日志
  let correlated_logs = log_correlator.correlate_by_trace_id(log_events, "trace-12345")
  
  // 验证关联结果
  assert_eq(correlated_logs.length(), 6)
  assert_eq(correlated_logs.all(|log| log.fields.get("trace_id").unwrap() == "trace-12345"), true)
  
  // 按关联ID关联日志
  let correlation_logs = log_correlator.correlate_by_correlation_id(log_events, "corr-abc-123")
  assert_eq(correlation_logs.length(), 6)
  
  // 构建时间线
  let timeline = log_correlator.build_timeline(correlated_logs)
  assert_eq(timeline.events.length(), 6)
  assert_eq(timeline.events[0].message, "Authentication request received")
  assert_eq(timeline.events[5].message, "Sending welcome email")
  
  // 验证时间线顺序
  for i = 1; i < timeline.events.length(); i = i + 1 {
    assert_eq(timeline.events[i].timestamp >= timeline.events[i-1].timestamp, true)
  }
  
  // 分析跨服务日志模式
  let log_analysis = log_correlator.analyze_cross_service_patterns(correlated_logs)
  assert_eq(log_analysis.involved_services.length(), 3)
  assert_eq(log_analysis.involved_services.contains("user-service"), true)
  assert_eq(log_analysis.involved_services.contains("auth-service"), true)
  assert_eq(log_analysis.involved_services.contains("notification-service"), true)
  
  // 验证服务间调用链
  let call_chain = log_correlator.extract_call_chain(correlated_logs)
  assert_eq(call_chain.length(), 3)
  assert_eq(call_chain[0].service, "user-service")
  assert_eq(call_chain[1].service, "auth-service")
  assert_eq(call_chain[2].service, "notification-service")
  
  // 验证错误和警告检测
  let issues = log_correlator.detect_issues(correlated_logs)
  assert_eq(issues.length(), 1)  // 一个警告
  assert_eq(issues[0].level, "WARN")
  assert_eq(issues[0].service, "auth-service")
  assert_eq(issues[0].message.contains("Slow database query"), true)
}

test "telemetry_cross_service_error_propagation" {
  // 测试遥测跨服务错误传播
  
  // 创建跨服务错误传播器
  let error_propagator = azimuth::telemetry::api::communication::ErrorPropagator::new()
    .with_error_classification(["client_error", "server_error", "timeout", "rate_limit"])
    .with_propagation_strategy("detailed")  // 详细错误信息传播
    .with_error_context_enrichment(true)
    .with_causality_tracking(true)
  
  // 模拟服务调用链中的错误
  let root_error = azimuth::telemetry::api::errors::ErrorContext::new()
    .with_error_id("err-001")
    .with_error_type("database_connection_failed")
    .with_error_category("server_error")
    .with_service("database-service")
    .with_message("Failed to connect to primary database")
    .with_timestamp(azimuth::telemetry::api::time::Timestamp::now())
    .with_details({
      "database_host": "db-primary.example.com",
      "port": "5432",
      "connection_timeout": "30s",
      "retry_count": "3"
    })
    .with_stack_trace([
      "database_service.connect() at line 45",
      "database_service.execute_query() at line 123",
      "user_repository.find_by_id() at line 67"
    ])
  
  // 上游服务传播错误
  let upstream_error = error_propagator.propagate_error(root_error, "user-service")
    .with_operation("get_user_profile")
    .with_context({
      "user_id": "user123",
      "request_id": "req-456"
    })
  
  // 验证错误传播
  assert_eq(upstream_error.root_error_id, "err-001")
  assert_eq(upstream_error.error_type, "database_connection_failed")
  assert_eq(upstream_error.propagating_service, "user-service")
  assert_eq(upstream_error.operation_name, "get_user_profile")
  assert_eq(upstream_error.context.get("user_id").unwrap(), "user123")
  
  // 进一步传播到API网关
  let gateway_error = error_propagator.propagate_error(upstream_error, "api-gateway")
    .with_operation("http_request")
    .with_context({
      "http_method": "GET",
      "endpoint": "/api/users/user123/profile",
      "client_ip": "192.168.1.100"
    })
    .with_client_safe_message("Unable to fetch user profile at this time")
  
  // 验证网关级别的错误处理
  assert_eq(gateway_error.root_error_id, "err-001")
  assert_eq(gateway_error.propagating_service, "api-gateway")
  assert_eq(gateway_error.client_safe_message.contains("Unable to fetch user profile"), true)
  assert_eq(gateway_error.context.get("client_ip").unwrap(), "192.168.1.100")
  
  // 构建错误因果链
  let causal_chain = error_propagator.build_causal_chain(gateway_error)
  assert_eq(causal_chain.length(), 3)
  assert_eq(causal_chain[0].service, "database-service")
  assert_eq(causal_chain[1].service, "user-service")
  assert_eq(causal_chain[2].service, "api-gateway")
  
  // 验证因果链顺序
  for i = 1; i < causal_chain.length(); i = i + 1 {
    assert_eq(causal_chain[i].timestamp >= causal_chain[i-1].timestamp, true)
    assert_eq(causal_chain[i].parent_error_id, causal_chain[i-1].error_id)
  }
  
  // 错误统计分析
  let error_stats = error_propagator.analyze_error_patterns([gateway_error])
  assert_eq(error_stats.total_errors, 1)
  assert_eq(error_stats.error_categories.get("server_error").unwrap(), 1)
  assert_eq(error_stats.affected_services.length(), 3)
  
  // 生成错误报告
  let error_report = error_propagator.generate_error_report([gateway_error])
  assert_eq(error_report.summary.total_errors, 1)
  assert_eq(error_report.summary.critical_errors, 1)
  assert_eq(error_report.summary.affected_users.length(), 1)
  assert_eq(error_report.recommendations.length() > 0, true)
}

test "telemetry_cross_service_protocol_compatibility" {
  // 测试遥测跨服务协议兼容性
  
  // 创建协议兼容性管理器
  let compatibility_manager = azimuth::telemetry::api::communication::ProtocolCompatibilityManager::new()
    .with_supported_protocols(["http/1.1", "http/2", "grpc", "websocket"])
    .with_telemetry_formats(["opentelemetry", "jaeger", "zipkin", "prometheus"])
    .with_serialization_formats(["json", "protobuf", "msgpack"])
    .with_version_compatibility_matrix({
      "user-service": {
        "min_version": "1.2.0",
        "max_version": "2.0.0",
        "preferred_version": "1.8.0"
      },
      "auth-service": {
        "min_version": "2.1.0",
        "max_version": "3.0.0",
        "preferred_version": "2.5.0"
      },
      "notification-service": {
        "min_version": "1.0.0",
        "max_version": "1.5.0",
        "preferred_version": "1.3.0"
      }
    })
  
  // 模拟服务注册表
  let service_registry = [
    azimuth::telemetry::api::communication::ServiceInstance::new("user-service")
      .with_version("1.8.2")
      .with_protocols(["http/2", "grpc"])
      .with_telemetry_formats(["opentelemetry", "jaeger"])
      .with_serialization_formats(["json", "protobuf"])
      .with_endpoints([
        "http://user-service:8080",
        "grpc://user-service:9090"
      ]),
    
    azimuth::telemetry::api::communication::ServiceInstance::new("auth-service")
      .with_version("2.5.1")
      .with_protocols(["http/1.1", "grpc"])
      .with_telemetry_formats(["opentelemetry", "prometheus"])
      .with_serialization_formats(["json", "protobuf"])
      .with_endpoints([
        "http://auth-service:8080",
        "grpc://auth-service:9090"
      ]),
    
    azimuth::telemetry::api::communication::ServiceInstance::new("notification-service")
      .with_version("1.3.0")
      .with_protocols(["http/1.1"])
      .with_telemetry_formats(["jaeger"])
      .with_serialization_formats(["json"])
      .with_endpoints([
        "http://notification-service:8080"
      ])
  ]
  
  // 检查服务间兼容性
  let compatibility_matrix = compatibility_manager.check_compatibility_matrix(service_registry)
  
  // 验证兼容性矩阵
  assert_eq(compatibility_matrix.total_services, 3)
  assert_eq(compatibility_matrix.compatibility_pairs.length(), 6)  // 3个服务的排列组合
  
  // 验证user-service和auth-service的兼容性
  let user_auth_compat = compatibility_matrix.compatibility_pairs.find(|p| 
    (p.service_a == "user-service" && p.service_b == "auth-service") ||
    (p.service_a == "auth-service" && p.service_b == "user-service")
  ).unwrap()
  
  assert_eq(user_auth_compat.protocol_compatible, true)   // 都支持grpc
  assert_eq(user_auth_compat.telemetry_compatible, true)  // 都支持opentelemetry
  assert_eq(user_auth_compat.serialization_compatible, true)  // 都支持json和protobuf
  assert_eq(user_auth_compat.version_compatible, true)   // 版本在兼容范围内
  assert_eq(user_auth_compat.overall_compatible, true)
  
  // 验证user-service和notification-service的兼容性
  let user_notification_compat = compatibility_matrix.compatibility_pairs.find(|p| 
    (p.service_a == "user-service" && p.service_b == "notification-service") ||
    (p.service_a == "notification-service" && p.service_b == "user-service")
  ).unwrap()
  
  assert_eq(user_notification_compat.protocol_compatible, true)   // user-service支持http/2，notification-service支持http/1.1
  assert_eq(user_notification_compat.telemetry_compatible, true)  // 都支持jaeger
  assert_eq(user_notification_compat.serialization_compatible, true)  // 都支持json
  assert_eq(user_notification_compat.version_compatible, true)
  assert_eq(user_notification_compat.overall_compatible, true)
  
  // 协议协商
  let negotiation_result = compatibility_manager.negotiate_protocol("user-service", "auth-service", service_registry)
  assert_eq(negotiation_result.success, true)
  assert_eq(negotiation_result.selected_protocol, "grpc")  // 两个服务都支持的最高性能协议
  assert_eq(negotiation_result.selected_telemetry_format, "opentelemetry")
  assert_eq(negotiation_result.selected_serialization, "protobuf")  // 比json更高效
  
  // 版本兼容性检查
  let version_check = compatibility_manager.check_version_compatibility("user-service", "1.9.0")
  assert_eq(version_check.compatible, true)
  assert_eq(version_check.preferred_upgrade, "2.0.0")
  
  let version_check_old = compatibility_manager.check_version_compatibility("user-service", "1.1.0")
  assert_eq(version_check_old.compatible, false)  // 低于最小版本
  assert_eq(version_check_old.required_upgrade, "1.2.0")
  
  // 生成兼容性报告
  let compatibility_report = compatibility_manager.generate_compatibility_report(compatibility_matrix)
  assert_eq(compatibility_report.overall_health, "healthy")
  assert_eq(compatibility_report.compatible_percentage, 100.0)
  assert_eq(compatibility_report.recommendations.length(), 0)  // 没有不兼容问题
  
  // 模拟版本升级场景
  let upgraded_service = azimuth::telemetry::api::communication::ServiceInstance::new("auth-service")
    .with_version("3.1.0")  // 超过最大版本
    .with_protocols(["http/1.1", "grpc", "http/3"])  // 新增http/3支持
    .with_telemetry_formats(["opentelemetry", "prometheus", "tempo"])  // 新增tempo支持
  
  let upgraded_registry = service_registry.map(|s| 
    if s.service_name == "auth-service" { upgraded_service } else { s }
  )
  
  let upgraded_compatibility = compatibility_manager.check_compatibility_matrix(upgraded_registry)
  let upgraded_report = compatibility_manager.generate_compatibility_report(upgraded_compatibility)
  
  // 验证升级后的兼容性
  assert_eq(upgraded_report.overall_health, "warning")  // 版本超出范围
  assert_eq(upgraded_report.compatible_percentage < 100.0, true)
  assert_eq(upgraded_report.recommendations.length() > 0, true)
  assert_eq(upgraded_report.recommendations.any(|r| r.contains("version compatibility")), true)
}
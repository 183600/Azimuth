// 跨服务遥测集成测试
// 测试Azimuth遥测系统在微服务架构中的跨服务集成功能

test "微服务端到端追踪集成" {
  // 模拟微服务架构：API Gateway -> Auth Service -> User Service -> Order Service
  let services = ["api-gateway", "auth-service", "user-service", "order-service"]
  let propagator = HttpTraceContextPropagator::new()
  
  // 在API Gateway创建初始追踪
  let gateway_tracer = TracerProvider::default().get_tracer("api-gateway")
  let gateway_span = gateway_tracer.start_span("api.request.process")
  Span::set_attribute(gateway_span, "service.name", "api-gateway")
  Span::set_attribute(gateway_span, "http.method", "POST")
  Span::set_attribute(gateway_span, "http.url", "/api/v1/orders")
  Span::set_attribute(gateway_span, "user.id", "user-12345")
  
  let gateway_ctx = Span::span_context(gateway_span)
  
  // 传播到Auth Service
  let gateway_carrier = HttpHeaderCarrier::new()
  propagator.inject(gateway_ctx, gateway_carrier)
  
  let auth_carrier = HttpHeaderCarrier::from_map(gateway_carrier.to_map())
  let auth_ctx = propagator.extract(auth_carrier)
  
  let auth_tracer = TracerProvider::default().get_tracer("auth-service")
  let auth_span = auth_tracer.start_span_with_context("authenticate.request", auth_ctx)
  Span::set_attribute(auth_span, "service.name", "auth-service")
  Span::set_attribute(auth_span, "operation.name", "validate_token")
  Span::set_attribute(auth_span, "token.type", "JWT")
  Span::set_attribute(auth_span, "auth.result", "success")
  
  // 传播到User Service
  let auth_carrier_out = HttpHeaderCarrier::new()
  propagator.inject(Span::span_context(auth_span), auth_carrier_out)
  
  let user_carrier = HttpHeaderCarrier::from_map(auth_carrier_out.to_map())
  let user_ctx = propagator.extract(user_carrier)
  
  let user_tracer = TracerProvider::default().get_tracer("user-service")
  let user_span = user_tracer.start_span_with_context("user.profile.fetch", user_ctx)
  Span::set_attribute(user_span, "service.name", "user-service")
  Span::set_attribute(user_span, "operation.name", "get_user_profile")
  Span::set_attribute(user_span, "user.id", "user-12345")
  Span::set_attribute(user_span, "profile.found", "true")
  
  // 传播到Order Service
  let user_carrier_out = HttpHeaderCarrier::new()
  propagator.inject(Span::span_context(user_span), user_carrier_out)
  
  let order_carrier = HttpHeaderCarrier::from_map(user_carrier_out.to_map())
  let order_ctx = propagator.extract(order_carrier)
  
  let order_tracer = TracerProvider::default().get_tracer("order-service")
  let order_span = order_tracer.start_span_with_context("order.creation", order_ctx)
  Span::set_attribute(order_span, "service.name", "order-service")
  Span::set_attribute(order_span, "operation.name", "create_order")
  Span::set_attribute(order_span, "order.id", "order-67890")
  Span::set_attribute(order_span, "order.amount", "299.99")
  
  // 验证所有span都在同一个追踪中
  let trace_ids = [
    SpanContext::trace_id(Span::span_context(gateway_span)),
    SpanContext::trace_id(Span::span_context(auth_span)),
    SpanContext::trace_id(Span::span_context(user_span)),
    SpanContext::trace_id(Span::span_context(order_span))
  ]
  
  for trace_id in trace_ids {
    assert_eq(trace_id, trace_ids.get(0))
  }
  
  // 验证父子关系
  let gateway_span_id = SpanContext::span_id(Span::span_context(gateway_span))
  let auth_parent_id = Span::get_parent_span_id(auth_span)
  let user_parent_id = Span::get_parent_span_id(user_span)
  let order_parent_id = Span::get_parent_span_id(order_span)
  
  assert_eq(auth_parent_id, Some(gateway_span_id))
  assert_eq(user_parent_id, Some(SpanContext::span_id(Span::span_context(auth_span))))
  assert_eq(order_parent_id, Some(SpanContext::span_id(Span::span_context(user_span))))
  
  Span::end(order_span)
  Span::end(user_span)
  Span::end(auth_span)
  Span::end(gateway_span)
}

test "跨服务度量数据一致性" {
  // 测试跨服务度量数据的一致性和聚合
  let services = ["web-frontend", "api-gateway", "user-service", "order-service", "notification-service"]
  
  // 为每个服务创建度量
  let service_metrics = []
  
  for service in services {
    let meter_provider = MeterProvider::default()
    let meter = MeterProvider::get_meter(meter_provider, service)
    
    let request_counter = Meter::create_counter(meter, "requests.total", Some("Total requests"), Some("count"))
    let response_time_histogram = Meter::create_histogram(meter, "response.time", Some("Response time"), Some("ms"))
    let error_counter = Meter::create_counter(meter, "errors.total", Some("Total errors"), Some("count"))
    
    // 模拟不同服务的请求模式
    match service {
      "web-frontend" => {
        Counter::add_with_attributes(request_counter, 1000.0, [
          ("service", service),
          ("endpoint", "/"),
          ("status", "200")
        ])
        Counter::add_with_attributes(error_counter, 10.0, [
          ("service", service),
          ("error.type", "client_error")
        ])
        
        for i in 1..=1000 {
          let response_time = 50.0 + Math::random() * 100.0
          Histogram::record_with_attributes(response_time_histogram, response_time, [
            ("service", service),
            ("endpoint", "/")
          ])
        }
      }
      "api-gateway" => {
        Counter::add_with_attributes(request_counter, 800.0, [
          ("service", service),
          ("endpoint", "/api/*"),
          ("status", "200")
        ])
        Counter::add_with_attributes(request_counter, 20.0, [
          ("service", service),
          ("endpoint", "/api/*"),
          ("status", "4xx")
        ])
        Counter::add_with_attributes(error_counter, 5.0, [
          ("service", service),
          ("error.type", "rate_limit")
        ])
        
        for i in 1..=820 {
          let response_time = 30.0 + Math::random() * 80.0
          Histogram::record_with_attributes(response_time_histogram, response_time, [
            ("service", service),
            ("endpoint", "/api/*")
          ])
        }
      }
      "user-service" => {
        Counter::add_with_attributes(request_counter, 300.0, [
          ("service", service),
          ("endpoint", "/api/users"),
          ("status", "200")
        ])
        Counter::add_with_attributes(error_counter, 3.0, [
          ("service", service),
          ("error.type", "database_error")
        ])
        
        for i in 1..=300 {
          let response_time = 100.0 + Math::random() * 200.0
          Histogram::record_with_attributes(response_time_histogram, response_time, [
            ("service", service),
            ("endpoint", "/api/users")
          ])
        }
      }
      "order-service" => {
        Counter::add_with_attributes(request_counter, 150.0, [
          ("service", service),
          ("endpoint", "/api/orders"),
          ("status", "200")
        ])
        Counter::add_with_attributes(request_counter, 5.0, [
          ("service", service),
          ("endpoint", "/api/orders"),
          ("status", "500")
        ])
        Counter::add_with_attributes(error_counter, 2.0, [
          ("service", service),
          ("error.type", "payment_error")
        ])
        
        for i in 1..=155 {
          let response_time = 200.0 + Math::random() * 300.0
          Histogram::record_with_attributes(response_time_histogram, response_time, [
            ("service", service),
            ("endpoint", "/api/orders")
          ])
        }
      }
      "notification-service" => {
        Counter::add_with_attributes(request_counter, 200.0, [
          ("service", service),
          ("endpoint", "/api/notifications"),
          ("status", "200")
        ])
        
        for i in 1..=200 {
          let response_time = 80.0 + Math::random() * 120.0
          Histogram::record_with_attributes(response_time_histogram, response_time, [
            ("service", service),
            ("endpoint", "/api/notifications")
          ])
        }
      }
    }
    
    service_metrics = service_metrics.push((service, request_counter, response_time_histogram, error_counter))
  }
  
  // 聚合跨服务度量
  let cross_service_aggregator = CrossServiceMetricsAggregator::new()
  
  for (service, counter, histogram, error_counter) in service_metrics {
    cross_service_aggregator.add_service_metrics(service, counter, histogram, error_counter)
  }
  
  // 验证全局请求计数
  let global_requests = cross_service_aggregator.get_global_request_count()
  assert_eq(global_requests, 2470.0)  // 1000 + 820 + 300 + 155 + 200
  
  // 验证全局错误计数
  let global_errors = cross_service_aggregator.get_global_error_count()
  assert_eq(global_errors, 20.0)  // 10 + 5 + 3 + 2 + 0
  
  // 验证错误率
  let global_error_rate = cross_service_aggregator.get_global_error_rate()
  let expected_error_rate = 20.0 / 2470.0
  assert_true(Math::abs(global_error_rate - expected_error_rate) < 0.001)
  
  // 验证服务级别的响应时间分布
  let service_response_times = cross_service_aggregator.get_service_response_times()
  
  for (service, stats) in service_response_times {
    let mean = stats.get_mean()
    let p95 = stats.get_percentile(95.0)
    
    assert_true(mean > 0.0)
    assert_true(p95 >= mean)
    
    // 验证响应时间符合预期模式
    match service {
      "web-frontend" => assert_true(mean < 150.0),
      "api-gateway" => assert_true(mean < 110.0),
      "user-service" => assert_true(mean > 150.0),
      "order-service" => assert_true(mean > 300.0),
      "notification-service" => assert_true(mean < 200.0),
    }
  }
}

test "跨服务日志关联和集成" {
  // 测试跨服务日志的关联和集成
  let services = ["auth-service", "order-service", "payment-service", "notification-service"]
  let propagator = HttpTraceContextPropagator::new()
  let baggage_propagator = BaggagePropagator::new()
  
  // 创建初始追踪上下文和baggage
  let auth_tracer = TracerProvider::default().get_tracer("auth-service")
  let auth_span = auth_tracer.start_span("user.authentication")
  
  let auth_ctx = Span::span_context(auth_span)
  let initial_baggage = Baggage::new()
  let enriched_baggage = Baggage::set_entry(initial_baggage, "user.id", "user-12345")
  let enriched_baggage = Baggage::set_entry(enriched_baggage, "request.id", "req-abc123")
  let enriched_baggage = Baggage::set_entry(enriched_baggage, "session.id", "sess-xyz789")
  
  // 为每个服务创建关联的日志记录
  let service_logs = []
  
  for service in services {
    let logger_provider = LoggerProvider::default()
    let logger = LoggerProvider::get_logger(logger_provider, service)
    
    // 模拟传播上下文和baggage
    let carrier = HttpHeaderCarrier::new()
    propagator.inject(auth_ctx, carrier)
    baggage_propagator.inject(auth_ctx, enriched_baggage, carrier)
    
    let receiving_carrier = HttpHeaderCarrier::from_map(carrier.to_map())
    let extracted_ctx = propagator.extract(receiving_carrier)
    let extracted_baggage = baggage_propagator.extract(auth_ctx, receiving_carrier)
    
    // 创建与服务相关的日志记录
    match service {
      "auth-service" => {
        let auth_log = LogRecord::new(Info, "User authentication successful")
        LogRecord::add_attribute(auth_log, "service.name", service)
        LogRecord::add_attribute(auth_log, "operation", "authenticate")
        LogRecord::add_attribute(auth_log, "user.id", "user-12345")
        LogRecord::add_attribute(auth_log, "auth.method", "JWT")
        LogRecord::add_attribute(auth_log, "trace.id", SpanContext::trace_id(extracted_ctx))
        LogRecord::add_attribute(auth_log, "span.id", SpanContext::span_id(extracted_ctx))
        
        service_logs = service_logs.push((service, auth_log))
        
        let auth_error_log = LogRecord::new(Warn, "Token expiration warning")
        LogRecord::add_attribute(auth_error_log, "service.name", service)
        LogRecord::add_attribute(auth_error_log, "operation", "token_validation")
        LogRecord::add_attribute(auth_error_log, "token.expires_in", "300")
        LogRecord::add_attribute(auth_error_log, "trace.id", SpanContext::trace_id(extracted_ctx))
        LogRecord::add_attribute(auth_error_log, "span.id", SpanContext::span_id(extracted_ctx))
        
        service_logs = service_logs.push((service, auth_error_log))
      }
      "order-service" => {
        let order_log = LogRecord::new(Info, "Order created successfully")
        LogRecord::add_attribute(order_log, "service.name", service)
        LogRecord::add_attribute(order_log, "operation", "create_order")
        LogRecord::add_attribute(order_log, "order.id", "order-45678")
        LogRecord::add_attribute(order_log, "order.amount", "199.99")
        LogRecord::add_attribute(order_log, "user.id", Baggage::get_value(extracted_baggage, "user.id").unwrap_or(""))
        LogRecord::add_attribute(order_log, "request.id", Baggage::get_value(extracted_baggage, "request.id").unwrap_or(""))
        LogRecord::add_attribute(order_log, "trace.id", SpanContext::trace_id(extracted_ctx))
        LogRecord::add_attribute(order_log, "span.id", SpanContext::span_id(extracted_ctx))
        
        service_logs = service_logs.push((service, order_log))
      }
      "payment-service" => {
        let payment_log = LogRecord::new(Info, "Payment processed")
        LogRecord::add_attribute(payment_log, "service.name", service)
        LogRecord::add_attribute(payment_log, "operation", "process_payment")
        LogRecord::add_attribute(payment_log, "payment.id", "pay-98765")
        LogRecord::add_attribute(payment_log, "payment.amount", "199.99")
        LogRecord::add_attribute(payment_log, "payment.method", "credit_card")
        LogRecord::add_attribute(payment_log, "user.id", Baggage::get_value(extracted_baggage, "user.id").unwrap_or(""))
        LogRecord::add_attribute(payment_log, "session.id", Baggage::get_value(extracted_baggage, "session.id").unwrap_or(""))
        LogRecord::add_attribute(payment_log, "trace.id", SpanContext::trace_id(extracted_ctx))
        LogRecord::add_attribute(payment_log, "span.id", SpanContext::span_id(extracted_ctx))
        
        service_logs = service_logs.push((service, payment_log))
        
        let payment_error_log = LogRecord::new(Error, "Payment retry attempt")
        LogRecord::add_attribute(payment_error_log, "service.name", service)
        LogRecord::add_attribute(payment_error_log, "operation", "retry_payment")
        LogRecord::add_attribute(payment_error_log, "retry.count", "1")
        LogRecord::add_attribute(payment_error_log, "retry.reason", "gateway_timeout")
        LogRecord::add_attribute(payment_error_log, "trace.id", SpanContext::trace_id(extracted_ctx))
        LogRecord::add_attribute(payment_error_log, "span.id", SpanContext::span_id(extracted_ctx))
        
        service_logs = service_logs.push((service, payment_error_log))
      }
      "notification-service" => {
        let notification_log = LogRecord::new(Info, "Order confirmation sent")
        LogRecord::add_attribute(notification_log, "service.name", service)
        LogRecord::add_attribute(notification_log, "operation", "send_notification")
        LogRecord::add_attribute(notification_log, "notification.type", "email")
        LogRecord::add_attribute(notification_log, "notification.recipient", "user@example.com")
        LogRecord::add_attribute(notification_log, "order.id", "order-45678")
        LogRecord::add_attribute(notification_log, "user.id", Baggage::get_value(extracted_baggage, "user.id").unwrap_or(""))
        LogRecord::add_attribute(notification_log, "trace.id", SpanContext::trace_id(extracted_ctx))
        LogRecord::add_attribute(notification_log, "span.id", SpanContext::span_id(extracted_ctx))
        
        service_logs = service_logs.push((service, notification_log))
      }
    }
  }
  
  // 验证跨服务日志关联
  let log_correlator = CrossServiceLogCorrelator::new()
  
  for (service, log) in service_logs {
    log_correlator.add_log(service, log)
  }
  
  // 按追踪ID分组日志
  let trace_id = SpanContext::trace_id(auth_ctx)
  let logs_by_trace = log_correlator.get_logs_by_trace_id(trace_id)
  
  assert_eq(logs_by_trace.length(), 6)  // 2 auth + 1 order + 2 payment + 1 notification
  
  // 按用户ID分组日志
  let logs_by_user = log_correlator.get_logs_by_attribute("user.id", "user-12345")
  assert_eq(logs_by_user.length(), 5)  // 除了auth错误日志外的所有日志
  
  // 按请求ID分组日志
  let logs_by_request = log_correlator.get_logs_by_attribute("request.id", "req-abc123")
  assert_eq(logs_by_request.length(), 4)  // order, payment(2), notification
  
  // 验证日志时间顺序
  let chronological_logs = log_correlator.get_logs_in chronological_order()
  assert_eq(chronological_logs.length(), 6)
  
  // 验证日志严重性分布
  let severity_distribution = log_correlator.get_severity_distribution()
  let info_count = severity_distribution.get("INFO").unwrap_or(0)
  let warn_count = severity_distribution.get("WARN").unwrap_or(0)
  let error_count = severity_distribution.get("ERROR").unwrap_or(0)
  
  assert_eq(info_count, 4)  // auth, order, payment, notification
  assert_eq(warn_count, 1)  // auth warning
  assert_eq(error_count, 1)  // payment error
  
  Span::end(auth_span)
}

test "跨服务配置同步和一致性" {
  // 测试跨服务遥测配置的同步和一致性
  let services = ["api-gateway", "user-service", "order-service", "inventory-service"]
  let config_manager = CrossServiceConfigManager::new()
  
  // 定义全局遥测配置
  let global_config = TelemetryConfig::new()
    .with_sampling_rate(0.1)  // 10% 采样率
    .with_export_interval(60000)  // 60秒导出间隔
    .with_batch_size(100)  // 批量大小100
    .with_max_attributes(32)  // 最大属性数32
    .with_max_events(128)  // 最大事件数128
    .with_max_links(32)  // 最大链接数32
    .with_enabled_metrics(true)
    .with_enabled_tracing(true)
    .with_enabled_logging(true)
  
  // 为每个服务创建特定配置
  let service_configs = []
  
  for service in services {
    let service_config = global_config.clone()
    
    // 根据服务特点调整配置
    match service {
      "api-gateway" => {
        let service_config = service_config
          .with_sampling_rate(0.05)  // 网关流量大，降低采样率
          .with_batch_size(200)      // 增大批量大小
          .with_max_attributes(64)   // 网关需要更多属性
      }
      "user-service" => {
        let service_config = service_config
          .with_sampling_rate(0.2)   // 用户服务需要更高采样率
          .with_export_interval(30000)  // 更频繁的导出
      }
      "order-service" => {
        let service_config = service_config
          .with_sampling_rate(0.15)  // 订单服务中等采样率
          .with_max_events(256)      // 订单服务可能有更多事件
      }
      "inventory-service" => {
        let service_config = service_config
          .with_sampling_rate(0.1)   // 保持默认采样率
          .with_enabled_logging(false)  // 库存服务关闭日志
      }
    }
    
    service_configs = service_configs.push((service, service_config))
  }
  
  // 注册服务配置
  for (service, config) in service_configs {
    config_manager.register_service_config(service, config)
  }
  
  // 验证配置注册
  for service in services {
    let registered_config = config_manager.get_service_config(service)
    assert_true(registered_config.is_some())
  }
  
  // 测试配置同步
  let sync_result = config_manager.synchronize_all_services()
  assert_true(sync_result.is_success())
  
  // 验证配置一致性
  let consistency_report = config_manager.validate_consistency()
  assert_true(consistency_report.is_consistent())
  
  // 测试配置更新传播
  let updated_global_config = global_config.with_sampling_rate(0.08)  // 降低全局采样率
  let update_result = config_manager.update_global_config(updated_global_config)
  assert_true(update_result.is_success())
  
  // 验证更新传播到所有服务
  for service in services {
    let updated_config = config_manager.get_service_config(service)
    assert_true(updated_config.is_some())
    
    let config = updated_config.some()
    // 所有服务都应该更新了采样率，但保留了其他特定配置
    assert_eq(config.get_sampling_rate(), 0.08)
    
    match service {
      "api-gateway" => {
        assert_eq(config.get_batch_size(), 200)
        assert_eq(config.get_max_attributes(), 64)
      }
      "user-service" => {
        assert_eq(config.get_export_interval(), 30000)
      }
      "order-service" => {
        assert_eq(config.get_max_events(), 256)
      }
      "inventory-service" => {
        assert_false(config.get_enabled_logging())
      }
    }
  }
  
  // 测试配置冲突检测和解决
  let conflicting_config = TelemetryConfig::new()
    .with_sampling_rate(0.5)  // 与全局配置冲突的高采样率
    .with_export_interval(10000)  // 与全局配置冲突的短间隔
  
  let conflict_result = config_manager.register_service_config("new-service", conflicting_config)
  assert_true(conflict_result.has_conflicts())
  
  let resolution_result = config_manager.resolve_conflicts("new-service", "global_priority")
  assert_true(resolution_result.is_resolved())
  
  let resolved_config = config_manager.get_service_config("new-service")
  assert_true(resolved_config.is_some())
  
  let final_config = resolved_config.some()
  assert_eq(final_config.get_sampling_rate(), 0.08)  // 应该采用全局配置
}

test "跨服务故障隔离和恢复" {
  // 测试跨服务遥测系统的故障隔离和恢复机制
  let services = ["primary-service", "secondary-service", "dependency-service"]
  let health_monitor = CrossServiceHealthMonitor::new()
  
  // 为每个服务注册健康监控
  for service in services {
    health_monitor.register_service(service)
  }
  
  // 模拟正常操作
  let telemetry_system = CrossServiceTelemetrySystem::new()
  
  // 初始化所有服务的遥测
  for service in services {
    let init_result = telemetry_system.initialize_service(service)
    assert_true(init_result.is_success())
    health_monitor.update_service_status(service, "healthy")
  }
  
  // 验证系统整体健康状态
  let system_health = health_monitor.get_system_health()
  assert_eq(system_health.get_status(), "healthy")
  assert_eq(system_health.get_healthy_services().length(), 3)
  assert_eq(system_health.get_unhealthy_services().length(), 0)
  
  // 模拟依赖服务故障
  let dependency_failure = telemetry_system.simulate_service_failure("dependency-service")
  assert_true(dependency_failure.is_simulated())
  
  // 更新健康状态
  health_monitor.update_service_status("dependency-service", "unhealthy")
  
  // 验证故障检测
  let failure_detection = health_monitor.detect_service_failures()
  assert_true(failure_detection.has_failures())
  assert_true(failure_detection.get_failed_services().contains("dependency-service"))
  
  // 测试故障隔离机制
  let isolation_result = telemetry_system.isolate_failed_service("dependency-service")
  assert_true(isolation_result.is_isolated())
  
  // 验证其他服务仍然正常运行
  let primary_status = health_monitor.get_service_status("primary-service")
  let secondary_status = health_monitor.get_service_status("secondary-service")
  
  assert_eq(primary_status, "healthy")
  assert_eq(secondary_status, "healthy")
  
  // 测试降级操作
  let degradation_result = telemetry_system.apply_degradation_strategy("circuit_breaker")
  assert_true(degradation_result.is_applied())
  
  // 验证降级后的系统行为
  let degraded_metrics = telemetry_system.get_metrics_with_degradation()
  assert_true(degraded_metrics.get_sample_rate() < 1.0)  // 应该降低采样率
  assert_true(degraded_metrics.get_enabled_features().length() < 3)  // 应该禁用一些功能
  
  // 模拟服务恢复
  let recovery_start = telemetry_system.start_service_recovery("dependency-service")
  assert_true(recovery_start.is_initiated())
  
  // 分阶段恢复
  let recovery_phases = ["connection_check", "basic_operations", "full_functionality"]
  
  for phase in recovery_phases {
    let phase_result = telemetry_system.execute_recovery_phase("dependency-service", phase)
    assert_true(phase_result.is_successful())
    
    // 更新恢复状态
    health_monitor.update_service_recovery_status("dependency-service", phase)
  }
  
  // 完成恢复
  let recovery_complete = telemetry_system.complete_service_recovery("dependency-service")
  assert_true(recovery_complete.is_successful())
  
  // 更新服务状态
  health_monitor.update_service_status("dependency-service", "healthy")
  
  // 验证系统完全恢复
  let final_system_health = health_monitor.get_system_health()
  assert_eq(final_system_health.get_status(), "healthy")
  assert_eq(final_system_health.get_healthy_services().length(), 3)
  assert_eq(final_system_health.get_unhealthy_services().length(), 0)
  
  // 验证恢复后的遥测功能
  for service in services {
    let service_telemetry = telemetry_system.get_service_telemetry(service)
    assert_true(service_telemetry.is_functional())
    assert_true(service_telemetry.can_collect_metrics())
    assert_true(service_telemetry.can_create_spans())
    assert_true(service_telemetry.can_emit_logs())
  }
  
  // 测试故障恢复报告
  let recovery_report = health_monitor.generate_recovery_report()
  assert_true(recovery_report.has_recovery_history())
  assert_true(recovery_report.get_failure_count("dependency-service") > 0)
  assert_true(recovery_report.get_recovery_time("dependency-service") > 0)
  assert_true(recovery_report.get_success_rate("dependency-service") > 0.0)
}
// Azimuth Telemetry System - Cross-Service Consistency Comprehensive Tests
// This file contains comprehensive test cases for cross-service telemetry consistency

// Test 1: Distributed Trace Consistency
test "distributed trace consistency across services" {
  let trace_coordinator = TraceCoordinator::new()
  
  // Initialize multiple services
  let auth_service = ServiceNode::new("auth-service", "http://localhost:8080")
  let user_service = ServiceNode::new("user-service", "http://localhost:8081")
  let order_service = ServiceNode::new("order-service", "http://localhost:8082")
  let payment_service = ServiceNode::new("payment-service", "http://localhost:8083")
  
  TraceCoordinator::register_service(trace_coordinator, auth_service)
  TraceCoordinator::register_service(trace_coordinator, user_service)
  TraceCoordinator::register_service(trace_coordinator, order_service)
  TraceCoordinator::register_service(trace_coordinator, payment_service)
  
  // Create distributed trace
  let trace_id = TraceCoordinator::create_distributed_trace(trace_coordinator, "user_checkout_flow")
  
  // Simulate request flow across services
  let auth_span = TraceCoordinator::create_span(trace_coordinator, auth_service, 
    "authenticate_user", trace_id)
  TraceCoordinator::add_span_attribute(trace_coordinator, auth_span, 
    "user.id", StringValue("user123"))
  TraceCoordinator::finish_span(trace_coordinator, auth_span)
  
  // Propagate trace context to user service
  let user_context = TraceCoordinator::extract_context(trace_coordinator, auth_span)
  let user_span = TraceCoordinator::create_span_with_context(trace_coordinator, 
    user_service, "get_user_profile", user_context)
  TraceCoordinator::add_span_attribute(trace_coordinator, user_span, 
    "profile.type", StringValue("premium"))
  TraceCoordinator::finish_span(trace_coordinator, user_span)
  
  // Continue to order service
  let order_context = TraceCoordinator::extract_context(trace_coordinator, user_span)
  let order_span = TraceCoordinator::create_span_with_context(trace_coordinator, 
    order_service, "create_order", order_context)
  TraceCoordinator::add_span_attribute(trace_coordinator, order_span, 
    "order.amount", FloatValue(99.99))
  TraceCoordinator::finish_span(trace_coordinator, order_span)
  
  // Finally to payment service
  let payment_context = TraceCoordinator::extract_context(trace_coordinator, order_span)
  let payment_span = TraceCoordinator::create_span_with_context(trace_coordinator, 
    payment_service, "process_payment", payment_context)
  TraceCoordinator::add_span_attribute(trace_coordinator, payment_span, 
    "payment.method", StringValue("credit_card"))
  TraceCoordinator::finish_span(trace_coordinator, payment_span)
  
  // Verify trace consistency
  let distributed_trace = TraceCoordinator::get_distributed_trace(trace_coordinator, trace_id)
  assert_eq(distributed_trace.trace_id, trace_id)
  assert_eq(distributed_trace.spans.length(), 4)
  
  // Verify parent-child relationships
  assert_eq(distributed_trace.spans[0].parent_span_id, "")
  assert_eq(distributed_trace.spans[1].parent_span_id, distributed_trace.spans[0].span_id)
  assert_eq(distributed_trace.spans[2].parent_span_id, distributed_trace.spans[1].span_id)
  assert_eq(distributed_trace.spans[3].parent_span_id, distributed_trace.spans[2].span_id)
  
  // Verify service attribution
  assert_eq(distributed_trace.spans[0].service_name, "auth-service")
  assert_eq(distributed_trace.spans[1].service_name, "user-service")
  assert_eq(distributed_trace.spans[2].service_name, "order-service")
  assert_eq(distributed_trace.spans[3].service_name, "payment-service")
}

// Test 2: Metrics Consistency Across Services
test "metrics consistency across services" {
  let metrics_coordinator = MetricsCoordinator::new()
  
  // Register metrics from different services
  let auth_metrics = MetricsCollector::new("auth-service")
  let user_metrics = MetricsCollector::new("user-service")
  let order_metrics = MetricsCollector::new("order-service")
  
  MetricsCoordinator::register_collector(metrics_coordinator, auth_metrics)
  MetricsCoordinator::register_collector(metrics_coordinator, user_metrics)
  MetricsCoordinator::register_collector(metrics_coordinator, order_metrics)
  
  // Simulate consistent metric collection
  for i in 0..=100 {
    // Auth service metrics
    MetricsCollector::record_counter(auth_metrics, "authentication_requests", 1.0, [
      ("status", StringValue("success")),
      ("user.type", StringValue("premium"))
    ])
    
    // User service metrics
    MetricsCollector::record_histogram(user_metrics, "profile_fetch_duration", 50.0 + i.to_float(), [
      ("cache.hit", StringValue(i % 2 == 0 ? "true" : "false"))
    ])
    
    // Order service metrics
    MetricsCollector::record_counter(order_metrics, "orders_created", 1.0, [
      ("order.type", StringValue("online")),
      ("payment.method", StringValue(i % 3 == 0 ? "credit_card" : "paypal"))
    ])
  }
  
  // Collect and aggregate metrics
  let aggregated_metrics = MetricsCoordinator::aggregate_metrics(metrics_coordinator)
  
  // Verify metric consistency
  let auth_counter = MetricsCoordinator::get_metric(aggregated_metrics, 
    "auth-service", "authentication_requests")
  match auth_counter {
    Some(counter) => assert_eq(counter.total_value, 100.0)
    None => assert_true(false)
  }
  
  let user_histogram = MetricsCoordinator::get_metric(aggregated_metrics, 
    "user-service", "profile_fetch_duration")
  match user_histogram {
    Some(histogram) => {
      assert_eq(histogram.count, 100)
      assert_true(histogram.sum > 5000.0)
    }
    None => assert_true(false)
  }
  
  let order_counter = MetricsCoordinator::get_metric(aggregated_metrics, 
    "order-service", "orders_created")
  match order_counter {
    Some(counter) => assert_eq(counter.total_value, 100.0)
    None => assert_true(false)
  }
  
  // Verify cross-service metric correlations
  let correlation = MetricsCoordinator::calculate_correlation(metrics_coordinator, 
    "auth-service", "authentication_requests", 
    "order-service", "orders_created")
  assert_true(correlation >= 0.0)
}

// Test 3: Log Consistency and Correlation
test "log consistency and correlation across services" {
  let log_coordinator = LogCoordinator::new()
  
  // Register loggers from different services
  let auth_logger = ServiceLogger::new("auth-service")
  let user_logger = ServiceLogger::new("user-service")
  let order_logger = ServiceLogger::new("order-service")
  
  LogCoordinator::register_logger(log_coordinator, auth_logger)
  LogCoordinator::register_logger(log_coordinator, user_logger)
  LogCoordinator::register_logger(log_coordinator, order_logger)
  
  // Create correlated log flow
  let correlation_id = "corr_123456789"
  let trace_id = "trace_987654321"
  
  // Auth service logs
  ServiceLogger::log_with_correlation(auth_logger, Info, 
    "User authentication started", correlation_id, trace_id, [
      ("user.id", StringValue("user123")),
      ("auth.method", StringValue("oauth2"))
    ])
  
  ServiceLogger::log_with_correlation(auth_logger, Info, 
    "User authentication successful", correlation_id, trace_id, [
      ("user.id", StringValue("user123")),
      ("session.id", StringValue("sess_abc123"))
    ])
  
  // User service logs
  ServiceLogger::log_with_correlation(user_logger, Info, 
    "Fetching user profile", correlation_id, trace_id, [
      ("user.id", StringValue("user123")),
      ("profile.source", StringValue("database"))
    ])
  
  ServiceLogger::log_with_correlation(user_logger, Warn, 
    "Profile cache miss", correlation_id, trace_id, [
      ("cache.key", StringValue("profile_user123"))
    ])
  
  // Order service logs
  ServiceLogger::log_with_correlation(order_logger, Info, 
    "Creating new order", correlation_id, trace_id, [
      ("user.id", StringValue("user123")),
      ("order.total", FloatValue(99.99))
    ])
  
  ServiceLogger::log_with_correlation(order_logger, Error, 
    "Payment processing failed", correlation_id, trace_id, [
      ("error.code", StringValue("PAYMENT_DECLINED")),
      ("payment.gateway", StringValue("stripe"))
    ])
  
  // Collect and correlate logs
  let correlated_logs = LogCoordinator::get_logs_by_correlation(log_coordinator, correlation_id)
  assert_eq(correlated_logs.length(), 6)
  
  // Verify log ordering by timestamp
  for i in 1..=correlated_logs.length() - 1 {
    assert_true(correlated_logs[i].timestamp >= correlated_logs[i-1].timestamp)
  }
  
  // Verify trace consistency across logs
  for log in correlated_logs {
    assert_eq(log.correlation_id, correlation_id)
    assert_eq(log.trace_id, trace_id)
  }
  
  // Verify service distribution
  let auth_logs = LogCoordinator::get_logs_by_service(log_coordinator, "auth-service", correlation_id)
  let user_logs = LogCoordinator::get_logs_by_service(log_coordinator, "user-service", correlation_id)
  let order_logs = LogCoordinator::get_logs_by_service(log_coordinator, "order-service", correlation_id)
  
  assert_eq(auth_logs.length(), 2)
  assert_eq(user_logs.length(), 2)
  assert_eq(order_logs.length(), 2)
}

// Test 4: Context Propagation Consistency
test "context propagation consistency across services" {
  let context_propagator = CrossServiceContextPropagator::new()
  
  // Create initial context
  let initial_context = Context::with_values(Context::root(), [
    ("request.id", StringValue("req_123")),
    ("user.id", StringValue("user456")),
    ("session.id", StringValue("sess_789")),
    ("trace.id", StringValue("trace_abc")),
    ("correlation.id", StringValue("corr_def"))
  ])
  
  // Simulate context propagation through service chain
  let service1_context = CrossServiceContextPropagator::propagate_to_service(
    context_propagator, initial_context, "auth-service")
  
  // Verify context integrity
  assert_eq(Context::get(service1_context, "request.id"), Some("req_123"))
  assert_eq(Context::get(service1_context, "user.id"), Some("user456"))
  assert_eq(Context::get(service1_context, "trace.id"), Some("trace_abc"))
  
  // Add service-specific context
  let service1_enhanced = Context::with_values(service1_context, [
    ("auth.method", StringValue("oauth2")),
    ("auth.duration", IntValue(150))
  ])
  
  // Propagate to next service
  let service2_context = CrossServiceContextPropagator::propagate_to_service(
    context_propagator, service1_enhanced, "user-service")
  
  // Verify original context is preserved
  assert_eq(Context::get(service2_context, "request.id"), Some("req_123"))
  assert_eq(Context::get(service2_context, "user.id"), Some("user456"))
  assert_eq(Context::get(service2_context, "trace.id"), Some("trace_abc"))
  
  // Verify service1 context is propagated
  assert_eq(Context::get(service2_context, "auth.method"), Some("oauth2"))
  assert_eq(Context::get(service2_context, "auth.duration"), Some("150"))
  
  // Add service2 specific context
  let service2_enhanced = Context::with_values(service2_context, [
    ("profile.cache_hit", StringValue("false")),
    ("profile.fetch_time", IntValue(75))
  ])
  
  // Test context extraction and injection
  let injected_headers = CrossServiceContextPropagator::inject_to_headers(
    context_propagator, service2_enhanced)
  
  assert_true(injected_headers.contains("x-trace-id"))
  assert_true(injected_headers.contains("x-correlation-id"))
  assert_true(injected_headers.contains("x-request-id"))
  
  let extracted_context = CrossServiceContextPropagator::extract_from_headers(
    context_propagator, injected_headers)
  
  assert_eq(Context::get(extracted_context, "trace.id"), Some("trace_abc"))
  assert_eq(Context::get(extracted_context, "correlation.id"), Some("corr_def"))
  assert_eq(Context::get(extracted_context, "request.id"), Some("req_123"))
}

// Test 5: Service Mesh Telemetry Consistency
test "service mesh telemetry consistency" {
  let mesh_coordinator = ServiceMeshCoordinator::new()
  
  // Simulate service mesh with sidecar proxies
  let services = [
    ("api-gateway", SidecarProxy::new("api-gateway-sidecar")),
    ("auth-service", SidecarProxy::new("auth-service-sidecar")),
    ("user-service", SidecarProxy::new("user-service-sidecar")),
    ("order-service", SidecarProxy::new("order-service-sidecar"))
  ]
  
  for (service_name, sidecar) in services {
    ServiceMeshCoordinator::register_service(mesh_coordinator, service_name, sidecar)
  }
  
  // Simulate request flow through service mesh
  let request_id = "mesh_req_123"
  
  // API Gateway receives request
  let gateway_span = ServiceMeshCoordinator::create_mesh_span(mesh_coordinator, 
    "api-gateway", "incoming_request", request_id)
  ServiceMeshCoordinator::add_mesh_attribute(mesh_coordinator, gateway_span, 
    "http.method", StringValue("POST"))
  ServiceMeshCoordinator::add_mesh_attribute(mesh_coordinator, gateway_span, 
    "http.path", StringValue("/api/orders"))
  
  // Request routed to auth service
  let auth_span = ServiceMeshCoordinator::create_mesh_child_span(mesh_coordinator, 
    "auth-service", "authenticate", gateway_span)
  ServiceMeshCoordinator::add_mesh_attribute(mesh_coordinator, auth_span, 
    "auth.token.valid", StringValue("true"))
  
  // Request routed to user service
  let user_span = ServiceMeshCoordinator::create_mesh_child_span(mesh_coordinator, 
    "user-service", "get_profile", gateway_span)
  ServiceMeshCoordinator::add_mesh_attribute(mesh_coordinator, user_span, 
    "user.exists", StringValue("true"))
  
  // Request routed to order service
  let order_span = ServiceMeshCoordinator::create_mesh_child_span(mesh_coordinator, 
    "order-service", "create_order", gateway_span)
  ServiceMeshCoordinator::add_mesh_attribute(mesh_coordinator, order_span, 
    "order.created", StringValue("true"))
  
  // Complete mesh trace
  ServiceMeshCoordinator::finish_mesh_span(mesh_coordinator, auth_span)
  ServiceMeshCoordinator::finish_mesh_span(mesh_coordinator, user_span)
  ServiceMeshCoordinator::finish_mesh_span(mesh_coordinator, order_span)
  ServiceMeshCoordinator::finish_mesh_span(mesh_coordinator, gateway_span)
  
  // Verify mesh trace consistency
  let mesh_trace = ServiceMeshCoordinator::get_mesh_trace(mesh_coordinator, request_id)
  assert_eq(mesh_trace.root_span.service_name, "api-gateway")
  assert_eq(mesh_trace.child_spans.length(), 3)
  
  // Verify sidecar telemetry collection
  for (service_name, sidecar) in services {
    let sidecar_telemetry = ServiceMeshCoordinator::get_sidecar_telemetry(
      mesh_coordinator, service_name)
    assert_true(sidecar_telemetry.incoming_requests > 0)
    assert_true(sidecar_telemetry.outgoing_requests >= 0)
  }
  
  // Verify mesh-wide metrics
  let mesh_metrics = ServiceMeshCoordinator::get_mesh_metrics(mesh_coordinator)
  assert_eq(mesh_metrics.total_requests, 1)
  assert_eq(mesh_metrics.total_spans, 4)
  assert_true(mesh_metrics.average_latency > 0)
}

// Test 6: Event-Driven Consistency
test "event-driven telemetry consistency" {
  let event_coordinator = EventCoordinator::new()
  
  // Register event handlers across services
  let auth_handler = EventHandler::new("auth-service", "user_authenticated")
  let user_handler = EventHandler::new("user-service", "profile_updated")
  let order_handler = EventHandler::new("order-service", "order_created")
  let notification_handler = EventHandler::new("notification-service", "order_completed")
  
  EventCoordinator::register_handler(event_coordinator, auth_handler)
  EventCoordinator::register_handler(event_coordinator, user_handler)
  EventCoordinator::register_handler(event_coordinator, order_handler)
  EventCoordinator::register_handler(event_coordinator, notification_handler)
  
  // Create event chain with consistent correlation
  let correlation_context = EventContext::new("event_chain_123", "user_journey")
  
  // Initial event: User authentication
  let auth_event = EventCoordinator::create_event(event_coordinator, 
    "user_authenticated", correlation_context, [
      ("user.id", StringValue("user789")),
      ("auth.method", StringValue("sso")),
      ("timestamp", StringValue("2023-01-01T10:00:00Z"))
    ])
  
  EventCoordinator::publish_event(event_coordinator, auth_event)
  
  // Follow-up event: Profile update
  let profile_event = EventCoordinator::create_event(event_coordinator, 
    "profile_updated", correlation_context, [
      ("user.id", StringValue("user789")),
      ("update.type", StringValue("last_login")),
      ("timestamp", StringValue("2023-01-01T10:00:05Z"))
    ])
  
  EventCoordinator::publish_event(event_coordinator, profile_event)
  
  // Order creation event
  let order_event = EventCoordinator::create_event(event_coordinator, 
    "order_created", correlation_context, [
      ("user.id", StringValue("user789")),
      ("order.id", StringValue("order456")),
      ("order.amount", FloatValue(149.99)),
      ("timestamp", StringValue("2023-01-01T10:01:00Z"))
    ])
  
  EventCoordinator::publish_event(event_coordinator, order_event)
  
  // Notification event
  let notification_event = EventCoordinator::create_event(event_coordinator, 
    "order_completed", correlation_context, [
      ("user.id", StringValue("user789")),
      ("order.id", StringValue("order456")),
      ("notification.type", StringValue("email")),
      ("timestamp", StringValue("2023-01-01T10:02:00Z"))
    ])
  
  EventCoordinator::publish_event(event_coordinator, notification_event)
  
  // Verify event consistency
  let event_chain = EventCoordinator::get_event_chain(event_coordinator, "event_chain_123")
  assert_eq(event_chain.events.length(), 4)
  
  // Verify correlation consistency
  for event in event_chain.events {
    assert_eq(event.correlation_id, "event_chain_123")
    assert_eq(event.chain_id, "user_journey")
  }
  
  // Verify temporal consistency
  for i in 1..=event_chain.events.length() - 1 {
    assert_true(event_chain.events[i].timestamp >= event_chain.events[i-1].timestamp)
  }
  
  // Verify event handler execution
  let handler_stats = EventCoordinator::get_handler_stats(event_coordinator)
  assert_eq(handler_stats["auth-service"].events_handled, 1)
  assert_eq(handler_stats["user-service"].events_handled, 1)
  assert_eq(handler_stats["order-service"].events_handled, 1)
  assert_eq(handler_stats["notification-service"].events_handled, 1)
}

// Test 7: Cross-Service Configuration Consistency
test "cross-service configuration consistency" {
  let config_coordinator = ConfigCoordinator::new()
  
  // Define global configuration
  let global_config = ServiceConfiguration::new("global", [
    ("telemetry.enabled", BoolValue(true)),
    ("telemetry.sampling_rate", FloatValue(0.1)),
    ("telemetry.export_interval", IntValue(10000)),
    ("logging.level", StringValue("INFO")),
    ("metrics.retention_days", IntValue(30))
  ])
  
  ConfigCoordinator::set_global_config(config_coordinator, global_config)
  
  // Register services
  let services = ["auth-service", "user-service", "order-service", "payment-service"]
  
  for service in services {
    ConfigCoordinator::register_service(config_coordinator, service)
    
    // Set service-specific configuration
    let service_config = ServiceConfiguration::new(service, [
      ("service.version", StringValue("1.2.3")),
      ("service.port", IntValue(8080 + services.index_of(service))),
      ("service.timeout", IntValue(5000))
    ])
    
    ConfigCoordinator::set_service_config(config_coordinator, service, service_config)
  }
  
  // Verify configuration inheritance
  for service in services {
    let merged_config = ConfigCoordinator::get_merged_config(config_coordinator, service)
    
    // Verify global config is inherited
    assert_eq(ConfigCoordinator::get_config_value(merged_config, "telemetry.enabled"), 
      Some(BoolValue(true)))
    assert_eq(ConfigCoordinator::get_config_value(merged_config, "telemetry.sampling_rate"), 
      Some(FloatValue(0.1)))
    
    // Verify service-specific config overrides
    assert_eq(ConfigCoordinator::get_config_value(merged_config, "service.version"), 
      Some(StringValue("1.2.3")))
  }
  
  // Test configuration propagation
  ConfigCoordinator::update_global_config(config_coordinator, "telemetry.sampling_rate", 
    FloatValue(0.2))
  
  // Verify propagation to all services
  for service in services {
    let updated_config = ConfigCoordinator::get_merged_config(config_coordinator, service)
    assert_eq(ConfigCoordinator::get_config_value(updated_config, "telemetry.sampling_rate"), 
      Some(FloatValue(0.2)))
  }
  
  // Test configuration consistency validation
  let validation_report = ConfigCoordinator::validate_consistency(config_coordinator)
  assert_true(validation_report.is_consistent)
  assert_eq(validation_report.inconsistent_services.length(), 0)
}

// Test 8: Cross-Service Health Monitoring
test "cross-service health monitoring consistency" {
  let health_coordinator = HealthCoordinator::new()
  
  // Register services for health monitoring
  let services = [
    ("api-gateway", HealthMonitor::new("api-gateway")),
    ("auth-service", HealthMonitor::new("auth-service")),
    ("user-service", HealthMonitor::new("user-service")),
    ("order-service", HealthMonitor::new("order-service")),
    ("payment-service", HealthMonitor::new("payment-service"))
  ]
  
  for (service_name, monitor) in services {
    HealthCoordinator::register_monitor(health_coordinator, service_name, monitor)
  }
  
  // Simulate health status updates
  HealthCoordinator::update_health_status(health_coordinator, "api-gateway", Healthy)
  HealthCoordinator::update_health_status(health_coordinator, "auth-service", Healthy)
  HealthCoordinator::update_health_status(health_coordinator, "user-service", Healthy)
  HealthCoordinator::update_health_status(health_coordinator, "order-service", Degraded("High latency"))
  HealthCoordinator::update_health_status(health_coordinator, "payment-service", Unhealthy("Connection timeout"))
  
  // Get overall system health
  let system_health = HealthCoordinator::get_system_health(health_coordinator)
  assert_eq(system_health.status, Degraded) // One service degraded, one unhealthy
  
  // Verify health distribution
  assert_eq(system_health.healthy_services, 3)
  assert_eq(system_health.degraded_services, 1)
  assert_eq(system_health.unhealthy_services, 1)
  
  // Test health dependency tracking
  HealthCoordinator::add_dependency(health_coordinator, "order-service", "payment-service")
  HealthCoordinator::add_dependency(health_coordinator, "user-service", "auth-service")
  
  let dependency_health = HealthCoordinator::get_dependency_health(health_coordinator)
  assert_true(dependency_health["order-service"].affected_by_unhealthy)
  assert_false(dependency_health["user-service"].affected_by_unhealthy)
  
  // Test health-based routing decisions
  let routing_decision = HealthCoordinator::get_routing_recommendation(health_coordinator, "order-service")
  assert_eq(routing_decision.action, RouteWithCaution) // Depends on unhealthy payment service
  
  // Test health recovery notification
  HealthCoordinator::update_health_status(health_coordinator, "payment-service", Healthy)
  let recovery_notification = HealthCoordinator::get_recovery_notifications(health_coordinator)
  assert_true(recovery_notification.contains("payment-service"))
  
  // Verify system health recovery
  let recovered_health = HealthCoordinator::get_system_health(health_coordinator)
  assert_eq(recovered_health.status, Degraded) // Still one degraded service
}

// Test 9: Cross-Service Performance Consistency
test "cross-service performance consistency" {
  let performance_coordinator = PerformanceCoordinator::new()
  
  // Register services for performance monitoring
  let services = ["auth-service", "user-service", "order-service"]
  
  for service in services {
    PerformanceCoordinator::register_service(performance_coordinator, service)
  }
  
  // Simulate performance metrics collection
  for i in 0..=100 {
    // Auth service performance
    PerformanceCoordinator::record_operation_time(performance_coordinator, 
      "auth-service", "authenticate", 50.0 + i.to_float() % 100.0)
    
    // User service performance
    PerformanceCoordinator::record_operation_time(performance_coordinator, 
      "user-service", "get_profile", 25.0 + i.to_float() % 50.0)
    
    // Order service performance
    PerformanceCoordinator::record_operation_time(performance_coordinator, 
      "order-service", "create_order", 100.0 + i.to_float() % 200.0)
  }
  
  // Calculate performance statistics
  let auth_performance = PerformanceCoordinator::get_performance_stats(
    performance_coordinator, "auth-service")
  let user_performance = PerformanceCoordinator::get_performance_stats(
    performance_coordinator, "user-service")
  let order_performance = PerformanceCoordinator::get_performance_stats(
    performance_coordinator, "order-service")
  
  // Verify performance consistency
  assert_eq(auth_performance.operation_count, 100)
  assert_eq(user_performance.operation_count, 100)
  assert_eq(order_performance.operation_count, 100)
  
  assert_true(auth_performance.average_time > 50.0)
  assert_true(user_performance.average_time > 25.0)
  assert_true(order_performance.average_time > 100.0)
  
  // Test performance threshold violations
  PerformanceCoordinator::set_threshold(performance_coordinator, "auth-service", 200.0)
  PerformanceCoordinator::set_threshold(performance_coordinator, "user-service", 100.0)
  PerformanceCoordinator::set_threshold(performance_coordinator, "order-service", 300.0)
  
  let violations = PerformanceCoordinator::get_threshold_violations(performance_coordinator)
  assert_eq(violations.length(), 0) // All operations within thresholds
  
  // Simulate performance degradation
  for i in 0..=10 {
    PerformanceCoordinator::record_operation_time(performance_coordinator, 
      "order-service", "create_order", 350.0) // Above threshold
  }
  
  let new_violations = PerformanceCoordinator::get_threshold_violations(performance_coordinator)
  assert_eq(new_violations.length(), 1)
  assert_eq(new_violations[0].service_name, "order-service")
  
  // Test cross-service performance correlation
  let correlation = PerformanceCoordinator::calculate_cross_service_correlation(
    performance_coordinator, "auth-service", "user-service")
  assert_true(correlation >= -1.0 && correlation <= 1.0)
}

// Test 10: Cross-Service Data Consistency
test "cross-service data consistency validation" {
  let data_consistency_validator = DataConsistencyValidator::new()
  
  // Define data consistency rules
  let user_consistency_rule = ConsistencyRule::new("user_data", [
    ("user.id", DataType::String, true),
    ("user.email", DataType::String, true),
    ("user.created_at", DataType::Timestamp, true),
    ("user.updated_at", DataType::Timestamp, false)
  ])
  
  let order_consistency_rule = ConsistencyRule::new("order_data", [
    ("order.id", DataType::String, true),
    ("user.id", DataType::String, true),
    ("order.amount", DataType::Float, true),
    ("order.status", DataType::String, true),
    ("order.created_at", DataType::Timestamp, true)
  ])
  
  DataConsistencyValidator::add_rule(data_consistency_validator, user_consistency_rule)
  DataConsistencyValidator::add_rule(data_consistency_validator, order_consistency_rule)
  
  // Register data sources
  DataConsistencyValidator::register_data_source(data_consistency_validator, 
    "auth-service", "user_data")
  DataConsistencyValidator::register_data_source(data_consistency_validator, 
    "user-service", "user_data")
  DataConsistencyValidator::register_data_source(data_consistency_validator, 
    "order-service", "order_data")
  
  // Submit consistent data
  let user_data = DataRecord::new("user_data", [
    ("user.id", StringValue("user123")),
    ("user.email", StringValue("user@example.com")),
    ("user.created_at", TimestampValue(1234567890L)),
    ("user.updated_at", TimestampValue(1234567895L))
  ])
  
  DataConsistencyValidator::submit_data(data_consistency_validator, 
    "auth-service", user_data)
  DataConsistencyValidator::submit_data(data_consistency_validator, 
    "user-service", user_data)
  
  let order_data = DataRecord::new("order_data", [
    ("order.id", StringValue("order456")),
    ("user.id", StringValue("user123")),
    ("order.amount", FloatValue(99.99)),
    ("order.status", StringValue("pending")),
    ("order.created_at", TimestampValue(1234567900L))
  ])
  
  DataConsistencyValidator::submit_data(data_consistency_validator, 
    "order-service", order_data)
  
  // Validate data consistency
  let consistency_report = DataConsistencyValidator::validate_consistency(
    data_consistency_validator, "user_data")
  
  assert_true(consistency_report.is_consistent)
  assert_eq(consistency_report.consistent_sources.length(), 2)
  assert_eq(consistency_report.inconsistent_sources.length(), 0)
  
  // Test referential integrity
  let integrity_report = DataConsistencyValidator::validate_referential_integrity(
    data_consistency_validator, "order_data", "user.id", "user_data", "user.id")
  
  assert_true(integrity_report.is_valid)
  assert_eq(integrity_report.valid_references, 1)
  assert_eq(integrity_report.invalid_references, 0)
  
  // Submit inconsistent data
  let inconsistent_user_data = DataRecord::new("user_data", [
    ("user.id", StringValue("user123")),
    ("user.email", StringValue("user@example.com")),
    ("user.created_at", TimestampValue(1234567890L))
    // Missing user.updated_at (required in some services)
  ])
  
  DataConsistencyValidator::submit_data(data_consistency_validator, 
    "user-service", inconsistent_user_data)
  
  let new_consistency_report = DataConsistencyValidator::validate_consistency(
    data_consistency_validator, "user_data")
  
  assert_false(new_consistency_report.is_consistent)
  assert_eq(new_consistency_report.inconsistent_sources.length(), 1)
  
  // Test data reconciliation
  let reconciliation_result = DataConsistencyValidator::reconcile_data(
    data_consistency_validator, "user_data")
  
  assert_true(reconciliation_result.reconciliation_successful)
  assert_eq(reconciliation_result.reconciled_records, 1)
}
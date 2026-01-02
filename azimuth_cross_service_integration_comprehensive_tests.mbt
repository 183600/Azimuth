// Azimuth Cross-Service Integration Test Suite
// 测试遥测系统在微服务环境中的跨服务集成功能

test "微服务链路追踪和传播" {
  // 创建跨服务遥测协调器
  let coordinator = CrossServiceTelemetryCoordinator::new()
  
  // 配置服务拓扑
  let service_topology = ServiceTopology::new()
  ServiceTopology::add_service(service_topology, "api-gateway", ["user-service", "order-service", "payment-service"])
  ServiceTopology::add_service(service_topology, "user-service", ["database"])
  ServiceTopology::add_service(service_topology, "order-service", ["database", "inventory-service"])
  ServiceTopology::add_service(service_topology, "payment-service", ["payment-processor"])
  ServiceTopology::add_service(service_topology, "inventory-service", ["database"])
  
  CrossServiceTelemetryCoordinator::configure_topology(coordinator, service_topology)
  
  // 创建各服务的遥测提供者
  let api_gateway_provider = TelemetryProvider::with_cross_service_support("api-gateway")
  let user_service_provider = TelemetryProvider::with_cross_service_support("user-service")
  let order_service_provider = TelemetryProvider::with_cross_service_support("order-service")
  let payment_service_provider = TelemetryProvider::with_cross_service_support("payment-service")
  
  // 创建各服务的tracer
  let api_gateway_tracer = TelemetryProvider::get_tracer(api_gateway_provider, "api-gateway")
  let user_service_tracer = TelemetryProvider::get_tracer(user_service_provider, "user-service")
  let order_service_tracer = TelemetryProvider::get_tracer(order_service_provider, "order-service")
  let payment_service_tracer = TelemetryProvider::get_tracer(payment_service_provider, "payment-service")
  
  // 模拟API网关接收请求
  let api_span = Tracer::start_span(api_gateway_tracer, "api.request")
  Span::set_attribute(api_span, "http.method", "POST")
  Span::set_attribute(api_span, "http.url", "/api/v1/orders")
  Span::set_attribute(api_span, "user.id", "user123")
  
  // 提取传播上下文
  let propagation_context = SpanContext::extract_from_span(api_span)
  
  // 模拟API网关调用用户服务
  let user_span = Tracer::start_span_with_context(user_service_tracer, "user.validation", propagation_context)
  Span::set_attribute(user_span, "operation.type", "validate_user")
  Span::set_attribute(user_span, "user.id", "user123")
  
  // 用户服务调用数据库
  let user_db_span = Tracer::start_span_with_context(user_service_tracer, "user.db.query", SpanContext::extract_from_span(user_span))
  Span::set_attribute(user_db_span, "db.operation", "SELECT")
  Span::set_attribute(user_db_span, "db.table", "users")
  Span::end(user_db_span)
  
  Span::end(user_span)
  
  // 模拟API网关调用订单服务
  let order_span = Tracer::start_span_with_context(order_service_tracer, "order.processing", propagation_context)
  Span::set_attribute(order_span, "operation.type", "create_order")
  Span::set_attribute(order_span, "order.id", "order456")
  
  // 订单服务调用库存服务
  let inventory_span = Tracer::start_span_with_context(order_service_tracer, "inventory.check", SpanContext::extract_from_span(order_span))
  Span::set_attribute(inventory_span, "operation.type", "check_availability")
  Span::set_attribute(inventory_span, "product.id", "prod789")
  Span::end(inventory_span)
  
  // 订单服务调用数据库
  let order_db_span = Tracer::start_span_with_context(order_service_tracer, "order.db.query", SpanContext::extract_from_span(order_span))
  Span::set_attribute(order_db_span, "db.operation", "INSERT")
  Span::set_attribute(order_db_span, "db.table", "orders")
  Span::end(order_db_span)
  
  Span::end(order_span)
  
  // 模拟API网关调用支付服务
  let payment_span = Tracer::start_span_with_context(payment_service_tracer, "payment.processing", propagation_context)
  Span::set_attribute(payment_span, "operation.type", "process_payment")
  Span::set_attribute(payment_span, "payment.id", "pay123")
  Span::set_attribute(payment_span, "amount", "99.99")
  
  // 支付服务调用支付处理器
  let processor_span = Tracer::start_span_with_context(payment_service_tracer, "payment.processor.call", SpanContext::extract_from_span(payment_span))
  Span::set_attribute(processor_span, "processor.type", "external")
  Span::set_attribute(processor_span, "processor.endpoint", "https://payment.example.com/api")
  Span::end(processor_span)
  
  Span::end(payment_span)
  
  // 结束API网关span
  Span::end(api_span)
  
  // 收集所有服务的遥测数据
  let all_spans = CrossServiceTelemetryCoordinator::collect_all_spans(coordinator)
  
  // 验证链路追踪完整性
  assert_true(all_spans.length() >= 8) // 至少有8个span
  
  // 验证所有span属于同一个trace
  let trace_ids = all_spans.map(fn(span) { SpanContext::trace_id(Span::span_context(span)) })
  let unique_trace_ids = trace_ids.unique()
  assert_eq(unique_trace_ids.length(), 1) // 所有span应该有相同的trace_id
  
  // 验证父子关系
  let api_context = Span::span_context(api_span)
  let user_context = Span::span_context(user_span)
  let order_context = Span::span_context(order_span)
  let payment_context = Span::span_context(payment_span)
  
  assert_eq(SpanContext::parent_span_id(user_context), Some(SpanContext::span_id(api_context)))
  assert_eq(SpanContext::parent_span_id(order_context), Some(SpanContext::span_id(api_context)))
  assert_eq(SpanContext::parent_span_id(payment_context), Some(SpanContext::span_id(api_context)))
  
  // 生成跨服务追踪报告
  let trace_report = CrossServiceTelemetryCoordinator::generate_trace_report(coordinator, SpanContext::trace_id(api_context))
  assert_true(TraceReport::total_spans(trace_report) >= 8)
  assert_true(TraceReport::services_involved(trace_report) >= 4) // 至少4个服务
  assert_true(TraceReport::total_duration(trace_report) > 0)
  
  assert_true(true)
}

test "跨服务度量聚合和关联" {
  // 创建跨服务度量协调器
  let metrics_coordinator = CrossServiceMetricsCoordinator::new()
  
  // 配置服务注册
  let services = ["api-gateway", "user-service", "order-service", "payment-service", "inventory-service"]
  for service in services {
    CrossServiceMetricsCoordinator::register_service(metrics_coordinator, service)
  }
  
  // 创建各服务的度量提供者
  let service_providers = {}
  for service in services {
    let provider = TelemetryProvider::with_cross_service_metrics(service)
    service_providers.set(service, provider)
  }
  
  // 创建各服务的meter
  let service_meters = {}
  for service in services {
    let provider = service_providers.get(service)
    let meter = MeterProvider::get_meter(provider, service)
    service_meters.set(service, meter)
  }
  
  // 创建度量仪器
  let service_counters = {}
  let service_histograms = {}
  let service_gauges = {}
  
  for service in services {
    let meter = service_meters.get(service)
    
    // 请求计数器
    let counter = Meter::create_counter(meter, "requests.total")
    service_counters.set(service, counter)
    
    // 响应时间直方图
    let histogram = Meter::create_histogram(meter, "response.time")
    service_histograms.set(service, histogram)
    
    // 并发连接数仪表
    let gauge = Meter::create_gauge(meter, "active.connections")
    service_gauges.set(service, gauge)
  }
  
  // 模拟各服务的度量记录
  let api_gateway_counter = service_counters.get("api-gateway")
  let api_gateway_histogram = service_histograms.get("api-gateway")
  let api_gateway_gauge = service_gauges.get("api-gateway")
  
  // API网关度量
  Counter::add_with_attributes(api_gateway_counter, 100.0, [
    ("endpoint", "/api/v1/orders"),
    ("method", "POST"),
    ("status", "200")
  ])
  
  Histogram::record_with_attributes(api_gateway_histogram, 150.5, [
    ("endpoint", "/api/v1/orders"),
    ("method", "POST"),
    ("status", "200")
  ])
  
  Gauge::set_with_attributes(api_gateway_gauge, 25.0, [
    ("connection.type", "http")
  ])
  
  // 用户服务度量
  let user_service_counter = service_counters.get("user-service")
  let user_service_histogram = service_histograms.get("user-service")
  
  Counter::add_with_attributes(user_service_counter, 95.0, [
    ("operation", "validate_user"),
    ("status", "success")
  ])
  
  Histogram::record_with_attributes(user_service_histogram, 25.3, [
    ("operation", "validate_user"),
    ("status", "success")
  ])
  
  // 订单服务度量
  let order_service_counter = service_counters.get("order-service")
  let order_service_histogram = service_histograms.get("order-service")
  
  Counter::add_with_attributes(order_service_counter, 85.0, [
    ("operation", "create_order"),
    ("status", "success")
  ])
  
  Histogram::record_with_attributes(order_service_histogram, 200.7, [
    ("operation", "create_order"),
    ("status", "success")
  ])
  
  // 支付服务度量
  let payment_service_counter = service_counters.get("payment-service")
  let payment_service_histogram = service_histograms.get("payment-service")
  
  Counter::add_with_attributes(payment_service_counter, 80.0, [
    ("operation", "process_payment"),
    ("status", "success")
  ])
  
  Histogram::record_with_attributes(payment_service_histogram, 500.2, [
    ("operation", "process_payment"),
    ("status", "success")
  ])
  
  // 库存服务度量
  let inventory_service_counter = service_counters.get("inventory-service")
  let inventory_service_histogram = service_histograms.get("inventory-service")
  
  Counter::add_with_attributes(inventory_service_counter, 90.0, [
    ("operation", "check_availability"),
    ("status", "success")
  ])
  
  Histogram::record_with_attributes(inventory_service_histogram, 75.8, [
    ("operation", "check_availability"),
    ("status", "success")
  ])
  
  // 收集所有服务的度量数据
  let all_metrics = CrossServiceMetricsCoordinator::collect_all_metrics(metrics_coordinator)
  
  // 验证度量数据收集
  assert_true(all_metrics.length() >= 5) // 至少5个服务的度量
  
  // 跨服务度量聚合
  let total_requests = CrossServiceMetricsCoordinator::aggregate_counter(
    metrics_coordinator, 
    "requests.total", 
    [("status", "200")]
  )
  assert_eq(total_requests, 100.0) // 只有API网关有requests.total度量
  
  // 按服务分组的度量
  let requests_by_service = CrossServiceMetricsCoordinator::group_by_service(
    metrics_coordinator, 
    "requests.total"
  )
  assert_eq(requests_by_service.get("api-gateway"), Some(100.0))
  
  // 跨服务响应时间分析
  let response_time_stats = CrossServiceMetricsCoordinator::analyze_histogram(
    metrics_coordinator, 
    "response.time"
  )
  assert_true(HistogramStats::count(response_time_stats) >= 5)
  assert_true(HistogramStats::sum(response_time_stats) > 0)
  assert_true(HistogramStats::average(response_time_stats) > 0)
  
  // 服务依赖关系度量分析
  let dependency_metrics = CrossServiceMetricsCoordinator::analyze_service_dependencies(
    metrics_coordinator, 
    service_topology
  )
  assert_true(DependencyMetrics::total_dependencies(dependency_metrics) >= 4)
  assert_true(DependencyMetrics::critical_path_latency(dependency_metrics) > 0)
  
  // 生成跨服务度量报告
  let metrics_report = CrossServiceMetricsCoordinator::generate_metrics_report(metrics_coordinator)
  assert_true(MetricsReport::services_covered(metrics_report) >= 5)
  assert_true(MetricsReport::total_metrics_collected(metrics_report) >= 15)
  assert_true(MetricsReport::cross_service_correlations(metrics_report) >= 3)
  
  assert_true(true)
}

test "分布式日志关联和分析" {
  // 创建分布式日志协调器
  let log_coordinator = DistributedLogCoordinator::new()
  
  // 配置日志收集器
  let log_collectors = {}
  let services = ["api-gateway", "user-service", "order-service", "payment-service"]
  
  for service in services {
    let collector = LogCollector::new(service)
    DistributedLogCoordinator::register_collector(log_coordinator, service, collector)
    log_collectors.set(service, collector)
  }
  
  // 创建各服务的logger
  let service_loggers = {}
  for service in services {
    let collector = log_collectors.get(service)
    let logger = LoggerProvider::get_logger(LoggerProvider::default(), service)
    service_loggers.set(service, logger)
  }
  
  // 模拟分布式请求的日志记录
  let request_id = "req-123456"
  let trace_id = "trace-abcdef123456"
  
  // API网关日志
  let api_logger = service_loggers.get("api-gateway")
  Logger::emit_log(api_logger, "INFO", "Request received", [
    ("request.id", request_id),
    ("trace.id", trace_id),
    ("endpoint", "/api/v1/orders"),
    ("method", "POST"),
    ("user.id", "user123")
  ])
  
  // 用户服务日志
  let user_logger = service_loggers.get("user-service")
  Logger::emit_log(user_logger, "INFO", "User validation started", [
    ("request.id", request_id),
    ("trace.id", trace_id),
    ("operation", "validate_user"),
    ("user.id", "user123")
  ])
  
  Logger::emit_log(user_logger, "INFO", "User validation completed", [
    ("request.id", request_id),
    ("trace.id", trace_id),
    ("operation", "validate_user"),
    ("user.id", "user123"),
    ("result", "success")
  ])
  
  // 订单服务日志
  let order_logger = service_loggers.get("order-service")
  Logger::emit_log(order_logger, "INFO", "Order processing started", [
    ("request.id", request_id),
    ("trace.id", trace_id),
    ("operation", "create_order"),
    ("order.id", "order456")
  ])
  
  Logger::emit_log(order_logger, "WARN", "Low inventory detected", [
    ("request.id", request_id),
    ("trace.id", trace_id),
    ("operation", "check_inventory"),
    ("product.id", "prod789"),
    ("available.quantity", "5")
  ])
  
  Logger::emit_log(order_logger, "INFO", "Order created", [
    ("request.id", request_id),
    ("trace.id", trace_id),
    ("operation", "create_order"),
    ("order.id", "order456"),
    ("result", "success")
  ])
  
  // 支付服务日志
  let payment_logger = service_loggers.get("payment-service")
  Logger::emit_log(payment_logger, "INFO", "Payment processing started", [
    ("request.id", request_id),
    ("trace.id", trace_id),
    ("operation", "process_payment"),
    ("payment.id", "pay123")
  ])
  
  Logger::emit_log(payment_logger, "ERROR", "Payment failed", [
    ("request.id", request_id),
    ("trace.id", trace_id),
    ("operation", "process_payment"),
    ("payment.id", "pay123"),
    ("error.code", "INSUFFICIENT_FUNDS"),
    ("error.message", "Insufficient funds in account")
  ])
  
  // 收集所有服务的日志
  let all_logs = DistributedLogCoordinator::collect_all_logs(log_coordinator)
  
  // 验证日志收集
  assert_true(all_logs.length() >= 7) // 至少7条日志
  
  // 按请求ID关联日志
  let request_logs = DistributedLogCoordinator::get_logs_by_request_id(log_coordinator, request_id)
  assert_eq(request_logs.length(), 7) // 所有日志应该有相同的request_id
  
  // 按trace ID关联日志
  let trace_logs = DistributedLogCoordinator::get_logs_by_trace_id(log_coordinator, trace_id)
  assert_eq(trace_logs.length(), 7) // 所有日志应该有相同的trace_id
  
  // 按服务分组日志
  let logs_by_service = DistributedLogCoordinator::group_logs_by_service(log_coordinator, request_id)
  assert_eq(logs_by_service.get("api-gateway").length(), 1)
  assert_eq(logs_by_service.get("user-service").length(), 2)
  assert_eq(logs_by_service.get("order-service").length(), 3)
  assert_eq(logs_by_service.get("payment-service").length(), 1)
  
  // 分析日志级别分布
  let log_level_distribution = DistributedLogCoordinator::analyze_log_levels(log_coordinator, request_id)
  assert_eq(log_level_distribution.get("INFO"), Some(6))
  assert_eq(log_level_distribution.get("WARN"), Some(1))
  assert_eq(log_level_distribution.get("ERROR"), Some(1))
  
  // 分析错误日志
  let error_logs = DistributedLogCoordinator::get_error_logs(log_coordinator, request_id)
  assert_eq(error_logs.length(), 1)
  
  let error_log = error_logs[0]
  assert_true(LogRecord::contains_attribute(error_log, "error.code"))
  assert_true(LogRecord::contains_attribute(error_log, "error.message"))
  
  // 生成分布式日志报告
  let log_report = DistributedLogCoordinator::generate_log_report(log_coordinator, request_id)
  assert_true(LogReport::total_logs(log_report) >= 7)
  assert_true(LogReport::services_involved(log_report) >= 4)
  assert_true(LogReport::error_count(log_report) >= 1)
  assert_true(LogReport::warning_count(log_report) >= 1)
  
  // 测试日志模式识别
  let common_patterns = DistributedLogCoordinator::identify_common_patterns(log_coordinator)
  assert_true(CommonPatterns::has_pattern(common_patterns, "user.validation"))
  assert_true(CommonPatterns::has_pattern(common_patterns, "order.processing"))
  assert_true(CommonPatterns::has_pattern(common_patterns, "payment.processing"))
  
  assert_true(true)
}

test "服务健康状态监控和告警" {
  // 创建跨服务健康监控协调器
  let health_coordinator = CrossServiceHealthCoordinator::new()
  
  // 配置服务健康检查
  let services = ["api-gateway", "user-service", "order-service", "payment-service", "inventory-service"]
  
  for service in services {
    let health_config = ServiceHealthConfig {
      check_interval: 30000,      // 30秒
      timeout: 5000,              // 5秒
      retry_count: 3,
      failure_threshold: 2,       // 连续2次失败后标记为不健康
      recovery_threshold: 2,      // 连续2次成功后标记为健康
      metrics_thresholds: {
        "response.time.p95" => 1000.0,  // 95%响应时间小于1秒
        "error.rate" => 0.05,           // 错误率小于5%
        "cpu.usage" => 0.8,             // CPU使用率小于80%
        "memory.usage" => 0.85          // 内存使用率小于85%
      }
    }
    
    CrossServiceHealthCoordinator::configure_service_health(health_coordinator, service, health_config)
  }
  
  // 创建健康检查器
  let health_checkers = {}
  for service in services {
    let checker = HealthChecker::new(service)
    CrossServiceHealthCoordinator::register_health_checker(health_coordinator, service, checker)
    health_checkers.set(service, checker)
  }
  
  // 模拟服务健康状态
  let api_gateway_checker = health_checkers.get("api-gateway")
  HealthChecker::simulate_health_status(api_gateway_checker, "healthy", {
    "response.time.p95": 850.0,
    "error.rate": 0.02,
    "cpu.usage": 0.65,
    "memory.usage": 0.75
  })
  
  let user_service_checker = health_checkers.get("user-service")
  HealthChecker::simulate_health_status(user_service_checker, "healthy", {
    "response.time.p95": 350.0,
    "error.rate": 0.01,
    "cpu.usage": 0.45,
    "memory.usage": 0.60
  })
  
  let order_service_checker = health_checkers.get("order-service")
  HealthChecker::simulate_health_status(order_service_checker, "degraded", {
    "response.time.p95": 1200.0,  // 超过阈值
    "error.rate": 0.03,
    "cpu.usage": 0.75,
    "memory.usage": 0.80
  })
  
  let payment_service_checker = health_checkers.get("payment-service")
  HealthChecker::simulate_health_status(payment_service_checker, "unhealthy", {
    "response.time.p95": 2000.0,  // 超过阈值
    "error.rate": 0.15,           // 超过阈值
    "cpu.usage": 0.90,            // 超过阈值
    "memory.usage": 0.95          // 超过阈值
  })
  
  let inventory_service_checker = health_checkers.get("inventory-service")
  HealthChecker::simulate_health_status(inventory_service_checker, "healthy", {
    "response.time.p95": 400.0,
    "error.rate": 0.01,
    "cpu.usage": 0.50,
    "memory.usage": 0.70
  })
  
  // 执行健康检查
  CrossServiceHealthCoordinator::execute_health_checks(health_coordinator)
  
  // 收集健康状态
  let health_status = CrossServiceHealthCoordinator::get_overall_health_status(health_coordinator)
  
  // 验证健康状态
  assert_eq(health_status.overall_status, "degraded") // 由于有服务处于degraded和unhealthy状态
  
  let service_statuses = health_status.service_statuses
  assert_eq(service_statuses.get("api-gateway").status, "healthy")
  assert_eq(service_statuses.get("user-service").status, "healthy")
  assert_eq(service_statuses.get("order-service").status, "degraded")
  assert_eq(service_statuses.get("payment-service").status, "unhealthy")
  assert_eq(service_statuses.get("inventory-service").status, "healthy")
  
  // 生成告警
  let alerts = CrossServiceHealthCoordinator::generate_alerts(health_coordinator)
  
  // 验证告警
  assert_true(alerts.length() >= 2) // 至少有2个告警（degraded和unhealthy服务）
  
  let degraded_alert = alerts.find(fn(alert) { 
    Alert::service_name(alert) == "order-service" && Alert::severity(alert) == "warning"
  })
  assert_not_eq(degraded_alert, None)
  
  let unhealthy_alert = alerts.find(fn(alert) { 
    Alert::service_name(alert) == "payment-service" && Alert::severity(alert) == "critical"
  })
  assert_not_eq(unhealthy_alert, None)
  
  // 测试服务依赖影响分析
  let dependency_impact = CrossServiceHealthCoordinator::analyze_dependency_impact(
    health_coordinator, 
    "payment-service",
    service_topology
  )
  
  assert_true(DependencyImpact::affected_services(dependency_impact).length() >= 1)
  assert_true(DependencyImpact::overall_impact_score(dependency_impact) > 0)
  
  // 测试自动恢复检测
  // 模拟支付服务恢复
  HealthChecker::simulate_health_status(payment_service_checker, "healthy", {
    "response.time.p95": 600.0,
    "error.rate": 0.02,
    "cpu.usage": 0.70,
    "memory.usage": 0.75
  })
  
  // 再次执行健康检查
  CrossServiceHealthCoordinator::execute_health_checks(health_coordinator)
  
  // 检查恢复状态
  let updated_health_status = CrossServiceHealthCoordinator::get_overall_health_status(health_coordinator)
  assert_eq(updated_health_status.service_statuses.get("payment-service").status, "healthy")
  
  // 生成健康报告
  let health_report = CrossServiceHealthCoordinator::generate_health_report(health_coordinator)
  assert_true(HealthReport::services_monitored(health_report) >= 5)
  assert_true(HealthReport::healthy_services(health_report) >= 3)
  assert_true(HealthReport::degraded_services(health_report) >= 1)
  assert_true(HealthReport::unhealthy_services(health_report) >= 0) // 支付服务已恢复
  assert_true(HealthReport::total_alerts(health_report) >= 1)
  
  assert_true(true)
}
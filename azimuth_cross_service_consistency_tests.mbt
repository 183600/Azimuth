// Azimuth Cross-Service Consistency Test Suite
// 跨服务一致性测试用例，专注于多个服务之间的遥测数据一致性，包括追踪链路、指标关联和日志关联

test "跨服务追踪链路一致性测试" {
  // 模拟微服务架构中的追踪链路
  let global_trace_id = "1234567890abcdef1234567890abcdef"
  
  // 网关服务
  let gateway_tracer_provider = azimuth::TracerProvider::default()
  let gateway_tracer = azimuth::TracerProvider::get_tracer(gateway_tracer_provider, "gateway_service")
  
  let gateway_span = azimuth::Tracer::start_span(gateway_tracer, "gateway_request")
  let gateway_span_context = azimuth::Span::span_context(gateway_span)
  
  // 验证网关服务Span
  assert_eq(azimuth::Span::name(gateway_span), "gateway_request")
  assert_true(azimuth::Span::is_recording(gateway_span))
  
  // 用户服务
  let user_tracer_provider = azimuth::TracerProvider::default()
  let user_tracer = azimuth::TracerProvider::get_tracer(user_tracer_provider, "user_service")
  
  let user_span = azimuth::Tracer::start_span(user_tracer, "user_validation")
  let user_span_context = azimuth::Span::span_context(user_span)
  
  // 验证用户服务Span
  assert_eq(azimuth::Span::name(user_span), "user_validation")
  assert_true(azimuth::Span::is_recording(user_span))
  
  // 订单服务
  let order_tracer_provider = azimuth::TracerProvider::default()
  let order_tracer = azimuth::TracerProvider::get_tracer(order_tracer_provider, "order_service")
  
  let order_span = azimuth::Tracer::start_span(order_tracer, "order_processing")
  let order_span_context = azimuth::Span::span_context(order_span)
  
  // 验证订单服务Span
  assert_eq(azimuth::Span::name(order_span), "order_processing")
  assert_true(azimuth::Span::is_recording(order_span))
  
  // 库存服务
  let inventory_tracer_provider = azimuth::TracerProvider::default()
  let inventory_tracer = azimuth::TracerProvider::get_tracer(inventory_tracer_provider, "inventory_service")
  
  let inventory_span = azimuth::Tracer::start_span(inventory_tracer, "inventory_check")
  let inventory_span_context = azimuth::Span::span_context(inventory_span)
  
  // 验证库存服务Span
  assert_eq(azimuth::Span::name(inventory_span), "inventory_check")
  assert_true(azimuth::Span::is_recording(inventory_span))
  
  // 支付服务
  let payment_tracer_provider = azimuth::TracerProvider::default()
  let payment_tracer = azimuth::TracerProvider::get_tracer(payment_tracer_provider, "payment_service")
  
  let payment_span = azimuth::Tracer::start_span(payment_tracer, "payment_processing")
  let payment_span_context = azimuth::Span::span_context(payment_span)
  
  // 验证支付服务Span
  assert_eq(azimuth::Span::name(payment_span), "payment_processing")
  assert_true(azimuth::Span::is_recording(payment_span))
  
  // 设置Span状态和事件
  azimuth::Span::set_status(gateway_span, azimuth::Ok, Some("网关请求处理成功"))
  azimuth::Span::add_event(gateway_span, "request_received", Some([
    ("http.method", StringValue("POST")),
    ("http.url", StringValue("/api/v1/orders")),
    ("user.id", StringValue("user123"))
  ]))
  
  azimuth::Span::set_status(user_span, azimuth::Ok, Some("用户验证成功"))
  azimuth::Span::add_event(user_span, "user_validated", Some([
    ("user.id", StringValue("user123")),
    ("validation.time_ms", IntValue(25))
  ]))
  
  azimuth::Span::set_status(order_span, azimuth::Ok, Some("订单处理成功"))
  azimuth::Span::add_event(order_span, "order_created", Some([
    ("order.id", StringValue("order456")),
    ("order.amount", FloatValue(99.99))
  ]))
  
  azimuth::Span::set_status(inventory_span, azimuth::Ok, Some("库存检查成功"))
  azimuth::Span::add_event(inventory_span, "inventory_checked", Some([
    ("product.id", StringValue("prod789")),
    ("stock.available", BoolValue(true)),
    ("stock.quantity", IntValue(10))
  ]))
  
  azimuth::Span::set_status(payment_span, azimuth::Error, Some("支付处理失败"))
  azimuth::Span::add_event(payment_span, "payment_failed", Some([
    ("payment.id", StringValue("pay101112")),
    ("error.code", StringValue("INSUFFICIENT_FUNDS")),
    ("error.message", StringValue("账户余额不足"))
  ]))
  
  // 结束所有Span
  azimuth::Span::end(gateway_span)
  azimuth::Span::end(user_span)
  azimuth::Span::end(order_span)
  azimuth::Span::end(inventory_span)
  azimuth::Span::end(payment_span)
  
  // 验证Span状态
  assert_eq(azimuth::Span::status(gateway_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(user_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(order_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(inventory_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(payment_span), azimuth::Error)
}

test "跨服务指标关联性测试" {
  // 创建各服务的Meter
  let gateway_meter_provider = azimuth::MeterProvider::default()
  let gateway_meter = azimuth::MeterProvider::get_meter(gateway_meter_provider, "gateway_service")
  
  let user_meter_provider = azimuth::MeterProvider::default()
  let user_meter = azimuth::MeterProvider::get_meter(user_meter_provider, "user_service")
  
  let order_meter_provider = azimuth::MeterProvider::default()
  let order_meter = azimuth::MeterProvider::get_meter(order_meter_provider, "order_service")
  
  let inventory_meter_provider = azimuth::MeterProvider::default()
  let inventory_meter = azimuth::MeterProvider::get_meter(inventory_meter_provider, "inventory_service")
  
  let payment_meter_provider = azimuth::MeterProvider::default()
  let payment_meter = azimuth::MeterProvider::get_meter(payment_meter_provider, "payment_service")
  
  // 创建各服务的指标
  let gateway_request_counter = azimuth::Meter::create_counter(gateway_meter, "requests_total")
  let gateway_duration_histogram = azimuth::Meter::create_histogram(gateway_meter, "request_duration_ms")
  
  let user_validation_counter = azimuth::Meter::create_counter(user_meter, "validations_total")
  let user_cache_hits = azimuth::Meter::create_counter(user_meter, "cache_hits_total")
  
  let order_creation_counter = azimuth::Meter::create_counter(order_meter, "orders_created_total")
  let order_amount_histogram = azimuth::Meter::create_histogram(order_meter, "order_amount")
  
  let inventory_check_counter = azimuth::Meter::create_counter(inventory_meter, "inventory_checks_total")
  let stock_level_gauge = azimuth::Meter::create_gauge(inventory_meter, "stock_level")
  
  let payment_attempts_counter = azimuth::Meter::create_counter(payment_meter, "payment_attempts_total")
  let payment_failures_counter = azimuth::Meter::create_counter(payment_meter, "payment_failures_total")
  
  // 记录指标值
  azimuth::Counter::add(gateway_request_counter, 1.0)
  azimuth::Histogram::record(gateway_duration_histogram, 150.0)
  
  azimuth::Counter::add(user_validation_counter, 1.0)
  azimuth::Counter::add(user_cache_hits, 1.0)
  
  azimuth::Counter::add(order_creation_counter, 1.0)
  azimuth::Histogram::record(order_amount_histogram, 99.99)
  
  azimuth::Counter::add(inventory_check_counter, 1.0)
  azimuth::Gauge::record(stock_level_gauge, 10.0)
  
  azimuth::Counter::add(payment_attempts_counter, 1.0)
  azimuth::Counter::add(payment_failures_counter, 1.0)
  
  // 验证指标名称
  assert_eq(azimuth::Counter::name(gateway_request_counter), "requests_total")
  assert_eq(azimuth::Histogram::name(gateway_duration_histogram), "request_duration_ms")
  assert_eq(azimuth::Counter::name(user_validation_counter), "validations_total")
  assert_eq(azimuth::Counter::name(user_cache_hits), "cache_hits_total")
  assert_eq(azimuth::Counter::name(order_creation_counter), "orders_created_total")
  assert_eq(azimuth::Histogram::name(order_amount_histogram), "order_amount")
  assert_eq(azimuth::Counter::name(inventory_check_counter), "inventory_checks_total")
  assert_eq(azimuth::Gauge::name(stock_level_gauge), "stock_level")
  assert_eq(azimuth::Counter::name(payment_attempts_counter), "payment_attempts_total")
  assert_eq(azimuth::Counter::name(payment_failures_counter), "payment_failures_total")
  
  // 创建带属性的指标记录
  let gateway_attrs = azimuth::Attributes::new()
  let user_attrs = azimuth::Attributes::new()
  let order_attrs = azimuth::Attributes::new()
  let inventory_attrs = azimuth::Attributes::new()
  let payment_attrs = azimuth::Attributes::new()
  
  // 记录带属性的指标
  azimuth::Counter::add(gateway_request_counter, 1.0, Some(gateway_attrs))
  azimuth::Counter::add(user_validation_counter, 1.0, Some(user_attrs))
  azimuth::Counter::add(order_creation_counter, 1.0, Some(order_attrs))
  azimuth::Counter::add(inventory_check_counter, 1.0, Some(inventory_attrs))
  azimuth::Counter::add(payment_attempts_counter, 1.0, Some(payment_attrs))
  azimuth::Counter::add(payment_failures_counter, 1.0, Some(payment_attrs))
}

test "跨服务日志关联性测试" {
  // 创建各服务的Logger
  let gateway_logger_provider = azimuth::LoggerProvider::default()
  let gateway_logger = azimuth::LoggerProvider::get_logger(gateway_logger_provider, "gateway_service")
  
  let user_logger_provider = azimuth::LoggerProvider::default()
  let user_logger = azimuth::LoggerProvider::get_logger(user_logger_provider, "user_service")
  
  let order_logger_provider = azimuth::LoggerProvider::default()
  let order_logger = azimuth::LoggerProvider::get_logger(order_logger_provider, "order_service")
  
  let inventory_logger_provider = azimuth::LoggerProvider::default()
  let inventory_logger = azimuth::LoggerProvider::get_logger(inventory_logger_provider, "inventory_service")
  
  let payment_logger_provider = azimuth::LoggerProvider::default()
  let payment_logger = azimuth::LoggerProvider::get_logger(payment_logger_provider, "payment_service")
  
  // 共享的追踪ID
  let global_trace_id = "fedcba0987654321fedcba0987654321"
  
  // 网关服务日志
  let gateway_log1 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("接收订单创建请求"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some(global_trace_id),
    Some("gateway_span_001"),
    None
  )
  
  let gateway_log2 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("请求验证通过，转发到用户服务"),
    None,
    Some(1735689600000001000L),
    Some(1735689600000001000L),
    Some(global_trace_id),
    Some("gateway_span_001"),
    None
  )
  
  // 用户服务日志
  let user_log1 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("验证用户权限"),
    None,
    Some(1735689600000002000L),
    Some(1735689600000002000L),
    Some(global_trace_id),
    Some("user_span_001"),
    None
  )
  
  let user_log2 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("用户验证成功，权限有效"),
    None,
    Some(1735689600000003000L),
    Some(1735689600000003000L),
    Some(global_trace_id),
    Some("user_span_001"),
    None
  )
  
  // 订单服务日志
  let order_log1 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("创建订单记录"),
    None,
    Some(1735689600000004000L),
    Some(1735689600000004000L),
    Some(global_trace_id),
    Some("order_span_001"),
    None
  )
  
  let order_log2 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("订单创建成功，订单ID: ORDER-12345"),
    None,
    Some(1735689600000005000L),
    Some(1735689600000005000L),
    Some(global_trace_id),
    Some("order_span_001"),
    None
  )
  
  // 库存服务日志
  let inventory_log1 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("检查商品库存"),
    None,
    Some(1735689600000006000L),
    Some(1735689600000006000L),
    Some(global_trace_id),
    Some("inventory_span_001"),
    None
  )
  
  let inventory_log2 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("库存充足，库存数量: 10"),
    None,
    Some(1735689600000007000L),
    Some(1735689600000007000L),
    Some(global_trace_id),
    Some("inventory_span_001"),
    None
  )
  
  // 支付服务日志
  let payment_log1 = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("处理支付请求"),
    None,
    Some(1735689600000008000L),
    Some(1735689600000008000L),
    Some(global_trace_id),
    Some("payment_span_001"),
    None
  )
  
  let payment_log2 = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("支付处理失败: 账户余额不足"),
    None,
    Some(1735689600000009000L),
    Some(1735689600000009000L),
    Some(global_trace_id),
    Some("payment_span_001"),
    None
  )
  
  // 验证所有日志的追踪关联
  let all_logs = [
    gateway_log1, gateway_log2,
    user_log1, user_log2,
    order_log1, order_log2,
    inventory_log1, inventory_log2,
    payment_log1, payment_log2
  ]
  
  for log in all_logs {
    assert_eq(azimuth::LogRecord::trace_id(log), Some(global_trace_id))
  }
  
  // 验证日志严重级别
  assert_eq(azimuth::LogRecord::severity_number(gateway_log1), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(user_log2), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(order_log2), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(inventory_log2), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(payment_log2), azimuth::Error)
  
  // 发出所有日志记录
  azimuth::Logger::emit(gateway_logger, gateway_log1)
  azimuth::Logger::emit(gateway_logger, gateway_log2)
  azimuth::Logger::emit(user_logger, user_log1)
  azimuth::Logger::emit(user_logger, user_log2)
  azimuth::Logger::emit(order_logger, order_log1)
  azimuth::Logger::emit(order_logger, order_log2)
  azimuth::Logger::emit(inventory_logger, inventory_log1)
  azimuth::Logger::emit(inventory_logger, inventory_log2)
  azimuth::Logger::emit(payment_logger, payment_log1)
  azimuth::Logger::emit(payment_logger, payment_log2)
}

test "跨服务资源一致性测试" {
  // 创建各服务的资源
  let gateway_resource = azimuth::Resource::new()
  let gateway_attrs = [
    ("service.name", StringValue("gateway-service")),
    ("service.version", StringValue("1.2.0")),
    ("service.instance.id", StringValue("gateway-pod-001")),
    ("deployment.environment", StringValue("production")),
    ("k8s.namespace", StringValue("azimuth-prod")),
    ("k8s.pod.name", StringValue("gateway-7f8d9c2b-4a5e-6f7g-8h9i-0j1k2l3m4n5o"))
  ]
  
  let gateway_resource_with_attrs = azimuth::Resource::with_attributes(gateway_resource, gateway_attrs)
  
  let user_resource = azimuth::Resource::new()
  let user_attrs = [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("user-pod-003")),
    ("deployment.environment", StringValue("production")),
    ("k8s.namespace", StringValue("azimuth-prod")),
    ("k8s.pod.name", StringValue("user-3a4b5c6d-7e8f-9g0h-1i2j-3k4l5m6n7o8p"))
  ]
  
  let user_resource_with_attrs = azimuth::Resource::with_attributes(user_resource, user_attrs)
  
  let order_resource = azimuth::Resource::new()
  let order_attrs = [
    ("service.name", StringValue("order-service")),
    ("service.version", StringValue("3.0.1")),
    ("service.instance.id", StringValue("order-pod-002")),
    ("deployment.environment", StringValue("production")),
    ("k8s.namespace", StringValue("azimuth-prod")),
    ("k8s.pod.name", StringValue("order-9c8d7e6f-5g4h-3i2j-1k0l-m9n8o7p6q5r4"))
  ]
  
  let order_resource_with_attrs = azimuth::Resource::with_attributes(order_resource, order_attrs)
  
  let inventory_resource = azimuth::Resource::new()
  let inventory_attrs = [
    ("service.name", StringValue("inventory-service")),
    ("service.version", StringValue("1.5.2")),
    ("service.instance.id", StringValue("inventory-pod-001")),
    ("deployment.environment", StringValue("production")),
    ("k8s.namespace", StringValue("azimuth-prod")),
    ("k8s.pod.name", StringValue("inventory-2b3c4d5e-6f7g-8h9i-0j1k-2l3m4n5o6p7q"))
  ]
  
  let inventory_resource_with_attrs = azimuth::Resource::with_attributes(inventory_resource, inventory_attrs)
  
  let payment_resource = azimuth::Resource::new()
  let payment_attrs = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("2.3.1")),
    ("service.instance.id", StringValue("payment-pod-004")),
    ("deployment.environment", StringValue("production")),
    ("k8s.namespace", StringValue("azimuth-prod")),
    ("k8s.pod.name", StringValue("payment-8d7e6f5g-4h3i-2j1k-0l9m-n8o7p6q5r4s3"))
  ]
  
  let payment_resource_with_attrs = azimuth::Resource::with_attributes(payment_resource, payment_attrs)
  
  // 验证各服务的资源属性
  assert_eq(azimuth::Resource::get_attribute(gateway_resource_with_attrs, "service.name"), Some(StringValue("gateway-service")))
  assert_eq(azimuth::Resource::get_attribute(gateway_resource_with_attrs, "deployment.environment"), Some(StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(gateway_resource_with_attrs, "k8s.namespace"), Some(StringValue("azimuth-prod")))
  
  assert_eq(azimuth::Resource::get_attribute(user_resource_with_attrs, "service.name"), Some(StringValue("user-service")))
  assert_eq(azimuth::Resource::get_attribute(user_resource_with_attrs, "deployment.environment"), Some(StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(user_resource_with_attrs, "k8s.namespace"), Some(StringValue("azimuth-prod")))
  
  assert_eq(azimuth::Resource::get_attribute(order_resource_with_attrs, "service.name"), Some(StringValue("order-service")))
  assert_eq(azimuth::Resource::get_attribute(order_resource_with_attrs, "deployment.environment"), Some(StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(order_resource_with_attrs, "k8s.namespace"), Some(StringValue("azimuth-prod")))
  
  assert_eq(azimuth::Resource::get_attribute(inventory_resource_with_attrs, "service.name"), Some(StringValue("inventory-service")))
  assert_eq(azimuth::Resource::get_attribute(inventory_resource_with_attrs, "deployment.environment"), Some(StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(inventory_resource_with_attrs, "k8s.namespace"), Some(StringValue("azimuth-prod")))
  
  assert_eq(azimuth::Resource::get_attribute(payment_resource_with_attrs, "service.name"), Some(StringValue("payment-service")))
  assert_eq(azimuth::Resource::get_attribute(payment_resource_with_attrs, "deployment.environment"), Some(StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(payment_resource_with_attrs, "k8s.namespace"), Some(StringValue("azimuth-prod")))
  
  // 验证跨服务的共同属性
  let all_resources = [
    gateway_resource_with_attrs,
    user_resource_with_attrs,
    order_resource_with_attrs,
    inventory_resource_with_attrs,
    payment_resource_with_attrs
  ]
  
  for resource in all_resources {
    assert_eq(azimuth::Resource::get_attribute(resource, "deployment.environment"), Some(StringValue("production")))
    assert_eq(azimuth::Resource::get_attribute(resource, "k8s.namespace"), Some(StringValue("azimuth-prod")))
  }
}

test "跨服务端到端一致性测试" {
  // 模拟完整的端到端请求流程
  let e2e_trace_id = "abc123def456ghi789jkl012mno345pqr"
  
  // 1. 客户端请求 -> 网关
  let gateway_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "gateway_service")
  let gateway_span = azimuth::Tracer::start_span(gateway_tracer, "gateway_request")
  
  let gateway_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "gateway_service")
  let gateway_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("接收客户端请求"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some(e2e_trace_id),
    Some("gateway_span_001"),
    None
  )
  
  let gateway_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "gateway_service")
  let gateway_counter = azimuth::Meter::create_counter(gateway_meter, "requests_total")
  
  // 2. 网关 -> 用户服务
  let user_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "user_service")
  let user_span = azimuth::Tracer::start_span(user_tracer, "user_validation")
  
  let user_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "user_service")
  let user_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("验证用户身份"),
    None,
    Some(1735689600000001000L),
    Some(1735689600000001000L),
    Some(e2e_trace_id),
    Some("user_span_001"),
    None
  )
  
  let user_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "user_service")
  let user_counter = azimuth::Meter::create_counter(user_meter, "validations_total")
  
  // 3. 用户服务 -> 订单服务
  let order_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "order_service")
  let order_span = azimuth::Tracer::start_span(order_tracer, "order_creation")
  
  let order_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "order_service")
  let order_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("创建订单"),
    None,
    Some(1735689600000002000L),
    Some(1735689600000002000L),
    Some(e2e_trace_id),
    Some("order_span_001"),
    None
  )
  
  let order_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "order_service")
  let order_counter = azimuth::Meter::create_counter(order_meter, "orders_created_total")
  
  // 4. 订单服务 -> 库存服务
  let inventory_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "inventory_service")
  let inventory_span = azimuth::Tracer::start_span(inventory_tracer, "inventory_check")
  
  let inventory_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "inventory_service")
  let inventory_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("检查库存"),
    None,
    Some(1735689600000003000L),
    Some(1735689600000003000L),
    Some(e2e_trace_id),
    Some("inventory_span_001"),
    None
  )
  
  let inventory_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "inventory_service")
  let inventory_counter = azimuth::Meter::create_counter(inventory_meter, "inventory_checks_total")
  
  // 5. 库存服务 -> 支付服务
  let payment_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "payment_service")
  let payment_span = azimuth::Tracer::start_span(payment_tracer, "payment_processing")
  
  let payment_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "payment_service")
  let payment_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("处理支付"),
    None,
    Some(1735689600000004000L),
    Some(1735689600000004000L),
    Some(e2e_trace_id),
    Some("payment_span_001"),
    None
  )
  
  let payment_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "payment_service")
  let payment_counter = azimuth::Meter::create_counter(payment_meter, "payment_attempts_total")
  
  // 记录所有遥测数据
  azimuth::Counter::add(gateway_counter, 1.0)
  azimuth::Counter::add(user_counter, 1.0)
  azimuth::Counter::add(order_counter, 1.0)
  azimuth::Counter::add(inventory_counter, 1.0)
  azimuth::Counter::add(payment_counter, 1.0)
  
  azimuth::Logger::emit(gateway_logger, gateway_log)
  azimuth::Logger::emit(user_logger, user_log)
  azimuth::Logger::emit(order_logger, order_log)
  azimuth::Logger::emit(inventory_logger, inventory_log)
  azimuth::Logger::emit(payment_logger, payment_log)
  
  // 设置Span状态
  azimuth::Span::set_status(gateway_span, azimuth::Ok)
  azimuth::Span::set_status(user_span, azimuth::Ok)
  azimuth::Span::set_status(order_span, azimuth::Ok)
  azimuth::Span::set_status(inventory_span, azimuth::Ok)
  azimuth::Span::set_status(payment_span, azimuth::Ok)
  
  // 结束所有Span
  azimuth::Span::end(gateway_span)
  azimuth::Span::end(user_span)
  azimuth::Span::end(order_span)
  azimuth::Span::end(inventory_span)
  azimuth::Span::end(payment_span)
  
  // 验证端到端一致性
  let all_spans = [gateway_span, user_span, order_span, inventory_span, payment_span]
  let all_logs = [gateway_log, user_log, order_log, inventory_log, payment_log]
  
  // 验证所有Span状态
  for span in all_spans {
    assert_eq(azimuth::Span::status(span), azimuth::Ok)
  }
  
  // 验证所有日志的追踪关联
  for log in all_logs {
    assert_eq(azimuth::LogRecord::trace_id(log), Some(e2e_trace_id))
  }
  
  // 验证指标名称
  assert_eq(azimuth::Counter::name(gateway_counter), "requests_total")
  assert_eq(azimuth::Counter::name(user_counter), "validations_total")
  assert_eq(azimuth::Counter::name(order_counter), "orders_created_total")
  assert_eq(azimuth::Counter::name(inventory_counter), "inventory_checks_total")
  assert_eq(azimuth::Counter::name(payment_counter), "payment_attempts_total")
}
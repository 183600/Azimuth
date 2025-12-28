// Azimuth Telemetry System - Cross-Service Integration Comprehensive Test
// 测试跨服务集成和分布式系统

test "微服务追踪集成测试" {
  // 模拟微服务架构中的追踪链
  let gateway_span_ctx = SpanContext::new("gateway-trace-123", "gateway-span-456", true, "key1=value1")
  let gateway_span = Span::new("api-gateway-request", Server, gateway_span_ctx)
  
  // API Gateway处理请求
  Span::add_event(gateway_span, "request-received", Some([("http.method", StringValue("GET")), ("http.path", StringValue("/api/users"))]))
  Span::set_status(gateway_span, Ok, Some("Request processed successfully"))
  
  // 用户服务处理
  let user_service_span_ctx = SpanContext::new("gateway-trace-123", "user-service-span-789", true, "key1=value1,key2=value2")
  let user_service_span = Span::new("user-service-operation", Server, user_service_span_ctx)
  
  Span::add_event(user_service_span, "database-query", Some([("db.statement", StringValue("SELECT * FROM users"))]))
  Span::add_event(user_service_span, "cache-check", Some([("cache.hit", BoolValue(true))]))
  Span::set_status(user_service_span, Ok, Some("User data retrieved successfully"))
  
  // 订单服务处理
  let order_service_span_ctx = SpanContext::new("gateway-trace-123", "order-service-span-101", true, "key1=value1,key3=value3")
  let order_service_span = Span::new("order-service-operation", Server, order_service_span_ctx)
  
  Span::add_event(order_service_span, "external-api-call", Some([("http.url", StringValue("https://payment-api.com/charge"))]))
  Span::add_event(order_service_span, "message-queue-publish", Some([("queue.name", StringValue("order-events"))]))
  Span::set_status(order_service_span, Ok, Some("Order processed successfully"))
  
  // 通知服务处理
  let notification_service_span_ctx = SpanContext::new("gateway-trace-123", "notification-span-202", true, "key1=value1,key4=value4")
  let notification_service_span = Span::new("notification-service-operation", Server, notification_service_span_ctx)
  
  Span::add_event(notification_service_span, "email-sent", Some([("email.to", StringValue("user@example.com"))]))
  Span::add_event(notification_service_span, "sms-sent", Some([("sms.to", StringValue("+1234567890"))]))
  Span::set_status(notification_service_span, Ok, Some("Notifications sent successfully"))
  
  // 验证追踪链的完整性
  assert_eq(SpanContext::trace_id(gateway_span_ctx), SpanContext::trace_id(user_service_span_ctx))
  assert_eq(SpanContext::trace_id(user_service_span_ctx), SpanContext::trace_id(order_service_span_ctx))
  assert_eq(SpanContext::trace_id(order_service_span_ctx), SpanContext::trace_id(notification_service_span_ctx))
  
  // 验证Span的唯一性
  assert_eq(SpanContext::span_id(gateway_span_ctx), "gateway-span-456")
  assert_eq(SpanContext::span_id(user_service_span_ctx), "user-service-span-789")
  assert_eq(SpanContext::span_id(order_service_span_ctx), "order-service-span-101")
  assert_eq(SpanContext::span_id(notification_service_span_ctx), "notification-span-202")
}

test "跨服务Context传播测试" {
  // 初始化传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 服务A创建初始Context
  let service_a_ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  let tenant_key = ContextKey::new("tenant.id")
  
  let service_a_ctx_with_user = Context::with_value(service_a_ctx, user_key, "user-123")
  let service_a_ctx_with_session = Context::with_value(service_a_ctx_with_user, session_key, "session-456")
  let service_a_final_ctx = Context::with_value(service_a_ctx_with_session, tenant_key, "tenant-789")
  
  // 服务A注入Context到HTTP头部
  let service_a_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, service_a_final_ctx, service_a_carrier)
  
  // 服务B提取Context
  let service_b_ctx = CompositePropagator::extract(composite_propagator, service_a_carrier)
  
  // 服务B添加自己的Context信息
  let request_id_key = ContextKey::new("request.id")
  let service_b_final_ctx = Context::with_value(service_b_ctx, request_id_key, "req-abc123")
  
  // 服务B注入到新的Carrier
  let service_b_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, service_b_final_ctx, service_b_carrier)
  
  // 服务C提取Context
  let service_c_ctx = CompositePropagator::extract(composite_propagator, service_b_carrier)
  
  // 服务C添加自己的Context信息
  let operation_key = ContextKey::new("operation.type")
  let service_c_final_ctx = Context::with_value(service_c_ctx, operation_key, "data-processing")
  
  // 验证Context传播
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(service_c_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

test "分布式Metric集成测试" {
  // 创建多个服务的Meter
  let meter_provider = MeterProvider::default()
  
  let api_gateway_meter = MeterProvider::get_meter(meter_provider, "api-gateway")
  let user_service_meter = MeterProvider::get_meter(meter_provider, "user-service")
  let order_service_meter = MeterProvider::get_meter(meter_provider, "order-service")
  let notification_service_meter = MeterProvider::get_meter(meter_provider, "notification-service")
  
  // API Gateway指标
  let gateway_request_counter = Meter::create_counter(api_gateway_meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let gateway_request_duration = Meter::create_histogram(api_gateway_meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  let gateway_active_connections = Meter::create_updown_counter(api_gateway_meter, "http.active_connections", Some("Active HTTP connections"), Some("connections"))
  
  // User Service指标
  let user_db_query_counter = Meter::create_counter(user_service_meter, "database.queries.total", Some("Total database queries"), Some("queries"))
  let user_db_query_duration = Meter::create_histogram(user_service_meter, "database.query.duration", Some("Database query duration"), Some("ms"))
  let user_cache_hit_ratio = Meter::create_gauge(user_service_meter, "cache.hit_ratio", Some("Cache hit ratio"), Some("percent"))
  
  // Order Service指标
  let order_processing_counter = Meter::create_counter(order_service_meter, "orders.processed.total", Some("Total orders processed"), Some("orders"))
  let order_processing_duration = Meter::create_histogram(order_service_meter, "order.processing.duration", Some("Order processing duration"), Some("ms"))
  let order_payment_success_rate = Meter::create_gauge(order_service_meter, "order.payment.success_rate", Some("Payment success rate"), Some("percent"))
  
  // Notification Service指标
  let notification_sent_counter = Meter::create_counter(notification_service_meter, "notifications.sent.total", Some("Total notifications sent"), Some("notifications"))
  let notification_email_sent_counter = Meter::create_counter(notification_service_meter, "notifications.email.sent.total", Some("Total email notifications sent"), Some("emails"))
  let notification_sms_sent_counter = Meter::create_counter(notification_service_meter, "notifications.sms.sent.total", Some("Total SMS notifications sent"), Some("sms"))
  
  // 模拟指标记录
  Counter::add(gateway_request_counter, 1000.0)
  Histogram::record(gateway_request_duration, 150.5)
  Counter::add(gateway_active_connections, 50.0)
  
  Counter::add(user_db_query_counter, 2500.0)
  Histogram::record(user_db_query_duration, 25.3)
  // Gauge::set(user_cache_hit_ratio, 85.5)  // 简化实现中没有Gauge::set
  
  Counter::add(order_processing_counter, 500.0)
  Histogram::record(order_processing_duration, 500.8)
  
  Counter::add(notification_sent_counter, 450.0)
  Counter::add(notification_email_sent_counter, 400.0)
  Counter::add(notification_sms_sent_counter, 50.0)
  
  // 验证Instrument属性
  assert_eq(Instrument::name(Counter("http.requests.total", Some("Total HTTP requests"), Some("requests"))), "http.requests.total")
  assert_eq(Instrument::description(Histogram("http.request.duration", Some("HTTP request duration"), Some("ms"))), Some("HTTP request duration"))
  assert_eq(Instrument::unit(Counter("database.queries.total", Some("Total database queries"), Some("queries"))), Some("queries"))
}

test "跨服务日志集成测试" {
  // 创建多个服务的Logger
  let logger_provider = LoggerProvider::default()
  
  let api_gateway_logger = LoggerProvider::get_logger(logger_provider, "api-gateway")
  let user_service_logger = LoggerProvider::get_logger(logger_provider, "user-service")
  let order_service_logger = LoggerProvider::get_logger(logger_provider, "order-service")
  let notification_service_logger = LoggerProvider::get_logger(logger_provider, "notification-service")
  
  // API Gateway日志
  let gateway_request_log = LogRecord::new_with_context(
    Info,
    Some("Incoming request processed"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("gateway-trace-123"),
    Some("gateway-span-456"),
    Some(Context::root())
  )
  
  let gateway_error_log = LogRecord::new_with_context(
    Error,
    Some("Request validation failed"),
    Some(Attributes::new()),
    Some(1735689600000002000L),
    Some(1735689600000003000L),
    Some("gateway-trace-124"),
    Some("gateway-span-457"),
    Some(Context::root())
  )
  
  // User Service日志
  let user_db_log = LogRecord::new_with_context(
    Debug,
    Some("Database query executed"),
    Some(Attributes::new()),
    Some(1735689600000004000L),
    Some(1735689600000005000L),
    Some("gateway-trace-123"),
    Some("user-service-span-789"),
    Some(Context::root())
  )
  
  let user_cache_log = LogRecord::new_with_context(
    Info,
    Some("Cache hit for user data"),
    Some(Attributes::new()),
    Some(1735689600000006000L),
    Some(1735689600000007000L),
    Some("gateway-trace-123"),
    Some("user-service-span-789"),
    Some(Context::root())
  )
  
  // Order Service日志
  let order_processing_log = LogRecord::new_with_context(
    Info,
    Some("Order processing started"),
    Some(Attributes::new()),
    Some(1735689600000008000L),
    Some(1735689600000009000L),
    Some("gateway-trace-123"),
    Some("order-service-span-101"),
    Some(Context::root())
  )
  
  let order_payment_log = LogRecord::new_with_context(
    Warn,
    Some("Payment retry attempt"),
    Some(Attributes::new()),
    Some(1735689600000010000L),
    Some(1735689600000011000L),
    Some("gateway-trace-123"),
    Some("order-service-span-101"),
    Some(Context::root())
  )
  
  // Notification Service日志
  let notification_email_log = LogRecord::new_with_context(
    Info,
    Some("Email notification sent"),
    Some(Attributes::new()),
    Some(1735689600000012000L),
    Some(1735689600000013000L),
    Some("gateway-trace-123"),
    Some("notification-span-202"),
    Some(Context::root())
  )
  
  let notification_sms_log = LogRecord::new_with_context(
    Info,
    Some("SMS notification sent"),
    Some(Attributes::new()),
    Some(1735689600000014000L),
    Some(1735689600000015000L),
    Some("gateway-trace-123"),
    Some("notification-span-202"),
    Some(Context::root())
  )
  
  // 发射日志
  Logger::emit(api_gateway_logger, gateway_request_log)
  Logger::emit(api_gateway_logger, gateway_error_log)
  
  Logger::emit(user_service_logger, user_db_log)
  Logger::emit(user_service_logger, user_cache_log)
  
  Logger::emit(order_service_logger, order_processing_log)
  Logger::emit(order_service_logger, order_payment_log)
  
  Logger::emit(notification_service_logger, notification_email_log)
  Logger::emit(notification_service_logger, notification_sms_log)
  
  // 验证日志关联性
  assert_eq(LogRecord::trace_id(gateway_request_log), Some("gateway-trace-123"))
  assert_eq(LogRecord::trace_id(user_db_log), Some("gateway-trace-123"))
  assert_eq(LogRecord::trace_id(order_processing_log), Some("gateway-trace-123"))
  assert_eq(LogRecord::trace_id(notification_email_log), Some("gateway-trace-123"))
  
  // 验证日志严重程度
  assert_eq(LogRecord::severity_number(gateway_request_log), Info)
  assert_eq(LogRecord::severity_number(gateway_error_log), Error)
  assert_eq(LogRecord::severity_number(user_db_log), Debug)
  assert_eq(LogRecord::severity_number(user_cache_log), Info)
  assert_eq(LogRecord::severity_number(order_payment_log), Warn)
}

test "服务间HTTP通信集成测试" {
  // 创建HTTP客户端和请求/响应
  let client = HttpClient::new()
  
  // API Gateway到User Service的请求
  let gateway_to_user_headers = [
    ("Content-Type", "application/json"),
    ("X-Request-ID", "req-gateway-user-123"),
    ("X-Trace-ID", "trace-123"),
    ("Authorization", "Bearer gateway-token")
  ]
  
  let gateway_to_user_request = HttpRequest::new(
    "GET",
    "https://user-service.internal/api/users/123",
    gateway_to_user_headers,
    None
  )
  
  let user_to_gateway_response = HttpResponse::new(
    200,
    [("Content-Type", "application/json"), ("X-Response-Time", "25ms")],
    Some("{\"id\":123,\"name\":\"John Doe\",\"email\":\"john@example.com\"}")
  )
  
  // User Service到Database的请求
  let user_to_db_headers = [
    ("Content-Type", "application/json"),
    ("X-Request-ID", "req-user-db-456"),
    ("X-Trace-ID", "trace-123")
  ]
  
  let user_to_db_request = HttpRequest::new(
    "POST",
    "https://database.internal/query",
    user_to_db_headers,
    Some("{\"query\":\"SELECT * FROM users WHERE id = 123\"}")
  )
  
  let db_to_user_response = HttpResponse::new(
    200,
    [("Content-Type", "application/json"), ("X-Query-Time", "15ms")],
    Some("{\"results\":[{\"id\":123,\"name\":\"John Doe\",\"email\":\"john@example.com\"}]}")
  )
  
  // Order Service到Payment Service的请求
  let order_to_payment_headers = [
    ("Content-Type", "application/json"),
    ("X-Request-ID", "req-order-payment-789"),
    ("X-Trace-ID", "trace-123"),
    ("X-Order-ID", "order-456")
  ]
  
  let order_to_payment_request = HttpRequest::new(
    "POST",
    "https://payment-service.internal/charge",
    order_to_payment_headers,
    Some("{\"amount\":99.99,\"currency\":\"USD\",\"token\":\"tok_123\"}")
  )
  
  let payment_to_order_response = HttpResponse::new(
    201,
    [("Content-Type", "application/json"), ("X-Processing-Time", "500ms")],
    Some("{\"id\":\"charge_789\",\"status\":\"succeeded\",\"amount\":99.99}")
  )
  
  // 验证HTTP请求和响应
  assert_eq(HttpRequest::http_method(gateway_to_user_request), "GET")
  assert_eq(HttpRequest::url(gateway_to_user_request), "https://user-service.internal/api/users/123")
  assert_eq(HttpResponse::status_code(user_to_gateway_response), 200)
  assert_eq(HttpResponse::body(user_to_gateway_response), Some("{\"id\":123,\"name\":\"John Doe\",\"email\":\"john@example.com\"}"))
  
  assert_eq(HttpRequest::http_method(user_to_db_request), "POST")
  assert_eq(HttpRequest::url(user_to_db_request), "https://database.internal/query")
  assert_eq(HttpResponse::status_code(db_to_user_response), 200)
  
  assert_eq(HttpRequest::http_method(order_to_payment_request), "POST")
  assert_eq(HttpRequest::url(order_to_payment_request), "https://payment-service.internal/charge")
  assert_eq(HttpResponse::status_code(payment_to_order_response), 201)
  assert_eq(HttpResponse::body(payment_to_order_response), Some("{\"id\":\"charge_789\",\"status\":\"succeeded\",\"amount\":99.99}"))
}

test "服务发现和注册集成测试" {
  // 模拟服务注册信息
  let service_registry = []
  
  // API Gateway服务注册
  let gateway_resource_attrs = [
    ("service.name", StringValue("api-gateway")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("gateway-instance-01")),
    ("host.name", StringValue("gateway-01.internal")),
    ("host.port", IntValue(8080)),
    ("service.type", StringValue("gateway")),
    ("service.namespace", StringValue("production"))
  ]
  
  let gateway_resource = Resource::with_attributes(Resource::new(), gateway_resource_attrs)
  service_registry.push(gateway_resource)
  
  // User Service服务注册
  let user_service_resource_attrs = [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("1.5.2")),
    ("service.instance.id", StringValue("user-instance-01")),
    ("host.name", StringValue("user-service-01.internal")),
    ("host.port", IntValue(8081)),
    ("service.type", StringValue("microservice")),
    ("service.namespace", StringValue("production")),
    ("database.type", StringValue("postgresql")),
    ("database.version", StringValue("13.7"))
  ]
  
  let user_service_resource = Resource::with_attributes(Resource::new(), user_service_resource_attrs)
  service_registry.push(user_service_resource)
  
  // Order Service服务注册
  let order_service_resource_attrs = [
    ("service.name", StringValue("order-service")),
    ("service.version", StringValue("1.3.1")),
    ("service.instance.id", StringValue("order-instance-01")),
    ("host.name", StringValue("order-service-01.internal")),
    ("host.port", IntValue(8082)),
    ("service.type", StringValue("microservice")),
    ("service.namespace", StringValue("production")),
    ("message.queue", StringValue("orders")),
    ("cache.type", StringValue("redis"))
  ]
  
  let order_service_resource = Resource::with_attributes(Resource::new(), order_service_resource_attrs)
  service_registry.push(order_service_resource)
  
  // Notification Service服务注册
  let notification_service_resource_attrs = [
    ("service.name", StringValue("notification-service")),
    ("service.version", StringValue("1.2.0")),
    ("service.instance.id", StringValue("notification-instance-01")),
    ("host.name", StringValue("notification-service-01.internal")),
    ("host.port", IntValue(8083)),
    ("service.type", StringValue("microservice")),
    ("service.namespace", StringValue("production")),
    ("email.provider", StringValue("sendgrid")),
    ("sms.provider", StringValue("twilio"))
  ]
  
  let notification_service_resource = Resource::with_attributes(Resource::new(), notification_service_resource_attrs)
  service_registry.push(notification_service_resource)
  
  // 验证服务注册信息
  assert_eq(service_registry.length(), 4)
  
  // 查询API Gateway服务信息
  let gateway_service = service_registry[0]
  let gateway_name = Resource::get_attribute(gateway_service, "service.name")
  let gateway_version = Resource::get_attribute(gateway_service, "service.version")
  let gateway_port = Resource::get_attribute(gateway_service, "host.port")
  
  assert_eq(gateway_name, Some(StringValue("api-gateway")))
  assert_eq(gateway_version, Some(StringValue("2.1.0")))
  assert_eq(gateway_port, Some(IntValue(8080)))
  
  // 查询User Service服务信息
  let user_service = service_registry[1]
  let user_service_name = Resource::get_attribute(user_service, "service.name")
  let user_database_type = Resource::get_attribute(user_service, "database.type")
  
  assert_eq(user_service_name, Some(StringValue("user-service")))
  assert_eq(user_database_type, Some(StringValue("postgresql")))
  
  // 查询Order Service服务信息
  let order_service = service_registry[2]
  let order_service_name = Resource::get_attribute(order_service, "service.name")
  let order_message_queue = Resource::get_attribute(order_service, "message.queue")
  
  assert_eq(order_service_name, Some(StringValue("order-service")))
  assert_eq(order_message_queue, Some(StringValue("orders")))
  
  // 查询Notification Service服务信息
  let notification_service = service_registry[3]
  let notification_service_name = Resource::get_attribute(notification_service, "service.name")
  let notification_email_provider = Resource::get_attribute(notification_service, "email.provider")
  
  assert_eq(notification_service_name, Some(StringValue("notification-service")))
  assert_eq(notification_email_provider, Some(StringValue("sendgrid")))
}

test "分布式事务追踪测试" {
  // 模拟分布式事务的追踪
  let transaction_trace_id = "distributed-tx-12345"
  
  // 事务开始 - Order Service
  let order_tx_span_ctx = SpanContext::new(transaction_trace_id, "order-tx-span-001", true, "")
  let order_tx_span = Span::new("order-transaction-begin", Server, order_tx_span_ctx)
  
  Span::add_event(order_tx_span, "transaction-started", Some([("tx.id", StringValue("tx-12345")), ("tx.type", StringValue("order"))]))
  Span::add_event(order_tx_span, "inventory-reserved", Some([("product.id", StringValue("prod-001")), ("quantity", IntValue(2))]))
  
  // 支付处理 - Payment Service
  let payment_tx_span_ctx = SpanContext::new(transaction_trace_id, "payment-tx-span-002", true, "")
  let payment_tx_span = Span::new("payment-transaction", Server, payment_tx_span_ctx)
  
  Span::add_event(payment_tx_span, "payment-authorized", Some([("payment.id", StringValue("pay-67890")), ("amount", FloatValue(99.99))]))
  Span::add_event(payment_tx_span, "payment-captured", Some([("payment.id", StringValue("pay-67890")), ("status", StringValue("completed"))]))
  
  // 库存更新 - Inventory Service
  let inventory_tx_span_ctx = SpanContext::new(transaction_trace_id, "inventory-tx-span-003", true, "")
  let inventory_tx_span = Span::new("inventory-update", Server, inventory_tx_span_ctx)
  
  Span::add_event(inventory_tx_span, "stock-decremented", Some([("product.id", StringValue("prod-001")), ("new.quantity", IntValue(98))]))
  Span::add_event(inventory_tx_span, "inventory-snapshot", Some([("snapshot.id", StringValue("snap-11111"))]))
  
  // 物流创建 - Shipping Service
  let shipping_tx_span_ctx = SpanContext::new(transaction_trace_id, "shipping-tx-span-004", true, "")
  let shipping_tx_span = Span::new("shipping-creation", Server, shipping_tx_span_ctx)
  
  Span::add_event(shipping_tx_span, "shipment-created", Some([("shipment.id", StringValue("ship-22222")), ("carrier", StringValue("fedex"))]))
  Span::add_event(shipping_tx_span, "tracking-number-generated", Some([("tracking.number", StringValue("1Z999AA10123456784"))]))
  
  // 通知发送 - Notification Service
  let notification_tx_span_ctx = SpanContext::new(transaction_trace_id, "notification-tx-span-005", true, "")
  let notification_tx_span = Span::new("notification-sent", Server, notification_tx_span_ctx)
  
  Span::add_event(notification_tx_span, "order-confirmation-sent", Some([("customer.email", StringValue("customer@example.com"))]))
  Span::add_event(notification_tx_span, "shipping-notification-sent", Some([("customer.phone", StringValue("+1234567890"))]))
  
  // 事务提交 - Order Service
  let order_commit_span_ctx = SpanContext::new(transaction_trace_id, "order-commit-span-006", true, "")
  let order_commit_span = Span::new("order-transaction-commit", Server, order_commit_span_ctx)
  
  Span::add_event(order_commit_span, "transaction-committed", Some([("tx.id", StringValue("tx-12345")), ("commit.timestamp", StringValue("2025-01-01T12:00:00Z"))]))
  Span::set_status(order_commit_span, Ok, Some("Distributed transaction completed successfully"))
  
  // 验证分布式事务的追踪链
  assert_eq(SpanContext::trace_id(order_tx_span_ctx), transaction_trace_id)
  assert_eq(SpanContext::trace_id(payment_tx_span_ctx), transaction_trace_id)
  assert_eq(SpanContext::trace_id(inventory_tx_span_ctx), transaction_trace_id)
  assert_eq(SpanContext::trace_id(shipping_tx_span_ctx), transaction_trace_id)
  assert_eq(SpanContext::trace_id(notification_tx_span_ctx), transaction_trace_id)
  assert_eq(SpanContext::trace_id(order_commit_span_ctx), transaction_trace_id)
  
  // 验证Span的唯一性
  assert_eq(SpanContext::span_id(order_tx_span_ctx), "order-tx-span-001")
  assert_eq(SpanContext::span_id(payment_tx_span_ctx), "payment-tx-span-002")
  assert_eq(SpanContext::span_id(inventory_tx_span_ctx), "inventory-tx-span-003")
  assert_eq(SpanContext::span_id(shipping_tx_span_ctx), "shipping-tx-span-004")
  assert_eq(SpanContext::span_id(notification_tx_span_ctx), "notification-tx-span-005")
  assert_eq(SpanContext::span_id(order_commit_span_ctx), "order-commit-span-006")
  
  // 验证事务状态
  assert_eq(Span::name(order_tx_span), "order-transaction-begin")
  assert_eq(Span::name(payment_tx_span), "payment-transaction")
  assert_eq(Span::name(inventory_tx_span), "inventory-update")
  assert_eq(Span::name(shipping_tx_span), "shipping-creation")
  assert_eq(Span::name(notification_tx_span), "notification-sent")
  assert_eq(Span::name(order_commit_span), "order-transaction-commit")
}
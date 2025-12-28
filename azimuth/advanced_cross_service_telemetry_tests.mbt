// 高级跨链路遥测测试用例
// 测试跨服务和分布式系统的复杂遥测场景

pub test "distributed trace with multiple service hops" {
  // 模拟分布式请求流：API Gateway -> Auth Service -> User Service -> Order Service
  
  // API Gateway创建根span
  let gateway_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "api-gateway"
  )
  
  let gateway_span = azimuth::Tracer::start_span(gateway_tracer, "gateway.process-request")
  let gateway_span_ctx = azimuth::Span::span_context(gateway_span)
  
  // 验证gateway span
  assert_eq(azimuth::Span::name(gateway_span), "gateway.process-request")
  assert_true(azimuth::SpanContext::is_valid(gateway_span_ctx))
  
  // 添加gateway事件
  azimuth::Span::add_event(gateway_span, "Request received from client")
  azimuth::Span::add_event(gateway_span, "Routing to auth service")
  
  // Auth Service创建子span
  let auth_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "auth-service"
  )
  
  let auth_span = azimuth::Tracer::start_span(auth_tracer, "auth.validate-token")
  azimuth::Span::add_event(auth_span, "Token validation started")
  azimuth::Span::add_event(auth_span, "Token validated successfully")
  azimuth::Span::set_status(auth_span, azimuth::Ok)
  azimuth::Span::end(auth_span)
  
  // User Service创建子span
  let user_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "user-service"
  )
  
  let user_span = azimuth::Tracer::start_span(user_tracer, "user.get-profile")
  azimuth::Span::add_event(user_span, "Database query started")
  azimuth::Span::add_event(user_span, "User profile retrieved")
  azimuth::Span::set_status(user_span, azimuth::Ok)
  azimuth::Span::end(user_span)
  
  // Order Service创建子span
  let order_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "order-service"
  )
  
  let order_span = azimuth::Tracer::start_span(order_tracer, "order.create-order")
  azimuth::Span::add_event(order_span, "Order validation started")
  azimuth::Span::add_event(order_span, "Payment processing started")
  azimuth::Span::add_event(order_span, "Order created successfully")
  azimuth::Span::set_status(order_span, azimuth::Ok)
  azimuth::Span::end(order_span)
  
  // 完成gateway span
  azimuth::Span::add_event(gateway_span, "All services completed successfully")
  azimuth::Span::set_status(gateway_span, azimuth::Ok)
  azimuth::Span::end(gateway_span)
  
  // 验证所有span的层次结构
  assert_eq(azimuth::Span::name(auth_span), "auth.validate-token")
  assert_eq(azimuth::Span::name(user_span), "user.get-profile")
  assert_eq(azimuth::Span::name(order_span), "order.create-order")
}

pub test "cross-service baggage propagation" {
  // 模拟跨服务的baggage传播
  
  // Service A设置初始baggage
  let service_a_baggage = azimuth::Baggage::new()
  let service_a_with_user = azimuth::Baggage::set_entry(
    service_a_baggage, 
    "user.id", 
    "user-12345"
  )
  let service_a_with_session = azimuth::Baggage::set_entry(
    service_a_with_user, 
    "session.id", 
    "session-67890"
  )
  let service_a_with_tenant = azimuth::Baggage::set_entry(
    service_a_with_session, 
    "tenant.id", 
    "tenant-abc123"
  )
  
  // 验证Service A的baggage
  assert_eq(azimuth::Baggage::get_entry(service_a_with_tenant, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(service_a_with_tenant, "session.id"), Some("session-67890"))
  assert_eq(azimuth::Baggage::get_entry(service_a_with_tenant, "tenant.id"), Some("tenant-abc123"))
  
  // Service B添加baggage条目
  let service_b_baggage = azimuth::Baggage::set_entry(
    service_a_with_tenant, 
    "request.id", 
    "req-def456"
  )
  let service_b_with_correlation = azimuth::Baggage::set_entry(
    service_b_baggage, 
    "correlation.id", 
    "corr-ghi789"
  )
  
  // 验证Service B的baggage（包含Service A的条目）
  assert_eq(azimuth::Baggage::get_entry(service_b_with_correlation, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(service_b_with_correlation, "session.id"), Some("session-67890"))
  assert_eq(azimuth::Baggage::get_entry(service_b_with_correlation, "tenant.id"), Some("tenant-abc123"))
  assert_eq(azimuth::Baggage::get_entry(service_b_with_correlation, "request.id"), Some("req-def456"))
  assert_eq(azimuth::Baggage::get_entry(service_b_with_correlation, "correlation.id"), Some("corr-ghi789"))
  
  // Service C添加更多baggage条目
  let service_c_baggage = azimuth::Baggage::set_entry(
    service_b_with_correlation, 
    "service.c.timestamp", 
    "2025-12-28T14:30:00Z"
  )
  let service_c_with_context = azimuth::Baggage::set_entry(
    service_c_baggage, 
    "business.context", 
    "order.processing"
  )
  
  // 验证Service C的baggage（包含所有前面的条目）
  assert_eq(azimuth::Baggage::get_entry(service_c_with_context, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(service_c_with_context, "session.id"), Some("session-67890"))
  assert_eq(azimuth::Baggage::get_entry(service_c_with_context, "tenant.id"), Some("tenant-abc123"))
  assert_eq(azimuth::Baggage::get_entry(service_c_with_context, "request.id"), Some("req-def456"))
  assert_eq(azimuth::Baggage::get_entry(service_c_with_context, "correlation.id"), Some("corr-ghi789"))
  assert_eq(azimuth::Baggage::get_entry(service_c_with_context, "service.c.timestamp"), Some("2025-12-28T14:30:00Z"))
  assert_eq(azimuth::Baggage::get_entry(service_c_with_context, "business.context"), Some("order.processing"))
}

pub test "distributed logging with trace correlation" {
  // 模拟分布式系统的日志记录和trace关联
  
  // 创建共享的trace ID
  let trace_id = "distributed-trace-12345678901234567890123456789012"
  
  // API Gateway日志
  let gateway_logger_provider = azimuth::LoggerProvider::default()
  let gateway_logger = azimuth::LoggerProvider::get_logger(
    gateway_logger_provider, 
    "api-gateway-logger"
  )
  
  let gateway_log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(gateway_log_attrs, "service.name", azimuth::StringValue("api-gateway"))
  azimuth::Attributes::set(gateway_log_attrs, "service.version", azimuth::StringValue("1.0.0"))
  azimuth::Attributes::set(gateway_log_attrs, "request.method", azimuth::StringValue("POST"))
  azimuth::Attributes::set(gateway_log_attrs, "request.path", azimuth::StringValue("/api/v1/orders"))
  azimuth::Attributes::set(gateway_log_attrs, "client.ip", azimuth::StringValue("203.0.113.1"))
  
  let gateway_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("API Gateway: Processing order creation request"),
    Some(gateway_log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some("gateway-span-123"),
    Some(azimuth::Context::root())
  )
  
  // Auth Service日志
  let auth_logger_provider = azimuth::LoggerProvider::default()
  let auth_logger = azimuth::LoggerProvider::get_logger(
    auth_logger_provider, 
    "auth-service-logger"
  )
  
  let auth_log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(auth_log_attrs, "service.name", azimuth::StringValue("auth-service"))
  azimuth::Attributes::set(auth_log_attrs, "auth.method", azimuth::StringValue("oauth2"))
  azimuth::Attributes::set(auth_log_attrs, "auth.user_id", azimuth::StringValue("user-123"))
  azimuth::Attributes::set(auth_log_attrs, "auth.success", azimuth::BoolValue(true))
  azimuth::Attributes::set(auth_log_attrs, "auth.duration_ms", azimuth::IntValue(25))
  
  let auth_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Auth Service: User authentication successful"),
    Some(auth_log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some("auth-span-456"),
    Some(azimuth::Context::root())
  )
  
  // Order Service日志
  let order_logger_provider = azimuth::LoggerProvider::default()
  let order_logger = azimuth::LoggerProvider::get_logger(
    order_logger_provider, 
    "order-service-logger"
  )
  
  let order_log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(order_log_attrs, "service.name", azimuth::StringValue("order-service"))
  azimuth::Attributes::set(order_log_attrs, "order.id", azimuth::StringValue("order-789"))
  azimuth::Attributes::set(order_log_attrs, "order.amount", azimuth::FloatValue(99.99))
  azimuth::Attributes::set(order_log_attrs, "order.currency", azimuth::StringValue("USD"))
  azimuth::Attributes::set(order_log_attrs, "order.status", azimuth::StringValue("created"))
  azimuth::Attributes::set(order_log_attrs, "db.query_count", azimuth::IntValue(3))
  azimuth::Attributes::set(order_log_attrs, "db.total_time_ms", azimuth::IntValue(45))
  
  let order_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Order Service: Order created successfully"),
    Some(order_log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some("order-span-789"),
    Some(azimuth::Context::root())
  )
  
  // Payment Service日志
  let payment_logger_provider = azimuth::LoggerProvider::default()
  let payment_logger = azimuth::LoggerProvider::get_logger(
    payment_logger_provider, 
    "payment-service-logger"
  )
  
  let payment_log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(payment_log_attrs, "service.name", azimuth::StringValue("payment-service"))
  azimuth::Attributes::set(payment_log_attrs, "payment.id", azimuth::StringValue("payment-012"))
  azimuth::Attributes::set(payment_log_attrs, "payment.method", azimuth::StringValue("credit_card"))
  azimuth::Attributes::set(payment_log_attrs, "payment.provider", azimuth::StringValue("stripe"))
  azimuth::Attributes::set(payment_log_attrs, "payment.status", azimuth::StringValue("completed"))
  azimuth::Attributes::set(payment_log_attrs, "payment.amount", azimuth::FloatValue(99.99))
  azimuth::Attributes::set(payment_log_attrs, "payment.fee", azimuth::FloatValue(2.99))
  
  let payment_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Payment Service: Payment processed successfully"),
    Some(payment_log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some("payment-span-012"),
    Some(azimuth::Context::root())
  )
  
  // Notification Service日志
  let notification_logger_provider = azimuth::LoggerProvider::default()
  let notification_logger = azimuth::LoggerProvider::get_logger(
    notification_logger_provider, 
    "notification-service-logger"
  )
  
  let notification_log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(notification_log_attrs, "service.name", azimuth::StringValue("notification-service"))
  azimuth::Attributes::set(notification_log_attrs, "notification.type", azimuth::StringValue("email"))
  azimuth::Attributes::set(notification_log_attrs, "notification.recipient", azimuth::StringValue("user@example.com"))
  azimuth::Attributes::set(notification_log_attrs, "notification.template", azimuth::StringValue("order_confirmation"))
  azimuth::Attributes::set(notification_log_attrs, "notification.status", azimuth::StringValue("sent"))
  azimuth::Attributes::set(notification_log_attrs, "notification.delivery_time_ms", azimuth::IntValue(150))
  
  let notification_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Notification Service: Order confirmation email sent"),
    Some(notification_log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some("notification-span-345"),
    Some(azimuth::Context::root())
  )
  
  // 验证所有日志记录具有相同的trace ID
  assert_eq(azimuth::LogRecord::trace_id(gateway_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::trace_id(auth_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::trace_id(order_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::trace_id(payment_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::trace_id(notification_log), Some(trace_id))
  
  // 验证每个服务有不同的span ID
  assert_eq(azimuth::LogRecord::span_id(gateway_log), Some("gateway-span-123"))
  assert_eq(azimuth::LogRecord::span_id(auth_log), Some("auth-span-456"))
  assert_eq(azimuth::LogRecord::span_id(order_log), Some("order-span-789"))
  assert_eq(azimuth::LogRecord::span_id(payment_log), Some("payment-span-012"))
  assert_eq(azimuth::LogRecord::span_id(notification_log), Some("notification-span-345"))
  
  // 发射所有日志
  azimuth::Logger::emit(gateway_logger, gateway_log)
  azimuth::Logger::emit(auth_logger, auth_log)
  azimuth::Logger::emit(order_logger, order_log)
  azimuth::Logger::emit(payment_logger, payment_log)
  azimuth::Logger::emit(notification_logger, notification_log)
}

pub test "cross-service metrics aggregation" {
  // 模拟跨服务的指标聚合
  
  // 各个服务创建相同的指标以便聚合
  
  // API Gateway指标
  let gateway_meter_provider = azimuth::MeterProvider::default()
  let gateway_meter = azimuth::MeterProvider::get_meter(
    gateway_meter_provider, 
    "api-gateway-metrics"
  )
  
  let gateway_request_counter = azimuth::Meter::create_counter(
    gateway_meter, 
    "http.requests.total",
    Some("Total HTTP requests across all services"),
    Some("requests")
  )
  
  let gateway_response_histogram = azimuth::Meter::create_histogram(
    gateway_meter, 
    "http.response.duration",
    Some("HTTP response duration across all services"),
    Some("ms")
  )
  
  // Auth Service指标
  let auth_meter_provider = azimuth::MeterProvider::default()
  let auth_meter = azimuth::MeterProvider::get_meter(
    auth_meter_provider, 
    "auth-service-metrics"
  )
  
  let auth_request_counter = azimuth::Meter::create_counter(
    auth_meter, 
    "http.requests.total",
    Some("Total HTTP requests across all services"),
    Some("requests")
  )
  
  let auth_response_histogram = azimuth::Meter::create_histogram(
    auth_meter, 
    "http.response.duration",
    Some("HTTP response duration across all services"),
    Some("ms")
  )
  
  // Order Service指标
  let order_meter_provider = azimuth::MeterProvider::default()
  let order_meter = azimuth::MeterProvider::get_meter(
    order_meter_provider, 
    "order-service-metrics"
  )
  
  let order_request_counter = azimuth::Meter::create_counter(
    order_meter, 
    "http.requests.total",
    Some("Total HTTP requests across all services"),
    Some("requests")
  )
  
  let order_response_histogram = azimuth::Meter::create_histogram(
    order_meter, 
    "http.response.duration",
    Some("HTTP response duration across all services"),
    Some("ms")
  )
  
  // Payment Service指标
  let payment_meter_provider = azimuth::MeterProvider::default()
  let payment_meter = azimuth::MeterProvider::get_meter(
    payment_meter_provider, 
    "payment-service-metrics"
  )
  
  let payment_request_counter = azimuth::Meter::create_counter(
    payment_meter, 
    "http.requests.total",
    Some("Total HTTP requests across all services"),
    Some("requests")
  )
  
  let payment_response_histogram = azimuth::Meter::create_histogram(
    payment_meter, 
    "http.response.duration",
    Some("HTTP response duration across all services"),
    Some("ms")
  )
  
  // 记录各服务的指标
  
  // API Gateway: 100个请求，平均响应时间50ms
  azimuth::Counter::add(gateway_request_counter, 100.0)
  for i in 0..100 {
    azimuth::Histogram::record(gateway_response_histogram, 50.0)
  }
  
  // Auth Service: 95个请求，平均响应时间25ms
  azimuth::Counter::add(auth_request_counter, 95.0)
  for i in 0..95 {
    azimuth::Histogram::record(auth_response_histogram, 25.0)
  }
  
  // Order Service: 80个请求，平均响应时间150ms
  azimuth::Counter::add(order_request_counter, 80.0)
  for i in 0..80 {
    azimuth::Histogram::record(order_response_histogram, 150.0)
  }
  
  // Payment Service: 75个请求，平均响应时间300ms
  azimuth::Counter::add(payment_request_counter, 75.0)
  for i in 0..75 {
    azimuth::Histogram::record(payment_response_histogram, 300.0)
  }
  
  // 验证所有指标具有相同的名称和描述
  assert_eq(gateway_request_counter.name, "http.requests.total")
  assert_eq(auth_request_counter.name, "http.requests.total")
  assert_eq(order_request_counter.name, "http.requests.total")
  assert_eq(payment_request_counter.name, "http.requests.total")
  
  assert_eq(gateway_request_counter.description, Some("Total HTTP requests across all services"))
  assert_eq(auth_request_counter.description, Some("Total HTTP requests across all services"))
  assert_eq(order_request_counter.description, Some("Total HTTP requests across all services"))
  assert_eq(payment_request_counter.description, Some("Total HTTP requests across all services"))
  
  assert_eq(gateway_response_histogram.name, "http.response.duration")
  assert_eq(auth_response_histogram.name, "http.response.duration")
  assert_eq(order_response_histogram.name, "http.response.duration")
  assert_eq(payment_response_histogram.name, "http.response.duration")
  
  // 创建业务指标
  let business_meter_provider = azimuth::MeterProvider::default()
  let business_meter = azimuth::MeterProvider::get_meter(
    business_meter_provider, 
    "business-metrics"
  )
  
  let order_counter = azimuth::Meter::create_counter(
    business_meter, 
    "business.orders.total",
    Some("Total orders processed"),
    Some("orders")
  )
  
  let revenue_counter = azimuth::Meter::create_counter(
    business_meter, 
    "business.revenue.total",
    Some("Total revenue generated"),
    Some("currency_units")
  )
  
  let error_counter = azimuth::Meter::create_counter(
    business_meter, 
    "business.errors.total",
    Some("Total errors encountered"),
    Some("errors")
  )
  
  // 记录业务指标
  azimuth::Counter::add(order_counter, 75.0)      // 75个订单
  azimuth::Counter::add(revenue_counter, 7499.25) // $7,499.25收入
  azimuth::Counter::add(error_counter, 5.0)       // 5个错误
  
  // 验证业务指标
  assert_eq(order_counter.name, "business.orders.total")
  assert_eq(revenue_counter.name, "business.revenue.total")
  assert_eq(error_counter.name, "business.errors.total")
}

pub test "distributed error handling and recovery" {
  // 模拟分布式系统中的错误处理和恢复
  
  // 创建错误追踪的trace
  let error_trace_id = "error-trace-12345678901234567890123456789012"
  
  // Service A遇到错误
  let service_a_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "service-a"
  )
  
  let service_a_span = azimuth::Tracer::start_span(service_a_tracer, "service-a.operation")
  azimuth::Span::add_event(service_a_span, "Operation started")
  azimuth::Span::add_event(service_a_span, "Database connection failed")
  azimuth::Span::set_status(service_a_span, azimuth::Error, Some("Database connection timeout"))
  azimuth::Span::end(service_a_span)
  
  // Service A的错误日志
  let service_a_logger_provider = azimuth::LoggerProvider::default()
  let service_a_logger = azimuth::LoggerProvider::get_logger(
    service_a_logger_provider, 
    "service-a-logger"
  )
  
  let service_a_error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_a_error_attrs, "service.name", azimuth::StringValue("service-a"))
  azimuth::Attributes::set(service_a_error_attrs, "error.type", azimuth::StringValue("DatabaseConnectionError"))
  azimuth::Attributes::set(service_a_error_attrs, "error.message", azimuth::StringValue("Connection timeout after 30 seconds"))
  azimuth::Attributes::set(service_a_error_attrs, "error.code", azimuth::IntValue(5001))
  azimuth::Attributes::set(service_a_error_attrs, "error.retry_count", azimuth::IntValue(3))
  azimuth::Attributes::set(service_a_error_attrs, "database.host", azimuth::StringValue("db-primary.example.com"))
  azimuth::Attributes::set(service_a_error_attrs, "database.port", azimuth::IntValue(5432))
  
  let service_a_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Service A: Database operation failed"),
    Some(service_a_error_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(error_trace_id),
    Some("service-a-error-span"),
    Some(azimuth::Context::root())
  )
  
  // Service B尝试恢复
  let service_b_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "service-b"
  )
  
  let service_b_span = azimuth::Tracer::start_span(service_b_tracer, "service-b.fallback-operation")
  azimuth::Span::add_event(service_b_span, "Fallback operation started")
  azimuth::Span::add_event(service_b_span, "Using backup database")
  azimuth::Span::add_event(service_b_span, "Fallback operation completed successfully")
  azimuth::Span::set_status(service_b_span, azimuth::Ok)
  azimuth::Span::end(service_b_span)
  
  // Service B的恢复日志
  let service_b_logger_provider = azimuth::LoggerProvider::default()
  let service_b_logger = azimuth::LoggerProvider::get_logger(
    service_b_logger_provider, 
    "service-b-logger"
  )
  
  let service_b_recovery_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_b_recovery_attrs, "service.name", azimuth::StringValue("service-b"))
  azimuth::Attributes::set(service_b_recovery_attrs, "recovery.action", azimuth::StringValue("fallback_database"))
  azimuth::Attributes::set(service_b_recovery_attrs, "recovery.success", azimuth::BoolValue(true))
  azimuth::Attributes::set(service_b_recovery_attrs, "recovery.duration_ms", azimuth::IntValue(150))
  azimuth::Attributes::set(service_b_recovery_attrs, "backup.database.host", azimuth::StringValue("db-backup.example.com"))
  
  let service_b_recovery_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Service B: Fallback operation completed successfully"),
    Some(service_b_recovery_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(error_trace_id),
    Some("service-b-recovery-span"),
    Some(azimuth::Context::root())
  )
  
  // Service C处理最终结果
  let service_c_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "service-c"
  )
  
  let service_c_span = azimuth::Tracer::start_span(service_c_tracer, "service-c.final-processing")
  azimuth::Span::add_event(service_c_span, "Final processing started")
  azimuth::Span::add_event(service_c_span, "Processing completed with degraded service")
  azimuth::Span::set_status(service_c_span, azimuth::Ok)
  azimuth::Span::end(service_c_span)
  
  // Service C的最终日志
  let service_c_logger_provider = azimuth::LoggerProvider::default()
  let service_c_logger = azimuth::LoggerProvider::get_logger(
    service_c_logger_provider, 
    "service-c-logger"
  )
  
  let service_c_final_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_c_final_attrs, "service.name", azimuth::StringValue("service-c"))
  azimuth::Attributes::set(service_c_final_attrs, "operation.status", azimuth::StringValue("completed_with_degradation"))
  azimuth::Attributes::set(service_c_final_attrs, "operation.degraded", azimuth::BoolValue(true))
  azimuth::Attributes::set(service_c_final_attrs, "operation.impact", azimuth::StringValue("minimal"))
  azimuth::Attributes::set(service_c_final_attrs, "user.affected", azimuth::BoolValue(false))
  
  let service_c_final_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service C: Operation completed with service degradation"),
    Some(service_c_final_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(error_trace_id),
    Some("service-c-final-span"),
    Some(azimuth::Context::root())
  )
  
  // 验证错误处理流程
  assert_eq(azimuth::Span::status(service_a_span), azimuth::Error)
  assert_eq(azimuth::Span::status(service_b_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(service_c_span), azimuth::Ok)
  
  assert_eq(azimuth::LogRecord::severity_number(service_a_error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(service_b_recovery_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(service_c_final_log), azimuth::Info)
  
  // 验证所有日志具有相同的trace ID
  assert_eq(azimuth::LogRecord::trace_id(service_a_error_log), Some(error_trace_id))
  assert_eq(azimuth::LogRecord::trace_id(service_b_recovery_log), Some(error_trace_id))
  assert_eq(azimuth::LogRecord::trace_id(service_c_final_log), Some(error_trace_id))
  
  // 发射所有日志
  azimuth::Logger::emit(service_a_logger, service_a_error_log)
  azimuth::Logger::emit(service_b_logger, service_b_recovery_log)
  azimuth::Logger::emit(service_c_logger, service_c_final_log)
  
  // 创建错误指标
  let error_meter_provider = azimuth::MeterProvider::default()
  let error_meter = azimuth::MeterProvider::get_meter(
    error_meter_provider, 
    "error-metrics"
  )
  
  let error_counter = azimuth::Meter::create_counter(
    error_meter, 
    "errors.total",
    Some("Total errors encountered"),
    Some("errors")
  )
  
  let recovery_counter = azimuth::Meter::create_counter(
    error_meter, 
    "recoveries.total",
    Some("Total successful recoveries"),
    Some("recoveries")
  )
  
  let degradation_gauge = azimuth::Meter::create_gauge(
    error_meter, 
    "service.degradation.level",
    Some("Current service degradation level"),
    Some("percent")
  )
  
  // 记录错误和恢复指标
  azimuth::Counter::add(error_counter, 1.0)
  azimuth::Counter::add(recovery_counter, 1.0)
  
  // 验证错误指标
  assert_eq(error_counter.name, "errors.total")
  assert_eq(recovery_counter.name, "recoveries.total")
  assert_eq(degradation_gauge.name, "service.degradation.level")
}

pub test "cross-service performance monitoring" {
  // 模拟跨服务性能监控
  
  // 创建性能监控的trace
  let perf_trace_id = "perf-trace-12345678901234567890123456789012"
  
  // 各服务创建性能span
  let services = [
    ("api-gateway", "gateway.request.processing"),
    ("auth-service", "auth.token.validation"),
    ("user-service", "user.profile.fetch"),
    ("inventory-service", "inventory.availability.check"),
    ("order-service", "order.creation"),
    ("payment-service", "payment.processing"),
    ("notification-service", "notification.sending")
  ]
  
  let mut spans = []
  let mut loggers = []
  
  // 为每个服务创建tracer、span和logger
  for (service_name, operation_name) in services {
    let tracer = azimuth::TracerProvider::get_tracer(
      azimuth::TracerProvider::default(), 
      service_name
    )
    
    let span = azimuth::Tracer::start_span(tracer, operation_name)
    spans.push(span)
    
    let logger = azimuth::LoggerProvider::get_logger(
      azimuth::LoggerProvider::default(), 
      service_name + "-perf-logger"
    )
    loggers.push(logger)
  }
  
  // 模拟不同服务的处理时间
  let processing_times = [10, 25, 45, 30, 120, 200, 50]  // 毫秒
  
  // 为每个span添加性能相关事件和日志
  for i in 0..spans.length() {
    let span = spans[i]
    let logger = loggers[i]
    let service_name = services[i].0
    let processing_time = processing_times[i]
    
    // 添加性能事件
    azimuth::Span::add_event(span, "Processing started")
    azimuth::Span::add_event(span, "Processing completed in " + processing_time.to_string() + "ms")
    azimuth::Span::set_status(span, azimuth::Ok)
    azimuth::Span::end(span)
    
    // 创建性能日志
    let perf_attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(perf_attrs, "service.name", azimuth::StringValue(service_name))
    azimuth::Attributes::set(perf_attrs, "operation.duration_ms", azimuth::IntValue(processing_time))
    azimuth::Attributes::set(perf_attrs, "operation.status", azimuth::StringValue("completed"))
    azimuth::Attributes::set(perf_attrs, "operation.type", azimuth::StringValue("performance_monitored"))
    
    // 添加系统资源使用情况
    azimuth::Attributes::set(perf_attrs, "system.cpu.usage_percent", azimuth::FloatValue(processing_time.to_double() / 10.0))
    azimuth::Attributes::set(perf_attrs, "system.memory.used_mb", azimuth::IntValue(512 + processing_time * 2))
    azimuth::Attributes::set(perf_attrs, "system.disk.read_ops", azimuth::IntValue(processing_time / 5))
    azimuth::Attributes::set(perf_attrs, "system.network.bytes_sent", azimuth::IntValue(1024 * processing_time / 10))
    
    let perf_log = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Performance: " + service_name + " operation completed"),
      Some(perf_attrs),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some(perf_trace_id),
      Some(service_name + "-perf-span"),
      Some(azimuth::Context::root())
    )
    
    // 验证性能日志
    assert_eq(azimuth::LogRecord::trace_id(perf_log), Some(perf_trace_id))
    assert_eq(azimuth::LogRecord::body(perf_log), Some("Performance: " + service_name + " operation completed"))
    
    // 发射性能日志
    azimuth::Logger::emit(logger, perf_log)
  }
  
  // 创建性能指标
  let perf_meter_provider = azimuth::MeterProvider::default()
  let perf_meter = azimuth::MeterProvider::get_meter(
    perf_meter_provider, 
    "performance-metrics"
  )
  
  let duration_histogram = azimuth::Meter::create_histogram(
    perf_meter, 
    "operation.duration",
    Some("Operation duration across all services"),
    Some("ms")
  )
  
  let throughput_counter = azimuth::Meter::create_counter(
    perf_meter, 
    "operations.total",
    Some("Total operations processed"),
    Some("operations")
  )
  
  let resource_usage_gauge = azimuth::Meter::create_gauge(
    perf_meter, 
    "resource.usage",
    Some("Current resource usage"),
    Some("percent")
  )
  
  // 记录性能指标
  for time in processing_times {
    azimuth::Histogram::record(duration_histogram, time.to_double())
    azimuth::Counter::add(throughput_counter, 1.0)
  }
  
  // 验证性能指标
  assert_eq(duration_histogram.name, "operation.duration")
  assert_eq(throughput_counter.name, "operations.total")
  assert_eq(resource_usage_gauge.name, "resource.usage")
}
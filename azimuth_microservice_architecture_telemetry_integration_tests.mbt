// 微服务架构遥测集成测试
import "azimuth/azimuth"

pub test "微服务架构遥测集成测试" {
  // 模拟微服务架构中的多个服务
  let gateway_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "gateway-service")
  let auth_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "auth-service")
  let user_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "user-service")
  let order_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "order-service")
  let payment_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "payment-service")
  
  // 网关服务：接收外部请求
  let gateway_span_ctx = azimuth::SpanContext::new("trace-gateway-123", "span-gateway-root", true, "")
  let gateway_span = azimuth::Span::new("gateway.request", azimuth::Server, gateway_span_ctx)
  
  azimuth::Span::add_event(gateway_span, "gateway.request.received", Some([
    ("http.method", azimuth::StringValue("POST")),
    ("http.url", azimuth::StringValue("/api/orders")),
    ("user.agent", azimuth::StringValue("MobileApp/1.0")),
    ("client.ip", azimuth::StringValue("192.168.1.100"))
  ]))
  
  // 网关调用认证服务
  let auth_span_ctx = azimuth::SpanContext::new("trace-gateway-123", "span-auth-child", true, "")
  let auth_span = azimuth::Span::new("auth.validate", azimuth::Client, auth_span_ctx)
  
  azimuth::Span::add_event(auth_span, "auth.request.sent", Some([
    ("auth.token", azimuth::StringValue("jwt-token-123")),
    ("auth.method", azimuth::StringValue("oauth2"))
  ]))
  
  azimuth::Span::set_status(auth_span, azimuth::Ok)
  azimuth::Span::add_event(auth_span, "auth.response.received", Some([
    ("auth.user.id", azimuth::StringValue("user-456")),
    ("auth.permissions", azimuth::ArrayStringValue(["read", "write"]))
  ]))
  
  // 认证服务调用用户服务
  let user_span_ctx = azimuth::SpanContext::new("trace-gateway-123", "span-user-grandchild", true, "")
  let user_span = azimuth::Span::new("user.get_profile", azimuth::Client, user_span_ctx)
  
  azimuth::Span::add_event(user_span, "user.request.sent", Some([
    ("user.id", azimuth::StringValue("user-456")),
    ("user.fields", azimuth::ArrayStringValue(["profile", "preferences"]))
  ]))
  
  azimuth::Span::set_status(user_span, azimuth::Ok)
  azimuth::Span::add_event(user_span, "user.response.received", Some([
    ("user.name", azimuth::StringValue("John Doe")),
    ("user.email", azimuth::StringValue("john.doe@example.com")),
    ("user.tier", azimuth::StringValue("premium"))
  ]))
  
  // 网关调用订单服务
  let order_span_ctx = azimuth::SpanContext::new("trace-gateway-123", "span-order-child", true, "")
  let order_span = azimuth::Span::new("order.create", azimuth::Client, order_span_ctx)
  
  azimuth::Span::add_event(order_span, "order.request.sent", Some([
    ("order.items.count", azimuth::IntValue(3)),
    ("order.total.amount", azimuth::FloatValue(299.99)),
    ("order.currency", azimuth::StringValue("USD"))
  ]))
  
  // 订单服务调用支付服务
  let payment_span_ctx = azimuth::SpanContext::new("trace-gateway-123", "span-payment-grandchild", true, "")
  let payment_span = azimuth::Span::new("payment.process", azimuth::Client, payment_span_ctx)
  
  azimuth::Span::add_event(payment_span, "payment.request.sent", Some([
    ("payment.amount", azimuth::FloatValue(299.99)),
    ("payment.method", azimuth::StringValue("credit_card")),
    ("payment.currency", azimuth::StringValue("USD"))
  ]))
  
  azimuth::Span::set_status(payment_span, azimuth::Ok)
  azimuth::Span::add_event(payment_span, "payment.response.received", Some([
    ("payment.transaction.id", azimuth::StringValue("txn-789")),
    ("payment.status", azimuth::StringValue("completed")),
    ("payment.gateway", azimuth::StringValue("stripe"))
  ]))
  
  azimuth::Span::set_status(order_span, azimuth::Ok)
  azimuth::Span::add_event(order_span, "order.response.received", Some([
    ("order.id", azimuth::StringValue("order-123")),
    ("order.status", azimuth::StringValue("created"))
  ]))
  
  azimuth::Span::set_status(gateway_span, azimuth::Ok)
  azimuth::Span::add_event(gateway_span, "gateway.response.sent", Some([
    ("http.status_code", azimuth::IntValue(201)),
    ("response.time.ms", azimuth::IntValue(250))
  ]))
  
  // 验证分布式追踪的一致性
  assert_eq(azimuth::SpanContext::trace_id(gateway_span_ctx), "trace-gateway-123")
  assert_eq(azimuth::SpanContext::trace_id(auth_span_ctx), "trace-gateway-123")
  assert_eq(azimuth::SpanContext::trace_id(user_span_ctx), "trace-gateway-123")
  assert_eq(azimuth::SpanContext::trace_id(order_span_ctx), "trace-gateway-123")
  assert_eq(azimuth::SpanContext::trace_id(payment_span_ctx), "trace-gateway-123")
  
  // 验证Span ID的唯一性
  let span_ids = [
    azimuth::SpanContext::span_id(gateway_span_ctx),
    azimuth::SpanContext::span_id(auth_span_ctx),
    azimuth::SpanContext::span_id(user_span_ctx),
    azimuth::SpanContext::span_id(order_span_ctx),
    azimuth::SpanContext::span_id(payment_span_ctx)
  ]
  
  // 验证所有Span ID都是唯一的
  for i in 0..span_ids.length() {
    for j in 0..span_ids.length() {
      if (i != j) {
        assert_true(span_ids[i] != span_ids[j])
      }
    }
  }
  
  // 测试微服务度量集成
  let gateway_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "gateway-meter")
  let auth_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "auth-meter")
  let user_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "user-meter")
  let order_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "order-meter")
  let payment_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "payment-meter")
  
  // 网关服务度量
  let gateway_request_counter = azimuth::Meter::create_counter(gateway_meter, "gateway.requests.total")
  let gateway_response_time = azimuth::Meter::create_histogram(gateway_meter, "gateway.response.time.ms")
  
  azimuth::Counter::add(gateway_request_counter, 1.0)
  azimuth::Histogram::record(gateway_response_time, 250.0)
  
  // 认证服务度量
  let auth_request_counter = azimuth::Meter::create_counter(auth_meter, "auth.requests.total")
  let auth_success_counter = azimuth::Meter::create_counter(auth_meter, "auth.success.total")
  let auth_response_time = azimuth::Meter::create_histogram(auth_meter, "auth.response.time.ms")
  
  azimuth::Counter::add(auth_request_counter, 1.0)
  azimuth::Counter::add(auth_success_counter, 1.0)
  azimuth::Histogram::record(auth_response_time, 50.0)
  
  // 用户服务度量
  let user_request_counter = azimuth::Meter::create_counter(user_meter, "user.requests.total")
  let user_response_time = azimuth::Meter::create_histogram(user_meter, "user.response.time.ms")
  
  azimuth::Counter::add(user_request_counter, 1.0)
  azimuth::Histogram::record(user_response_time, 30.0)
  
  // 订单服务度量
  let order_request_counter = azimuth::Meter::create_counter(order_meter, "order.requests.total")
  let order_created_counter = azimuth::Meter::create_counter(order_meter, "order.created.total")
  let order_response_time = azimuth::Meter::create_histogram(order_meter, "order.response.time.ms")
  
  azimuth::Counter::add(order_request_counter, 1.0)
  azimuth::Counter::add(order_created_counter, 1.0)
  azimuth::Histogram::record(order_response_time, 150.0)
  
  // 支付服务度量
  let payment_request_counter = azimuth::Meter::create_counter(payment_meter, "payment.requests.total")
  let payment_success_counter = azimuth::Meter::create_counter(payment_meter, "payment.success.total")
  let payment_amount_histogram = azimuth::Meter::create_histogram(payment_meter, "payment.amount")
  let payment_response_time = azimuth::Meter::create_histogram(payment_meter, "payment.response.time.ms")
  
  azimuth::Counter::add(payment_request_counter, 1.0)
  azimuth::Counter::add(payment_success_counter, 1.0)
  azimuth::Histogram::record(payment_amount_histogram, 299.99)
  azimuth::Histogram::record(payment_response_time, 100.0)
  
  // 测试微服务日志集成
  let gateway_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "gateway-logger")
  let auth_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "auth-logger")
  let user_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "user-logger")
  let order_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "order-logger")
  let payment_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "payment-logger")
  
  // 网关服务日志
  let gateway_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Gateway request processed successfully"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(gateway_span_ctx)),
    Some(azimuth::SpanContext::span_id(gateway_span_ctx)),
    Some(azimuth::Context::root())
  )
  
  // 认证服务日志
  let auth_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User authenticated successfully"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(auth_span_ctx)),
    Some(azimuth::SpanContext::span_id(auth_span_ctx)),
    Some(azimuth::Context::root())
  )
  
  // 用户服务日志
  let user_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User profile retrieved"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(user_span_ctx)),
    Some(azimuth::SpanContext::span_id(user_span_ctx)),
    Some(azimuth::Context::root())
  )
  
  // 订单服务日志
  let order_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Order created successfully"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(order_span_ctx)),
    Some(azimuth::SpanContext::span_id(order_span_ctx)),
    Some(azimuth::Context::root())
  )
  
  // 支付服务日志
  let payment_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Payment processed successfully"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(payment_span_ctx)),
    Some(azimuth::SpanContext::span_id(payment_span_ctx)),
    Some(azimuth::Context::root())
  )
  
  // 发送所有日志
  azimuth::Logger::emit(gateway_logger, gateway_log)
  azimuth::Logger::emit(auth_logger, auth_log)
  azimuth::Logger::emit(user_logger, user_log)
  azimuth::Logger::emit(order_logger, order_log)
  azimuth::Logger::emit(payment_logger, payment_log)
  
  // 测试微服务Resource集成
  let gateway_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("gateway-service")),
    ("service.version", azimuth::StringValue("1.2.0")),
    ("service.instance.id", azimuth::StringValue("gateway-instance-1")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("kubernetes.namespace", azimuth::StringValue("api-gateway")),
    ("kubernetes.pod.name", azimuth::StringValue("gateway-pod-123"))
  ])
  
  let auth_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("auth-service")),
    ("service.version", azimuth::StringValue("2.1.0")),
    ("service.instance.id", azimuth::StringValue("auth-instance-1")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("kubernetes.namespace", azimuth::StringValue("auth")),
    ("kubernetes.pod.name", azimuth::StringValue("auth-pod-456"))
  ])
  
  let user_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("user-service")),
    ("service.version", azimuth::StringValue("3.0.1")),
    ("service.instance.id", azimuth::StringValue("user-instance-1")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("kubernetes.namespace", azimuth::StringValue("user")),
    ("kubernetes.pod.name", azimuth::StringValue("user-pod-789"))
  ])
  
  let order_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("order-service")),
    ("service.version", azimuth::StringValue("2.5.0")),
    ("service.instance.id", azimuth::StringValue("order-instance-1")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("kubernetes.namespace", azimuth::StringValue("order")),
    ("kubernetes.pod.name", azimuth::StringValue("order-pod-012"))
  ])
  
  let payment_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("payment-service")),
    ("service.version", azimuth::StringValue("1.8.0")),
    ("service.instance.id", azimuth::StringValue("payment-instance-1")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("kubernetes.namespace", azimuth::StringValue("payment")),
    ("kubernetes.pod.name", azimuth::StringValue("payment-pod-345"))
  ])
  
  // 验证微服务Resource
  assert_eq(azimuth::Resource::get_attribute(gateway_resource, "service.name"), Some(azimuth::StringValue("gateway-service")))
  assert_eq(azimuth::Resource::get_attribute(auth_resource, "service.name"), Some(azimuth::StringValue("auth-service")))
  assert_eq(azimuth::Resource::get_attribute(user_resource, "service.name"), Some(azimuth::StringValue("user-service")))
  assert_eq(azimuth::Resource::get_attribute(order_resource, "service.name"), Some(azimuth::StringValue("order-service")))
  assert_eq(azimuth::Resource::get_attribute(payment_resource, "service.name"), Some(azimuth::StringValue("payment-service")))
  
  // 测试微服务Baggage传播
  let initial_baggage = azimuth::Baggage::new()
  let gateway_baggage = azimuth::Baggage::set_entry(initial_baggage, "user.id", "user-456")
  let auth_baggage = azimuth::Baggage::set_entry(gateway_baggage, "auth.token", "jwt-token-123")
  let user_baggage = azimuth::Baggage::set_entry(auth_baggage, "user.tier", "premium")
  let order_baggage = azimuth::Baggage::set_entry(user_baggage, "order.id", "order-123")
  let payment_baggage = azimuth::Baggage::set_entry(order_baggage, "payment.method", "credit_card")
  
  // 验证Baggage传播
  assert_eq(azimuth::Baggage::get_entry(payment_baggage, "user.id"), Some("user-456"))
  assert_eq(azimuth::Baggage::get_entry(payment_baggage, "auth.token"), Some("jwt-token-123"))
  assert_eq(azimuth::Baggage::get_entry(payment_baggage, "user.tier"), Some("premium"))
  assert_eq(azimuth::Baggage::get_entry(payment_baggage, "order.id"), Some("order-123"))
  assert_eq(azimuth::Baggage::get_entry(payment_baggage, "payment.method"), Some("credit_card"))
  
  // 结束所有Span
  azimuth::Span::end(payment_span)
  azimuth::Span::end(user_span)
  azimuth::Span::end(auth_span)
  azimuth::Span::end(order_span)
  azimuth::Span::end(gateway_span)
}
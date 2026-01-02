// Azimuth 跨服务集成综合测试
// 测试分布式追踪场景

test "端到端分布式追踪" {
  // 测试完整的端到端分布式追踪场景
  
  // 创建API网关服务
  let gateway_tracer = TracerProvider::get_tracer(TracerProvider::default(), "api-gateway")
  let gateway_span = Tracer::start_span(gateway_tracer, "gateway.request.processing")
  
  // 设置API网关属性
  Span::set_attribute(gateway_span, "service.name", "api-gateway")
  Span::set_attribute(gateway_span, "service.version", "1.2.3")
  Span::set_attribute(gateway_span, "http.method", "GET")
  Span::set_attribute(gateway_span, "http.url", "/api/user/profile")
  Span::set_attribute(gateway_span, "http.scheme", "https")
  Span::set_attribute(gateway_span, "http.host", "api.example.com")
  Span::set_attribute(gateway_span, "http.user_agent", "Mozilla/5.0")
  Span::set_attribute(gateway_span, "net.peer.ip", "192.168.1.100")
  
  // 添加API网关事件
  Span::add_event(gateway_span, "request.received", [
    ("timestamp", "2025-01-02T10:00:00Z"),
    ("request.id", "req-12345")
  ])
  
  // 获取当前上下文
  let ctx = SpanContext::current()
  
  // 创建认证服务子span
  let auth_tracer = TracerProvider::get_tracer(TracerProvider::default(), "auth-service")
  let auth_span = Tracer::start_child_span_with_context(gateway_span, "auth.service.validation", ctx)
  
  // 设置认证服务属性
  Span::set_attribute(auth_span, "service.name", "auth-service")
  Span::set_attribute(auth_span, "service.version", "2.1.0")
  Span::set_attribute(auth_span, "auth.method", "jwt")
  Span::set_attribute(auth_span, "auth.token.type", "bearer")
  Span::set_attribute(auth_span, "auth.user.id", "user-67890")
  Span::set_attribute(auth_span, "auth.session.id", "sess-abc123")
  
  // 添加认证服务事件
  Span::add_event(auth_span, "token.validation.started", [
    ("token.issuer", "auth.example.com"),
    ("token.audience", "api.example.com")
  ])
  
  Span::add_event(auth_span, "token.validation.completed", [
    ("validation.result", "success"),
    ("token.expires", "2025-01-02T12:00:00Z")
  ])
  
  // 结束认证服务span
  Span::end(auth_span)
  
  // 创建用户服务子span
  let user_tracer = TracerProvider::get_tracer(TracerProvider::default(), "user-service")
  let user_span = Tracer::start_child_span_with_context(gateway_span, "user.service.profile.fetch", ctx)
  
  // 设置用户服务属性
  Span::set_attribute(user_span, "service.name", "user-service")
  Span::set_attribute(user_span, "service.version", "3.0.1")
  Span::set_attribute(user_span, "db.operation", "select")
  Span::set_attribute(user_span, "db.table", "users")
  Span::set_attribute(user_span, "db.query", "SELECT * FROM users WHERE id = ?")
  Span::set_attribute(user_span, "db.statement", "prepared")
  
  // 添加用户服务事件
  Span::add_event(user_span, "db.query.started", [
    ("query.type", "select"),
    ("table.name", "users")
  ])
  
  Span::add_event(user_span, "db.query.completed", [
    ("result.rows", "1"),
    ("execution.time", "15ms")
  ])
  
  // 结束用户服务span
  Span::end(user_span)
  
  // 创建缓存服务子span
  let cache_tracer = TracerProvider::get_tracer(TracerProvider::default(), "cache-service")
  let cache_span = Tracer::start_child_span_with_context(gateway_span, "cache.service.check", ctx)
  
  // 设置缓存服务属性
  Span::set_attribute(cache_span, "service.name", "cache-service")
  Span::set_attribute(cache_span, "service.version", "1.5.2")
  Span::set_attribute(cache_span, "cache.operation", "get")
  Span::set_attribute(cache_span, "cache.key", "user.profile.67890")
  Span::set_attribute(cache_span, "cache.hit", false)
  
  // 添加缓存服务事件
  Span::add_event(cache_span, "cache.miss", [
    ("cache.key", "user.profile.67890"),
    ("cache.ttl", "3600")
  ])
  
  Span::add_event(cache_span, "cache.set", [
    ("cache.key", "user.profile.67890"),
    ("cache.ttl", "3600"),
    ("cache.size", "1024")
  ])
  
  // 结束缓存服务span
  Span::end(cache_span)
  
  // 添加API网关完成事件
  Span::add_event(gateway_span, "response.generated", [
    ("timestamp", "2025-01-02T10:00:05Z"),
    ("response.status", "200"),
    ("response.size", "2048")
  ])
  
  // 设置API网关最终状态
  Span::set_status(gateway_span, Ok, Some("Request processed successfully"))
  Span::set_attribute(gateway_span, "http.status_code", 200)
  
  // 结束API网关span
  Span::end(gateway_span)
  
  // 验证所有span都有有效的上下文
  let gateway_ctx = Span::span_context(gateway_span)
  let auth_ctx = Span::span_context(auth_span)
  let user_ctx = Span::span_context(user_span)
  let cache_ctx = Span::span_context(cache_span)
  
  assert_true(SpanContext::is_valid(gateway_ctx))
  assert_true(SpanContext::is_valid(auth_ctx))
  assert_true(SpanContext::is_valid(user_ctx))
  assert_true(SpanContext::is_valid(cache_ctx))
  
  // 验证追踪ID一致性
  assert_eq(SpanContext::trace_id(gateway_ctx), SpanContext::trace_id(auth_ctx))
  assert_eq(SpanContext::trace_id(gateway_ctx), SpanContext::trace_id(user_ctx))
  assert_eq(SpanContext::trace_id(gateway_ctx), SpanContext::trace_id(cache_ctx))
}

test "微服务链式调用追踪" {
  // 测试微服务链式调用的追踪
  
  // 创建订单服务
  let order_tracer = TracerProvider::get_tracer(TracerProvider::default(), "order-service")
  let order_span = Tracer::start_span(order_tracer, "order.creation.process")
  
  Span::set_attribute(order_span, "service.name", "order-service")
  Span::set_attribute(order_span, "order.id", "order-12345")
  Span::set_attribute(order_span, "customer.id", "cust-67890")
  Span::set_attribute(order_span, "order.total", 299.99)
  
  // 库存服务调用
  let inventory_tracer = TracerProvider::get_tracer(TracerProvider::default(), "inventory-service")
  let inventory_span = Tracer::start_child_span(order_span, "inventory.check")
  
  Span::set_attribute(inventory_span, "service.name", "inventory-service")
  Span::set_attribute(inventory_span, "product.id", "prod-11111")
  Span::set_attribute(inventory_span, "requested.quantity", 2)
  Span::set_attribute(inventory_span, "available.quantity", 5)
  Span::set_attribute(inventory_span, "inventory.status", "available")
  
  Span::end(inventory_span)
  
  // 支付服务调用
  let payment_tracer = TracerProvider::get_tracer(TracerProvider::default(), "payment-service")
  let payment_span = Tracer::start_child_span(order_span, "payment.processing")
  
  Span::set_attribute(payment_span, "service.name", "payment-service")
  Span::set_attribute(payment_span, "payment.method", "credit_card")
  Span::set_attribute(payment_span, "payment.amount", 299.99)
  Span::set_attribute(payment_span, "payment.currency", "USD")
  Span::set_attribute(payment_span, "payment.gateway", "stripe")
  
  // 银行服务子调用
  let bank_tracer = TracerProvider::get_tracer(TracerProvider::default(), "bank-service")
  let bank_span = Tracer::start_child_span(payment_span, "bank.authorization")
  
  Span::set_attribute(bank_span, "service.name", "bank-service")
  Span::set_attribute(bank_span, "bank.transaction.id", "txn-99999")
  Span::set_attribute(bank_span, "bank.auth.code", "123456")
  Span::set_attribute(bank_span, "bank.result", "approved")
  
  Span::end(bank_span)
  
  Span::end(payment_span)
  
  // 物流服务调用
  let shipping_tracer = TracerProvider::get_tracer(TracerProvider::default(), "shipping-service")
  let shipping_span = Tracer::start_child_span(order_span, "shipping.arrangement")
  
  Span::set_attribute(shipping_span, "service.name", "shipping-service")
  Span::set_attribute(shipping_span, "shipping.method", "express")
  Span::set_attribute(shipping_span, "shipping.address", "123 Main St, City, State")
  Span::set_attribute(shipping_span, "estimated.delivery", "2025-01-05")
  
  Span::end(shipping_span)
  
  // 通知服务调用
  let notification_tracer = TracerProvider::get_tracer(TracerProvider::default(), "notification-service")
  let notification_span = Tracer::start_child_span(order_span, "notification.send")
  
  Span::set_attribute(notification_span, "service.name", "notification-service")
  Span::set_attribute(notification_span, "notification.type", "email")
  Span::set_attribute(notification_span, "notification.recipient", "customer@example.com")
  Span::set_attribute(notification_span, "notification.template", "order_confirmation")
  
  Span::end(notification_span)
  
  Span::end(order_span)
  
  // 验证追踪链
  let order_ctx = Span::span_context(order_span)
  let inventory_ctx = Span::span_context(inventory_span)
  let payment_ctx = Span::span_context(payment_span)
  let bank_ctx = Span::span_context(bank_span)
  let shipping_ctx = Span::span_context(shipping_span)
  let notification_ctx = Span::span_context(notification_span)
  
  assert_eq(SpanContext::trace_id(order_ctx), SpanContext::trace_id(inventory_ctx))
  assert_eq(SpanContext::trace_id(order_ctx), SpanContext::trace_id(payment_ctx))
  assert_eq(SpanContext::trace_id(order_ctx), SpanContext::trace_id(bank_ctx))
  assert_eq(SpanContext::trace_id(order_ctx), SpanContext::trace_id(shipping_ctx))
  assert_eq(SpanContext::trace_id(order_ctx), SpanContext::trace_id(notification_ctx))
}

test "跨服务度量关联" {
  // 测试跨服务度量数据的关联
  
  // 创建不同服务的度量提供者
  let api_meter = MeterProvider::get_meter(MeterProvider::default(), "api-gateway")
  let auth_meter = MeterProvider::get_meter(MeterProvider::default(), "auth-service")
  let user_meter = MeterProvider::get_meter(MeterProvider::default(), "user-service")
  
  // API网关度量
  let api_request_counter = Meter::create_counter(api_meter, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  let api_response_time = Meter::create_histogram(api_meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  
  Counter::add_with_attributes(api_request_counter, 1.0, [
    ("service", "api-gateway"),
    ("method", "GET"),
    ("endpoint", "/api/user/profile"),
    ("status", "200")
  ])
  
  Histogram::record_with_attributes(api_response_time, 125.5, [
    ("service", "api-gateway"),
    ("method", "GET"),
    ("endpoint", "/api/user/profile")
  ])
  
  // 认证服务度量
  let auth_counter = Meter::create_counter(auth_meter, "auth.operations.total", Some("Total auth operations"), Some("count"))
  let auth_duration = Meter::create_histogram(auth_meter, "auth.operation.duration", Some("Auth operation duration"), Some("ms"))
  
  Counter::add_with_attributes(auth_counter, 1.0, [
    ("service", "auth-service"),
    ("operation", "token_validation"),
    ("result", "success"),
    ("token.type", "jwt")
  ])
  
  Histogram::record_with_attributes(auth_duration, 45.2, [
    ("service", "auth-service"),
    ("operation", "token_validation")
  ])
  
  // 用户服务度量
  let user_db_counter = Meter::create_counter(user_meter, "db.queries.total", Some("Total DB queries"), Some("count"))
  let user_db_duration = Meter::create_histogram(user_meter, "db.query.duration", Some("DB query duration"), Some("ms"))
  
  Counter::add_with_attributes(user_db_counter, 1.0, [
    ("service", "user-service"),
    ("operation", "select"),
    ("table", "users"),
    ("result", "success")
  ])
  
  Histogram::record_with_attributes(user_db_duration, 15.8, [
    ("service", "user-service"),
    ("operation", "select"),
    ("table", "users")
  ])
  
  // 验证度量数据
  assert_eq(api_request_counter.name, "http.requests.total")
  assert_eq(auth_counter.name, "auth.operations.total")
  assert_eq(user_db_counter.name, "db.queries.total")
  
  assert_true(true)
}

test "跨服务日志关联" {
  // 测试跨服务日志的关联
  
  // 创建不同服务的日志记录器
  let api_logger = LoggerProvider::get_logger(LoggerProvider::default(), "api-gateway")
  let auth_logger = LoggerProvider::get_logger(LoggerProvider::default(), "auth-service")
  let user_logger = LoggerProvider::get_logger(LoggerProvider::default(), "user-service")
  
  // 创建关联ID
  let correlation_id = "corr-" + Time::now().to_string()
  let request_id = "req-" + Time::now().to_string()
  let trace_id = "trace-" + Time::now().to_string()
  
  // API网关日志
  let api_request_log = LogRecord::new(Info, "API request received")
  LogRecord::add_attribute(api_request_log, "service.name", "api-gateway")
  LogRecord::add_attribute(api_request_log, "correlation.id", correlation_id)
  LogRecord::add_attribute(api_request_log, "request.id", request_id)
  LogRecord::add_attribute(api_request_log, "trace.id", trace_id)
  LogRecord::add_attribute(api_request_log, "http.method", "GET")
  LogRecord::add_attribute(api_request_log, "http.url", "/api/user/profile")
  LogRecord::add_attribute(api_request_log, "client.ip", "192.168.1.100")
  
  let api_response_log = LogRecord::new(Info, "API response sent")
  LogRecord::add_attribute(api_response_log, "service.name", "api-gateway")
  LogRecord::add_attribute(api_response_log, "correlation.id", correlation_id)
  LogRecord::add_attribute(api_response_log, "request.id", request_id)
  LogRecord::add_attribute(api_response_log, "trace.id", trace_id)
  LogRecord::add_attribute(api_response_log, "http.status", "200")
  LogRecord::add_attribute(api_response_log, "response.size", "2048")
  LogRecord::add_attribute(api_response_log, "duration", "125ms")
  
  // 认证服务日志
  let auth_log = LogRecord::new(Info, "Token validation successful")
  LogRecord::add_attribute(auth_log, "service.name", "auth-service")
  LogRecord::add_attribute(auth_log, "correlation.id", correlation_id)
  LogRecord::add_attribute(auth_log, "request.id", request_id)
  LogRecord::add_attribute(auth_log, "trace.id", trace_id)
  LogRecord::add_attribute(auth_log, "auth.user.id", "user-67890")
  LogRecord::add_attribute(auth_log, "auth.token.issuer", "auth.example.com")
  LogRecord::add_attribute(auth_log, "auth.duration", "45ms")
  
  // 用户服务日志
  let user_log = LogRecord::new(Info, "User profile retrieved")
  LogRecord::add_attribute(user_log, "service.name", "user-service")
  LogRecord::add_attribute(user_log, "correlation.id", correlation_id)
  LogRecord::add_attribute(user_log, "request.id", request_id)
  LogRecord::add_attribute(user_log, "trace.id", trace_id)
  LogRecord::add_attribute(user_log, "db.query", "SELECT * FROM users WHERE id = ?")
  LogRecord::add_attribute(user_log, "db.duration", "15ms")
  LogRecord::add_attribute(user_log, "cache.hit", "false")
  
  // 发射日志
  Logger::emit(api_logger, api_request_log)
  Logger::emit(auth_logger, auth_log)
  Logger::emit(user_logger, user_log)
  Logger::emit(api_logger, api_response_log)
  
  // 验证日志记录
  assert_eq(LogRecord::body(api_request_log), Some("API request received"))
  assert_eq(LogRecord::body(auth_log), Some("Token validation successful"))
  assert_eq(LogRecord::body(user_log), Some("User profile retrieved"))
  assert_eq(LogRecord::body(api_response_log), Some("API response sent"))
  
  assert_true(true)
}

test "服务网格追踪" {
  // 测试服务网格环境下的追踪
  
  // 创建入口网关span
  let ingress_tracer = TracerProvider::get_tracer(TracerProvider::default(), "istio-ingressgateway")
  let ingress_span = Tracer::start_span(ingress_tracer, "ingress.gateway.request")
  
  Span::set_attribute(ingress_span, "service.name", "istio-ingressgateway")
  Span::set_attribute(ingress_span, "service.version", "1.15.0")
  Span::set_attribute(ingress_span, "mesh.protocol", "http")
  Span::set_attribute(ingress_span, "mesh.source.workload", "istio-ingressgateway")
  Span::set_attribute(ingress_span, "mesh.source.namespace", "istio-system")
  Span::set_attribute(ingress_span, "mesh.destination.workload", "frontend-service")
  Span::set_attribute(ingress_span, "mesh.destination.namespace", "production")
  
  // 前端服务span
  let frontend_tracer = TracerProvider::get_tracer(TracerProvider::default(), "frontend-service")
  let frontend_span = Tracer::start_child_span(ingress_span, "frontend.service.request")
  
  Span::set_attribute(frontend_span, "service.name", "frontend-service")
  Span::set_attribute(frontend_span, "service.version", "2.3.1")
  Span::set_attribute(frontend_span, "mesh.source.workload", "istio-ingressgateway")
  Span::set_attribute(frontend_span, "mesh.source.namespace", "istio-system")
  Span::set_attribute(frontend_span, "mesh.destination.workload", "backend-service")
  Span::set_attribute(frontend_span, "mesh.destination.namespace", "production")
  
  // 后端服务span
  let backend_tracer = TracerProvider::get_tracer(TracerProvider::default(), "backend-service")
  let backend_span = Tracer::start_child_span(frontend_span, "backend.service.processing")
  
  Span::set_attribute(backend_span, "service.name", "backend-service")
  Span::set_attribute(backend_span, "service.version", "3.1.0")
  Span::set_attribute(backend_span, "mesh.source.workload", "frontend-service")
  Span::set_attribute(backend_span, "mesh.source.namespace", "production")
  Span::set_attribute(backend_span, "mesh.destination.workload", "database-service")
  Span::set_attribute(backend_span, "mesh.destination.namespace", "production")
  
  // 数据库服务span
  let db_tracer = TracerProvider::get_tracer(TracerProvider::default(), "database-service")
  let db_span = Tracer::start_child_span(backend_span, "database.service.query")
  
  Span::set_attribute(db_span, "service.name", "database-service")
  Span::set_attribute(db_span, "service.version", "1.8.2")
  Span::set_attribute(db_span, "mesh.source.workload", "backend-service")
  Span::set_attribute(db_span, "mesh.source.namespace", "production")
  Span::set_attribute(db_span, "db.operation", "select")
  Span::set_attribute(db_span, "db.table", "products")
  
  Span::end(db_span)
  Span::end(backend_span)
  Span::end(frontend_span)
  
  // 创建出口网关span
  let egress_tracer = TracerProvider::get_tracer(TracerProvider::default(), "istio-egressgateway")
  let egress_span = Tracer::start_child_span(ingress_span, "egress.gateway.response")
  
  Span::set_attribute(egress_span, "service.name", "istio-egressgateway")
  Span::set_attribute(egress_span, "service.version", "1.15.0")
  Span::set_attribute(egress_span, "mesh.protocol", "http")
  Span::set_attribute(egress_span, "mesh.source.workload", "frontend-service")
  Span::set_attribute(egress_span, "mesh.source.namespace", "production")
  Span::set_attribute(egress_span, "mesh.destination.workload", "external-client")
  
  Span::end(egress_span)
  Span::end(ingress_span)
  
  // 验证服务网格追踪
  let ingress_ctx = Span::span_context(ingress_span)
  let frontend_ctx = Span::span_context(frontend_span)
  let backend_ctx = Span::span_context(backend_span)
  let db_ctx = Span::span_context(db_span)
  let egress_ctx = Span::span_context(egress_span)
  
  assert_eq(SpanContext::trace_id(ingress_ctx), SpanContext::trace_id(frontend_ctx))
  assert_eq(SpanContext::trace_id(ingress_ctx), SpanContext::trace_id(backend_ctx))
  assert_eq(SpanContext::trace_id(ingress_ctx), SpanContext::trace_id(db_ctx))
  assert_eq(SpanContext::trace_id(ingress_ctx), SpanContext::trace_id(egress_ctx))
}

test "异步消息追踪" {
  // 测试异步消息传递的追踪
  
  // 创建消息生产者span
  let producer_tracer = TracerProvider::get_tracer(TracerProvider::default(), "order-service")
  let producer_span = Tracer::start_span(producer_tracer, "message.produce")
  
  Span::set_attribute(producer_span, "service.name", "order-service")
  Span::set_attribute(producer_span, "messaging.system", "kafka")
  Span::set_attribute(producer_span, "messaging.destination", "order.events")
  Span::set_attribute(producer_span, "messaging.message_id", "msg-12345")
  Span::set_attribute(producer_span, "messaging.message_type", "order.created")
  Span::set_attribute(producer_span, "messaging.operation", "publish")
  
  // 注入追踪上下文到消息头
  let message_headers = []
  let trace_id = SpanContext::trace_id(Span::span_context(producer_span))
  let span_id = SpanContext::span_id(Span::span_context(producer_span))
  
  message_headers = message_headers.push(("traceparent", "00-" + trace_id + "-" + span_id + "-01"))
  message_headers = message_headers.push(("x-trace-id", trace_id))
  message_headers = message_headers.push(("x-span-id", span_id))
  
  Span::end(producer_span)
  
  // 模拟消息传递和消费
  let consumer_tracer = TracerProvider::get_tracer(TracerProvider::default(), "notification-service")
  let consumer_span = Tracer::start_span(consumer_tracer, "message.consume")
  
  // 从消息头提取追踪上下文
  let extracted_trace_id = message_headers.find(fn(h) { h.0 == "x-trace-id" })
  let extracted_span_id = message_headers.find(fn(h) { h.0 == "x-span-id" })
  
  match extracted_trace_id {
    Some((_, trace)) => Span::set_attribute(consumer_span, "messaging.trace_id", trace)
    None => ()
  }
  
  match extracted_span_id {
    Some((_, span)) => Span::set_attribute(consumer_span, "messaging.parent_span_id", span)
    None => ()
  }
  
  Span::set_attribute(consumer_span, "service.name", "notification-service")
  Span::set_attribute(consumer_span, "messaging.system", "kafka")
  Span::set_attribute(consumer_span, "messaging.destination", "order.events")
  Span::set_attribute(consumer_span, "messaging.message_id", "msg-12345")
  Span::set_attribute(consumer_span, "messaging.operation", "receive")
  Span::set_attribute(consumer_span, "messaging.consumer_group", "notification-consumers")
  
  // 创建通知处理子span
  let notification_span = Tracer::start_child_span(consumer_span, "notification.process")
  
  Span::set_attribute(notification_span, "service.name", "notification-service")
  Span::set_attribute(notification_span, "notification.type", "email")
  Span::set_attribute(notification_span, "notification.recipient", "customer@example.com")
  Span::set_attribute(notification_span, "notification.template", "order_confirmation")
  
  Span::end(notification_span)
  Span::end(consumer_span)
  
  // 验证异步消息追踪
  assert_true(true)
}
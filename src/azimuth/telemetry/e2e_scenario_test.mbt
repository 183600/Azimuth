// 端到端场景测试 - 模拟真实使用场景的综合测试
use azimuth.telemetry.api.common.{AttributeValue, Resource}
use azimuth.telemetry.api.trace.{SpanContext, Span, SpanKind, StatusCode, SpanEvent, SpanLink, NoopTracer, NoopTracerProvider}
use azimuth.telemetry.api.metrics.{NoopMeter, NoopMeterProvider}
use azimuth.telemetry.api.logs.{SeverityNumber, LogRecordBuilder, NoopLogger, NoopLoggerProvider}
use azimuth.telemetry.api.context.{Context, ContextKey, Baggage, create_key}

test "e2e_web_request_processing" {
  // 端到端测试：模拟Web请求处理场景
  
  // 1. 初始化服务和资源
  let web_service_resource = Resource::default("web-api-service")
  let ctx = Context::empty()
  
  // 2. 创建请求上下文和baggage
  let request_id_key = create_key("request.id")
  let user_id_key = create_key("user.id")
  let session_id_key = create_key("session.id")
  
  let request_ctx = ctx
    .with_value(request_id_key, "req-12345-67890")
    .with_value(user_id_key, "user-abc123")
    .with_value(session_id_key, "sess-def456")
  
  let request_baggage = Baggage::empty()
    .with_entry("correlation.id", "corr-789012")
    .with_entry("trace.id", "trace-345678")
    .with_entry("client.version", "1.2.3")
    .with_entry("request.source", "web")
  
  // 3. 创建Tracer、Meter、Logger
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("web-api-tracer", Some("1.0.0"))
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("web-api-metrics", Some("1.0.0"))
  
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("web-api-logger", Some("1.0.0"))
  
  // 4. 开始HTTP请求处理span
  let http_attributes = [
    ("service.name", AttributeValue::string(web_service_resource.service_name)),
    ("http.method", AttributeValue::string("POST")),
    ("http.url", AttributeValue::string("/api/v1/orders")),
    ("http.scheme", AttributeValue::string("https")),
    ("http.host", AttributeValue::string("api.example.com")),
    ("http.user_agent", AttributeValue::string("Mozilla/5.0 (compatible; API-Client/1.0)")),
    ("http.client_ip", AttributeValue::string("192.168.1.100")),
    ("request.id", AttributeValue::string("req-12345-67890")),
    ("user.id", AttributeValue::string("user-abc123")),
    ("correlation.id", AttributeValue::string("corr-789012"))
  ]
  
  let (processing_ctx, http_span) = tracer.start_span(
    request_ctx, 
    "HTTP POST /api/v1/orders", 
    Server, 
    Some(http_attributes)
  )
  
  // 5. 记录请求指标
  let request_counter = meter.create_counter("http.requests.total", Some("count"), Some("Total HTTP requests"))
  let request_duration = meter.create_histogram("http.request.duration", Some("ms"), Some("HTTP request duration"))
  let active_connections = meter.create_gauge("http.active_connections", Some("count"), Some("Active HTTP connections"))
  
  request_counter.add(1L, Some([
    ("service.name", AttributeValue::string(web_service_resource.service_name)),
    ("http.method", AttributeValue::string("POST")),
    ("http.route", AttributeValue::string("/api/v1/orders")),
    ("http.status_code", AttributeValue::int(200L))
  ]))
  
  active_connections.record(25.0, Some([
    ("service.name", AttributeValue::string(web_service_resource.service_name)),
    ("listener.type", AttributeValue::string("http"))
  ]))
  
  // 6. 记录请求开始日志
  let start_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(SeverityNumber::Info)
    .body("HTTP request started: POST /api/v1/orders")
    .with_attribute("service.name", AttributeValue::string(web_service_resource.service_name))
    .with_attribute("request.id", AttributeValue::string("req-12345-67890"))
    .with_attribute("user.id", AttributeValue::string("user-abc123"))
    .with_attribute("http.method", AttributeValue::string("POST"))
    .with_attribute("http.url", AttributeValue::string("/api/v1/orders"))
    .with_attribute("client.ip", AttributeValue::string("192.168.1.100"))
    .build()
  
  logger.emit(start_log)
  
  // 7. 模拟数据库查询子操作
  let db_attributes = [
    ("service.name", AttributeValue::string(web_service_resource.service_name)),
    ("db.system", AttributeValue::string("postgresql")),
    ("db.name", AttributeValue::string("orders_db")),
    ("db.operation", AttributeValue::string("SELECT")),
    ("db.statement", AttributeValue::string("SELECT * FROM orders WHERE user_id = $1")),
    ("user.id", AttributeValue::string("user-abc123")),
    ("request.id", AttributeValue::string("req-12345-67890"))
  ]
  
  let (db_ctx, db_span) = tracer.start_span(
    processing_ctx, 
    "database.query", 
    Internal, 
    Some(db_attributes)
  )
  
  // 记录数据库查询指标
  let db_counter = meter.create_counter("db.queries.total", Some("count"), Some("Total database queries"))
  let db_duration = meter.create_histogram("db.query.duration", Some("ms"), Some("Database query duration"))
  
  db_counter.add(1L, Some([
    ("service.name", AttributeValue::string(web_service_resource.service_name)),
    ("db.system", AttributeValue::string("postgresql")),
    ("db.operation", AttributeValue::string("SELECT"))
  ]))
  
  db_duration.record(45.2, Some([
    ("service.name", AttributeValue::string(web_service_resource.service_name)),
    ("db.operation", AttributeValue::string("SELECT"))
  ]))
  
  // 记录数据库查询日志
  let db_log = LogRecord::builder()
    .timestamp(1640995200000000050L)
    .severity(SeverityNumber::Debug)
    .body("Database query executed: SELECT * FROM orders WHERE user_id = $1")
    .with_attribute("service.name", AttributeValue::string(web_service_resource.service_name))
    .with_attribute("db.system", AttributeValue::string("postgresql"))
    .with_attribute("db.operation", AttributeValue::string("SELECT"))
    .with_attribute("db.duration_ms", AttributeValue::int(45L))
    .with_attribute("rows.affected", AttributeValue::int(10L))
    .build()
  
  logger.emit(db_log)
  
  // 8. 模拟缓存查询子操作
  let cache_attributes = [
    ("service.name", AttributeValue::string(web_service_resource.service_name)),
    ("cache.system", AttributeValue::string("redis")),
    ("cache.operation", AttributeValue::string("GET")),
    ("cache.key", AttributeValue::string("user_profile:user-abc123")),
    ("request.id", AttributeValue::string("req-12345-67890"))
  ]
  
  let (cache_ctx, cache_span) = tracer.start_span(
    processing_ctx, 
    "cache.get", 
    Internal, 
    Some(cache_attributes)
  )
  
  // 记录缓存指标
  let cache_counter = meter.create_counter("cache.operations.total", Some("count"), Some("Total cache operations"))
  let cache_hit_ratio = meter.create_gauge("cache.hit_ratio", Some("ratio"), Some("Cache hit ratio"))
  
  cache_counter.add(1L, Some([
    ("service.name", AttributeValue::string(web_service_resource.service_name)),
    ("cache.system", AttributeValue::string("redis")),
    ("cache.operation", AttributeValue::string("GET")),
    ("cache.hit", AttributeValue::bool(true))
  ]))
  
  cache_hit_ratio.record(0.85, Some([
    ("service.name", AttributeValue::string(web_service_resource.service_name)),
    ("cache.system", AttributeValue::string("redis"))
  ]))
  
  // 9. 记录业务逻辑处理事件
  let business_event = SpanEvent::{
    name: "business_logic_executed",
    timestamp_unix_nanos: 1640995200000000100L,
    attributes: [
      ("business.rule", AttributeValue::string("order_validation")),
      ("validation.result", AttributeValue::string("passed")),
      ("order.value", AttributeValue::float(299.99)),
      ("currency", AttributeValue::string("USD"))
    ]
  }
  
  // 10. 完成请求处理并记录最终指标
  request_duration.record(125.5, Some([
    ("service.name", AttributeValue::string(web_service_resource.service_name)),
    ("http.method", AttributeValue::string("POST")),
    ("http.route", AttributeValue::string("/api/v1/orders")),
    ("http.status_code", AttributeValue::int(200L))
  ]))
  
  // 11. 记录请求完成日志
  let complete_log = LogRecord::builder()
    .timestamp(1640995200000000125L)
    .severity(SeverityNumber::Info)
    .body("HTTP request completed successfully")
    .with_attribute("service.name", AttributeValue::string(web_service_resource.service_name))
    .with_attribute("request.id", AttributeValue::string("req-12345-67890"))
    .with_attribute("user.id", AttributeValue::string("user-abc123"))
    .with_attribute("http.method", AttributeValue::string("POST"))
    .with_attribute("http.status_code", AttributeValue::int(200L))
    .with_attribute("response.duration_ms", AttributeValue::int(125L))
    .with_attribute("response.size_bytes", AttributeValue::int(1024L))
    .build()
  
  logger.emit(complete_log)
  
  // 12. 验证端到端场景的完整性
  assert_eq(http_span.name, "HTTP POST /api/v1/orders")
  assert_eq(http_span.kind, Server)
  assert_eq(http_span.attributes.length(), 10)
  
  assert_eq(db_span.name, "database.query")
  assert_eq(db_span.kind, Internal)
  assert_eq(db_span.attributes.length(), 7)
  
  assert_eq(cache_span.name, "cache.get")
  assert_eq(cache_span.kind, Internal)
  assert_eq(cache_span.attributes.length(), 5)
  
  // 验证上下文传播
  match processing_ctx.get(request_id_key) {
    Some(request_id) => assert_eq(request_id, "req-12345-67890")
    None => @test.fail("Test failed")
  }
  
  // 验证baggage传播
  match request_baggage.get("correlation.id") {
    Some(correlation_id) => assert_eq(correlation_id, "corr-789012")
    None => @test.fail("Test failed")
  }
  
  // 验证日志记录完整性
  assert_eq(start_log.severity_number, SeverityNumber::Info)
  match start_log.body {
    Some(body) => assert_eq(body, "HTTP request started: POST /api/v1/orders")
    None => @test.fail("Test failed")
  }
  
  assert_eq(complete_log.severity_number, SeverityNumber::Info)
  match complete_log.body {
    Some(body) => assert_eq(body, "HTTP request completed successfully")
    None => @test.fail("Test failed")
  }
}

test "e2e_microservice_communication" {
  // 端到端测试：模拟微服务间通信场景
  
  // 1. 初始化API Gateway服务
  let gateway_resource = Resource::default("api-gateway")
  let ctx = Context::empty()
  
  // 2. 创建分布式追踪上下文
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x0a_byte })
  let parent_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte })
  
  let gateway_span_context = SpanContext::{
    trace_id,
    span_id: parent_span_id,
    trace_flags: 1_byte,
    trace_state: "sampling.decision=true"
  }
  
  // 3. API Gateway创建根span
  let tracer_provider = NoopTracerProvider::{}
  let gateway_tracer = tracer_provider.get_tracer("api-gateway-tracer", Some("1.0.0"))
  
  let gateway_attributes = [
    ("service.name", AttributeValue::string(gateway_resource.service_name)),
    ("service.version", AttributeValue::string("2.1.0")),
    ("operation.name", AttributeValue::string("process_user_request")),
    ("gateway.request.id", AttributeValue::string("gw-req-12345")),
    ("client.ip", AttributeValue::string("10.0.1.100")),
    ("user.id", AttributeValue::string("user-789"))
  ]
  
  let (gateway_ctx, gateway_span) = gateway_tracer.start_span(
    ctx, 
    "gateway.process_request", 
    Server, 
    Some(gateway_attributes)
  )
  
  // 4. 创建传播上下文和baggage
  let propagation_ctx = gateway_ctx.with_value(create_key("trace.id"), "trace-abcdef123456")
  let propagation_baggage = Baggage::empty()
    .with_entry("user.id", "user-789")
    .with_entry("request.id", "gw-req-12345")
    .with_entry("gateway.version", "2.1.0")
    .with_entry("routing.decision", "to_user_service")
  
  // 5. API Gateway调用用户服务
  let user_service_tracer = tracer_provider.get_tracer("user-service-tracer", Some("1.5.2"))
  
  // 创建从Gateway到User Service的链接
  let user_service_context = SpanContext::{
    trace_id,
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }),
    trace_flags: 1_byte,
    trace_state: "sampling.decision=true"
  }
  
  let user_service_link = SpanLink::{
    context: gateway_span_context,
    attributes: [
      ("link.type", AttributeValue::string("parent")),
      ("parent.service", AttributeValue::string("api-gateway")),
      ("parent.span", AttributeValue::string("gateway.process_request"))
    ]
  }
  
  let user_attributes = [
    ("service.name", AttributeValue::string("user-service")),
    ("service.version", AttributeValue::string("1.5.2")),
    ("operation.name", AttributeValue::string("get_user_profile")),
    ("parent.service", AttributeValue::string("api-gateway")),
    ("gateway.request.id", AttributeValue::string("gw-req-12345")),
    ("user.id", AttributeValue::string("user-789")),
    ("upstream.call", AttributeValue::bool(true))
  ]
  
  let (user_ctx, user_span) = user_service_tracer.start_span(
    propagation_ctx, 
    "user_service.get_profile", 
    Client, 
    Some(user_attributes)
  )
  
  // 6. 用户服务调用数据库
  let db_tracer = tracer_provider.get_tracer("database-tracer", Some("1.0.0"))
  
  let db_attributes = [
    ("service.name", AttributeValue::string("user-service")),
    ("db.system", AttributeValue::string("mysql")),
    ("db.name", AttributeValue::string("user_profiles")),
    ("db.operation", AttributeValue::string("SELECT")),
    ("db.query", AttributeValue::string("SELECT * FROM profiles WHERE user_id = ?")),
    ("user.id", AttributeValue::string("user-789")),
    ("upstream.service", AttributeValue::string("api-gateway"))
  ]
  
  let (_, db_span) = db_tracer.start_span(
    user_ctx, 
    "database.user_profile_query", 
    Internal, 
    Some(db_attributes)
  )
  
  // 7. 用户服务调用缓存服务
  let cache_tracer = tracer_provider.get_tracer("cache-service-tracer", Some("1.2.1"))
  
  let cache_attributes = [
    ("service.name", AttributeValue::string("cache-service")),
    ("service.version", AttributeValue::string("1.2.1")),
    ("cache.system", AttributeValue::string("memcached")),
    ("cache.operation", AttributeValue::string("GET")),
    ("cache.key", AttributeValue::string("profile:user-789")),
    ("caller.service", AttributeValue::string("user-service")),
    ("original.request.id", AttributeValue::string("gw-req-12345"))
  ]
  
  let (_, cache_span) = cache_tracer.start_span(
    user_ctx, 
    "cache_service.get_profile", 
    Server, 
    Some(cache_attributes)
  )
  
  // 8. 记录微服务通信指标
  let gateway_meter_provider = NoopMeterProvider::{}
  let gateway_meter = gateway_meter_provider.get_meter("api-gateway-metrics", Some("1.0.0"))
  
  let gateway_request_counter = gateway_meter.create_counter("gateway.requests.total", Some("count"), Some("Gateway requests"))
  let gateway_latency = gateway_meter.create_histogram("gateway.request.duration", Some("ms"), Some("Gateway request duration"))
  
  gateway_request_counter.add(1L, Some([
    ("service.name", AttributeValue::string(gateway_resource.service_name)),
    ("operation.name", AttributeValue::string("process_user_request")),
    ("upstream.service", AttributeValue::string("user-service"))
  ]))
  
  gateway_latency.record(85.5, Some([
    ("service.name", AttributeValue::string(gateway_resource.service_name)),
    ("upstream.service", AttributeValue::string("user-service"))
  ]))
  
  // 9. 记录微服务通信日志
  let gateway_logger_provider = NoopLoggerProvider::{}
  let gateway_logger = gateway_logger_provider.get_logger("api-gateway-logger", Some("1.0.0"))
  
  let gateway_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(SeverityNumber::Info)
    .body("Gateway: Processing user request, routing to user-service")
    .with_attribute("service.name", AttributeValue::string(gateway_resource.service_name))
    .with_attribute("request.id", AttributeValue::string("gw-req-12345"))
    .with_attribute("user.id", AttributeValue::string("user-789"))
    .with_attribute("upstream.service", AttributeValue::string("user-service"))
    .with_attribute("trace.id", AttributeValue::string("trace-abcdef123456"))
    .build()
  
  gateway_logger.emit(gateway_log)
  
  // 10. 验证微服务通信的端到端追踪
  assert_eq(gateway_span.name, "gateway.process_request")
  assert_eq(gateway_span.kind, Server)
  assert_eq(gateway_span.attributes.length(), 6)
  
  assert_eq(user_span.name, "user_service.get_profile")
  assert_eq(user_span.kind, Client)
  assert_eq(user_span.attributes.length(), 7)
  
  assert_eq(db_span.name, "database.user_profile_query")
  assert_eq(db_span.kind, Internal)
  assert_eq(db_span.attributes.length(), 7)
  
  assert_eq(cache_span.name, "cache_service.get_profile")
  assert_eq(cache_span.kind, Server)
  assert_eq(cache_span.attributes.length(), 7)
  
  // 验证分布式追踪上下文传播
  assert_eq(gateway_span_context.trace_id.length(), 16)
  assert_eq(user_service_context.trace_id.length(), 16)
  
  // 验证baggage传播
  match propagation_baggage.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user-789")
    None => @test.fail("Test failed")
  }
  
  match propagation_baggage.get("request.id") {
    Some(request_id) => assert_eq(request_id, "gw-req-12345")
    None => @test.fail("Test failed")
  }
  
  // 验证服务间链接关系
  assert_eq(user_service_link.context.trace_id.length(), 16)
  assert_eq(user_service_link.attributes.length(), 3)
}

test "e2e_error_handling_and_recovery" {
  // 端到端测试：模拟错误处理和恢复场景
  
  // 1. 初始化服务资源
  let service_resource = Resource::default("payment-service")
  let ctx = Context::empty()
  
  // 2. 创建Tracer和Logger
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("payment-tracer", Some("1.0.0"))
  
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("payment-logger", Some("1.0.0"))
  
  // 3. 开始支付处理span
  let payment_attributes = [
    ("service.name", AttributeValue::string(service_resource.service_name)),
    ("operation.name", AttributeValue::string("process_payment")),
    ("payment.id", AttributeValue::string("pay-12345")),
    ("user.id", AttributeValue::string("user-abc")),
    ("amount", AttributeValue::float(99.99)),
    ("currency", AttributeValue::string("USD"))
  ]
  
  let (payment_ctx, payment_span) = tracer.start_span(
    ctx, 
    "payment.process", 
    Server, 
    Some(payment_attributes)
  )
  
  // 4. 记录支付开始日志
  let start_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(SeverityNumber::Info)
    .body("Payment processing started")
    .with_attribute("service.name", AttributeValue::string(service_resource.service_name))
    .with_attribute("payment.id", AttributeValue::string("pay-12345"))
    .with_attribute("user.id", AttributeValue::string("user-abc"))
    .with_attribute("amount", AttributeValue::float(99.99))
    .build()
  
  logger.emit(start_log)
  
  // 5. 模拟支付网关调用失败
  let gateway_attributes = [
    ("service.name", AttributeValue::string(service_resource.service_name)),
    ("external.service", AttributeValue::string("payment-gateway")),
    ("operation.name", AttributeValue::string("charge_payment")),
    ("payment.id", AttributeValue::string("pay-12345")),
    ("gateway.provider", AttributeValue::string("stripe"))
  ]
  
  let (_, gateway_span) = tracer.start_span(
    payment_ctx, 
    "payment_gateway.charge", 
    Client, 
    Some(gateway_attributes)
  )
  
  // 6. 记录支付网关错误事件
  let error_event = SpanEvent::{
    name: "payment_gateway_error",
    timestamp_unix_nanos: 1640995200000000100L,
    attributes: [
      ("error.type", AttributeValue::string("gateway_timeout")),
      ("error.message", AttributeValue::string("Payment gateway timed out after 30 seconds")),
      ("error.code", AttributeValue::string("PGW_TIMEOUT")),
      ("retry.count", AttributeValue::int(1L)),
      ("gateway.provider", AttributeValue::string("stripe"))
    ]
  }
  
  // 7. 记录错误日志
  let error_log = LogRecord::builder()
    .timestamp(1640995200000000100L)
    .severity(SeverityNumber::Error)
    .body("Payment gateway error: timeout occurred")
    .with_attribute("service.name", AttributeValue::string(service_resource.service_name))
    .with_attribute("payment.id", AttributeValue::string("pay-12345"))
    .with_attribute("error.type", AttributeValue::string("gateway_timeout"))
    .with_attribute("error.code", AttributeValue::string("PGW_TIMEOUT"))
    .with_attribute("retry.count", AttributeValue::int(1L))
    .build()
  
  logger.emit(error_log)
  
  // 8. 模拟重试逻辑
  let retry_attributes = [
    ("service.name", AttributeValue::string(service_resource.service_name)),
    ("operation.name", AttributeValue::string("retry_payment")),
    ("payment.id", AttributeValue::string("pay-12345")),
    ("retry.attempt", AttributeValue::int(2L)),
    ("max.retries", AttributeValue::int(3L))
  ]
  
  let (retry_ctx, retry_span) = tracer.start_span(
    payment_ctx, 
    "payment.retry", 
    Internal, 
    Some(retry_attributes)
  )
  
  // 9. 记录重试日志
  let retry_log = LogRecord::builder()
    .timestamp(1640995200000000200L)
    .severity(SeverityNumber::Warn)
    .body("Retrying payment attempt 2/3")
    .with_attribute("service.name", AttributeValue::string(service_resource.service_name))
    .with_attribute("payment.id", AttributeValue::string("pay-12345"))
    .with_attribute("retry.attempt", AttributeValue::int(2L))
    .with_attribute("max.retries", AttributeValue::int(3L))
    .build()
  
  logger.emit(retry_log)
  
  // 10. 模拟备用支付网关成功
  let backup_attributes = [
    ("service.name", AttributeValue::string(service_resource.service_name)),
    ("external.service", AttributeValue::string("backup-payment-gateway")),
    ("operation.name", AttributeValue::string("charge_payment_backup")),
    ("payment.id", AttributeValue::string("pay-12345")),
    ("gateway.provider", AttributeValue::string("paypal")),
    ("fallback.reason", AttributeValue::string("primary_gateway_failure"))
  ]
  
  let (_, backup_span) = tracer.start_span(
    retry_ctx, 
    "backup_payment_gateway.charge", 
    Client, 
    Some(backup_attributes)
  )
  
  // 11. 记录成功恢复日志
  let recovery_log = LogRecord::builder()
    .timestamp(1640995200000000300L)
    .severity(SeverityNumber::Info)
    .body("Payment processed successfully via backup gateway")
    .with_attribute("service.name", AttributeValue::string(service_resource.service_name))
    .with_attribute("payment.id", AttributeValue::string("pay-12345"))
    .with_attribute("backup.gateway", AttributeValue::string("paypal"))
    .with_attribute("total.duration_ms", AttributeValue::int(300L))
    .with_attribute("recovery.success", AttributeValue::bool(true))
    .build()
  
  logger.emit(recovery_log)
  
  // 12. 验证错误处理和恢复流程
  assert_eq(payment_span.name, "payment.process")
  assert_eq(payment_span.kind, Server)
  assert_eq(payment_span.attributes.length(), 6)
  
  assert_eq(gateway_span.name, "payment_gateway.charge")
  assert_eq(gateway_span.kind, Client)
  assert_eq(gateway_span.attributes.length(), 5)
  
  assert_eq(retry_span.name, "payment.retry")
  assert_eq(retry_span.kind, Internal)
  assert_eq(retry_span.attributes.length(), 5)
  
  assert_eq(backup_span.name, "backup_payment_gateway.charge")
  assert_eq(backup_span.kind, Client)
  assert_eq(backup_span.attributes.length(), 6)
  
  // 验证错误事件
  assert_eq(error_event.name, "payment_gateway_error")
  assert_eq(error_event.timestamp_unix_nanos, 1640995200000000100L)
  assert_eq(error_event.attributes.length(), 5)
  
  // 验证日志记录的严重程度和内容
  assert_eq(start_log.severity_number, SeverityNumber::Info)
  assert_eq(error_log.severity_number, SeverityNumber::Error)
  assert_eq(retry_log.severity_number, SeverityNumber::Warn)
  assert_eq(recovery_log.severity_number, SeverityNumber::Info)
  
  match error_log.body {
    Some(body) => assert_eq(body, "Payment gateway error: timeout occurred")
    None => @test.fail("Test failed")
  }
  
  match recovery_log.body {
    Some(body) => assert_eq(body, "Payment processed successfully via backup gateway")
    None => @test.fail("Test failed")
  }
}
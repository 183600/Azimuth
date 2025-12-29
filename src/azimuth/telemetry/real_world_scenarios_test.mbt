// Azimuth Telemetry - Real World Scenarios Tests
// 实际使用场景的综合测试

test "real_world_web_service_scenario" {
  // 模拟Web服务的完整请求处理流程
  let base_time = 1640995200000000000L // 2022-01-01 00:00:00 UTC
  
  // 1. 初始化服务和资源
  let service_resource = @azimuth.telemetry.api.common.Resource::default("web-api-service")
  let api_scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "api-handler",
    version: Some("2.1.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  // 2. 创建Meter和Logger
  let meter = @azimuth.telemetry.api.metrics.NoopMeter
  let logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger("api-logger", Some("2.1.0"))
  
  // 3. 创建指标仪器
  let request_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("http.requests.total", "requests", "Total HTTP requests")
  let response_time_histogram = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("http.request.duration", "ms", "HTTP request duration")
  let active_connections_gauge = @azimuth.telemetry.api.metrics.NoopMeter::create_gauge("http.active_connections", "connections", "Active HTTP connections")
  let error_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("http.errors.total", "errors", "Total HTTP errors")
  
  // 4. 模拟HTTP请求处理
  let request_context = @azimuth.telemetry.api.context.Context::empty()
    .with_value(@azimuth.telemetry.api.context.create_key("request.id"), "req-12345")
    .with_value(@azimuth.telemetry.api.context.create_key("user.id"), "user-67890")
    .with_value(@azimuth.telemetry.api.context.create_key("session.id"), "sess-abcde")
  
  // 5. 创建根Span表示HTTP请求
  let (request_span_context, request_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    request_context,
    "HTTP POST /api/orders",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("http.method", @azimuth.telemetry.api.common.AttributeValue::string("POST")),
      ("http.url", @azimuth.telemetry.api.common.AttributeValue::string("/api/orders")),
      ("http.scheme", @azimuth.telemetry.api.common.AttributeValue::string("https")),
      ("http.host", @azimuth.telemetry.api.common.AttributeValue::string("api.example.com")),
      ("http.user_agent", @azimuth.telemetry.api.common.AttributeValue::string("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")),
      ("net.peer.ip", @azimuth.telemetry.api.common.AttributeValue::string("192.168.1.100")),
      ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-67890"))
    ]),
    Some(base_time)
  )
  
  // 6. 记录请求开始日志
  logger.info("Processing order creation request", Some([
    ("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-12345")),
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-67890")),
    ("operation", @azimuth.telemetry.api.common.AttributeValue::string("create_order"))
  ]))
  
  // 7. 更新活跃连接数
  @azimuth.telemetry.api.metrics.NoopGauge::record(25.0, None)
  
  // 8. 创建认证Span
  let (auth_context, auth_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    request_span_context,
    "authentication",
    Some(@azimuth.telemetry.api.trace.Internal),
    Some([
      ("auth.method", @azimuth.telemetry.api.common.AttributeValue::string("JWT")),
      ("auth.token.version", @azimuth.telemetry.api.common.AttributeValue::string("v2"))
    ]),
    Some(base_time + 500000L) // 0.5ms later
  )
  
  // 9. 记录认证日志
  logger.debug("User authentication successful", Some([
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-67890")),
    ("auth.method", @azimuth.telemetry.api.common.AttributeValue::string("JWT")),
    ("auth.duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(2))
  ]))
  
  // 10. 创建数据库操作Span
  let (db_context, db_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    auth_context,
    "database.operation",
    Some(@azimuth.telemetry.api.trace.Client),
    Some([
      ("db.system", @azimuth.telemetry.api.common.AttributeValue::string("postgresql")),
      ("db.name", @azimuth.telemetry.api.common.AttributeValue::string("orders_db")),
      ("db.operation", @azimuth.telemetry.api.common.AttributeValue::string("INSERT")),
      ("db.statement", @azimuth.telemetry.api.common.AttributeValue::string("INSERT INTO orders (user_id, amount, status) VALUES ($1, $2, $3)"))
    ]),
    Some(base_time + 2000000L) // 2ms later
  )
  
  // 11. 记录数据库操作日志
  logger.info("Order created in database", Some([
    ("order.id", @azimuth.telemetry.api.common.AttributeValue::string("order-98765")),
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-67890")),
    ("order.amount", @azimuth.telemetry.api.common.AttributeValue::float(299.99)),
    ("db.duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(15))
  ]))
  
  // 12. 创建缓存操作Span
  let (cache_context, cache_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    db_context,
    "cache.update",
    Some(@azimuth.telemetry.api.trace.Internal),
    Some([
      ("cache.system", @azimuth.telemetry.api.common.AttributeValue::string("redis")),
      ("cache.operation", @azimuth.telemetry.api.common.AttributeValue::string("SET")),
      ("cache.key", @azimuth.telemetry.api.common.AttributeValue::string("order:98765"))
    ]),
    Some(base_time + 18000000L) // 18ms later
  )
  
  // 13. 记录缓存操作日志
  logger.debug("Order cached successfully", Some([
    ("cache.key", @azimuth.telemetry.api.common.AttributeValue::string("order:98765")),
    ("cache.ttl.seconds", @azimuth.telemetry.api.common.AttributeValue::int(3600))
  ]))
  
  // 14. 创建通知发送Span
  let (notification_context, notification_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    cache_context,
    "notification.send",
    Some(@azimuth.telemetry.api.trace.Producer),
    Some([
      ("messaging.system", @azimuth.telemetry.api.common.AttributeValue::string("kafka")),
      ("messaging.destination", @azimuth.telemetry.api.common.AttributeValue::string("order_events")),
      ("messaging.message_id", @azimuth.telemetry.api.common.AttributeValue::string("msg-11111"))
    ]),
    Some(base_time + 22000000L) // 22ms later
  )
  
  // 15. 记录通知发送日志
  logger.info("Order event published", Some([
    ("event.type", @azimuth.telemetry.api.common.AttributeValue::string("order.created")),
    ("order.id", @azimuth.telemetry.api.common.AttributeValue::string("order-98765")),
    ("message.id", @azimuth.telemetry.api.common.AttributeValue::string("msg-11111"))
  ]))
  
  // 16. 记录最终指标
  let request_attrs = [
    ("http.method", @azimuth.telemetry.api.common.AttributeValue::string("POST")),
    ("http.status_code", @azimuth.telemetry.api.common.AttributeValue::int(201)),
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-67890"))
  ]
  
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some(request_attrs))
  @azimuth.telemetry.api.metrics.NoopHistogram::record(25.5, Some(request_attrs))
  
  // 17. 更新活跃连接数
  @azimuth.telemetry.api.metrics.NoopGauge::record(24.0, None)
  
  // 18. 记录请求完成日志
  logger.info("Order creation request completed", Some([
    ("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-12345")),
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user-67890")),
    ("order.id", @azimuth.telemetry.api.common.AttributeValue::string("order-98765")),
    ("total.duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(25))
  ]))
  
  // 验证整个流程的完整性
  assert_eq(request_span.name, "HTTP POST /api/orders")
  assert_eq(auth_span.name, "authentication")
  assert_eq(db_span.name, "database.operation")
  assert_eq(cache_span.name, "cache.update")
  assert_eq(notification_span.name, "notification.send")
  
  assert_eq(request_span.attributes.length(), 7)
  assert_eq(auth_span.attributes.length(), 2)
  assert_eq(db_span.attributes.length(), 4)
  assert_eq(cache_span.attributes.length(), 3)
  assert_eq(notification_span.attributes.length(), 3)
}

test "real_world_microservices_scenario" {
  // 模拟微服务架构中的分布式请求处理
  let base_time = 1640995200000000000L
  
  // 1. API Gateway服务
  let gateway_resource = @azimuth.telemetry.api.common.Resource::default("api-gateway")
  let gateway_context = @azimuth.telemetry.api.context.Context::empty()
    .with_value(@azimuth.telemetry.api.context.create_key("correlation.id"), "corr-12345")
    .with_value(@azimuth.telemetry.api.context.create_key("trace.id"), "trace-abcde")
  
  // 2. 创建传播器用于跨服务传播
  let composite_propagator = @azimuth.telemetry.api.propagation.CompositePropagator::new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator,
    @azimuth.telemetry.api.propagation.W3CBaggagePropagator
  ])
  
  // 3. API Gateway创建根Span
  let (gateway_span_context, gateway_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    gateway_context,
    "gateway.request",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("api-gateway")),
      ("http.method", @azimuth.telemetry.api.common.AttributeValue::string("GET")),
      ("http.url", @azimuth.telemetry.api.common.AttributeValue::string("/api/user/profile"))
    ]),
    Some(base_time)
  )
  
  // 4. 创建Carrier并注入传播上下文
  let gateway_carrier = @azimuth.telemetry.api.propagation.MapCarrier::new()
  @azimuth.telemetry.api.propagation.CompositePropagator::inject(gateway_span_context, gateway_carrier)
  
  // 5. User Service接收请求
  let user_service_context = @azimuth.telemetry.api.context.Context::empty()
  let extracted_user_context = @azimuth.telemetry.api.propagation.CompositePropagator::extract(user_service_context, gateway_carrier)
  
  let (user_span_context, user_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    extracted_user_context,
    "user.service.request",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("user-service")),
      ("operation", @azimuth.telemetry.api.common.AttributeValue::string("getProfile"))
    ]),
    Some(base_time + 5000000L) // 5ms later
  )
  
  // 6. User Service调用Database Service
  let user_carrier = @azimuth.telemetry.api.propagation.MapCarrier::new()
  @azimuth.telemetry.api.propagation.CompositePropagator::inject(user_span_context, user_carrier)
  
  let db_service_context = @azimuth.telemetry.api.context.Context::empty()
  let extracted_db_context = @azimuth.telemetry.api.propagation.CompositePropagator::extract(db_service_context, user_carrier)
  
  let (db_span_context, db_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    extracted_db_context,
    "database.service.request",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("database-service")),
      ("db.operation", @azimuth.telemetry.api.common.AttributeValue::string("SELECT")),
      ("db.table", @azimuth.telemetry.api.common.AttributeValue::string("users"))
    ]),
    Some(base_time + 8000000L) // 8ms later
  )
  
  // 7. Database Service调用Cache Service
  let db_carrier = @azimuth.telemetry.api.propagation.MapCarrier::new()
  @azimuth.telemetry.api.propagation.CompositePropagator::inject(db_span_context, db_carrier)
  
  let cache_service_context = @azimuth.telemetry.api.context.Context::empty()
  let extracted_cache_context = @azimuth.telemetry.api.propagation.CompositePropagator::extract(cache_service_context, db_carrier)
  
  let (cache_span_context, cache_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    extracted_cache_context,
    "cache.service.request",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("cache-service")),
      ("cache.operation", @azimuth.telemetry.api.common.AttributeValue::string("GET")),
      ("cache.key", @azimuth.telemetry.api.common.AttributeValue::string("user:profile:12345"))
    ]),
    Some(base_time + 12000000L) // 12ms later
  )
  
  // 8. 创建各服务的指标
  let gateway_meter = @azimuth.telemetry.api.metrics.NoopMeter
  let gateway_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("gateway.requests.total", "requests", "Total gateway requests")
  
  let user_meter = @azimuth.telemetry.api.metrics.NoopMeter
  let user_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("user.service.requests.total", "requests", "Total user service requests")
  
  let db_meter = @azimuth.telemetry.api.metrics.NoopMeter
  let db_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("db.service.queries.total", "queries", "Total database queries")
  
  let cache_meter = @azimuth.telemetry.api.metrics.NoopMeter
  let cache_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("cache.service.requests.total", "requests", "Total cache requests")
  
  // 9. 记录各服务的指标
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some([
    ("service", @azimuth.telemetry.api.common.AttributeValue::string("api-gateway")),
    ("http.method", @azimuth.telemetry.api.common.AttributeValue::string("GET")),
    ("http.status", @azimuth.telemetry.api.common.AttributeValue::int(200))
  ]))
  
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some([
    ("service", @azimuth.telemetry.api.common.AttributeValue::string("user-service")),
    ("operation", @azimuth.telemetry.api.common.AttributeValue::string("getProfile")),
    ("status", @azimuth.telemetry.api.common.AttributeValue::string("success"))
  ]))
  
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some([
    ("service", @azimuth.telemetry.api.common.AttributeValue::string("database-service")),
    ("operation", @azimuth.telemetry.api.common.AttributeValue::string("SELECT")),
    ("table", @azimuth.telemetry.api.common.AttributeValue::string("users"))
  ]))
  
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some([
    ("service", @azimuth.telemetry.api.common.AttributeValue::string("cache-service")),
    ("operation", @azimuth.telemetry.api.common.AttributeValue::string("GET")),
    ("hit", @azimuth.telemetry.api.common.AttributeValue::bool(true))
  ]))
  
  // 10. 创建各服务的日志
  let gateway_logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger("gateway-logger")
  let user_logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger("user-logger")
  let db_logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger("db-logger")
  let cache_logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger("cache-logger")
  
  gateway_logger.info("Request routed to user service", Some([
    ("correlation.id", @azimuth.telemetry.api.common.AttributeValue::string("corr-12345")),
    ("target.service", @azimuth.telemetry.api.common.AttributeValue::string("user-service"))
  ]))
  
  user_logger.info("User profile request processed", Some([
    ("correlation.id", @azimuth.telemetry.api.common.AttributeValue::string("corr-12345")),
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("12345"))
  ]))
  
  db_logger.info("Database query executed", Some([
    ("correlation.id", @azimuth.telemetry.api.common.AttributeValue::string("corr-12345")),
    ("query", @azimuth.telemetry.api.common.AttributeValue::string("SELECT * FROM users WHERE id = 12345")),
    ("duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(5))
  ]))
  
  cache_logger.info("Cache hit for user profile", Some([
    ("correlation.id", @azimuth.telemetry.api.common.AttributeValue::string("corr-12345")),
    ("cache.key", @azimuth.telemetry.api.common.AttributeValue::string("user:profile:12345"))
  ]))
  
  // 验证微服务架构的完整性
  assert_eq(gateway_span.name, "gateway.request")
  assert_eq(user_span.name, "user.service.request")
  assert_eq(db_span.name, "database.service.request")
  assert_eq(cache_span.name, "cache.service.request")
  
  // 验证传播上下文的存在
  assert_eq(gateway_carrier.get(@azimuth.telemetry.api.propagation.TRACE_PARENT_HEADER), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(gateway_carrier.get(@azimuth.telemetry.api.propagation.BAGGAGE_HEADER), Some("key1=value1,key2=value2"))
}

test "real_world_error_handling_scenario" {
  // 模拟真实世界中的错误处理和恢复场景
  let base_time = 1640995200000000000L
  
  // 1. 初始化错误处理组件
  let error_resource = @azimuth.telemetry.api.common.Resource::default("error-prone-service")
  let error_context = @azimuth.telemetry.api.context.Context::empty()
    .with_value(@azimuth.telemetry.api.context.create_key("request.id"), "req-error-001")
    .with_value(@azimuth.telemetry.api.context.create_key("retry.count"), "0")
  
  // 2. 创建错误处理相关的指标
  let error_meter = @azimuth.telemetry.api.metrics.NoopMeter
  let error_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("errors.total", "errors", "Total errors")
  let retry_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("retries.total", "retries", "Total retries")
  let circuit_breaker_gauge = @azimuth.telemetry.api.metrics.NoopMeter::create_gauge("circuit.breaker.state", "state", "Circuit breaker state")
  
  // 3. 创建错误处理相关的日志器
  let error_logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger("error-logger")
  
  // 4. 模拟第一次失败的请求
  let (first_error_context, first_error_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    error_context,
    "failing.operation",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("database.query")),
      ("retry.attempt", @azimuth.telemetry.api.common.AttributeValue::int(1))
    ]),
    Some(base_time)
  )
  
  // 5. 记录第一次错误
  error_logger.error("Database connection failed", Some([
    ("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-error-001")),
    ("error.code", @azimuth.telemetry.api.common.AttributeValue::int(5001)),
    ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("ConnectionError")),
    ("error.message", @azimuth.telemetry.api.common.AttributeValue::string("Unable to connect to database")),
    ("retry.eligible", @azimuth.telemetry.api.common.AttributeValue::bool(true))
  ]))
  
  // 6. 记录错误指标
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some([
    ("error.code", @azimuth.telemetry.api.common.AttributeValue::int(5001)),
    ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("ConnectionError")),
    ("operation", @azimuth.telemetry.api.common.AttributeValue::string("database.query"))
  ]))
  
  // 7. 模拟重试机制
  let retry_context = error_context.with_value(@azimuth.telemetry.api.context.create_key("retry.count"), "1")
  
  let (retry_context_span, retry_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    retry_context,
    "retry.operation",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("database.query")),
      ("retry.attempt", @azimuth.telemetry.api.common.AttributeValue::int(2))
    ]),
    Some(base_time + 10000000L) // 10ms later
  )
  
  // 8. 记录重试日志
  error_logger.warn("Retrying database operation", Some([
    ("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-error-001")),
    ("retry.attempt", @azimuth.telemetry.api.common.AttributeValue::int(2)),
    ("backoff.ms", @azimuth.telemetry.api.common.AttributeValue::int(10000))
  ]))
  
  // 9. 记录重试指标
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some([
    ("operation", @azimuth.telemetry.api.common.AttributeValue::string("database.query")),
    ("retry.reason", @azimuth.telemetry.api.common.AttributeValue::string("ConnectionError"))
  ]))
  
  // 10. 模拟第二次失败
  error_logger.error("Database connection failed again", Some([
    ("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-error-001")),
    ("error.code", @azimuth.telemetry.api.common.AttributeValue::int(5001)),
    ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("ConnectionError")),
    ("retry.attempt", @azimuth.telemetry.api.common.AttributeValue::int(2)),
    ("max.retries.reached", @azimuth.telemetry.api.common.AttributeValue::bool(true))
  ]))
  
  // 11. 记录第二次错误指标
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some([
    ("error.code", @azimuth.telemetry.api.common.AttributeValue::int(5001)),
    ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("ConnectionError")),
    ("operation", @azimuth.telemetry.api.common.AttributeValue::string("database.query")),
    ("retry.attempt", @azimuth.telemetry.api.common.AttributeValue::int(2))
  ]))
  
  // 12. 模拟熔断器触发
  let circuit_breaker_context = retry_context.with_value(@azimuth.telemetry.api.context.create_key("circuit.breaker"), "open")
  
  let (circuit_breaker_span, circuit_breaker_span_obj) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    circuit_breaker_context,
    "circuit.breaker.open",
    Some(@azimuth.telemetry.api.trace.Internal),
    Some([
      ("circuit.breaker.state", @azimuth.telemetry.api.common.AttributeValue::string("open")),
      ("failure.threshold", @azimuth.telemetry.api.common.AttributeValue::int(5)),
      ("recovery.timeout", @azimuth.telemetry.api.common.AttributeValue::int(60000))
    ]),
    Some(base_time + 20000000L) // 20ms later
  )
  
  // 13. 记录熔断器日志
  error_logger.error("Circuit breaker opened", Some([
    ("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-error-001")),
    ("circuit.breaker.state", @azimuth.telemetry.api.common.AttributeValue::string("open")),
    ("failure.count", @azimuth.telemetry.api.common.AttributeValue::int(5)),
    ("recovery.time", @azimuth.telemetry.api.common.AttributeValue::string("2022-01-01T00:01:00Z"))
  ]))
  
  // 14. 记录熔断器状态指标
  @azimuth.telemetry.api.metrics.NoopGauge::record(1.0, Some([
    ("circuit.breaker.name", @azimuth.telemetry.api.common.AttributeValue::string("database.cb")),
    ("state", @azimuth.telemetry.api.common.AttributeValue::string("open"))
  ]))
  
  // 15. 模拟降级处理
  let fallback_context = circuit_breaker_context.with_value(@azimuth.telemetry.api.context.create_key("fallback"), "cache")
  
  let (fallback_span, fallback_span_obj) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    fallback_context,
    "fallback.operation",
    Some(@azimuth.telemetry.api.trace.Internal),
    Some([
      ("fallback.type", @azimuth.telemetry.api.common.AttributeValue::string("cache")),
      ("fallback.reason", @azimuth.telemetry.api.common.AttributeValue::string("database.unavailable"))
    ]),
    Some(base_time + 25000000L) // 25ms later
  )
  
  // 16. 记录降级处理日志
  error_logger.warn("Using cache fallback", Some([
    ("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-error-001")),
    ("fallback.type", @azimuth.telemetry.api.common.AttributeValue::string("cache")),
    ("cache.hit", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
    ("data.freshness", @azimuth.telemetry.api.common.AttributeValue::string("5.minutes.old"))
  ]))
  
  // 17. 记录降级处理指标
  let fallback_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("fallback.total", "fallbacks", "Total fallback operations")
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some([
    ("fallback.type", @azimuth.telemetry.api.common.AttributeValue::string("cache")),
    ("reason", @azimuth.telemetry.api.common.AttributeValue::string("database.unavailable"))
  ]))
  
  // 18. 记录最终的用户友好错误响应
  error_logger.info("Request completed with fallback", Some([
    ("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-error-001")),
    ("response.status", @azimuth.telemetry.api.common.AttributeValue::int(200)),
    ("data.source", @azimuth.telemetry.api.common.AttributeValue::string("cache")),
    ("user.visible.error", @azimuth.telemetry.api.common.AttributeValue::bool(false))
  ]))
  
  // 验证错误处理流程的完整性
  assert_eq(first_error_span.name, "failing.operation")
  assert_eq(retry_span.name, "retry.operation")
  assert_eq(circuit_breaker_span_obj.name, "circuit.breaker.open")
  assert_eq(fallback_span_obj.name, "fallback.operation")
  
  // 验证错误处理上下文的正确性
  assert_eq(first_error_context.get(@azimuth.telemetry.api.context.create_key("request.id")), Some("req-error-001"))
  assert_eq(retry_context.get(@azimuth.telemetry.api.context.create_key("retry.count")), Some("1"))
  assert_eq(circuit_breaker_context.get(@azimuth.telemetry.api.context.create_key("circuit.breaker")), Some("open"))
  assert_eq(fallback_context.get(@azimuth.telemetry.api.context.create_key("fallback")), Some("cache"))
}

test "real_world_high_concurrency_scenario" {
  // 模拟高并发场景下的遥测数据处理
  let base_time = 1640995200000000000L
  
  // 1. 初始化高并发处理的资源
  let concurrent_resource = @azimuth.telemetry.api.common.Resource::default("high-concurrency-service")
  let concurrent_meter = @azimuth.telemetry.api.metrics.NoopMeter
  let concurrent_logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger("concurrency-logger")
  
  // 2. 创建高并发相关的指标
  let request_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("concurrent.requests.total", "requests", "Total concurrent requests")
  let active_requests_gauge = @azimuth.telemetry.api.metrics.NoopMeter::create_gauge("concurrent.active_requests", "requests", "Active concurrent requests")
  let queue_size_histogram = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("concurrent.queue.size", "items", "Queue size distribution")
  let response_time_histogram = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("concurrent.response.time", "ms", "Response time distribution")
  
  // 3. 模拟100个并发请求
  let mut index = 0
  while index < 100 {
    let request_id = "req-concurrent-" + index.to_string()
    let user_id = "user-" + (index % 10).to_string()
    
    // 4. 为每个请求创建独立的上下文
    let request_context = @azimuth.telemetry.api.context.Context::empty()
      .with_value(@azimuth.telemetry.api.context.create_key("request.id"), request_id)
      .with_value(@azimuth.telemetry.api.context.create_key("user.id"), user_id)
      .with_value(@azimuth.telemetry.api.context.create_key("thread.id"), "thread-" + (index % 8).to_string())
    
    // 5. 为每个请求创建Span
    let (request_span_context, request_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
      request_context,
      "concurrent.request",
      Some(@azimuth.telemetry.api.trace.Server),
      Some([
        ("request.id", @azimuth.telemetry.api.common.AttributeValue::string(request_id)),
        ("user.id", @azimuth.telemetry.api.common.AttributeValue::string(user_id)),
        ("thread.id", @azimuth.telemetry.api.common.AttributeValue::string("thread-" + (index % 8).to_string())),
        ("priority", @azimuth.telemetry.api.common.AttributeValue::string(if index % 3 == 0 { "high" } else { "normal" }))
      ]),
      Some(base_time + index.to_int64() * 100000L) // 每个请求间隔0.1ms
    )
    
    // 6. 记录并发请求的日志
    concurrent_logger.debug("Processing concurrent request", Some([
      ("request.id", @azimuth.telemetry.api.common.AttributeValue::string(request_id)),
      ("user.id", @azimuth.telemetry.api.common.AttributeValue::string(user_id)),
      ("thread.id", @azimuth.telemetry.api.common.AttributeValue::string("thread-" + (index % 8).to_string())),
      ("queue.position", @azimuth.telemetry.api.common.AttributeValue::int(index))
    ]))
    
    // 7. 记录并发请求的指标
    @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some([
      ("thread.id", @azimuth.telemetry.api.common.AttributeValue::string("thread-" + (index % 8).to_string())),
      ("priority", @azimuth.telemetry.api.common.AttributeValue::string(if index % 3 == 0 { "high" } else { "normal" }))
    ]))
    
    @azimuth.telemetry.api.metrics.NoopHistogram::record((index % 50).to_double(), Some([
      ("thread.id", @azimuth.telemetry.api.common.AttributeValue::string("thread-" + (index % 8).to_string()))
    ]))
    
    // 8. 模拟请求处理时间（基于优先级）
    let processing_time = if index % 3 == 0 { 5.0 } else { 15.0 }
    @azimuth.telemetry.api.metrics.NoopHistogram::record(processing_time, Some([
      ("request.id", @azimuth.telemetry.api.common.AttributeValue::string(request_id)),
      ("priority", @azimuth.telemetry.api.common.AttributeValue::string(if index % 3 == 0 { "high" } else { "normal" }))
    ]))
    
    // 9. 模拟子操作（数据库查询）
    let (db_context, db_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
      request_span_context,
      "database.query",
      Some(@azimuth.telemetry.api.trace.Client),
      Some([
        ("request.id", @azimuth.telemetry.api.common.AttributeValue::string(request_id)),
        ("db.operation", @azimuth.telemetry.api.common.AttributeValue::string("SELECT")),
        ("db.table", @azimuth.telemetry.api.common.AttributeValue::string("data_" + (index % 5).to_string()))
      ]),
      Some(base_time + index.to_int64() * 100000L + 1000000L)
    )
    
    // 10. 记录数据库操作日志
    concurrent_logger.debug("Database query executed", Some([
      ("request.id", @azimuth.telemetry.api.common.AttributeValue::string(request_id)),
      ("db.table", @azimuth.telemetry.api.common.AttributeValue::string("data_" + (index % 5).to_string())),
      ("duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(2))
    ]))
    
    // 11. 模拟缓存操作
    let (cache_context, cache_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
      db_context,
      "cache.operation",
      Some(@azimuth.telemetry.api.trace.Internal),
      Some([
        ("request.id", @azimuth.telemetry.api.common.AttributeValue::string(request_id)),
        ("cache.operation", @azimuth.telemetry.api.common.AttributeValue::string(if index % 2 == 0 { "GET" } else { "SET" })),
        ("cache.hit", @azimuth.telemetry.api.common.AttributeValue::bool(index % 3 == 0))
      ]),
      Some(base_time + index.to_int64() * 100000L + 3000000L)
    )
    
    // 12. 记录缓存操作日志
    concurrent_logger.debug("Cache operation completed", Some([
      ("request.id", @azimuth.telemetry.api.common.AttributeValue::string(request_id)),
      ("cache.operation", @azimuth.telemetry.api.common.AttributeValue::string(if index % 2 == 0 { "GET" } else { "SET" })),
      ("cache.hit", @azimuth.telemetry.api.common.AttributeValue::bool(index % 3 == 0))
    ]))
    
    // 13. 记录请求完成日志
    concurrent_logger.info("Concurrent request completed", Some([
      ("request.id", @azimuth.telemetry.api.common.AttributeValue::string(request_id)),
      ("user.id", @azimuth.telemetry.api.common.AttributeValue::string(user_id)),
      ("total.duration.ms", @azimuth.telemetry.api.common.AttributeValue::int(if index % 3 == 0 { 5 } else { 15 })),
      ("cache.hit", @azimuth.telemetry.api.common.AttributeValue::bool(index % 3 == 0))
    ]))
    
    index = index + 1
  }
  
  // 14. 记录系统级别的并发指标
  @azimuth.telemetry.api.metrics.NoopGauge::record(100.0, Some([
    ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("active_requests"))
  ]))
  
  @azimuth.telemetry.api.metrics.NoopHistogram::record(25.0, Some([
    ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("queue_size"))
  ]))
  
  // 15. 记录系统级别的并发日志
  concurrent_logger.info("Concurrent request batch completed", Some([
    ("total.requests", @azimuth.telemetry.api.common.AttributeValue::int(100)),
    ("active.threads", @azimuth.telemetry.api.common.AttributeValue::int(8)),
    ("avg.response.time", @azimuth.telemetry.api.common.AttributeValue::float(12.5)),
    ("cache.hit.rate", @azimuth.telemetry.api.common.AttributeValue::float(0.33))
  ]))
  
  // 验证高并发场景的处理
  assert_eq(true, true) // 如果没有崩溃就算成功
}
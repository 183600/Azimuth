// 跨服务集成测试
// Cross-service integration tests

test "微服务链路追踪测试" {
  // 测试微服务间的链路追踪
  
  // 初始化各服务的Tracer
  let gateway_provider = TracerProvider::default()
  let gateway_tracer = TracerProvider::get_tracer(gateway_provider, "api-gateway")
  
  let user_provider = TracerProvider::default()
  let user_tracer = TracerProvider::get_tracer(user_provider, "user-service")
  
  let order_provider = TracerProvider::default()
  let order_tracer = TracerProvider::get_tracer(order_provider, "order-service")
  
  let payment_provider = TracerProvider::default()
  let payment_tracer = TracerProvider::get_tracer(payment_provider, "payment-service")
  
  // API Gateway处理请求
  let gateway_ctx = Context::root()
  let (gateway_ctx_with_span, gateway_span) = Tracer::start_span(gateway_tracer, gateway_ctx, "api-gateway.request")
  
  Span::add_event(gateway_span, "请求接收", Some([
    ("http.method", StringValue("POST")),
    ("http.path", StringValue("/api/orders")),
    ("user.id", StringValue("user-12345"))
  ]))
  
  // 调用用户服务
  let user_ctx = Context::with_value(gateway_ctx_with_span, ContextKey::new("service.call"), "user-service")
  let (user_ctx_with_span, user_span) = Tracer::start_span(user_tracer, user_ctx, "user-service.validate")
  
  Span::add_event(user_span, "用户验证", Some([
    ("validation.type", StringValue("authentication")),
    ("user.status", StringValue("active"))
  ]))
  
  Span::set_status(user_span, Ok, Some("用户验证成功"))
  Span::end(user_span)
  
  // 在Gateway记录用户服务调用结果
  Span::add_event(gateway_span, "用户服务调用完成", Some([
    ("service.name", StringValue("user-service")),
    ("call.duration", IntValue(50)),
    ("call.status", StringValue("success"))
  ]))
  
  // 调用订单服务
  let order_ctx = Context::with_value(gateway_ctx_with_span, ContextKey::new("service.call"), "order-service")
  let (order_ctx_with_span, order_span) = Tracer::start_span(order_tracer, order_ctx, "order-service.create")
  
  Span::add_event(order_span, "订单创建", Some([
    ("order.id", StringValue("order-67890")),
    ("order.amount", FloatValue(299.99)),
    ("product.count", IntValue(3))
  ]))
  
  // 调用支付服务（从订单服务）
  let payment_ctx = Context::with_value(order_ctx_with_span, ContextKey::new("service.call"), "payment-service")
  let (payment_ctx_with_span, payment_span) = Tracer::start_span(payment_tracer, payment_ctx, "payment-service.process")
  
  Span::add_event(payment_span, "支付处理", Some([
    ("payment.method", StringValue("credit_card")),
    ("payment.amount", FloatValue(299.99)),
    ("currency", StringValue("USD"))
  ]))
  
  // 模拟支付处理失败
  Span::add_event(payment_span, "支付失败", Some([
    ("error.code", StringValue("PAYMENT_DECLINED")),
    ("error.reason", StringValue("insufficient_funds"))
  ]))
  
  Span::set_status(payment_span, Error, Some("支付被拒绝"))
  Span::end(payment_span)
  
  // 在订单服务记录支付结果
  Span::add_event(order_span, "支付服务调用完成", Some([
    ("service.name", StringValue("payment-service")),
    ("call.duration", IntValue(200)),
    ("call.status", StringValue("error"))
  ]))
  
  Span::set_status(order_span, Error, Some("订单创建失败：支付失败"))
  Span::end(order_span)
  
  // 在Gateway记录订单服务调用结果
  Span::add_event(gateway_span, "订单服务调用完成", Some([
    ("service.name", StringValue("order-service")),
    ("call.duration", IntValue(300)),
    ("call.status", StringValue("error"))
  ]))
  
  Span::set_status(gateway_span, Error, Some("请求处理失败：支付失败"))
  Span::end(gateway_span)
}

test "服务间指标聚合测试" {
  // 测试服务间指标的聚合
  
  // 各服务的Meter
  let gateway_meter_provider = MeterProvider::default()
  let gateway_meter = MeterProvider::get_meter(gateway_meter_provider, "api-gateway")
  
  let user_meter_provider = MeterProvider::default()
  let user_meter = MeterProvider::get_meter(user_meter_provider, "user-service")
  
  let order_meter_provider = MeterProvider::default()
  let order_meter = MeterProvider::get_meter(order_meter_provider, "order-service")
  
  let payment_meter_provider = MeterProvider::default()
  let payment_meter = MeterProvider::get_meter(payment_meter_provider, "payment-service")
  
  // API Gateway指标
  let gateway_requests = Meter::create_counter(gateway_meter, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  let gateway_duration = Meter::create_histogram(gateway_meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  
  Counter::add(gateway_requests, 1000.0)
  Histogram::record(gateway_duration, 150.0)
  Histogram::record(gateway_duration, 200.0)
  Histogram::record(gateway_duration, 120.0)
  
  // 用户服务指标
  let user_validations = Meter::create_counter(user_meter, "user.validations.total", Some("Total user validations"), Some("count"))
  let user_db_queries = Meter::create_histogram(user_meter, "user.db.query.duration", Some("User database query duration"), Some("ms"))
  
  Counter::add(user_validations, 950.0)
  Histogram::record(user_db_queries, 25.0)
  Histogram::record(user_db_queries, 30.0)
  Histogram::record(user_db_queries, 20.0)
  
  // 订单服务指标
  let order_creations = Meter::create_counter(order_meter, "order.creations.total", Some("Total order creations"), Some("count"))
  let order_amount = Meter::create_histogram(order_meter, "order.amount", Some("Order amount distribution"), Some("USD"))
  
  Counter::add(order_creations, 300.0)
  Histogram::record(order_amount, 99.99)
  Histogram::record(order_amount, 299.99)
  Histogram::record(order_amount, 199.99)
  
  // 支付服务指标
  let payment_attempts = Meter::create_counter(payment_meter, "payment.attempts.total", Some("Total payment attempts"), Some("count"))
  let payment_success = Meter::create_counter(payment_meter, "payment.success.total", Some("Total successful payments"), Some("count"))
  let payment_failures = Meter::create_counter(payment_meter, "payment.failures.total", Some("Total failed payments"), Some("count"))
  
  Counter::add(payment_attempts, 300.0)
  Counter::add(payment_success, 270.0)
  Counter::add(payment_failures, 30.0)
  
  // 验证指标值
  assert_eq(Instrument::name(gateway_requests), "http.requests.total")
  assert_eq(Instrument::name(user_validations), "user.validations.total")
  assert_eq(Instrument::name(order_creations), "order.creations.total")
  assert_eq(Instrument::name(payment_attempts), "payment.attempts.total")
}

test "分布式日志关联测试" {
  // 测试分布式日志的关联
  
  // 各服务的Logger
  let gateway_logger_provider = LoggerProvider::default()
  let gateway_logger = LoggerProvider::get_logger(gateway_logger_provider, "api-gateway")
  
  let user_logger_provider = LoggerProvider::default()
  let user_logger = LoggerProvider::get_logger(user_logger_provider, "user-service")
  
  let order_logger_provider = LoggerProvider::default()
  let order_logger = LoggerProvider::get_logger(order_logger_provider, "order-service")
  
  let payment_logger_provider = LoggerProvider::default()
  let payment_logger = LoggerProvider::get_logger(payment_logger_provider, "payment-service")
  
  // 创建关联的日志记录
  let trace_id = "trace-12345"
  let request_id = "req-67890"
  
  // Gateway日志
  let gateway_log = LogRecord::new_with_context(
    Info,
    Some("Processing order creation request"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(trace_id),
    Some("gateway-span"),
    Some(Context::with_value(Context::root(), ContextKey::new("request.id"), request_id))
  )
  
  Logger::emit(gateway_logger, gateway_log)
  
  // 用户服务日志
  let user_log = LogRecord::new_with_context(
    Info,
    Some("Validating user authentication"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(trace_id),
    Some("user-span"),
    Some(Context::with_value(Context::root(), ContextKey::new("request.id"), request_id))
  )
  
  Logger::emit(user_logger, user_log)
  
  // 订单服务日志
  let order_log = LogRecord::new_with_context(
    Info,
    Some("Creating new order"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(trace_id),
    Some("order-span"),
    Some(Context::with_value(Context::root(), ContextKey::new("request.id"), request_id))
  )
  
  Logger::emit(order_logger, order_log)
  
  // 支付服务错误日志
  let payment_error_log = LogRecord::new_with_context(
    Error,
    Some("Payment processing failed"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(trace_id),
    Some("payment-span"),
    Some(Context::with_value(Context::root(), ContextKey::new("request.id"), request_id))
  )
  
  Logger::emit(payment_logger, payment_error_log)
  
  // 验证日志记录的trace_id关联
  assert_eq(LogRecord::trace_id(gateway_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(user_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(order_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(payment_error_log), Some(trace_id))
}

test "服务发现和注册测试" {
  // 测试服务发现和注册的遥测
  
  // 服务注册中心的遥测
  let registry_provider = TracerProvider::default()
  let registry_tracer = TracerProvider::get_tracer(registry_provider, "service-registry")
  
  // 服务注册操作
  let register_span = Tracer::start_span(registry_tracer, "service.register")
  Span::add_event(register_span, "服务注册", Some([
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.port", IntValue(8081)),
    ("service.host", StringValue("10.0.1.10")),
    ("health.check.url", StringValue("/health"))
  ]))
  Span::end(register_span)
  
  // 服务发现操作
  let discover_span = Tracer::start_span(registry_tracer, "service.discover")
  Span::add_event(discover_span, "服务发现", Some([
    ("query.service", StringValue("order-service")),
    ("query.version", StringValue("latest")),
    ("result.count", IntValue(3))
  ]))
  Span::end(discover_span)
  
  // 健康检查操作
  let health_span = Tracer::start_span(registry_tracer, "health.check")
  Span::add_event(health_span, "健康检查", Some([
    ("service.name", StringValue("payment-service")),
    ("service.instance", StringValue("payment-01")),
    ("health.status", StringValue("healthy")),
    ("response.time", IntValue(15))
  ]))
  Span::end(health_span)
  
  // 服务注销操作
  let unregister_span = Tracer::start_span(registry_tracer, "service.unregister")
  Span::add_event(unregister_span, "服务注销", Some([
    ("service.name", StringValue("legacy-service")),
    ("service.instance", StringValue("legacy-03")),
    ("reason", StringValue("decommissioned"))
  ]))
  Span::end(unregister_span)
}

test "API网关路由测试" {
  // 测试API网关路由的遥测
  
  let gateway_provider = TracerProvider::default()
  let gateway_tracer = TracerProvider::get_tracer(gateway_provider, "api-gateway")
  
  let gateway_meter_provider = MeterProvider::default()
  let gateway_meter = MeterProvider::get_meter(gateway_meter_provider, "api-gateway")
  
  // 路由指标
  let route_requests = Meter::create_counter(gateway_meter, "route.requests.total", Some("Total routed requests"), Some("count"))
  let route_duration = Meter::create_histogram(gateway_meter, "route.duration", Some("Route processing duration"), Some("ms"))
  
  // 不同路由的处理
  let routes = [
    ("/api/users", "user-service", "GET"),
    ("/api/orders", "order-service", "POST"),
    ("/api/payments", "payment-service", "POST"),
    ("/api/products", "product-service", "GET"),
    ("/api/inventory", "inventory-service", "PUT")
  ]
  
  for i = 0; i < routes.length(); i = i + 1 {
    let route = routes[i]
    let path = route.0
    let service = route.1
    let method = route.2
    
    // 创建路由处理Span
    let route_span = Tracer::start_span(gateway_tracer, "route.request")
    
    Span::add_event(route_span, "路由开始", Some([
      ("http.path", StringValue(path)),
      ("http.method", StringValue(method)),
      ("target.service", StringValue(service))
    ]))
    
    // 模拟路由处理时间
    let processing_time = (i + 1) * 25
    Span::add_event(route_span, "路由完成", Some([
      ("processing.time", IntValue(processing_time)),
      ("target.service", StringValue(service)),
      ("status", StringValue("success"))
    ]))
    
    Span::end(route_span)
    
    // 记录路由指标
    Counter::add(route_requests, 1.0)
    Histogram::record(route_duration, processing_time.to_double())
  }
  
  // 错误路由处理
  let error_route_span = Tracer::start_span(gateway_tracer, "route.error")
  Span::add_event(error_route_span, "路由失败", Some([
    ("http.path", StringValue("/api/nonexistent")),
    ("http.method", StringValue("GET")),
    ("error.code", StringValue("SERVICE_NOT_FOUND")),
    ("error.message", StringValue("No service found for requested path"))
  ]))
  Span::set_status(error_route_span, Error, Some("路由失败：服务未找到"))
  Span::end(error_route_span)
}

test "消息队列集成测试" {
  // 测试消息队列集成的遥测
  
  let producer_provider = TracerProvider::default()
  let producer_tracer = TracerProvider::get_tracer(producer_provider, "message-producer")
  
  let consumer_provider = TracerProvider::default()
  let consumer_tracer = TracerProvider::get_tracer(consumer_provider, "message-consumer")
  
  // 消息生产
  let produce_span = Tracer::start_span(producer_tracer, "message.produce")
  Span::add_event(produce_span, "消息生成", Some([
    ("message.type", StringValue("order.created")),
    ("message.id", StringValue("msg-12345")),
    ("topic", StringValue("orders")),
    ("partition", IntValue(0)),
    ("message.size", IntValue(1024))
  ]))
  Span::end(produce_span)
  
  // 消息发送
  let send_span = Tracer::start_span(producer_tracer, "message.send")
  Span::add_event(send_span, "消息发送", Some([
    ("topic", StringValue("orders")),
    ("broker", StringValue("kafka-01:9092")),
    ("acknowledged", BoolValue(true)),
    ("send.time", IntValue(25))
  ]))
  Span::end(send_span)
  
  // 消息消费
  let consume_span = Tracer::start_span(consumer_tracer, "message.consume")
  Span::add_event(consume_span, "消息接收", Some([
    ("message.type", StringValue("order.created")),
    ("message.id", StringValue("msg-12345")),
    ("topic", StringValue("orders")),
    ("consumer.group", StringValue("order-processors")),
    ("partition", IntValue(0)),
    ("offset", IntValue(123456))
  ]))
  Span::end(consume_span)
  
  // 消息处理
  let process_span = Tracer::start_span(consumer_tracer, "message.process")
  Span::add_event(process_span, "消息处理", Some([
    ("message.id", StringValue("msg-12345")),
    ("processing.time", IntValue(150)),
    ("processing.result", StringValue("success")),
    ("order.id", StringValue("order-67890"))
  ]))
  Span::end(process_span)
  
  // 消息处理失败场景
  let error_process_span = Tracer::start_span(consumer_tracer, "message.process.error")
  Span::add_event(error_process_span, "消息处理失败", Some([
    ("message.id", StringValue("msg-54321")),
    ("error.type", StringValue("validation_error")),
    ("error.message", StringValue("Invalid order format")),
    ("retry.count", IntValue(3))
  ]))
  Span::set_status(error_process_span, Error, Some("消息处理失败"))
  Span::end(error_process_span)
}

test "数据库连接池监控测试" {
  // 测试数据库连接池的监控
  
  let db_provider = TracerProvider::default()
  let db_tracer = TracerProvider::get_tracer(db_provider, "database-service")
  
  let db_meter_provider = MeterProvider::default()
  let db_meter = MeterProvider::get_meter(db_meter_provider, "database-service")
  
  // 数据库指标
  let pool_connections = Meter::create_histogram(db_meter, "db.pool.connections", Some("Database pool connections"), Some("count"))
  let query_duration = Meter::create_histogram(db_meter, "db.query.duration", Some("Database query duration"), Some("ms"))
  let query_errors = Meter::create_counter(db_meter, "db.query.errors", Some("Database query errors"), Some("count"))
  
  // 连接池操作
  let pool_span = Tracer::start_span(db_tracer, "db.pool.operation")
  Span::add_event(pool_span, "连接池初始化", Some([
    ("pool.size", IntValue(20)),
    ("pool.min.idle", IntValue(5)),
    ("pool.max.idle", IntValue(15)),
    ("pool.timeout", IntValue(30000))
  ]))
  Span::end(pool_span)
  
  // 数据库查询操作
  let queries = [
    ("SELECT * FROM users WHERE id = ?", "user.query", 25),
    ("INSERT INTO orders VALUES (...)", "order.insert", 150),
    ("UPDATE products SET stock = stock - 1", "product.update", 80),
    ("SELECT COUNT(*) FROM payments", "payment.count", 45)
  ]
  
  for i = 0; i < queries.length(); i = i + 1 {
    let query = queries[i]
    let sql = query.0
    let query_type = query.1
    let duration = query.2
    
    let query_span = Tracer::start_span(db_tracer, "db.query")
    Span::add_event(query_span, "SQL查询", Some([
      ("db.statement", StringValue(sql)),
      ("db.type", StringValue("postgresql")),
      ("db.instance", StringValue("orders-db")),
      ("query.type", StringValue(query_type))
    ]))
    
    Span::add_event(query_span, "查询完成", Some([
      ("execution.time", IntValue(duration)),
      ("rows.affected", IntValue(1)),
      ("result.status", StringValue("success"))
    ]))
    
    Span::end(query_span)
    
    // 记录查询指标
    Histogram::record(query_duration, duration.to_double())
    Histogram::record(pool_connections, 8.0) // 模拟当前连接数
  }
  
  // 数据库错误查询
  let error_query_span = Tracer::start_span(db_tracer, "db.query.error")
  Span::add_event(error_query_span, "SQL查询失败", Some([
    ("db.statement", StringValue("SELECT * FROM invalid_table")),
    ("db.type", StringValue("postgresql")),
    ("error.code", StringValue("42P01")), // PostgreSQL undefined_table
    ("error.message", StringValue("relation \"invalid_table\" does not exist"))
  ]))
  Span::set_status(error_query_span, Error, Some("SQL查询失败"))
  Span::end(error_query_span)
  
  // 记录错误指标
  Counter::add(query_errors, 1.0)
}
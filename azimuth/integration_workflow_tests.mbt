// 集成工作流测试用例
// 测试Azimuth遥测系统的端到端集成场景

// 测试1: 完整的Web请求处理工作流
pub test "完整的Web请求处理工作流" {
  // 初始化遥测组件
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "web-server", Some("1.0.0"))
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "web-metrics", Some("1.0.0"))
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "web-logger", Some("1.0.0"))
  
  // 创建度量仪器
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total", Some("HTTP请求总数"), Some("requests"))
  let response_histogram = azimuth::Meter::create_histogram(meter, "http.response.duration", Some("HTTP响应时间"), Some("ms"))
  let error_counter = azimuth::Meter::create_counter(meter, "http.errors.total", Some("HTTP错误总数"), Some("errors"))
  let active_connections = azimuth::Meter::create_updown_counter(meter, "http.active_connections", Some("活跃连接数"), Some("connections"))
  
  // 设置资源
  let resource_attrs = [
    ("service.name", azimuth::StringValue("web-api")),
    ("service.version", azimuth::StringValue("2.1.0")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("web-prod-01"))
  ]
  let service_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource_attrs)
  
  // 模拟Web请求处理
  let request_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 1. 创建根Span
  let root_span = azimuth::Tracer::start_span(tracer, "HTTP POST /api/users")
  azimuth::Span::add_event(root_span, "请求接收", Some([
    ("http.method", azimuth::StringValue("POST")),
    ("http.url", azimuth::StringValue("/api/users")),
    ("user.agent", azimuth::StringValue("Mozilla/5.0")),
    ("remote.addr", azimuth::StringValue("192.168.1.100"))
  ]))
  
  // 2. 增加活跃连接数
  azimuth::UpDownCounter::add(active_connections, 1.0)
  
  // 3. 记录请求日志
  let request_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("接收HTTP请求: POST /api/users"),
    Some(azimuth::Attributes::new()),
    Some(request_start_time),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(root_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(root_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, request_log)
  
  // 4. 身份验证子Span
  let auth_span = azimuth::Tracer::start_span(tracer, "身份验证")
  azimuth::Span::add_event(auth_span, "验证JWT令牌", Some([
    ("token.type", azimuth::StringValue("JWT")),
    ("token.issuer", azimuth::StringValue("auth-service"))
  ]))
  
  // 模拟验证延迟
  let auth_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  azimuth::Span::set_status(auth_span, azimuth::Ok)
  azimuth::Span::end(auth_span)
  
  // 5. 数据库查询子Span
  let db_span = azimuth::Tracer::start_span(tracer, "数据库查询")
  azimuth::Span::add_event(db_span, "执行用户查询", Some([
    ("db.query", azimuth::StringValue("SELECT * FROM users WHERE id = ?")),
    ("db.connection", azimuth::StringValue("pool-1"))
  ]))
  
  // 模拟数据库查询
  let db_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  azimuth::Span::set_status(db_span, azimuth::Ok)
  azimuth::Span::end(db_span)
  
  // 6. 业务逻辑处理子Span
  let business_span = azimuth::Tracer::start_span(tracer, "业务逻辑处理")
  azimuth::Span::add_event(business_span, "处理用户数据", Some([
    ("operation", azimuth::StringValue("create_user")),
    ("user.role", azimuth::StringValue("standard"))
  ]))
  
  // 模拟业务处理
  let business_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  azimuth::Span::set_status(business_span, azimuth::Ok)
  azimuth::Span::end(business_span)
  
  // 7. 响应生成
  let response_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let total_duration = (response_time - request_start_time).to_double() / 1000000.0  // 转换为毫秒
  
  // 8. 记录度量
  azimuth::Counter::add(request_counter, 1.0, Some([
    ("method", azimuth::StringValue("POST")),
    ("endpoint", azimuth::StringValue("/api/users")),
    ("status", azimuth::IntValue(201))
  ]))
  
  azimuth::Histogram::record(response_histogram, total_duration, Some([
    ("method", azimuth::StringValue("POST")),
    ("endpoint", azimuth::StringValue("/api/users"))
  ]))
  
  // 9. 减少活跃连接数
  azimuth::UpDownCounter::add(active_connections, -1.0)
  
  // 10. 记录响应日志
  let response_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("发送HTTP响应: 201 Created"),
    Some(azimuth::Attributes::new()),
    Some(response_time),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(root_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(root_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, response_log)
  
  // 11. 完成根Span
  azimuth::Span::add_event(root_span, "请求完成", Some([
    ("http.status_code", azimuth::IntValue(201)),
    ("response.duration_ms", azimuth::FloatValue(total_duration)),
    ("user.id", azimuth::StringValue("user-12345"))
  ]))
  
  azimuth::Span::set_status(root_span, azimuth::Ok)
  azimuth::Span::end(root_span)
  
  // 验证工作流完成
  assert_eq(azimuth::Span::name(root_span), "HTTP POST /api/users")
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.duration")
  assert_true(total_duration > 0.0)
}

// 测试2: 微服务调用链集成测试
pub test "微服务调用链集成测试" {
  // 服务A: API网关
  let gateway_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "api-gateway", Some("1.5.0"))
  let gateway_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "gateway-metrics")
  
  // 服务B: 用户服务
  let user_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "user-service", Some("2.0.0"))
  let user_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "user-metrics")
  
  // 服务C: 订单服务
  let order_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "order-service", Some("1.8.0"))
  let order_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "order-metrics")
  
  // 创建度量仪器
  let gateway_counter = azimuth::Meter::create_counter(gateway_meter, "gateway.requests.total")
  let user_counter = azimuth::Meter::create_counter(user_meter, "user.requests.total")
  let order_counter = azimuth::Meter::create_counter(order_meter, "order.requests.total")
  
  // 1. API网关接收请求
  let gateway_span = azimuth::Tracer::start_span(gateway_tracer, "Gateway: Process order request")
  let gateway_trace_id = azimuth::SpanContext::trace_id(azimuth::Span::span_context(gateway_span))
  
  azimuth::Span::add_event(gateway_span, "请求路由", Some([
    ("service.target", azimuth::StringValue("user-service")),
    ("operation", azimuth::StringValue("getUserInfo"))
  ]))
  
  // 2. 调用用户服务
  let user_span_ctx = azimuth::SpanContext::new(gateway_trace_id, "user-span-123", true, "")
  let user_span = azimuth::Span::new("User: Get user info", azimuth::Server, user_span_ctx)
  
  azimuth::Span::add_event(user_span, "查询用户信息", Some([
    ("user.id", azimuth::StringValue("user-67890")),
    ("db.query", azimuth::StringValue("SELECT * FROM users WHERE id = 'user-67890'"))
  ]))
  
  azimuth::Counter::add(user_counter, 1.0, Some([
    ("operation", azimuth::StringValue("getUserInfo")),
    ("user.id", azimuth::StringValue("user-67890"))
  ]))
  
  azimuth::Span::set_status(user_span, azimuth::Ok)
  azimuth::Span::end(user_span)
  
  // 3. 网关继续处理，调用订单服务
  azimuth::Span::add_event(gateway_span, "请求路由", Some([
    ("service.target", azimuth::StringValue("order-service")),
    ("operation", azimuth::StringValue("createOrder"))
  ]))
  
  // 4. 调用订单服务
  let order_span_ctx = azimuth::SpanContext::new(gateway_trace_id, "order-span-456", true, "")
  let order_span = azimuth::Span::new("Order: Create order", azimuth::Server, order_span_ctx)
  
  azimuth::Span::add_event(order_span, "创建订单", Some([
    ("user.id", azimuth::StringValue("user-67890")),
    ("order.amount", azimuth::FloatValue(99.99)),
    ("order.currency", azimuth::StringValue("USD"))
  ]))
  
  azimuth::Counter::add(order_counter, 1.0, Some([
    ("operation", azimuth::StringValue("createOrder")),
    ("user.id", azimuth::StringValue("user-67890")),
    ("amount.range", azimuth::StringValue("0-100"))
  ]))
  
  azimuth::Span::set_status(order_span, azimuth::Ok)
  azimuth::Span::end(order_span)
  
  // 5. 网关完成处理
  azimuth::Counter::add(gateway_counter, 1.0, Some([
    ("operation", azimuth::StringValue("processOrder")),
    ("status", azimuth::StringValue("success"))
  ]))
  
  azimuth::Span::add_event(gateway_span, "请求完成", Some([
    ("services.called", azimuth::ArrayStringValue(["user-service", "order-service"])),
    ("total.duration", azimuth::FloatValue(250.5))
  ]))
  
  azimuth::Span::set_status(gateway_span, azimuth::Ok)
  azimuth::Span::end(gateway_span)
  
  // 验证调用链一致性
  assert_eq(azimuth::SpanContext::trace_id(user_span_ctx), gateway_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(order_span_ctx), gateway_trace_id)
  assert_true(azimuth::SpanContext::span_id(user_span_ctx) != azimuth::SpanContext::span_id(order_span_ctx))
}

// 测试3: 错误处理和恢复工作流
pub test "错误处理和恢复工作流" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "error-handling-service")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "error-metrics")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "error-logger")
  
  // 创建度量仪器
  let error_counter = azimuth::Meter::create_counter(meter, "errors.total", Some("错误总数"), Some("errors"))
  let retry_counter = azimuth::Meter::create_counter(meter, "retries.total", Some("重试总数"), Some("retries"))
  let recovery_counter = azimuth::Meter::create_counter(meter, "recoveries.total", Some("恢复总数"), Some("recoveries"))
  
  // 1. 主操作Span
  let main_span = azimuth::Tracer::start_span(tracer, "主业务操作")
  
  // 2. 子操作1: 数据库连接（失败）
  let db_span = azimuth::Tracer::start_span(tracer, "数据库连接")
  azimuth::Span::add_event(db_span, "尝试连接数据库", Some([
    ("db.host", azimuth::StringValue("db-primary")),
    ("db.port", azimuth::IntValue(5432))
  ]))
  
  // 记录错误
  azimuth::Counter::add(error_counter, 1.0, Some([
    ("error.type", azimuth::StringValue("DatabaseConnectionError")),
    ("db.host", azimuth::StringValue("db-primary"))
  ]))
  
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("数据库连接失败: Connection timeout"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(main_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(db_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, error_log)
  
  azimuth::Span::set_status(db_span, azimuth::Error)
  azimuth::Span::end(db_span)
  
  // 3. 重试逻辑
  let retry_span = azimuth::Tracer::start_span(tracer, "重试逻辑")
  azimuth::Span::add_event(retry_span, "开始重试", Some([
    ("retry.count", azimuth::IntValue(1)),
    ("max.retries", azimuth::IntValue(3))
  ]))
  
  azimuth::Counter::add(retry_counter, 1.0, Some([
    ("operation", azimuth::StringValue("database.connection")),
    ("retry.attempt", azimuth::IntValue(1))
  ]))
  
  // 4. 子操作2: 连接备用数据库（成功）
  let backup_db_span = azimuth::Tracer::start_span(tracer, "备用数据库连接")
  azimuth::Span::add_event(backup_db_span, "连接备用数据库", Some([
    ("db.host", azimuth::StringValue("db-backup")),
    ("db.port", azimuth::IntValue(5432))
  ]))
  
  azimuth::Span::set_status(backup_db_span, azimuth::Ok)
  azimuth::Span::end(backup_db_span)
  
  // 5. 记录恢复成功
  azimuth::Counter::add(recovery_counter, 1.0, Some([
    ("recovery.type", azimuth::StringValue("database.failover")),
    ("backup.db", azimuth::StringValue("db-backup"))
  ]))
  
  let recovery_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("系统恢复成功: 已切换到备用数据库"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(main_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(retry_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, recovery_log)
  
  azimuth::Span::set_status(retry_span, azimuth::Ok)
  azimuth::Span::end(retry_span)
  
  // 6. 继续主业务操作
  azimuth::Span::add_event(main_span, "业务操作继续", Some([
    ("db.source", azimuth::StringValue("backup")),
    ("recovery.time", azimuth::FloatValue(1500.0))
  ]))
  
  azimuth::Span::set_status(main_span, azimuth::Ok)
  azimuth::Span::end(main_span)
  
  // 验证错误处理工作流
  assert_eq(error_counter.name, "errors.total")
  assert_eq(retry_counter.name, "retries.total")
  assert_eq(recovery_counter.name, "recoveries.total")
}

// 测试4: 批处理工作流集成测试
pub test "批处理工作流集成测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "batch-processor")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "batch-metrics")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "batch-logger")
  
  // 创建度量仪器
  let batch_counter = azimuth::Meter::create_counter(meter, "batch.jobs.total", Some("批处理任务总数"), Some("jobs"))
  let item_histogram = azimuth::Meter::create_histogram(meter, "batch.items.processed", Some("批处理项目数"), Some("items"))
  let duration_histogram = azimuth::Meter::create_histogram(meter, "batch.duration", Some("批处理持续时间"), Some("seconds"))
  
  // 1. 批处理主Span
  let batch_span = azimuth::Tracer::start_span(tracer, "批处理任务: 数据同步")
  let batch_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  azimuth::Span::add_event(batch_span, "批处理开始", Some([
    ("batch.id", azimuth::StringValue("batch-20250101-001")),
    ("batch.type", azimuth::StringValue("data.sync")),
    ("total.items", azimuth::IntValue(1000))
  ]))
  
  // 2. 记录批处理开始日志
  let start_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("批处理任务开始: batch-20250101-001, 共1000项"),
    Some(azimuth::Attributes::new()),
    Some(batch_start_time),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(batch_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(batch_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, start_log)
  
  // 3. 模拟批处理子任务
  let processed_items = 0
  for i in 0..10 {  // 模拟10个子批次
    let sub_batch_span = azimuth::Tracer::start_span(tracer, "子批次处理: " + i.to_string())
    
    let start_item = i * 100
    let end_item = (i + 1) * 100
    
    azimuth::Span::add_event(sub_batch_span, "子批次开始", Some([
      ("sub.batch.id", azimuth::StringValue("sub-" + i.to_string())),
      ("start.item", azimuth::IntValue(start_item)),
      ("end.item", azimuth::IntValue(end_item))
    ]))
    
    // 模拟处理延迟
    let process_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // 记录子批次完成
    azimuth::Span::add_event(sub_batch_span, "子批次完成", Some([
      ("processed.count", azimuth::IntValue(100)),
      ("success.count", azimuth::IntValue(98)),
      ("error.count", azimuth::IntValue(2))
    ]))
    
    azimuth::Span::set_status(sub_batch_span, azimuth::Ok)
    azimuth::Span::end(sub_batch_span)
  }
  
  // 4. 批处理完成
  let batch_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let batch_duration = (batch_end_time - batch_start_time).to_double() / 1000000000.0  // 转换为秒
  
  // 5. 记录度量
  azimuth::Counter::add(batch_counter, 1.0, Some([
    ("batch.type", azimuth::StringValue("data.sync")),
    ("status", azimuth::StringValue("completed"))
  ]))
  
  azimuth::Histogram::record(item_histogram, 1000.0, Some([
    ("batch.type", azimuth::StringValue("data.sync"))
  ]))
  
  azimuth::Histogram::record(duration_histogram, batch_duration, Some([
    ("batch.type", azimuth::StringValue("data.sync"))
  ]))
  
  // 6. 记录批处理完成日志
  let end_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("批处理任务完成: batch-20250101-001, 处理1000项，耗时" + batch_duration.to_string() + "秒"),
    Some(azimuth::Attributes::new()),
    Some(batch_end_time),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(batch_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(batch_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, end_log)
  
  // 7. 完成主Span
  azimuth::Span::add_event(batch_span, "批处理完成", Some([
    ("total.processed", azimuth::IntValue(1000)),
    ("total.errors", azimuth::IntValue(20)),
    ("duration.seconds", azimuth::FloatValue(batch_duration))
  ]))
  
  azimuth::Span::set_status(batch_span, azimuth::Ok)
  azimuth::Span::end(batch_span)
  
  // 验证批处理工作流
  assert_eq(batch_counter.name, "batch.jobs.total")
  assert_eq(item_histogram.name, "batch.items.processed")
  assert_eq(duration_histogram.name, "batch.duration")
  assert_true(batch_duration > 0.0)
}

// 测试5: 上下文传播集成测试
pub test "上下文传播集成测试" {
  // 创建传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 服务1: 初始上下文创建
  let service1_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-1")
  let service1_span = azimuth::Tracer::start_span(service1_tracer, "服务1操作")
  
  // 创建初始上下文
  let ctx = azimuth::Context::root()
  let user_key = azimuth::ContextKey::new("user.id")
  let session_key = azimuth::ContextKey::new("session.id")
  let request_key = azimuth::ContextKey::new("request.id")
  
  let ctx_with_user = azimuth::Context::with_value(ctx, user_key, "user-12345")
  let ctx_with_session = azimuth::Context::with_value(ctx_with_user, session_key, "sess-67890")
  let ctx_with_request = azimuth::Context::with_value(ctx_with_session, request_key, "req-11111")
  
  // 创建Baggage
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "correlation.id", "corr-99999")
  let baggage_with_locale = azimuth::Baggage::set_entry(baggage_with_user, "user.locale", "zh-CN")
  
  // 注入上下文到HTTP头
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx_with_request, carrier)
  
  // 模拟HTTP调用到服务2
  let service2_carrier = azimuth::TextMapCarrier::new()
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  if let Some(header) = trace_header {
    azimuth::TextMapCarrier::set(service2_carrier, "traceparent", header)
  }
  azimuth::TextMapCarrier::set(service2_carrier, "baggage", "correlation.id=corr-99999,user.locale=zh-CN")
  
  // 服务2: 提取上下文
  let service2_ctx = azimuth::CompositePropagator::extract(composite_propagator, service2_carrier)
  let service2_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-2")
  let service2_span = azimuth::Tracer::start_span(service2_tracer, "服务2操作")
  
  // 验证上下文传播
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(service2_ctx, extracted_key)
  
  // 验证Baggage传播
  let correlation_id = azimuth::Baggage::get_entry(baggage_with_locale, "correlation.id")
  let user_locale = azimuth::Baggage::get_entry(baggage_with_locale, "user.locale")
  
  // 服务2继续传播到服务3
  let service3_carrier = azimuth::TextMapCarrier::new()
  let trace_header2 = azimuth::TextMapCarrier::get(service2_carrier, "traceparent")
  if let Some(header) = trace_header2 {
    azimuth::TextMapCarrier::set(service3_carrier, "traceparent", header)
  }
  azimuth::TextMapCarrier::set(service3_carrier, "baggage", "correlation.id=corr-99999,user.locale=zh-CN,additional.info=extra")
  
  // 服务3: 提取上下文
  let service3_ctx = azimuth::CompositePropagator::extract(composite_propagator, service3_carrier)
  let service3_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-3")
  let service3_span = azimuth::Tracer::start_span(service3_tracer, "服务3操作")
  
  // 结束所有Span
  azimuth::Span::end(service1_span)
  azimuth::Span::end(service2_span)
  azimuth::Span::end(service3_span)
  
  // 验证传播结果
  assert_eq(extracted_value, Some("true"))
  assert_eq(correlation_id, Some("corr-99999"))
  assert_eq(user_locale, Some("zh-CN"))
}

// 测试6: 实时监控工作流集成测试
pub test "实时监控工作流集成测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "monitoring-service")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "monitoring-metrics")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "monitoring-logger")
  
  // 创建实时监控度量仪器
  let cpu_gauge = azimuth::Meter::create_gauge(meter, "system.cpu.usage", Some("CPU使用率"), Some("percent"))
  let memory_gauge = azimuth::Meter::create_gauge(meter, "system.memory.usage", Some("内存使用率"), Some("percent"))
  let request_rate = azimuth::Meter::create_counter(meter, "requests.rate", Some("请求速率"), Some("requests/sec"))
  let error_rate = azimuth::Meter::create_counter(meter, "errors.rate", Some("错误率"), Some("errors/sec"))
  let response_time = azimuth::Meter::create_histogram(meter, "response.time", Some("响应时间"), Some("ms"))
  
  // 1. 监控主循环
  let monitor_span = azimuth::Tracer::start_span(tracer, "实时监控循环")
  
  // 2. 模拟系统指标收集
  for i in 0..5 {
    let metrics_span = azimuth::Tracer::start_span(tracer, "指标收集: " + i.to_string())
    
    // 模拟CPU和内存使用率
    let cpu_usage = 50.0 + (i.to_double() * 10.0)
    let memory_usage = 60.0 + (i.to_double() * 5.0)
    
    // 记录系统指标
    azimuth::Gauge::set(cpu_gauge, cpu_usage)  // 注意：简化实现可能不支持set操作
    azimuth::Gauge::set(memory_gauge, memory_usage)
    
    // 模拟请求和错误
    azimuth::Counter::add(request_rate, 100.0 + (i.to_double() * 20.0))
    if i % 2 == 0 {
      azimuth::Counter::add(error_rate, 2.0)
    }
    
    // 模拟响应时间分布
    for j in 0..10 {
      let response_time_value = 50.0 + (j.to_double() * 10.0) + (i.to_double() * 5.0)
      azimuth::Histogram::record(response_time, response_time_value)
    }
    
    // 记录监控日志
    let metrics_log = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("系统指标更新: CPU=" + cpu_usage.to_string() + "%, 内存=" + memory_usage.to_string() + "%"),
      Some(azimuth::Attributes::new()),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(monitor_span))),
      Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(metrics_span))),
      Some(azimuth::Context::root())
    )
    azimuth::Logger::emit(logger, metrics_log)
    
    azimuth::Span::end(metrics_span)
  }
  
  // 3. 异常检测和告警
  let alert_span = azimuth::Tracer::start_span(tracer, "异常检测")
  
  // 模拟异常检测
  let alert_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("系统告警: CPU使用率超过阈值"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(monitor_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(alert_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, alert_log)
  
  azimuth::Span::end(alert_span)
  
  // 4. 完成监控循环
  azimuth::Span::end(monitor_span)
  
  // 验证监控工作流
  assert_eq(cpu_gauge.name, "system.cpu.usage")
  assert_eq(memory_gauge.name, "system.memory.usage")
  assert_eq(request_rate.name, "requests.rate")
  assert_eq(error_rate.name, "errors.rate")
  assert_eq(response_time.name, "response.time")
}

// 测试7: 数据管道集成测试
pub test "数据管道集成测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "data-pipeline")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "pipeline-metrics")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "pipeline-logger")
  
  // 创建数据管道度量仪器
  let data_counter = azimuth::Meter::create_counter(meter, "data.records.processed", Some("处理记录数"), Some("records"))
  let throughput_histogram = azimuth::Meter::create_histogram(meter, "pipeline.throughput", Some("管道吞吐量"), Some("records/sec"))
  let latency_histogram = azimuth::Meter::create_histogram(meter, "pipeline.latency", Some("管道延迟"), Some("ms"))
  
  // 1. 数据管道主Span
  let pipeline_span = azimuth::Tracer::start_span(tracer, "数据管道处理")
  let pipeline_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 2. 数据提取阶段
  let extract_span = azimuth::Tracer::start_span(tracer, "数据提取")
  azimuth::Span::add_event(extract_span, "开始数据提取", Some([
    ("source", azimuth::StringValue("database")),
    ("table", azimuth::StringValue("events"))
  ]))
  
  // 模拟数据提取
  let extracted_records = 5000
  azimuth::Counter::add(data_counter, extracted_records.to_double(), Some([
    ("stage", azimuth::StringValue("extract")),
    ("source", azimuth::StringValue("database"))
  ]))
  
  azimuth::Span::add_event(extract_span, "数据提取完成", Some([
    ("records.extracted", azimuth::IntValue(extracted_records))
  ]))
  
  azimuth::Span::end(extract_span)
  
  // 3. 数据转换阶段
  let transform_span = azimuth::Tracer::start_span(tracer, "数据转换")
  azimuth::Span::add_event(transform_span, "开始数据转换", Some([
    ("transformation.type", azimuth::StringValue("normalize"))
  ]))
  
  // 模拟数据转换
  let transformed_records = 4800  // 有些记录被过滤
  azimuth::Counter::add(data_counter, transformed_records.to_double(), Some([
    ("stage", azimuth::StringValue("transform")),
    ("transformation", azimuth::StringValue("normalize"))
  ]))
  
  azimuth::Span::add_event(transform_span, "数据转换完成", Some([
    ("records.transformed", azimuth::IntValue(transformed_records)),
    ("records.filtered", azimuth::IntValue(200))
  ]))
  
  azimuth::Span::end(transform_span)
  
  // 4. 数据加载阶段
  let load_span = azimuth::Tracer::start_span(tracer, "数据加载")
  azimuth::Span::add_event(load_span, "开始数据加载", Some([
    ("target", azimuth::StringValue("data-warehouse")),
    ("format", azimuth::StringValue("parquet"))
  ]))
  
  // 模拟数据加载
  let loaded_records = 4750  // 有些记录加载失败
  azimuth::Counter::add(data_counter, loaded_records.to_double(), Some([
    ("stage", azimuth::StringValue("load")),
    ("target", azimuth::StringValue("data-warehouse"))
  ]))
  
  azimuth::Span::add_event(load_span, "数据加载完成", Some([
    ("records.loaded", azimuth::IntValue(loaded_records)),
    ("records.failed", azimuth::IntValue(50))
  ]))
  
  azimuth::Span::end(load_span)
  
  // 5. 计算管道性能指标
  let pipeline_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let pipeline_duration = (pipeline_end_time - pipeline_start_time).to_double() / 1000000.0  // 转换为毫秒
  let throughput = loaded_records.to_double() / (pipeline_duration / 1000.0)  // records/sec
  
  // 记录性能指标
  azimuth::Histogram::record(throughput_histogram, throughput, Some([
    ("pipeline.type", azimuth::StringValue("etl"))
  ]))
  
  azimuth::Histogram::record(latency_histogram, pipeline_duration, Some([
    ("pipeline.type", azimuth::StringValue("etl"))
  ]))
  
  // 6. 记录管道完成日志
  let pipeline_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("数据管道完成: 提取" + extracted_records.to_string() + "条，转换" + transformed_records.to_string() + "条，加载" + loaded_records.to_string() + "条，耗时" + pipeline_duration.to_string() + "ms"),
    Some(azimuth::Attributes::new()),
    Some(pipeline_end_time),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(pipeline_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(pipeline_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, pipeline_log)
  
  // 7. 完成管道Span
  azimuth::Span::add_event(pipeline_span, "管道处理完成", Some([
    ("total.records", azimuth::IntValue(loaded_records)),
    ("pipeline.duration", azimuth::FloatValue(pipeline_duration)),
    ("throughput", azimuth::FloatValue(throughput))
  ]))
  
  azimuth::Span::end(pipeline_span)
  
  // 验证数据管道工作流
  assert_eq(data_counter.name, "data.records.processed")
  assert_eq(throughput_histogram.name, "pipeline.throughput")
  assert_eq(latency_histogram.name, "pipeline.latency")
  assert_true(throughput > 0.0)
  assert_true(pipeline_duration > 0.0)
}

// 测试8: 缓存系统集成测试
pub test "缓存系统集成测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "cache-service")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "cache-metrics")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "cache-logger")
  
  // 创建缓存度量仪器
  let cache_hits = azimuth::Meter::create_counter(meter, "cache.hits", Some("缓存命中数"), Some("hits"))
  let cache_misses = azimuth::Meter::create_counter(meter, "cache.misses", Some("缓存未命中数"), Some("misses"))
  let cache_evictions = azimuth::Meter::create_counter(meter, "cache.evictions", Some("缓存驱逐数"), Some("evictions"))
  let cache_size = azimuth::Meter::create_gauge(meter, "cache.size", Some("缓存大小"), Some("items"))
  let cache_latency = azimuth::Meter::create_histogram(meter, "cache.latency", Some("缓存延迟"), Some("ms"))
  
  // 1. 缓存操作主Span
  let cache_span = azimuth::Tracer::start_span(tracer, "缓存操作")
  
  // 2. 模拟缓存操作
  for i in 0..20 {
    let operation_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // 模拟缓存查找
    if i % 3 == 0 {
      // 缓存命中
      azimuth::Counter::add(cache_hits, 1.0, Some([
        ("cache.key", azimuth::StringValue("key-" + i.to_string())),
        ("operation", azimuth::StringValue("get"))
      ]))
      
      let hit_log = azimuth::LogRecord::new_with_context(
        azimuth::Debug,
        Some("缓存命中: key-" + i.to_string()),
        Some(azimuth::Attributes::new()),
        Some(operation_start),
        None,
        Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(cache_span))),
        Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(cache_span))),
        Some(azimuth::Context::root())
      )
      azimuth::Logger::emit(logger, hit_log)
    } else {
      // 缓存未命中
      azimuth::Counter::add(cache_misses, 1.0, Some([
        ("cache.key", azimuth::StringValue("key-" + i.to_string())),
        ("operation", azimuth::StringValue("get"))
      ]))
      
      // 从数据源加载
      let load_span = azimuth::Tracer::start_span(tracer, "从数据源加载")
      azimuth::Span::add_event(load_span, "数据库查询", Some([
        ("query", azimuth::StringValue("SELECT * FROM data WHERE key = 'key-" + i.to_string() + "'"))
      ]))
      
      // 存入缓存
      azimuth::Span::add_event(load_span, "存入缓存", Some([
        ("cache.key", azimuth::StringValue("key-" + i.to_string())),
        ("cache.ttl", azimuth::IntValue(3600))
      ]))
      
      azimuth::Span::end(load_span)
      
      let miss_log = azimuth::LogRecord::new_with_context(
        azimuth::Debug,
        Some("缓存未命中: key-" + i.to_string() + "，已从数据源加载"),
        Some(azimuth::Attributes::new()),
        Some(operation_start),
        None,
        Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(cache_span))),
        Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(cache_span))),
        Some(azimuth::Context::root())
      )
      azimuth::Logger::emit(logger, miss_log)
    }
    
    let operation_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let operation_duration = (operation_end - operation_start).to_double() / 1000000.0  // 转换为毫秒
    
    // 记录操作延迟
    azimuth::Histogram::record(cache_latency, operation_duration, Some([
      ("operation", azimuth::StringValue("get")),
      ("cache.result", azimuth::StringValue(if i % 3 == 0 { "hit" } else { "miss" }))
    ]))
  }
  
  // 3. 模拟缓存驱逐
  for i in 0..3 {
    azimuth::Counter::add(cache_evictions, 1.0, Some([
      ("eviction.reason", azimuth::StringValue("ttl_expired")),
      ("cache.key", azimuth::StringValue("evicted-key-" + i.to_string()))
    ]))
  }
  
  // 4. 更新缓存大小
  let final_cache_size = 17.0  // 假设最终缓存大小
  azimuth::Gauge::set(cache_size, final_cache_size)  // 注意：简化实现可能不支持set操作
  
  // 5. 记录缓存统计日志
  let stats_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("缓存统计: 命中率66.7%，当前大小17项"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(cache_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(cache_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, stats_log)
  
  // 6. 完成缓存Span
  azimuth::Span::end(cache_span)
  
  // 验证缓存工作流
  assert_eq(cache_hits.name, "cache.hits")
  assert_eq(cache_misses.name, "cache.misses")
  assert_eq(cache_evictions.name, "cache.evictions")
  assert_eq(cache_size.name, "cache.size")
  assert_eq(cache_latency.name, "cache.latency")
}

// 测试9: 消息队列集成测试
pub test "消息队列集成测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "message-queue")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "queue-metrics")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "queue-logger")
  
  // 创建消息队列度量仪器
  let messages_published = azimuth::Meter::create_counter(meter, "messages.published", Some("发布消息数"), Some("messages"))
  let messages_consumed = azimuth::Meter::create_counter(meter, "messages.consumed", Some("消费消息数"), Some("messages"))
  let messages_failed = azimuth::Meter::create_counter(meter, "messages.failed", Some("失败消息数"), Some("messages"))
  let queue_depth = azimuth::Meter::create_gauge(meter, "queue.depth", Some("队列深度"), Some("messages"))
  let processing_time = azimuth::Meter::create_histogram(meter, "message.processing.time", Some("消息处理时间"), Some("ms"))
  
  // 1. 消息生产者
  let producer_span = azimuth::Tracer::start_span(tracer, "消息生产者")
  
  for i in 0..10 {
    let publish_span = azimuth::Tracer::start_span(tracer, "发布消息")
    
    azimuth::Span::add_event(publish_span, "消息发布", Some([
      ("message.id", azimuth::StringValue("msg-" + i.to_string())),
      ("topic", azimuth::StringValue("events")),
      ("partition", azimuth::IntValue(i % 3))
    ]))
    
    azimuth::Counter::add(messages_published, 1.0, Some([
      ("topic", azimuth::StringValue("events")),
      ("partition", azimuth::IntValue(i % 3))
    ]))
    
    let publish_log = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("消息发布成功: msg-" + i.to_string()),
      Some(azimuth::Attributes::new()),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(producer_span))),
      Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(publish_span))),
      Some(azimuth::Context::root())
    )
    azimuth::Logger::emit(logger, publish_log)
    
    azimuth::Span::end(publish_span)
  }
  
  azimuth::Span::end(producer_span)
  
  // 2. 更新队列深度
  azimuth::Gauge::set(queue_depth, 10.0)  // 注意：简化实现可能不支持set操作
  
  // 3. 消息消费者
  let consumer_span = azimuth::Tracer::start_span(tracer, "消息消费者")
  
  for i in 0..10 {
    let consume_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    let consume_span = azimuth::Tracer::start_span(tracer, "消费消息")
    
    azimuth::Span::add_event(consume_span, "消息接收", Some([
      ("message.id", azimuth::StringValue("msg-" + i.to_string())),
      ("topic", azimuth::StringValue("events"))
    ]))
    
    // 模拟消息处理
    if i % 8 == 0 {
      // 模拟处理失败
      azimuth::Counter::add(messages_failed, 1.0, Some([
        ("topic", azimuth::StringValue("events")),
        ("error.type", azimuth::StringValue("processing.error"))
      ]))
      
      let error_log = azimuth::LogRecord::new_with_context(
        azimuth::Error,
        Some("消息处理失败: msg-" + i.to_string()),
        Some(azimuth::Attributes::new()),
        Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
        None,
        Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(consumer_span))),
        Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(consume_span))),
        Some(azimuth::Context::root())
      )
      azimuth::Logger::emit(logger, error_log)
      
      azimuth::Span::set_status(consume_span, azimuth::Error)
    } else {
      // 处理成功
      azimuth::Counter::add(messages_consumed, 1.0, Some([
        ("topic", azimuth::StringValue("events")),
        ("processing.time", azimuth::FloatValue((i.to_double() + 1.0) * 10.0))
      ]))
      
      let success_log = azimuth::LogRecord::new_with_context(
        azimuth::Info,
        Some("消息处理成功: msg-" + i.to_string()),
        Some(azimuth::Attributes::new()),
        Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
        None,
        Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(consumer_span))),
        Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(consume_span))),
        Some(azimuth::Context::root())
      )
      azimuth::Logger::emit(logger, success_log)
      
      azimuth::Span::set_status(consume_span, azimuth::Ok)
    }
    
    azimuth::Span::add_event(consume_span, "消息处理完成", Some([
      ("message.id", azimuth::StringValue("msg-" + i.to_string())),
      ("result", azimuth::StringValue(if i % 8 == 0 { "failed" } else { "success" }))
    ]))
    
    let consume_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let processing_duration = (consume_end - consume_start).to_double() / 1000000.0  // 转换为毫秒
    
    // 记录处理时间
    azimuth::Histogram::record(processing_time, processing_duration, Some([
      ("topic", azimuth::StringValue("events")),
      ("result", azimuth::StringValue(if i % 8 == 0 { "failed" } else { "success" }))
    ]))
    
    azimuth::Span::end(consume_span)
  }
  
  // 4. 更新队列深度
  azimuth::Gauge::set(queue_depth, 1.0)  // 剩余1条失败消息
  
  // 5. 记录队列统计日志
  let queue_stats_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("队列统计: 发布10条，消费9条，失败1条，剩余1条"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(consumer_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(consumer_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, queue_stats_log)
  
  azimuth::Span::end(consumer_span)
  
  // 验证消息队列工作流
  assert_eq(messages_published.name, "messages.published")
  assert_eq(messages_consumed.name, "messages.consumed")
  assert_eq(messages_failed.name, "messages.failed")
  assert_eq(queue_depth.name, "queue.depth")
  assert_eq(processing_time.name, "message.processing.time")
}

// 测试10: 分布式事务集成测试
pub test "分布式事务集成测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "transaction-coordinator")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "transaction-metrics")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "transaction-logger")
  
  // 创建事务度量仪器
  let transactions_started = azimuth::Meter::create_counter(meter, "transactions.started", Some("启动事务数"), Some("transactions"))
  let transactions_committed = azimuth::Meter::create_counter(meter, "transactions.committed", Some("提交事务数"), Some("transactions"))
  let transactions_aborted = azimuth::Meter::create_counter(meter, "transactions.aborted", Some("中止事务数"), Some("transactions"))
  let transaction_duration = azimuth::Meter::create_histogram(meter, "transaction.duration", Some("事务持续时间"), Some("ms"))
  let participants = azimuth::Meter::create_histogram(meter, "transaction.participants", Some("事务参与者数"), Some("participants"))
  
  // 1. 分布式事务协调器主Span
  let coordinator_span = azimuth::Tracer::start_span(tracer, "分布式事务协调")
  let transaction_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 记录事务启动
  azimuth::Counter::add(transactions_started, 1.0, Some([
    ("transaction.type", azimuth::StringValue("distributed")),
    ("coordinator", azimuth::StringValue("tx-coordinator-01"))
  ]))
  
  azimuth::Span::add_event(coordinator_span, "事务开始", Some([
    ("transaction.id", azimuth::StringValue("tx-20250101-001")),
    ("isolation.level", azimuth::StringValue("READ_COMMITTED"))
  ]))
  
  let start_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("分布式事务开始: tx-20250101-001"),
    Some(azimuth::Attributes::new()),
    Some(transaction_start_time),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(coordinator_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(coordinator_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, start_log)
  
  // 2. 参与者1: 数据库事务
  let db_span = azimuth::Tracer::start_span(tracer, "数据库事务参与者")
  
  azimuth::Span::add_event(db_span, "开始数据库事务", Some([
    ("participant.id", azimuth::StringValue("db-participant-01")),
    ("database", azimuth::StringValue("orders-db"))
  ]))
  
  azimuth::Span::add_event(db_span, "执行SQL操作", Some([
    ("sql", azimuth::StringValue("INSERT INTO orders (id, amount) VALUES (?, ?)")),
    ("params", azimuth::StringValue("[order-123, 99.99]"))
  ]))
  
  azimuth::Span::add_event(db_span, "数据库事务准备完成", Some([
    ("participant.id", azimuth::StringValue("db-participant-01")),
    ("status", azimuth::StringValue("prepared"))
  ]))
  
  azimuth::Span::end(db_span)
  
  // 3. 参与者2: 消息队列事务
  let mq_span = azimuth::Tracer::start_span(tracer, "消息队列事务参与者")
  
  azimuth::Span::add_event(mq_span, "开始消息事务", Some([
    ("participant.id", azimuth::StringValue("mq-participant-01")),
    ("topic", azimuth::StringValue("order.events"))
  ]))
  
  azimuth::Span::add_event(mq_span, "发送消息", Some([
    ("message.id", azimuth::StringValue("msg-order-123")),
    ("message.type", azimuth::StringValue("order.created"))
  ]))
  
  azimuth::Span::add_event(mq_span, "消息事务准备完成", Some([
    ("participant.id", azimuth::StringValue("mq-participant-01")),
    ("status", azimuth::StringValue("prepared"))
  ]))
  
  azimuth::Span::end(mq_span)
  
  // 4. 参与者3: 缓存事务
  let cache_span = azimuth::Tracer::start_span(tracer, "缓存事务参与者")
  
  azimuth::Span::add_event(cache_span, "开始缓存事务", Some([
    ("participant.id", azimuth::StringValue("cache-participant-01")),
    ("cache.type", azimuth::StringValue("redis"))
  ]))
  
  azimuth::Span::add_event(cache_span, "更新缓存", Some([
    ("cache.key", azimuth::StringValue("order:order-123")),
    ("cache.operation", azimuth::StringValue("SET"))
  ]))
  
  azimuth::Span::add_event(cache_span, "缓存事务准备完成", Some([
    ("participant.id", azimuth::StringValue("cache-participant-01")),
    ("status", azimuth::StringValue("prepared"))
  ]))
  
  azimuth::Span::end(cache_span)
  
  // 5. 两阶段提交: 第一阶段 - 准备
  let prepare_span = azimuth::Tracer::start_span(tracer, "两阶段提交-准备阶段")
  
  azimuth::Span::add_event(prepare_span, "询问所有参与者", Some([
    ("phase", azimuth::StringValue("prepare")),
    ("participants.count", azimuth::IntValue(3))
  ]))
  
  azimuth::Span::add_event(prepare_span, "所有参与者准备就绪", Some([
    ("phase", azimuth::StringValue("prepare")),
    ("all.ready", azimuth::BoolValue(true))
  ]))
  
  azimuth::Span::end(prepare_span)
  
  // 6. 两阶段提交: 第二阶段 - 提交
  let commit_span = azimuth::Tracer::start_span(tracer, "两阶段提交-提交阶段")
  
  azimuth::Span::add_event(commit_span, "提交数据库事务", Some([
    ("participant.id", azimuth::StringValue("db-participant-01")),
    ("phase", azimuth::StringValue("commit"))
  ]))
  
  azimuth::Span::add_event(commit_span, "提交消息事务", Some([
    ("participant.id", azimuth::StringValue("mq-participant-01")),
    ("phase", azimuth::StringValue("commit"))
  ]))
  
  azimuth::Span::add_event(commit_span, "提交缓存事务", Some([
    ("participant.id", azimuth::StringValue("cache-participant-01")),
    ("phase", azimuth::StringValue("commit"))
  ]))
  
  azimuth::Span::add_event(commit_span, "所有参与者提交完成", Some([
    ("phase", azimuth::StringValue("commit")),
    ("all.committed", azimuth::BoolValue(true))
  ]))
  
  azimuth::Span::end(commit_span)
  
  // 7. 记录事务提交
  let transaction_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let transaction_duration_ms = (transaction_end_time - transaction_start_time).to_double() / 1000000.0
  
  azimuth::Counter::add(transactions_committed, 1.0, Some([
    ("transaction.type", azimuth::StringValue("distributed")),
    ("participants.count", azimuth::IntValue(3))
  ]))
  
  azimuth::Histogram::record(transaction_duration, transaction_duration_ms, Some([
    ("transaction.type", azimuth::StringValue("distributed")),
    ("result", azimuth::StringValue("committed"))
  ]))
  
  azimuth::Histogram::record(participants, 3.0, Some([
    ("transaction.type", azimuth::StringValue("distributed"))
  ]))
  
  // 8. 记录事务完成日志
  let commit_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("分布式事务提交成功: tx-20250101-001，耗时" + transaction_duration_ms.to_string() + "ms，3个参与者"),
    Some(azimuth::Attributes::new()),
    Some(transaction_end_time),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(coordinator_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(coordinator_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, commit_log)
  
  // 9. 完成协调器Span
  azimuth::Span::add_event(coordinator_span, "分布式事务完成", Some([
    ("transaction.id", azimuth::StringValue("tx-20250101-001")),
    ("result", azimuth::StringValue("committed")),
    ("participants", azimuth::IntValue(3)),
    ("duration", azimuth::FloatValue(transaction_duration_ms))
  ]))
  
  azimuth::Span::set_status(coordinator_span, azimuth::Ok)
  azimuth::Span::end(coordinator_span)
  
  // 验证分布式事务工作流
  assert_eq(transactions_started.name, "transactions.started")
  assert_eq(transactions_committed.name, "transactions.committed")
  assert_eq(transactions_aborted.name, "transactions.aborted")
  assert_eq(transaction_duration.name, "transaction.duration")
  assert_eq(participants.name, "transaction.participants")
  assert_true(transaction_duration_ms > 0.0)
}
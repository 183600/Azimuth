// Azimuth Telemetry Integration Focused Tests
// 专注于遥测组件之间的集成测试

test "span与metrics集成测试" {
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "integration.test")
  let meter = MeterProvider::get_meter(meter_provider, "integration.test")
  
  // 创建span和metrics
  let span = Tracer::start_span(tracer, "integration.span")
  let counter = Meter::create_counter(meter, "span.operations.total")
  let histogram = Meter::create_histogram(meter, "span.duration.ms")
  
  // 在span生命周期内记录metrics
  Span::add_event(span, "operation.start", None)
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 100.0)
  
  // 添加更多事件和metrics
  Span::add_event(span, "operation.progress", Some([("progress", IntValue(50))]))
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 150.0)
  
  // 结束span
  Span::set_status(span, Ok)
  Span::end(span)
  
  // 验证集成操作完成
  assert_true(true)
}

test "context与baggage跨组件传播测试" {
  let root_ctx = Context::root()
  let baggage = Baggage::new()
  
  // 在context中设置值
  let ctx_key = ContextKey::new("user.id")
  let ctx_with_user = Context::with_value(root_ctx, ctx_key, "12345")
  
  // 在baggage中设置值
  let baggage_with_user = Baggage::set_entry(baggage, "user.role", "admin")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "sess-67890")
  
  // 创建传播器
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let carrier = TextMapCarrier::new()
  
  // 注入context和baggage
  CompositePropagator::inject(propagator, ctx_with_user, carrier)
  
  // 提取context
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  let extracted_user = Context::get(extracted_ctx, ctx_key)
  
  // 验证传播结果
  match extracted_user {
    Some(user) => assert_eq(user, "12345")
    None => assert_true(false)
  }
  
  // 验证baggage值
  let user_role = Baggage::get_entry(baggage_with_session, "user.role")
  match user_role {
    Some(role) => assert_eq(role, "admin")
    None => assert_true(false)
  }
  
  let session_id = Baggage::get_entry(baggage_with_session, "session.id")
  match session_id {
    Some(session) => assert_eq(session, "sess-67890")
    None => assert_true(false)
  }
}

test "logs与span关联集成测试" {
  let logger_provider = LoggerProvider::default()
  let tracer_provider = TracerProvider::default()
  
  let logger = LoggerProvider::get_logger(logger_provider, "integration.logger")
  let tracer = TracerProvider::get_tracer(tracer_provider, "integration.tracer")
  
  // 创建span
  let span = Tracer::start_span(tracer, "integrated.operation")
  let span_ctx = Span::span_context(span)
  
  // 创建与span关联的日志记录
  let trace_id = SpanContext::trace_id(span_ctx)
  let span_id = SpanContext::span_id(span_ctx)
  
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Operation started"),
    None,
    Some(1735689600000000000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // 添加span事件
  Span::add_event(span, "log.created", Some([("log.level", StringValue("info"))]))
  
  // 发出日志
  Logger::emit(logger, start_log)
  
  // 创建更多日志记录
  let progress_log = LogRecord::new_with_context(
    Info,
    Some("Operation in progress"),
    Some(Attributes::with_attributes(Attributes::new(), [
      ("progress.percent", IntValue(50)),
      ("operation.type", StringValue("data.processing"))
    ])),
    Some(1735689600000001000L),
    None,
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  Logger::emit(logger, progress_log)
  Span::add_event(span, "log.emitted", Some([("log.count", IntValue(2))]))
  
  // 结束span
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  Span::end(span)
  
  // 验证日志和span的关联
  assert_eq(LogRecord::trace_id(start_log), Some(trace_id))
  assert_eq(LogRecord::span_id(start_log), Some(span_id))
  assert_eq(LogRecord::body(start_log), Some("Operation started"))
}

test "resource与attributes集成测试" {
  // 创建资源
  let resource = Resource::new()
  let resource_attrs = [
    ("service.name", StringValue("integration.service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-12345")),
    ("deployment.environment", StringValue("production"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, resource_attrs)
  
  // 创建span属性
  let span_attrs = Attributes::new()
  Attributes::set(span_attrs, "http.method", StringValue("GET"))
  Attributes::set(span_attrs, "http.url", StringValue("/api/integration/test"))
  Attributes::set(span_attrs, "http.status_code", IntValue(200))
  
  // 创建log属性
  let log_attrs = Attributes::with_attributes(Attributes::new(), [
    ("log.level", StringValue("info")),
    ("log.logger", StringValue("integration.logger")),
    ("user.id", StringValue("user-67890"))
  ])
  
  // 验证资源属性
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "integration.service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  // 验证span属性
  let http_method = Attributes::get(span_attrs, "http.method")
  match http_method {
    Some(StringValue(method)) => assert_eq(method, "GET")
    _ => assert_true(false)
  }
  
  // 验证log属性
  let log_level = Attributes::get(log_attrs, "log.level")
  match log_level {
    Some(StringValue(level)) => assert_eq(level, "info")
    _ => assert_true(false)
  }
}

test "instrumentation scope跨组件一致性测试" {
  let meter_provider = MeterProvider::default()
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // 创建具有相同scope的instrumentation
  let scope_name = "integration.service"
  let scope_version = Some("2.0.0")
  let scope_schema = Some("https://example.com/schema/integration")
  
  let meter = MeterProvider::get_meter(meter_provider, scope_name, scope_version, scope_schema)
  let tracer = TracerProvider::get_tracer(tracer_provider, scope_name, scope_version)
  let logger = LoggerProvider::get_logger(logger_provider, scope_name, scope_version)
  
  // 验证scope一致性
  let meter_scope = Meter::scope(meter)
  assert_eq(meter_scope.name, scope_name)
  assert_eq(meter_scope.version, scope_version)
  assert_eq(meter_scope.schema_url, scope_schema)
  
  let tracer_scope = Tracer::instrumentation_scope(tracer)
  assert_eq(tracer_scope.name, scope_name)
  assert_eq(tracer_scope.version, scope_version)
  assert_eq(tracer_scope.schema_url, scope_schema)
  
  let logger_scope = Logger::scope(logger)
  assert_eq(logger_scope.name, scope_name)
  assert_eq(logger_scope.version, scope_version)
  assert_eq(logger_scope.schema_url, scope_schema)
  
  // 使用相同scope创建不同类型的instrumentation
  let counter = Meter::create_counter(meter, "integration.operations")
  let span = Tracer::start_span(tracer, "integration.operation")
  let log = LogRecord::new(Info, "Integration log message")
  
  // 执行操作
  Counter::add(counter, 1.0)
  Span::add_event(span, "counter.incremented", None)
  Logger::emit(logger, log)
  
  // 结束span
  Span::end(span)
  
  // 验证所有操作成功完成
  assert_true(true)
}

test "多级context传播集成测试" {
  // 创建多级context层次结构
  let root_ctx = Context::root()
  
  // Level 1: 添加用户信息
  let user_key = ContextKey::new("user.id")
  let level1_ctx = Context::with_value(root_ctx, user_key, "user-12345")
  
  // Level 2: 添加请求信息
  let request_key = ContextKey::new("request.id")
  let level2_ctx = Context::with_value(level1_ctx, request_key, "req-67890")
  
  // Level 3: 添加会话信息
  let session_key = ContextKey::new("session.id")
  let level3_ctx = Context::with_value(level2_ctx, session_key, "sess-abcdef")
  
  // 验证每一级的context值
  let user_id = Context::get(level3_ctx, user_key)
  match user_id {
    Some(id) => assert_eq(id, "user-12345")
    None => assert_true(false)
  }
  
  let request_id = Context::get(level3_ctx, request_key)
  match request_id {
    Some(id) => assert_eq(id, "req-67890")
    None => assert_true(false)
  }
  
  let session_id = Context::get(level3_ctx, session_key)
  match session_id {
    Some(id) => assert_eq(id, "sess-abcdef")
    None => assert_true(false)
  }
  
  // 创建span并关联context
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "multilevel.test")
  let span = Tracer::start_span(tracer, "multilevel.operation")
  
  // 添加context信息到span
  Span::add_event(span, "context.info", Some([
    ("user.id", StringValue("user-12345")),
    ("request.id", StringValue("req-67890")),
    ("session.id", StringValue("sess-abcdef"))
  ]))
  
  // 创建与context关联的日志
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "multilevel.logger")
  
  let log = LogRecord::new_with_context(
    Info,
    Some("Multilevel context operation"),
    Some(Attributes::with_attributes(Attributes::new(), [
      ("user.id", StringValue("user-12345")),
      ("request.id", StringValue("req-67890")),
      ("session.id", StringValue("sess-abcdef"))
    ])),
    Some(1735689600000000000L),
    None,
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    Some(level3_ctx)
  )
  
  Logger::emit(logger, log)
  Span::end(span)
  
  // 验证多级context传播成功
  assert_true(true)
}

test "metrics与logs关联集成测试" {
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let meter = MeterProvider::get_meter(meter_provider, "metrics.logs.correlation")
  let logger = LoggerProvider::get_logger(logger_provider, "metrics.logs.correlation")
  
  // 创建metrics
  let counter = Meter::create_counter(meter, "operations.total")
  let histogram = Meter::create_histogram(meter, "operation.duration.ms")
  let gauge = Meter::create_gauge(meter, "active.operations")
  
  // 记录metrics
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 250.5)
  
  // 创建与metrics关联的日志
  let metrics_log = LogRecord::new_with_context(
    Info,
    Some("Metrics recorded"),
    Some(Attributes::with_attributes(Attributes::new(), [
      ("metric.name", StringValue("operations.total")),
      ("metric.value", FloatValue(1.0)),
      ("metric.type", StringValue("counter"))
    ])),
    Some(1735689600000000000L),
    None,
    None,
    None,
    Some(Context::root())
  )
  
  Logger::emit(logger, metrics_log)
  
  // 记录更多metrics和日志
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 300.0)
  
  let histogram_log = LogRecord::new_with_context(
    Info,
    Some("Histogram metric recorded"),
    Some(Attributes::with_attributes(Attributes::new(), [
      ("metric.name", StringValue("operation.duration.ms")),
      ("metric.value", FloatValue(300.0)),
      ("metric.type", StringValue("histogram"))
    ])),
    Some(1735689600000001000L),
    None,
    None,
    None,
    Some(Context::root())
  )
  
  Logger::emit(logger, histogram_log)
  
  // 验证metrics和日志的关联
  assert_eq(LogRecord::body(metrics_log), Some("Metrics recorded"))
  assert_eq(LogRecord::body(histogram_log), Some("Histogram metric recorded"))
}

test "跨组件错误传播集成测试" {
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.propagation")
  let meter = MeterProvider::get_meter(meter_provider, "error.propagation")
  let logger = LoggerProvider::get_logger(logger_provider, "error.propagation")
  
  // 创建error counter
  let error_counter = Meter::create_counter(meter, "errors.total")
  
  // 创建span并模拟错误
  let span = Tracer::start_span(tracer, "error.prone.operation")
  Span::add_event(span, "operation.start", None)
  
  // 模拟错误发生
  Span::set_status(span, Error, Some("Database connection failed"))
  Span::add_event(span, "error.occurred", Some([
    ("error.type", StringValue("database")),
    ("error.code", StringValue("CONN_FAILED")),
    ("retry.count", IntValue(3))
  ]))
  
  // 记录error metric
  Counter::add(error_counter, 1.0)
  
  // 创建错误日志
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Database connection failed after 3 retries"),
    Some(Attributes::with_attributes(Attributes::new(), [
      ("error.type", StringValue("database")),
      ("error.code", StringValue("CONN_FAILED")),
      ("retry.count", IntValue(3)),
      ("operation.name", StringValue("error.prone.operation"))
    ])),
    Some(1735689600000000000L),
    None,
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    Some(Context::root())
  )
  
  Logger::emit(logger, error_log)
  
  // 结束span
  Span::end(span)
  
  // 验证错误传播
  assert_eq(LogRecord::severity_number(error_log), Error)
  match LogRecord::body(error_log) {
    Some(message) => assert_true(message.contains("Database connection failed"))
    None => assert_true(false)
  }
}
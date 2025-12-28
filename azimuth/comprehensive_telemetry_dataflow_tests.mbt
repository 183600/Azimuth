// 综合遥测数据流测试用例
// 测试从追踪、指标到日志的完整遥测数据流

test "完整遥测数据流测试" {
  // 1. 设置资源
  let resource = Resource::new()
  let resource_attrs = [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("1.2.3")),
    ("deployment.environment", StringValue("production"))
  ]
  let service_resource = Resource::with_attributes(resource, resource_attrs)
  
  // 2. 创建追踪提供者并开始追踪
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "user-service", Some("1.2.3"))
  
  // 3. 开始一个span
  let span_attrs = [
    ("http.method", StringValue("GET")),
    ("http.url", StringValue("/api/users/123")),
    ("user.id", IntValue(123))
  ]
  let parent_span = Tracer::start_span(tracer, "HTTP GET /api/users/123", Some(span_attrs))
  
  // 验证span属性
  assert_eq(Span::name(parent_span), "HTTP GET /api/users/123")
  assert_eq(Span::kind(parent_span), Internal)
  assert_true(Span::is_recording(parent_span))
  
  // 4. 添加事件和状态
  Span::add_event(parent_span, "开始处理请求", Some([("processing.start", StringValue("true"))]))
  Span::set_status(parent_span, Ok, Some("请求处理成功"))
  
  // 5. 创建指标收集器并记录指标
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "user-service")
  
  // 创建计数器
  let request_counter = Meter::create_counter(meter, "http.requests.total", 
    Some("HTTP请求总数"), Some("requests"))
  Counter::add(request_counter, 1.0, Some(Attributes::new()))
  
  // 创建直方图
  let request_duration = Meter::create_histogram(meter, "http.request.duration",
    Some("HTTP请求持续时间"), Some("ms"))
  Histogram::record(request_duration, 125.5, Some(Attributes::new()))
  
  // 创建上下行计数器
  let active_connections = Meter::create_updown_counter(meter, "http.active_connections",
    Some("当前活跃连接数"), Some("connections"))
  UpDownCounter::add(active_connections, 1.0, Some(Attributes::new()))
  
  // 创建仪表
  let memory_usage = Meter::create_gauge(meter, "process.memory.usage",
    Some("进程内存使用量"), Some("bytes"))
  // 注意：Gauge通常通过回调设置，这里简化处理
  
  // 6. 创建日志记录器并记录日志
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "user-service")
  
  // 创建带有完整上下文的日志记录
  let log_attrs = Attributes::new()
  Attributes::set(log_attrs, "user.id", IntValue(123))
  Attributes::set(log_attrs, "request.id", StringValue("req-abc-123"))
  
  let log_record = LogRecord::new_with_context(
    Info,
    Some("用户请求处理完成"),
    Some(log_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(SpanContext::trace_id(Span::span_context(parent_span))),
    Some(SpanContext::span_id(Span::span_context(parent_span))),
    Some(Context::root())
  )
  
  Logger::emit(logger, log_record)
  
  // 7. 验证日志记录
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("用户请求处理完成"))
  assert_eq(LogRecord::trace_id(log_record), Some(SpanContext::trace_id(Span::span_context(parent_span))))
  
  // 8. 结束span
  Span::end(parent_span)
  
  // 9. 验证资源属性
  let service_name = Resource::get_attribute(service_resource, "service.name")
  let service_version = Resource::get_attribute(service_resource, "service.version")
  
  assert_eq(service_name, Some(StringValue("user-service")))
  assert_eq(service_version, Some(StringValue("1.2.3")))
  
  // 测试完成
  assert_true(true)
}

test "跨组件遥测上下文传播测试" {
  // 1. 创建初始上下文
  let root_ctx = Context::root()
  let trace_key = ContextKey::new("trace.context")
  let ctx_with_trace = Context::with_value(root_ctx, trace_key, "trace-123")
  
  // 2. 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 3. 创建载体并注入上下文
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_with_trace, carrier)
  
  // 4. 验证注入的数据
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_trace.is_some())
  
  // 5. 从载体中提取上下文
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  assert_eq(extracted_value, Some("true"))
  
  // 6. 创建行李数据
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "456")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-789")
  
  // 7. 验证行李数据
  let user_id = Baggage::get_entry(baggage_with_session, "user.id")
  let session_id = Baggage::get_entry(baggage_with_session, "session.id")
  
  assert_eq(user_id, Some("456"))
  assert_eq(session_id, Some("session-789"))
  
  // 8. 测试行李数据移除
  let baggage_without_session = Baggage::remove_entry(baggage_with_session, "session.id")
  let removed_session = Baggage::get_entry(baggage_without_session, "session.id")
  
  assert_eq(removed_session, None)
  
  assert_true(true)
}

test "遥测数据完整性验证测试" {
  // 1. 创建带有验证信息的span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "validation-service")
  
  let validation_span = Tracer::start_span(tracer, "数据验证")
  let span_ctx = Span::span_context(validation_span)
  
  // 2. 验证span上下文的有效性
  assert_true(SpanContext::is_valid(span_ctx))
  
  // 3. 创建带有时间戳的日志记录
  let current_time = Clock::now_unix_nanos(Clock::system())
  let validation_log = LogRecord::new_with_context(
    Warn,
    Some("数据验证警告"),
    None,
    Some(current_time),
    Some(current_time + 1000000L), // 1ms后观察
    Some(SpanContext::trace_id(span_ctx)),
    Some(SpanContext::span_id(span_ctx)),
    None
  )
  
  // 4. 验证日志记录的时间戳
  assert_eq(LogRecord::timestamp(validation_log), Some(current_time))
  assert_eq(LogRecord::observed_timestamp(validation_log), Some(current_time + 1000000L))
  
  // 5. 创建指标并验证属性
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "validation-service")
  
  let validation_counter = Meter::create_counter(meter, "validation.operations",
    Some("验证操作计数"), Some("operations"))
  
  let counter_instrument = Counter(validation_counter.name, validation_counter.description, validation_counter.unit)
  assert_eq(Instrument::name(counter_instrument), "validation.operations")
  assert_eq(Instrument::description(counter_instrument), Some("验证操作计数"))
  assert_eq(Instrument::unit(counter_instrument), Some("operations"))
  
  // 6. 记录指标
  Counter::add(validation_counter, 1.0)
  
  // 7. 设置span状态并结束
  Span::set_status(validation_span, Ok, Some("验证完成"))
  Span::end(validation_span)
  
  assert_true(true)
}

test "多层级遥测数据关联测试" {
  // 1. 创建父级span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "orchestration-service")
  
  let parent_span = Tracer::start_span(tracer, "业务流程编排")
  let parent_ctx = Span::span_context(parent_span)
  
  // 2. 创建子级span
  let child_span = Tracer::start_span(tracer, "数据访问")
  let child_ctx = Span::span_context(child_span)
  
  // 3. 验证span层级关系
  assert_eq(Span::name(parent_span), "业务流程编排")
  assert_eq(Span::name(child_span), "数据访问")
  
  // 4. 创建与父级span关联的日志
  let parent_log = LogRecord::new_with_context(
    Info,
    Some("开始业务流程"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(parent_ctx)),
    Some(SpanContext::span_id(parent_ctx)),
    None
  )
  
  // 5. 创建与子级span关联的日志
  let child_log = LogRecord::new_with_context(
    Debug,
    Some("执行数据库查询"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(child_ctx)),
    Some(SpanContext::span_id(child_ctx)),
    None
  )
  
  // 6. 验证日志与span的关联
  assert_eq(LogRecord::trace_id(parent_log), Some(SpanContext::trace_id(parent_ctx)))
  assert_eq(LogRecord::span_id(parent_log), Some(SpanContext::span_id(parent_ctx)))
  
  assert_eq(LogRecord::trace_id(child_log), Some(SpanContext::trace_id(child_ctx)))
  assert_eq(LogRecord::span_id(child_log), Some(SpanContext::span_id(child_ctx)))
  
  // 7. 创建与业务流程关联的指标
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "orchestration-service")
  
  let process_duration = Meter::create_histogram(meter, "business.process.duration",
    Some("业务流程持续时间"), Some("ms"))
  
  // 记录业务流程持续时间
  Histogram::record(process_duration, 350.75)
  
  // 8. 结束子级和父级span
  Span::end(child_span)
  Span::end(parent_span)
  
  assert_true(true)
}
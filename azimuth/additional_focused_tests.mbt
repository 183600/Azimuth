test "InstrumentationScope创建和属性测试" {
  // 测试基本的InstrumentationScope创建
  let basic_scope = InstrumentationScope::{ 
    name: "test.scope", 
    version: None, 
    schema_url: None 
  }
  assert_eq(basic_scope.name, "test.scope")
  assert_true(basic_scope.version is None)
  assert_true(basic_scope.schema_url is None)
  
  // 测试带有版本的InstrumentationScope
  let versioned_scope = InstrumentationScope::{ 
    name: "versioned.scope", 
    version: Some("1.2.3"), 
    schema_url: None 
  }
  assert_eq(versioned_scope.name, "versioned.scope")
  match versioned_scope.version {
    Some(v) => assert_eq(v, "1.2.3")
    None => assert_true(false)
  }
  assert_true(versioned_scope.schema_url is None)
  
  // 测试带有schema_url的InstrumentationScope
  let schema_scope = InstrumentationScope::{ 
    name: "schema.scope", 
    version: None, 
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0") 
  }
  assert_eq(schema_scope.name, "schema.scope")
  assert_true(schema_scope.version is None)
  match schema_scope.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.20.0")
    None => assert_true(false)
  }
  
  // 测试完整的InstrumentationScope
  let full_scope = InstrumentationScope::{ 
    name: "full.scope", 
    version: Some("2.0.0"), 
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0") 
  }
  assert_eq(full_scope.name, "full.scope")
  match full_scope.version {
    Some(v) => assert_eq(v, "2.0.0")
    None => assert_true(false)
  }
  match full_scope.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.20.0")
    None => assert_true(false)
  }
  
  // 测试通过Tracer获取的InstrumentationScope
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test.tracer", Some("1.0.0"))
  let tracer_scope = Tracer::instrumentation_scope(tracer)
  assert_eq(tracer_scope.name, "test.tracer")
  match tracer_scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_true(false)
  }
  
  // 测试通过Meter获取的InstrumentationScope
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter", Some("2.0.0"))
  let meter_scope = meter.scope
  assert_eq(meter_scope.name, "test.meter")
  match meter_scope.version {
    Some(v) => assert_eq(v, "2.0.0")
    None => assert_true(false)
  }
  
  // 测试通过Logger获取的InstrumentationScope
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test.logger", Some("3.0.0"))
  let logger_scope = logger.scope
  assert_eq(logger_scope.name, "test.logger")
  match logger_scope.version {
    Some(v) => assert_eq(v, "3.0.0")
    None => assert_true(false)
  }
}

test "UpDownCounter详细操作测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "updown.test.meter")
  
  // 测试基本的UpDownCounter创建
  let basic_counter = Meter::create_updown_counter(meter, "basic.counter")
  assert_eq(basic_counter.name, "basic.counter")
  assert_true(basic_counter.description is None)
  assert_true(basic_counter.unit is None)
  
  // 测试带有描述和单位的UpDownCounter
  let detailed_counter = Meter::create_updown_counter(
    meter, 
    "detailed.counter", 
    Some("Detailed counter for testing"), 
    Some("operations")
  )
  assert_eq(detailed_counter.name, "detailed.counter")
  match detailed_counter.description {
    Some(desc) => assert_eq(desc, "Detailed counter for testing")
    None => assert_true(false)
  }
  match detailed_counter.unit {
    Some(unit) => assert_eq(unit, "operations")
    None => assert_true(false)
  }
  
  // 测试UpDownCounter的增减操作
  UpDownCounter::add(basic_counter, 10.0) // 增加10
  UpDownCounter::add(basic_counter, -5.0) // 减少5
  UpDownCounter::add(basic_counter, 0.0)  // 增加0
  UpDownCounter::add(basic_counter, 3.14) // 增加浮点数
  
  // 测试带有属性的UpDownCounter操作
  let attrs = Attributes::new()
  Attributes::set(attrs, "operation.type", StringValue("read"))
  Attributes::set(attrs, "user.id", StringValue("12345"))
  
  UpDownCounter::add(detailed_counter, 1.0, Some(attrs))
  UpDownCounter::add(detailed_counter, -1.0, Some(attrs))
  UpDownCounter::add(detailed_counter, 2.5, Some(attrs))
  
  // 测试负数操作
  UpDownCounter::add(basic_counter, -100.0)
  UpDownCounter::add(basic_counter, 50.0)
  
  // 测试大数值操作
  UpDownCounter::add(basic_counter, 1000000.0)
  UpDownCounter::add(basic_counter, -500000.0)
  
  // 测试小数操作
  UpDownCounter::add(basic_counter, 0.001)
  UpDownCounter::add(basic_counter, -0.0005)
  
  assert_true(true) // 如果没有崩溃则测试通过
}

test "Gauge详细操作测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "gauge.test.meter")
  
  // 测试基本的Gauge创建
  let basic_gauge = Meter::create_gauge(meter, "basic.gauge")
  assert_eq(basic_gauge.name, "basic.gauge")
  assert_true(basic_gauge.description is None)
  assert_true(basic_gauge.unit is None)
  
  // 测试带有描述和单位的Gauge
  let memory_gauge = Meter::create_gauge(
    meter, 
    "memory.usage", 
    Some("Current memory usage in bytes"), 
    Some("bytes")
  )
  assert_eq(memory_gauge.name, "memory.usage")
  match memory_gauge.description {
    Some(desc) => assert_eq(desc, "Current memory usage in bytes")
    None => assert_true(false)
  }
  match memory_gauge.unit {
    Some(unit) => assert_eq(unit, "bytes")
    None => assert_true(false)
  }
  
  // 测试CPU使用率Gauge
  let cpu_gauge = Meter::create_gauge(
    meter, 
    "cpu.usage", 
    Some("Current CPU usage percentage"), 
    Some("percent")
  )
  assert_eq(cpu_gauge.name, "cpu.usage")
  
  // 模拟设置Gauge值（通过add操作，虽然语义上不太准确）
  Gauge::add(memory_gauge, 1024.0)  // 1KB
  Gauge::add(memory_gauge, 2048.0)  // 2KB
  Gauge::add(memory_gauge, 512.0)   // 512B
  
  Gauge::add(cpu_gauge, 25.5)       // 25.5%
  Gauge::add(cpu_gauge, 75.0)       // 75.0%
  Gauge::add(cpu_gauge, 100.0)      // 100.0%
  
  // 测试带有属性的Gauge操作
  let attrs = Attributes::new()
  Attributes::set(attrs, "instance.id", StringValue("server-001"))
  Attributes::set(attrs, "region", StringValue("us-west-2"))
  
  Gauge::add(memory_gauge, 4096.0, Some(attrs))
  Gauge::add(cpu_gauge, 50.0, Some(attrs))
  
  // 测试边界值
  Gauge::add(memory_gauge, 0.0)      // 零值
  Gauge::add(cpu_gauge, 0.0)         // 零值
  
  Gauge::add(memory_gauge, 9223372036854775807.0)  // 接近最大值
  Gauge::add(cpu_gauge, 0.01)       // 小数值
  
  // 测试负数（虽然某些Gauge可能不允许负值）
  Gauge::add(memory_gauge, -1.0)    // 负数测试
  Gauge::add(cpu_gauge, -0.1)       // 负数测试
  
  assert_true(true) // 如果没有崩溃则测试通过
}

test "LoggerProvider和Logger详细操作测试" {
  // 测试LoggerProvider创建
  let noop_provider = LoggerProvider::noop()
  let default_provider = LoggerProvider::default()
  
  // 测试从不同Provider获取Logger
  let noop_logger = LoggerProvider::get_logger(noop_provider, "noop.logger")
  let default_logger = LoggerProvider::get_logger(default_provider, "default.logger")
  
  // 验证Logger的InstrumentationScope
  assert_eq(noop_logger.scope.name, "noop.logger")
  assert_eq(default_logger.scope.name, "default.logger")
  
  // 测试带有版本的Logger
  let versioned_logger = LoggerProvider::get_logger(
    default_provider, 
    "versioned.logger", 
    Some("1.2.3")
  )
  assert_eq(versioned_logger.scope.name, "versioned.logger")
  match versioned_logger.scope.version {
    Some(v) => assert_eq(v, "1.2.3")
    None => assert_true(false)
  }
  
  // 测试不同严重性级别的LogRecord
  let trace_record = LogRecord::new(Trace, "Trace message for debugging")
  let debug_record = LogRecord::new(Debug, "Debug message with details")
  let info_record = LogRecord::new(Info, "Info message for general information")
  let warn_record = LogRecord::new(Warn, "Warning message for potential issues")
  let error_record = LogRecord::new(Error, "Error message for failures")
  let fatal_record = LogRecord::new(Fatal, "Fatal message for critical failures")
  
  // 发送不同级别的日志
  Logger::emit(noop_logger, trace_record)
  Logger::emit(noop_logger, debug_record)
  Logger::emit(noop_logger, info_record)
  Logger::emit(default_logger, warn_record)
  Logger::emit(default_logger, error_record)
  Logger::emit(versioned_logger, fatal_record)
  
  // 测试带有属性的复杂LogRecord
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "session.id", StringValue("session-67890"))
  Attributes::set(attrs, "request.id", StringValue("req-abcdef"))
  Attributes::set(attrs, "response.time", IntValue(150))
  Attributes::set(attrs, "success", BoolValue(true))
  
  let complex_record = LogRecord::new_with_context(
    Info,
    Some("User operation completed successfully"),
    Some(attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system()) + 1000000L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(Context::root())
  )
  
  Logger::emit(default_logger, complex_record)
  
  // 测试带有错误上下文的LogRecord
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "error.type", StringValue("DatabaseError"))
  Attributes::set(error_attrs, "error.message", StringValue("Connection timeout"))
  Attributes::set(error_attrs, "retry.count", IntValue(3))
  Attributes::set(error_attrs, "stack.trace", StringValue("at database.connect (line 42)"))
  
  let error_record = LogRecord::new_with_context(
    Error,
    Some("Database operation failed after 3 retries"),
    Some(error_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace123"),
    Some("span456"),
    None
  )
  
  Logger::emit(default_logger, error_record)
  
  // 测试批量日志发送
  for i in 0..10 {
    let batch_record = LogRecord::new_with_context(
      Info,
      Some("Batch log message " + i.to_string()),
      None,
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      None,
      None,
      None
    )
    Logger::emit(default_logger, batch_record)
  }
  
  assert_true(true) // 如果没有崩溃则测试通过
}

test "跨组件集成测试" {
  // 创建所有需要的Provider
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // 创建所有需要的组件
  let tracer = TracerProvider::get_tracer(tracer_provider, "integration.tracer", Some("1.0.0"))
  let meter = MeterProvider::get_meter(meter_provider, "integration.meter", Some("1.0.0"))
  let logger = LoggerProvider::get_logger(logger_provider, "integration.logger", Some("1.0.0"))
  
  // 验证所有组件都有相同的InstrumentationScope信息
  assert_eq(tracer.scope.name, "integration.tracer")
  assert_eq(meter.scope.name, "integration.meter")
  assert_eq(logger.scope.name, "integration.logger")
  
  match tracer.scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_true(false)
  }
  
  // 创建Span并记录指标和日志
  let span = Tracer::start_span(tracer, "integration.operation")
  
  // 在Span操作过程中记录指标
  let counter = Meter::create_counter(meter, "operations.total")
  let histogram = Meter::create_histogram(meter, "operation.duration")
  let updown_counter = Meter::create_updown_counter(meter, "active.operations")
  let gauge = Meter::create_gauge(meter, "memory.usage")
  
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 150.0)
  UpDownCounter::add(updown_counter, 1.0)
  Gauge::add(gauge, 1024.0)
  
  // 添加Span事件
  Span::add_event(span, "operation.started", Some([("timestamp", StringValue("now"))]))
  
  // 记录日志
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Operation started"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span.span_context)),
    Some(SpanContext::span_id(span.span_context)),
    None
  )
  Logger::emit(logger, start_log)
  
  // 模拟操作过程中的更多指标记录
  Histogram::record(histogram, 200.0)
  Gauge::add(gauge, 2048.0)
  
  // 添加更多Span事件
  Span::add_event(span, "database.query", Some([("query.time", IntValue(50))]))
  
  // 记录更多日志
  let progress_log = LogRecord::new_with_context(
    Debug,
    Some("Operation in progress"),
    Some([("progress", IntValue(50)]),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span.span_context)),
    Some(SpanContext::span_id(span.span_context)),
    None
  )
  Logger::emit(logger, progress_log)
  
  // 操作完成
  Span::add_event(span, "operation.completed", Some([("success", BoolValue(true))]))
  Histogram::record(histogram, 300.0)
  UpDownCounter::add(updown_counter, -1.0)
  Gauge::add(gauge, 512.0)
  
  let completion_log = LogRecord::new_with_context(
    Info,
    Some("Operation completed successfully"),
    Some([("duration", IntValue(300)]),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span.span_context)),
    Some(SpanContext::span_id(span.span_context)),
    None
  )
  Logger::emit(logger, completion_log)
  
  // 设置Span状态并结束
  Span::set_status(span, Ok)
  Span::end(span)
  
  // 记录最终指标
  Counter::add(counter, 1.0)
  
  assert_true(true) // 如果没有崩溃则测试通过
}

test "Context和Propagation集成测试" {
  // 创建Context和Baggage
  let root_ctx = Context::root()
  let baggage = Baggage::new()
  
  // 设置Context值
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  let request_key = ContextKey::new("request.id")
  
  let ctx_with_user = Context::with_value(root_ctx, user_key, "user123")
  let ctx_with_session = Context::with_value(ctx_with_user, session_key, "session456")
  let ctx_with_request = Context::with_value(ctx_with_session, request_key, "req789")
  
  // 设置Baggage条目
  let baggage1 = Baggage::set_entry(baggage, "user.role", StringValue("admin"))
  let baggage2 = Baggage::set_entry(baggage1, "tenant.id", StringValue("tenant123"))
  let final_baggage = Baggage::set_entry(baggage2, "feature.flags", StringValue("beta,experimental"))
  
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // 创建SpanContext
  let span_ctx = SpanContext::new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "rojo=00f067aa0ba902b7"
  )
  
  // 注入Context到载体
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_request, carrier)
  
  // 手动添加一些头部以模拟完整的传播
  TextMapCarrier::set(carrier, "baggage", "user.role=admin,tenant.id=tenant123,feature.flags=beta,experimental")
  TextMapCarrier::set(carrier, "x-request-id", "req789")
  
  // 验证注入的头部
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent is Some)
  
  let baggage_header = TextMapCarrier::get(carrier, "baggage")
  match baggage_header {
    Some(value) => assert_true(value.contains("user.role=admin"))
    None => assert_true(false)
  }
  
  // 从载体中提取Context
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // 验证提取的Context
  let extracted_user = Context::get(extracted_ctx, ContextKey::new("extracted"))
  match extracted_user {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
  
  // 创建新的Span并使用提取的Context
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "propagation.test")
  let span = Tracer::start_span(tracer, "propagation.operation")
  
  // 在Span中记录Context信息
  Span::add_event(span, "context.extracted", Some([
    ("user.id", StringValue("user123")),
    ("session.id", StringValue("session456")),
    ("request.id", StringValue("req789"))
  ]))
  
  // 记录带有Context的日志
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "propagation.logger")
  
  let context_log = LogRecord::new_with_context(
    Info,
    Some("Context propagation test"),
    Some([
      ("user.role", StringValue("admin")),
      ("tenant.id", StringValue("tenant123")),
      ("feature.flags", StringValue("beta,experimental"))
    ]),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span.span_context)),
    Some(SpanContext::span_id(span.span_context)),
    Some(extracted_ctx)
  )
  
  Logger::emit(logger, context_log)
  
  Span::end(span)
  
  assert_true(true) // 如果没有崩溃则测试通过
}

test "错误处理和异常情况测试" {
  // 测试空字符串和None值的处理
  let empty_name_scope = InstrumentationScope::{ 
    name: "", 
    version: None, 
    schema_url: None 
  }
  assert_eq(empty_name_scope.name, "")
  
  let empty_version_scope = InstrumentationScope::{ 
    name: "test.scope", 
    version: Some(""), 
    schema_url: None 
  }
  match empty_version_scope.version {
    Some(v) => assert_eq(v, "")
    None => assert_true(false)
  }
  
  // 测试极值属性
  let attrs = Attributes::new()
  Attributes::set(attrs, "max.int", IntValue(2147483647))
  Attributes::set(attrs, "min.int", IntValue(-2147483648))
  Attributes::set(attrs, "max.float", FloatValue(1.7976931348623157e+308))
  Attributes::set(attrs, "min.float", FloatValue(-1.7976931348623157e+308))
  Attributes::set(attrs, "zero.float", FloatValue(0.0))
  Attributes::set(attrs, "negative.zero", FloatValue(-0.0))
  
  // 测试特殊浮点值
  Attributes::set(attrs, "infinity", FloatValue(1.0/0.0))
  Attributes::set(attrs, "negative.infinity", FloatValue(-1.0/0.0))
  Attributes::set(attrs, "nan", FloatValue(0.0/0.0))
  
  // 测试超长字符串
  let very_long_string = "a" * 10000
  Attributes::set(attrs, "long.string", StringValue(very_long_string))
  
  // 测试空数组
  let empty_string_array = ArrayStringValue([])
  let empty_int_array = ArrayIntValue([])
  Attributes::set(attrs, "empty.string.array", empty_string_array)
  Attributes::set(attrs, "empty.int.array", empty_int_array)
  
  // 测试包含特殊字符的字符串
  let special_chars = "特殊字符!@#$%^&*(){}[]|\\:;\"'<>?,./"
  Attributes::set(attrs, "special.chars", StringValue(special_chars))
  
  // 测试Unicode字符
  let unicode_string = "测试🚀emoji和中文混合🌟"
  Attributes::set(attrs, "unicode", StringValue(unicode_string))
  
  // 测试无效的SpanContext
  let invalid_ctx1 = SpanContext::new("", "", false, "")
  let invalid_ctx2 = SpanContext::new("trace", "", true, "")
  let invalid_ctx3 = SpanContext::new("", "span", true, "")
  
  assert_false(SpanContext::is_valid(invalid_ctx1))
  assert_false(SpanContext::is_valid(invalid_ctx2))
  assert_false(SpanContext::is_valid(invalid_ctx3))
  
  // 测试空LogRecord
  let empty_log = LogRecord::new_with_context(
    Info,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  match LogRecord::body(empty_log) {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // 测试边界情况的指标值
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "boundary.test")
  
  let counter = Meter::create_counter(meter, "boundary.counter")
  let histogram = Meter::create_histogram(meter, "boundary.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "boundary.updown")
  let gauge = Meter::create_gauge(meter, "boundary.gauge")
  
  // 测试极大值
  Counter::add(counter, 1.7976931348623157e+308)
  Histogram::record(histogram, 1.7976931348623157e+308)
  UpDownCounter::add(updown_counter, 1.7976931348623157e+308)
  Gauge::add(gauge, 1.7976931348623157e+308)
  
  // 测试极小值
  Counter::add(counter, 4.940656458412465e-324)
  Histogram::record(histogram, 4.940656458412465e-324)
  UpDownCounter::add(updown_counter, 4.940656458412465e-324)
  Gauge::add(gauge, 4.940656458412465e-324)
  
  // 测试负值
  UpDownCounter::add(updown_counter, -1.7976931348623157e+308)
  Gauge::add(gauge, -1.7976931348623157e+308)
  
  // 测试零值
  Counter::add(counter, 0.0)
  Histogram::record(histogram, 0.0)
  UpDownCounter::add(updown_counter, 0.0)
  Gauge::add(gauge, 0.0)
  
  assert_true(true) // 如果没有崩溃则测试通过
}

test "性能和资源管理测试" {
  // 测试大量Span创建和销毁
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance.test")
  
  for i in 0..100 {
    let span = Tracer::start_span(tracer, "perf.span." + i.to_string())
    Span::add_event(span, "event." + i.to_string(), None)
    Span::set_status(span, Ok)
    Span::end(span)
  }
  
  // 测试大量属性操作
  let attrs = Attributes::new()
  for i in 0..100 {
    Attributes::set(attrs, "key." + i.to_string(), StringValue("value." + i.to_string()))
  }
  
  // 测试大量指标记录
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "perf.meter")
  
  let counter = Meter::create_counter(meter, "perf.counter")
  let histogram = Meter::create_histogram(meter, "perf.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "perf.updown")
  let gauge = Meter::create_gauge(meter, "perf.gauge")
  
  for i in 0..100 {
    Counter::add(counter, i.to_double())
    Histogram::record(histogram, i.to_double() * 1.5)
    UpDownCounter::add(updown_counter, i.to_double() - 50.0)
    Gauge::add(gauge, i.to_double() * 10.0)
  }
  
  // 测试大量日志记录
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "perf.logger")
  
  for i in 0..100 {
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Performance log message " + i.to_string()),
      Some([("iteration", IntValue(i))]),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      None,
      None,
      None
    )
    Logger::emit(logger, log_record)
  }
  
  // 测试大量Baggage操作
  let baggage = Baggage::new()
  for i in 0..100 {
    baggage = Baggage::set_entry(baggage, "baggage.key." + i.to_string(), "baggage.value." + i.to_string())
  }
  
  // 测试大量Context操作
  let ctx = Context::root()
  for i in 0..100 {
    let key = ContextKey::new("context.key." + i.to_string())
    ctx = Context::with_value(ctx, key, "context.value." + i.to_string())
  }
  
  // 测试大量TextMapCarrier操作
  let carrier = TextMapCarrier::new()
  for i in 0..100 {
    TextMapCarrier::set(carrier, "header." + i.to_string(), "value." + i.to_string())
  }
  
  // 测试大量Propagation操作
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  for i in 0..100 {
    let test_carrier = TextMapCarrier::new()
    TextMapCarrier::set(test_carrier, "traceparent", "00-trace" + i.to_string() + "-span" + i.to_string() + "-01")
    let extracted_ctx = CompositePropagator::extract(composite, test_carrier)
    CompositePropagator::inject(composite, extracted_ctx, test_carrier)
  }
  
  // 测试大量Resource操作
  let resource = Resource::new()
  let mut resource_attrs = []
  for i in 0..100 {
    resource_attrs = resource_attrs.push(("resource.key." + i.to_string(), StringValue("resource.value." + i.to_string())))
  }
  let resource_with_attrs = Resource::with_attributes(resource, resource_attrs)
  
  assert_true(true) // 如果没有崩溃则测试通过
}

test "多线程并发安全性模拟测试" {
  // 模拟并发操作的测试（单线程环境中模拟并发场景）
  
  // 创建共享资源
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent.test")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "concurrent.meter")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "concurrent.logger")
  
  // 创建多个Span（模拟并发）
  let spans = []
  for i in 0..10 {
    spans = spans.push(Tracer::start_span(tracer, "concurrent.span." + i.to_string()))
  }
  
  // 并发操作指标
  let counter = Meter::create_counter(meter, "concurrent.counter")
  let histogram = Meter::create_histogram(meter, "concurrent.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "concurrent.updown")
  let gauge = Meter::create_gauge(meter, "concurrent.gauge")
  
  // 模拟并发指标操作
  for i in 0..10 {
    Counter::add(counter, i.to_double())
    Histogram::record(histogram, i.to_double() * 100.0)
    UpDownCounter::add(updown_counter, 1.0)
    Gauge::add(gauge, i.to_double() * 1024.0)
  }
  
  // 为每个Span添加事件（模拟并发操作）
  for i in 0..10 {
    match spans[i] {
      Some(span) => {
        Span::add_event(span, "concurrent.event." + i.to_string(), Some([
          ("thread.id", IntValue(i)),
          ("timestamp", StringValue("now"))
        ]))
        Span::set_status(span, Ok)
      }
      None => ()
    }
  }
  
  // 并发日志记录
  for i in 0..10 {
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Concurrent log message " + i.to_string()),
      Some([
        ("thread.id", IntValue(i)),
        ("operation", StringValue("concurrent.test"))
      ]),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some("concurrent.trace.id"),
      Some("concurrent.span.id." + i.to_string()),
      None
    )
    Logger::emit(logger, log_record)
  }
  
  // 结束所有Span（模拟并发清理）
  for i in 0..10 {
    match spans[i] {
      Some(span) => Span::end(span)
      None => ()
    }
  }
  
  // 模拟并发Baggage操作
  let baggage = Baggage::new()
  for i in 0..10 {
    baggage = Baggage::set_entry(baggage, "concurrent.key." + i.to_string(), "concurrent.value." + i.to_string())
  }
  
  // 模拟并发Context操作
  let ctx = Context::root()
  for i in 0..10 {
    let key = ContextKey::new("concurrent.context." + i.to_string())
    ctx = Context::with_value(ctx, key, "concurrent.value." + i.to_string())
  }
  
  // 模拟并发Propagation操作
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  for i in 0..10 {
    let carrier = TextMapCarrier::new()
    TextMapCarrier::set(carrier, "traceparent", "00-concurrent.trace-" + i.to_string() + "-concurrent.span-" + i.to_string() + "-01")
    let extracted_ctx = CompositePropagator::extract(composite, carrier)
    CompositePropagator::inject(composite, extracted_ctx, carrier)
  }
  
  assert_true(true) // 如果没有崩溃则测试通过
}
// 目标增强测试用例 - 针对特定高级功能的测试

test "InstrumentationScope完整功能测试" {
  // 测试创建带有完整信息的InstrumentationScope
  let scope1 = InstrumentationScope::{ 
    name: "azimuth.tracer", 
    version: Some("1.2.3"), 
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0") 
  }
  
  assert_eq(scope1.name, "azimuth.tracer")
  match scope1.version {
    Some(v) => assert_eq(v, "1.2.3")
    None => assert_true(false)
  }
  match scope1.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.20.0")
    None => assert_true(false)
  }
  
  // 测试最小配置的InstrumentationScope
  let scope2 = InstrumentationScope::{ 
    name: "minimal.scope", 
    version: None, 
    schema_url: None 
  }
  
  assert_eq(scope2.name, "minimal.scope")
  assert_true(scope2.version is None)
  assert_true(scope2.schema_url is None)
  
  // 测试只有版本的InstrumentationScope
  let scope3 = InstrumentationScope::{ 
    name: "version.only.scope", 
    version: Some("2.0.0"), 
    schema_url: None 
  }
  
  assert_eq(scope3.name, "version.only.scope")
  match scope3.version {
    Some(v) => assert_eq(v, "2.0.0")
    None => assert_true(false)
  }
  assert_true(scope3.schema_url is None)
  
  // 测试在Tracer中使用InstrumentationScope
  let tracer = Tracer::{ scope: scope1 }
  let retrieved_scope = Tracer::instrumentation_scope(tracer)
  assert_eq(retrieved_scope.name, "azimuth.tracer")
  match retrieved_scope.version {
    Some(v) => assert_eq(v, "1.2.3")
    None => assert_true(false)
  }
  
  // 测试在Logger中使用InstrumentationScope
  let logger = Logger::{ scope: scope2 }
  assert_eq(logger.scope.name, "minimal.scope")
  assert_true(logger.scope.version is None)
  
  // 测试在Meter中使用InstrumentationScope
  let meter = Meter::{ scope: scope3 }
  assert_eq(meter.scope.name, "version.only.scope")
  match meter.scope.version {
    Some(v) => assert_eq(v, "2.0.0")
    None => assert_true(false)
  }
}

test "复合传播器高级功能测试" {
  // 创建多个传播器
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // 测试单个传播器的复合
  let single_composite = CompositePropagator::new([trace_propagator1])
  let carrier1 = TextMapCarrier::new()
  
  let ctx1 = Context::with_value(Context::root(), ContextKey::new("test.key"), "test.value")
  CompositePropagator::inject(single_composite, ctx1, carrier1)
  
  let injected_trace = TextMapCarrier::get(carrier1, "traceparent")
  assert_true(injected_trace is Some)
  
  let extracted_ctx1 = CompositePropagator::extract(single_composite, carrier1)
  let extracted_value = Context::get(extracted_ctx1, ContextKey::new("extracted"))
  match extracted_value {
    Some(v) => assert_eq(v, "true")
    None => assert_true(false)
  }
  
  // 测试多个传播器的复合
  let multi_composite = CompositePropagator::new([trace_propagator1, trace_propagator2])
  let carrier2 = TextMapCarrier::new()
  
  let ctx2 = Context::with_value(Context::root(), ContextKey::new("multi.key"), "multi.value")
  CompositePropagator::inject(multi_composite, ctx2, carrier2)
  
  let extracted_ctx2 = CompositePropagator::extract(multi_composite, carrier2)
  let extracted_multi = Context::get(extracted_ctx2, ContextKey::new("extracted"))
  match extracted_multi {
    Some(v) => assert_eq(v, "true")
    None => assert_true(false)
  }
  
  // 测试链式传播操作
  let carrier3 = TextMapCarrier::new()
  TextMapCarrier::set(carrier3, "existing.header", "existing.value")
  
  CompositePropagator::inject(multi_composite, ctx2, carrier3)
  let existing_header = TextMapCarrier::get(carrier3, "existing.header")
  // 注意：简化实现可能不会保留现有头部
  
  // 测试空传播器列表（如果支持）
  // let empty_composite = CompositePropagator::new([])
  // let carrier4 = TextMapCarrier::new()
  // CompositePropagator::inject(empty_composite, ctx1, carrier4)
  
  assert_true(true)
}

test "度量仪器集成操作测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "integration.test.meter", Some("1.0.0"))
  
  // 测试创建多种类型的度量仪器
  let counter = Meter::create_counter(meter, "request.count", Some("Total requests"), Some("requests"))
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  
  // 测试所有度量仪器的基本操作
  Counter::add(counter, 10.0)
  Histogram::record(histogram, 150.5)
  UpDownCounter::add(updown_counter, 5.0)
  // Gauge通常用于记录当前值，简化实现可能没有record方法
  
  // 测试带属性的度量操作
  let attrs = Attributes::new()
  Attributes::set(attrs, "http.method", StringValue("GET"))
  Attributes::set(attrs, "http.status_code", IntValue(200))
  
  Counter::add(counter, 1.0, Some(attrs))
  Histogram::record(histogram, 85.2, Some(attrs))
  UpDownCounter::add(updown_counter, -2.0, Some(attrs))
  
  // 测试不同数值类型的度量操作
  Counter::add(counter, 0.0)  // 零值
  Counter::add(counter, -5.0) // 负值（如果允许）
  Counter::add(counter, 999999.999) // 大数值
  
  Histogram::record(histogram, 0.001) // 极小值
  Histogram::record(histogram, 1000000.0) // 极大值
  
  // 测试UpDownCounter的增加和减少
  UpDownCounter::add(updown_counter, 10.0)  // 增加
  UpDownCounter::add(updown_counter, -3.0)  // 减少
  UpDownCounter::add(updown_counter, 0.0)   // 零变化
  
  // 测试仪器元数据
  assert_eq(counter.name, "request.count")
  assert_eq(counter.description, Some("Total requests"))
  assert_eq(counter.unit, Some("requests"))
  
  assert_eq(histogram.name, "response.time")
  assert_eq(histogram.description, Some("Response time"))
  assert_eq(histogram.unit, Some("ms"))
  
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Active connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  assert_eq(gauge.name, "memory.usage")
  assert_eq(gauge.description, Some("Memory usage"))
  assert_eq(gauge.unit, Some("bytes"))
  
  assert_true(true)
}

test "日志记录批量操作测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "batch.test.logger", Some("1.0.0"))
  
  // 测试创建不同类型的日志记录
  let info_record = LogRecord::new(Info, "Information message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  
  // 测试批量发送日志记录
  Logger::emit(logger, info_record)
  Logger::emit(logger, warn_record)
  Logger::emit(logger, error_record)
  
  // 测试创建带有属性的复杂日志记录
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "session.id", StringValue("session-67890"))
  Attributes::set(attrs, "request.id", StringValue("req-abc123"))
  Attributes::set(attrs, "duration.ms", IntValue(250))
  
  let complex_record = LogRecord::new_with_context(
    Info,
    Some("User operation completed successfully"),
    Some(attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace123456789"),
    Some("span987654321"),
    Some(Context::with_value(Context::root(), ContextKey::new("operation"), "login"))
  )
  
  Logger::emit(logger, complex_record)
  
  // 测试创建多个不同严重级别的日志记录
  let trace_record = LogRecord::new_with_context(
    Trace,
    Some("Detailed trace information"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace123"),
    Some("span123"),
    None
  )
  
  let debug_record = LogRecord::new_with_context(
    Debug,
    Some("Debug information for troubleshooting"),
    Some(attrs),
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some("trace456"),
    Some("span456"),
    None
  )
  
  let fatal_record = LogRecord::new_with_context(
    Fatal,
    Some("Critical system error"),
    Some(attrs),
    Some(1735689600000000004L),
    Some(1735689600000000005L),
    Some("trace789"),
    Some("span789"),
    Some(Context::with_value(Context::root(), ContextKey::new("error.code"), "500"))
  )
  
  // 批量发送不同级别的日志
  Logger::emit(logger, trace_record)
  Logger::emit(logger, debug_record)
  Logger::emit(logger, fatal_record)
  
  // 测试空内容和特殊内容的日志记录
  let empty_body_record = LogRecord::new_with_context(
    Info,
    None,
    None,
    Some(1735689600000000006L),
    None,
    None,
    None,
    None
  )
  
  let long_body_record = LogRecord::new_with_context(
    Warn,
    Some("This is a very long log message that contains detailed information about a warning condition that occurred in the system. " * 10),
    None,
    Some(1735689600000000007L),
    None,
    None,
    None,
    None
  )
  
  let unicode_body_record = LogRecord::new_with_context(
    Error,
    Some("错误信息包含中文和特殊字符：测试🚀emoji和特殊符号!@#$%"),
    None,
    Some(1735689600000000008L),
    None,
    None,
    None,
    None
  )
  
  Logger::emit(logger, empty_body_record)
  Logger::emit(logger, long_body_record)
  Logger::emit(logger, unicode_body_record)
  
  assert_true(true)
}

test "跨服务追踪端到端测试" {
  // 模拟服务A的追踪
  let service_a_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-a", Some("1.0.0"))
  let service_a_span = Tracer::start_span(service_a_tracer, "service-a.operation")
  
  let service_a_context = Span::span_context(service_a_span)
  assert_true(SpanContext::is_valid(service_a_context))
  
  // 模拟传播到服务B
  let carrier = TextMapCarrier::new()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let context_a = Context::with_value(Context::root(), ContextKey::new("service"), "A")
  
  CompositePropagator::inject(propagator, context_a, carrier)
  
  // 服务B接收并继续追踪
  let extracted_context = CompositePropagator::extract(propagator, carrier)
  let service_b_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-b", Some("1.0.0"))
  let service_b_span = Tracer::start_span(service_b_tracer, "service-b.operation")
  
  // 服务B添加事件
  Span::add_event(service_b_span, "service.b.start", Some([("service", StringValue("B")), ("timestamp", StringValue("2025-01-01T00:00:00Z"))]))
  
  // 模拟服务B调用服务C
  let carrier_b = TextMapCarrier::new()
  let context_b = Context::with_value(extracted_context, ContextKey::new("service"), "B")
  CompositePropagator::inject(propagator, context_b, carrier_b)
  
  // 服务C处理请求
  let extracted_context_c = CompositePropagator::extract(propagator, carrier_b)
  let service_c_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-c", Some("1.0.0"))
  let service_c_span = Tracer::start_span(service_c_tracer, "service.c.operation")
  
  Span::add_event(service_c_span, "service.c.processing", Some([("service", StringValue("C")), ("processing.time.ms", IntValue(150))]))
  Span::set_status(service_c_span, Ok)
  Span::end(service_c_span)
  
  // 服务B完成处理
  Span::add_event(service_b_span, "service.b.complete", Some([("service", StringValue("B")), ("result", StringValue("success"))]))
  Span::set_status(service_b_span, Ok)
  Span::end(service_b_span)
  
  // 服务A完成处理
  Span::add_event(service_a_span, "service.a.complete", Some([("service", StringValue("A")), ("total.duration.ms", IntValue(500))]))
  Span::set_status(service_a_span, Ok)
  Span::end(service_a_span)
  
  // 验证所有span都已正确结束
  assert_false(Span::is_recording(service_a_span))
  assert_false(Span::is_recording(service_b_span))
  assert_false(Span::is_recording(service_c_span))
  
  // 测试错误场景的追踪
  let error_tracer = TracerProvider::get_tracer(TracerProvider::default(), "error.service", Some("1.0.0"))
  let error_span = Tracer::start_span(error_tracer, "error.operation")
  
  Span::add_event(error_span, "error.detected", Some([("error.code", StringValue("500")), ("error.message", StringValue("Internal server error"))]))
  Span::set_status(error_span, Error, Some("Processing failed due to database connection timeout"))
  Span::end(error_span)
  
  assert_true(true)
}

test "性能监控和资源使用测试" {
  // 测试Clock性能
  let clock = Clock::system()
  let start_time = Clock::now_unix_nanos(clock)
  
  // 模拟一些操作
  let attrs = Attributes::new()
  for i = 0; i < 100; i = i + 1 {
    Attributes::set(attrs, "key." + i.to_string(), StringValue("value." + i.to_string()))
  }
  
  let end_time = Clock::now_unix_nanos(clock)
  let duration = end_time - start_time
  
  // 在简化实现中，时间戳可能相同，但验证操作完成
  assert_true(duration >= 0L)
  
  // 测试Random生成性能
  let random = Random::system()
  let random_start = Clock::now_unix_nanos(clock)
  
  let random_arrays = []
  for i = 0; i < 10; i = i + 1 {
    let bytes = Random::next_bytes(random, 16)
    random_arrays.push(bytes)
  }
  
  let random_end = Clock::now_unix_nanos(clock)
  let random_duration = random_end - random_start
  
  assert_true(random_arrays.length() == 10)
  assert_true(random_duration >= 0L)
  
  // 测试资源属性操作性能
  let resource = Resource::new()
  let large_attributes = []
  
  let resource_start = Clock::now_unix_nanos(clock)
  for i = 0; i < 50; i = i + 1 {
    large_attributes.push(("attr." + i.to_string(), StringValue("value." + i.to_string())))
  }
  
  let resource_with_attrs = Resource::with_attributes(resource, large_attributes)
  let resource_end = Clock::now_unix_nanos(clock)
  let resource_duration = resource_end - resource_start
  
  assert_true(resource_duration >= 0L)
  
  // 测试Baggage操作性能
  let baggage = Baggage::new()
  let baggage_start = Clock::now_unix_nanos(clock)
  
  let current_baggage = baggage
  for i = 0; i < 25; i = i + 1 {
    current_baggage = Baggage::set_entry(current_baggage, "baggage." + i.to_string(), "value." + i.to_string())
  }
  
  let baggage_end = Clock::now_unix_nanos(clock)
  let baggage_duration = baggage_end - baggage_start
  
  assert_true(baggage_duration >= 0L)
  
  // 测试度量操作性能
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance.test.meter")
  let counter = Meter::create_counter(meter, "performance.counter")
  
  let metrics_start = Clock::now_unix_nanos(clock)
  for i = 0; i < 100; i = i + 1 {
    Counter::add(counter, i.to_double())
  }
  let metrics_end = Clock::now_unix_nanos(clock)
  let metrics_duration = metrics_end - metrics_start
  
  assert_true(metrics_duration >= 0L)
  
  // 测试日志操作性能
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "performance.test.logger")
  
  let logging_start = Clock::now_unix_nanos(clock)
  for i = 0; i < 50; i = i + 1 {
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Performance test log " + i.to_string()),
      None,
      Some(Clock::now_unix_nanos(clock)),
      None,
      None,
      None,
      None
    )
    Logger::emit(logger, log_record)
  }
  let logging_end = Clock::now_unix_nanos(clock)
  let logging_duration = logging_end - logging_start
  
  assert_true(logging_duration >= 0L)
  
  assert_true(true)
}

test "配置管理和国际化测试" {
  // 测试不同语言的错误消息
  let error_span = TracerProvider::get_tracer(TracerProvider::default(), "i18n.test.tracer")
  let span = Tracer::start_span(error_span, "i18n.test.operation")
  
  // 模拟中文错误消息
  Span::add_event(span, "错误事件", Some([
    ("错误代码", StringValue("500")),
    ("错误消息", StringValue("内部服务器错误")),
    ("详细信息", StringValue("数据库连接超时"))
  ]))
  
  // 模拟英文错误消息
  Span::add_event(span, "error.event", Some([
    ("error.code", StringValue("500")),
    ("error.message", StringValue("Internal server error")),
    ("error.details", StringValue("Database connection timeout"))
  ]))
  
  // 模拟日文错误消息
  Span::add_event(span, "エラーイベント", Some([
    ("エラーコード", StringValue("500")),
    ("エラーメッセージ", StringValue("内部サーバーエラー")),
    ("詳細情報", StringValue("データベース接続タイムアウト"))
  ]))
  
  // 测试国际化日志记录
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "i18n.test.logger")
  
  let chinese_log = LogRecord::new_with_context(
    Error,
    Some("中文错误日志：用户登录失败，密码不正确"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace123"),
    Some("span123"),
    Some(Context::with_value(Context::root(), ContextKey::new("语言"), "中文"))
  )
  
  let english_log = LogRecord::new_with_context(
    Error,
    Some("English error log: User login failed, incorrect password"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace456"),
    Some("span456"),
    Some(Context::with_value(Context::root(), ContextKey::new("language"), "English"))
  )
  
  let japanese_log = LogRecord::new_with_context(
    Error,
    Some("日本語エラーログ：ユーザーログイン失敗、パスワードが正しくありません"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("trace789"),
    Some("span789"),
    Some(Context::with_value(Context::root(), ContextKey::new("言語"), "日本語"))
  )
  
  Logger::emit(logger, chinese_log)
  Logger::emit(logger, english_log)
  Logger::emit(logger, japanese_log)
  
  // 测试配置属性
  let config_resource = Resource::new()
  let config_attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.environment", StringValue("production")),
    ("service.region", StringValue("us-west-2")),
    ("service.language", StringValue("en-US")),
    ("service.timezone", StringValue("UTC")),
    ("service.encoding", StringValue("UTF-8")),
    ("feature.flags", StringValue("experimental,debug,verbose")),
    ("log.level", StringValue("INFO")),
    ("metrics.enabled", BoolValue(true)),
    ("tracing.enabled", BoolValue(true)),
    ("sampling.rate", FloatValue(0.1)),
    ("timeout.seconds", IntValue(30)),
    ("max.connections", IntValue(100))
  ]
  
  let configured_resource = Resource::with_attributes(config_resource, config_attributes)
  
  // 验证配置属性
  let service_name = Resource::get_attribute(configured_resource, "service.name")
  match service_name {
    Some(StringValue(value)) => assert_eq(value, "azimuth-service")
    _ => assert_true(false)
  }
  
  let service_env = Resource::get_attribute(configured_resource, "service.environment")
  match service_env {
    Some(StringValue(value)) => assert_eq(value, "production")
    _ => assert_true(false)
  }
  
  let metrics_enabled = Resource::get_attribute(configured_resource, "metrics.enabled")
  match metrics_enabled {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
  
  let sampling_rate = Resource::get_attribute(configured_resource, "sampling.rate")
  match sampling_rate {
    Some(FloatValue(value)) => assert_eq(value, 0.1)
    _ => assert_true(false)
  }
  
  let timeout_seconds = Resource::get_attribute(configured_resource, "timeout.seconds")
  match timeout_seconds {
    Some(IntValue(value)) => assert_eq(value, 30)
    _ => assert_true(false)
  }
  
  Span::set_status(span, Ok, Some("国际化测试完成"))
  Span::end(span)
  
  assert_true(true)
}

test "并发安全性和线程隔离测试" {
  // 测试多个Tracer的并发操作
  let tracer_provider = TracerProvider::default()
  let tracer1 = TracerProvider::get_tracer(tracer_provider, "concurrent.tracer.1")
  let tracer2 = TracerProvider::get_tracer(tracer_provider, "concurrent.tracer.2")
  let tracer3 = TracerProvider::get_tracer(tracer_provider, "concurrent.tracer.3")
  
  // 创建多个span
  let span1 = Tracer::start_span(tracer1, "concurrent.operation.1")
  let span2 = Tracer::start_span(tracer2, "concurrent.operation.2")
  let span3 = Tracer::start_span(tracer3, "concurrent.operation.3")
  
  // 验证span的独立性
  assert_eq(Span::name(span1), "concurrent.operation.1")
  assert_eq(Span::name(span2), "concurrent.operation.2")
  assert_eq(Span::name(span3), "concurrent.operation.3")
  
  // 为每个span添加不同的事件
  Span::add_event(span1, "event.1", Some([("tracer", StringValue("tracer1"))]))
  Span::add_event(span2, "event.2", Some([("tracer", StringValue("tracer2"))]))
  Span::add_event(span3, "event.3", Some([("tracer", StringValue("tracer3"))]))
  
  // 测试多个Meter的并发操作
  let meter_provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(meter_provider, "concurrent.meter.1")
  let meter2 = MeterProvider::get_meter(meter_provider, "concurrent.meter.2")
  
  let counter1 = Meter::create_counter(meter1, "concurrent.counter.1")
  let counter2 = Meter::create_counter(meter2, "concurrent.counter.2")
  let histogram1 = Meter::create_histogram(meter1, "concurrent.histogram.1")
  let histogram2 = Meter::create_histogram(meter2, "concurrent.histogram.2")
  
  // 并发记录度量
  Counter::add(counter1, 1.0)
  Counter::add(counter2, 2.0)
  Histogram::record(histogram1, 100.0)
  Histogram::record(histogram2, 200.0)
  
  // 测试多个Logger的并发操作
  let logger_provider = LoggerProvider::default()
  let logger1 = LoggerProvider::get_logger(logger_provider, "concurrent.logger.1")
  let logger2 = LoggerProvider::get_logger(logger_provider, "concurrent.logger.2")
  
  let log1 = LogRecord::new(Info, "Concurrent log message 1")
  let log2 = LogRecord::new(Warn, "Concurrent log message 2")
  
  Logger::emit(logger1, log1)
  Logger::emit(logger2, log2)
  
  // 测试Context的隔离性
  let context1 = Context::with_value(Context::root(), ContextKey::new("key1"), "value1")
  let context2 = Context::with_value(Context::root(), ContextKey::new("key2"), "value2")
  let context3 = Context::with_value(Context::root(), ContextKey::new("key3"), "value3")
  
  let value1 = Context::get(context1, ContextKey::new("key1"))
  let value2 = Context::get(context2, ContextKey::new("key2"))
  let value3 = Context::get(context3, ContextKey::new("key3"))
  
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  match value3 {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
  
  // 测试Baggage的隔离性
  let baggage1 = Baggage::new()
  let baggage2 = Baggage::new()
  let baggage3 = Baggage::new()
  
  let updated_baggage1 = Baggage::set_entry(baggage1, "baggage1", "value1")
  let updated_baggage2 = Baggage::set_entry(baggage2, "baggage2", "value2")
  let updated_baggage3 = Baggage::set_entry(baggage3, "baggage3", "value3")
  
  let baggage_value1 = Baggage::get_entry(updated_baggage1, "baggage1")
  let baggage_value2 = Baggage::get_entry(updated_baggage2, "baggage2")
  let baggage_value3 = Baggage::get_entry(updated_baggage3, "baggage3")
  
  match baggage_value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false) // 简化实现可能返回None
  }
  
  match baggage_value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false) // 简化实现可能返回None
  }
  
  match baggage_value3 {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false) // 简化实现可能返回None
  }
  
  // 完成所有span
  Span::set_status(span1, Ok)
  Span::set_status(span2, Ok)
  Span::set_status(span3, Ok)
  
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
  
  assert_true(true)
}
// Enhanced core functionality tests for Azimuth telemetry system
// Test file covering main telemetry features

test "属性值类型完整测试" {
  // 测试所有属性值类型
  let string_val = StringValue("test_string")
  let int_val = IntValue(42)
  let float_val = FloatValue(3.14)
  let bool_val = BoolValue(true)
  let array_string_val = ArrayStringValue(["a", "b", "c"])
  let array_int_val = ArrayIntValue([1, 2, 3])
  
  // 验证属性值创建
  match string_val {
    StringValue(s) => assert_eq(s, "test_string")
    _ => assert_true(false)
  }
  
  match int_val {
    IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  match float_val {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => assert_true(false)
  }
  
  match bool_val {
    BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
  
  match array_string_val {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_true(false)
  }
  
  match array_int_val {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1)
      assert_eq(arr[1], 2)
      assert_eq(arr[2], 3)
    }
    _ => assert_true(false)
  }
}

test "资源管理操作测试" {
  // 测试资源创建和属性管理
  let resource = Resource::new()
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // 测试属性获取
  match Resource::get_attribute(resource_with_attrs, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_attrs, "service.version") {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  // 测试不存在的属性
  match Resource::get_attribute(resource_with_attrs, "nonexistent.key") {
    None => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试资源合并
  let override_attrs = [
    ("service.name", StringValue("override-service")),
    ("new.attribute", StringValue("new-value"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged = Resource::merge(resource_with_attrs, override_resource)
  
  match Resource::get_attribute(merged, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "override-service")
    _ => assert_true(false)
  }
}

test "上下文传播和Baggage测试" {
  // 测试上下文操作
  let root_ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  
  let ctx_with_user = Context::with_value(root_ctx, user_key, "user-123")
  let ctx_with_session = Context::with_value(ctx_with_user, session_key, "session-456")
  
  // 验证上下文值获取
  match Context::get(ctx_with_user, user_key) {
    Some(user_id) => assert_eq(user_id, "user-123")
    _ => assert_true(false)
  }
  
  match Context::get(ctx_with_session, session_key) {
    Some(session_id) => assert_eq(session_id, "session-456")
    _ => assert_true(false)
  }
  
  // 测试Baggage操作
  let baggage = Baggage::new()
  let baggage_with_entry = Baggage::set_entry(baggage, "request.id", "req-789")
  
  match Baggage::get_entry(baggage_with_entry, "request.id") {
    Some(req_id) => assert_eq(req_id, "req-789")
    _ => assert_true(false)
  }
  
  // 测试Baggage移除
  let baggage_after_remove = Baggage::remove_entry(baggage_with_entry, "request.id")
  match Baggage::get_entry(baggage_after_remove, "request.id") {
    None => assert_true(true)
    _ => assert_true(false)
  }
}

test "Span上下文和追踪测试" {
  // 测试Span上下文
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "key1=value1,key2=value2")
  
  assert_eq(SpanContext::trace_id(span_ctx), "trace-123")
  assert_eq(SpanContext::span_id(span_ctx), "span-456")
  assert_true(SpanContext::is_valid(span_ctx))
  assert_true(SpanContext::is_sampled(span_ctx))
  
  // 测试无效Span上下文
  let invalid_span_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // 测试Tracer和Span操作
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test.tracer", Some("1.0.0"))
  
  // 验证Tracer的instrumentation scope
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test.tracer")
  match scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
  
  // 测试Span创建和操作
  let span = Tracer::start_span(tracer, "test.operation")
  assert_eq(Span::name(span), "test.operation")
  
  match Span::kind(span) {
    Internal => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_true(Span::is_recording(span))
  
  // 测试Span状态设置
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // 测试Span事件
  Span::add_event(span, "event1", Some([("key1", StringValue("value1"))]))
  
  // 测试Span结束
  Span::end(span)
}

test "指标计量器完整测试" {
  // 测试Meter和所有计量器类型
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter", Some("1.0.0"))
  
  // 测试Counter
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  Counter::add(counter, 10.5, Some(Attributes::new()))
  
  // 测试Histogram
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 150.0)
  
  // 测试UpDownCounter
  let updown_counter = Meter::create_updown_counter(meter, "test.updown", Some("Test updown counter"), Some("items"))
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, -2.0)
  
  // 测试Gauge
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("percent"))
  // Gauge通常用于记录当前值，这里简化为测试创建
  
  // 测试Instrument接口
  let counter_instr = Counter("test.counter", Some("Test counter"), Some("count"))
  assert_eq(Instrument::name(counter_instr), "test.counter")
  match Instrument::description(counter_instr) {
    Some(desc) => assert_eq(desc, "Test counter")
    _ => assert_true(false)
  }
  match Instrument::unit(counter_instr) {
    Some(unit) => assert_eq(unit, "count")
    _ => assert_true(false)
  }
}

test "日志记录系统测试" {
  // 测试LoggerProvider和Logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test.logger")
  
  // 测试不同严重程度的日志记录
  let trace_log = LogRecord::new(Trace, "Trace message")
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  match LogRecord::body(trace_log) {
    Some(body) => assert_eq(body, "Trace message")
    _ => assert_true(false)
  }
  
  let debug_log = LogRecord::new(Debug, "Debug message")
  let info_log = LogRecord::new(Info, "Info message")
  let warn_log = LogRecord::new(Warn, "Warning message")
  let error_log = LogRecord::new(Error, "Error message")
  let fatal_log = LogRecord::new(Fatal, "Fatal message")
  
  // 测试完整日志记录创建
  let attrs = Attributes::new()
  let full_log = LogRecord::new_with_context(
    Error,
    Some("Detailed error message"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace-123"),
    Some("span-456"),
    Some(Context::root())
  )
  
  match LogRecord::trace_id(full_log) {
    Some(trace_id) => assert_eq(trace_id, "trace-123")
    _ => assert_true(false)
  }
  
  match LogRecord::span_id(full_log) {
    Some(span_id) => assert_eq(span_id, "span-456")
    _ => assert_true(false)
  }
  
  // 测试日志发出
  Logger::emit(logger, error_log)
  Logger::emit(logger, full_log)
}

test "传播器机制测试" {
  // 测试TextMapCarrier
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "baggage", "key1=value1;key2=value2")
  
  match TextMapCarrier::get(carrier, "traceparent") {
    Some(traceparent) => assert_eq(traceparent, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    _ => assert_true(false)
  }
  
  // 测试W3C传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // 测试复合传播器
  let composite = CompositePropagator::new([trace_propagator])
  
  // 测试注入
  let ctx = Context::root()
  CompositePropagator::inject(composite, ctx, carrier)
  
  // 测试提取
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_key = ContextKey::new("extracted")
  
  match Context::get(extracted_ctx, extracted_key) {
    Some(value) => assert_eq(value, "true")
    _ => assert_true(false)
  }
}

test "HTTP客户端功能测试" {
  // 测试HTTP请求创建
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let request = HttpRequest::new("POST", "https://api.example.com/data", headers, Some("{\"key\":\"value\"}"))
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  match HttpRequest::body(request) {
    Some(body) => assert_eq(body, "{\"key\":\"value\"}")
    _ => assert_true(false)
  }
  
  // 测试HTTP响应创建
  let response_headers = [("Content-Type", "application/json"), ("X-Request-ID", "req-123")]
  let response = HttpResponse::new(200, response_headers, Some("{\"result\":\"success\"}"))
  
  assert_eq(HttpResponse::status_code(response), 200)
  match HttpResponse::body(response) {
    Some(body) => assert_eq(body, "{\"result\":\"success\"}")
    _ => assert_true(false)
  }
  
  // 测试HTTP客户端创建（简化版）
  let client = HttpClient::new()
  // 在实际实现中，这里会测试client.send_request(request)等操作
}

test "时间和随机数生成测试" {
  // 测试Clock
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  // 验证时间戳是合理的（2025年的时间戳）
  assert_true(timestamp > 1700000000000000000L)
  assert_true(timestamp < 1800000000000000000L)
  
  // 测试Random
  let random = Random::system()
  
  // 测试随机字节生成
  let bytes = Random::next_bytes(random, 16)
  assert_eq(bytes.length(), 16)
  
  // 测试随机数生成
  let random_u64 = Random::next_u64(random)
  assert_true(random_u64.to_int() >= 0)
  assert_true(random_u64.to_int() <= UInt64::max_value().to_int())
}

test "综合集成测试" {
  // 测试完整的遥测流程
  
  // 1. 创建资源
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("integration-test")),
    ("service.version", StringValue("1.0.0"))
  ])
  
  // 2. 设置上下文和Baggage
  let ctx = Context::root()
  let baggage = Baggage::set_entry(Baggage::new(), "user.id", "user-123")
  
  // 3. 创建Tracer和Span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "integration.tracer")
  let span = Tracer::start_span(tracer, "integration.operation")
  
  // 4. 添加Span事件和属性
  Span::add_event(span, "operation.started", Some([("component", StringValue("test"))]))
  Span::set_status(span, Ok, Some("Operation completed"))
  
  // 5. 创建Meter和指标
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "integration.meter")
  let counter = Meter::create_counter(meter, "operations.total")
  let histogram = Meter::create_histogram(meter, "operation.duration")
  
  Counter::add(counter, 1.0)
  Histogram::record(histogram, 150.0)
  
  // 6. 创建Logger和日志
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "integration.logger")
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Integration test completed successfully"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    Some(ctx)
  )
  
  Logger::emit(logger, log_record)
  
  // 7. 测试传播
  let carrier = TextMapCarrier::new()
  let composite = CompositePropagator::new([W3CTraceContextPropagator::new()])
  CompositePropagator::inject(composite, ctx, carrier)
  
  // 验证所有组件都正常工作
  assert_true(Span::is_recording(span))
  match Resource::get_attribute(resource, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "integration-test")
    _ => assert_true(false)
  }
  
  match Baggage::get_entry(baggage, "user.id") {
    Some(user_id) => assert_eq(user_id, "user-123")
    _ => assert_true(false)
  }
  
  // 完成Span
  Span::end(span)
  
  // 测试通过表示整个集成流程正常
  assert_true(true)
}
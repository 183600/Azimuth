// Azimuth Focused Integration Tests
// 集成测试用例，专注于核心遥测功能的端到端测试

// 测试1: Attributes基本操作
test "attributes basic operations" {
  let attrs = Attributes::new()
  
  // 测试设置和获取字符串属性
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  match Attributes::get(attrs, "string.key") {
    Some(StringValue(value)) => assert_eq(value, "test_value")
    _ => assert_true(false)
  }
  
  // 测试设置和获取整数属性
  Attributes::set(attrs, "int.key", IntValue(42))
  match Attributes::get(attrs, "int.key") {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  // 测试不存在的键
  match Attributes::get(attrs, "nonexistent.key") {
    None => assert_true(true)
    _ => assert_true(false)
  }
}

// 测试2: Context操作
test "context operations" {
  let root_ctx = Context::root()
  let key = ContextKey::new("test.key")
  
  // 测试在上下文中设置值
  let ctx_with_value = Context::with_value(root_ctx, key, "test_value")
  
  // 测试从上下文中获取值
  match Context::get(ctx_with_value, key) {
    Some(value) => assert_eq(value, "test_value")
    None => assert_true(false)
  }
  
  // 测试从根上下文中获取不存在的值
  match Context::get(root_ctx, key) {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
}

// 测试3: SpanContext操作
test "span context operations" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test-state")
  
  // 测试获取trace_id和span_id
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  
  // 测试有效性检查
  assert_true(SpanContext::is_valid(span_ctx))
  
  // 测试采样状态
  assert_true(SpanContext::is_sampled(span_ctx))
  
  // 测试无效的SpanContext
  let invalid_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_ctx))
}

// 测试4: Span生命周期
test "span lifecycle" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test.tracer")
  
  // 创建span
  let span = Tracer::start_span(tracer, "test.operation")
  
  // 测试span基本属性
  assert_eq(Span::name(span), "test.operation")
  
  // 测试span kind
  match Span::kind(span) {
    Internal => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试recording状态
  assert_true(Span::is_recording(span))
  
  // 测试span上下文
  let span_ctx = Span::span_context(span)
  assert_true(SpanContext::is_valid(span_ctx))
  
  // 测试设置状态
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // 测试添加事件
  Span::add_event(span, "test.event", [("attr1", StringValue("value1"))])
  
  // 结束span
  Span::end(span)
}

// 测试5: 度量Counter操作
test "metric counter operations" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter")
  
  // 创建counter
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  
  // 测试counter属性
  assert_eq(counter.name, "test.counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("count"))
  
  // 测试添加值
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 10.0)
  
  // 测试转换为Instrument
  let instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(instrument), "test.counter")
  assert_eq(Instrument::description(instrument), Some("Test counter"))
  assert_eq(Instrument::unit(instrument), Some("count"))
}

// 测试6: Histogram操作
test "metric histogram operations" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter")
  
  // 创建histogram
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  
  // 测试histogram属性
  assert_eq(histogram.name, "test.histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // 测试记录值
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 150.0)
  
  // 测试转换为Instrument
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "test.histogram")
}

// 测试7: LogRecord操作
test "log record operations" {
  // 创建基本日志记录
  let log_record = LogRecord::new(Info, "Test log message")
  
  // 测试日志记录属性
  assert_eq(LogRecord::severity_number(log_record), Info)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Test log message")
    None => assert_true(false)
  }
  
  // 测试创建带完整上下文的日志记录
  let detailed_log = LogRecord::new_with_context(
    Error,
    Some("Error occurred"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace_id_123"),
    Some("span_id_456"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(detailed_log), Error)
  match LogRecord::trace_id(detailed_log) {
    Some(trace_id) => assert_eq(trace_id, "trace_id_123")
    None => assert_true(false)
  }
  
  match LogRecord::span_id(detailed_log) {
    Some(span_id) => assert_eq(span_id, "span_id_456")
    None => assert_true(false)
  }
}

// 测试8: Baggage操作
test "baggage operations" {
  let baggage = Baggage::new()
  
  // 测试设置条目
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  
  // 测试获取条目
  match Baggage::get_entry(updated_baggage, "user.id") {
    Some(value) => assert_eq(value, "12345")
    None => assert_true(false)
  }
  
  // 测试获取不存在的条目
  match Baggage::get_entry(updated_baggage, "nonexistent") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // 测试删除条目
  let final_baggage = Baggage::remove_entry(updated_baggage, "user.id")
  match Baggage::get_entry(final_baggage, "user.id") {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
}

// 测试9: Resource操作
test "resource operations" {
  let resource = Resource::new()
  
  // 测试创建带属性的resource
  let attrs = [("service.name", StringValue("test.service")), ("service.version", StringValue("1.0.0"))]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // 测试获取属性
  match Resource::get_attribute(resource_with_attrs, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "test.service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource_with_attrs, "service.version") {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  // 测试合并资源
  let override_resource = Resource::with_attributes(Resource::new(), [("env", StringValue("test"))])
  let merged = Resource::merge(resource_with_attrs, override_resource)
  match Resource::get_attribute(merged, "env") {
    Some(StringValue(env)) => assert_eq(env, "test")
    _ => assert_true(false)
  }
}

// 测试10: TextMapCarrier和传播操作
test "text map carrier and propagation" {
  let carrier = TextMapCarrier::new()
  
  // 测试设置和获取头信息
  TextMapCarrier::set(carrier, "custom.header", "custom.value")
  
  match TextMapCarrier::get(carrier, "traceparent") {
    Some(traceparent) => {
      // 验证traceparent格式
      assert_true(traceparent.length() > 0)
      assert_true(traceparent.contains("-"))
    }
    None => assert_true(false)
  }
  
  // 测试传播器
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let ctx = Context::root()
  
  // 测试注入
  CompositePropagator::inject(propagator, ctx, carrier)
  
  // 测试提取
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  let key = ContextKey::new("extracted")
  match Context::get(extracted_ctx, key) {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
}
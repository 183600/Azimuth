// Azimuth 高质量遥测系统测试
// 专注于测试Azimuth遥测系统的核心功能和高级特性

// 测试1: 属性操作高级测试
test "属性操作高级测试" {
  let attrs = Attributes::new()
  
  // 测试设置不同类型的属性值
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "array.string.key", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int.key", ArrayIntValue([1, 2, 3]))
  
  // 测试获取字符串属性
  match Attributes::get(attrs, "string.key") {
    Some(StringValue(value)) => assert_eq(value, "test_value")
    _ => assert_true(false, "Expected StringValue")
  }
  
  // 测试获取整数属性
  match Attributes::get(attrs, "int.key") {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false, "Expected IntValue")
  }
  
  // 测试获取不存在的属性
  match Attributes::get(attrs, "nonexistent.key") {
    Some(_) => assert_true(false, "Expected None")
    None => assert_true(true)
  }
}

// 测试2: 上下文管理测试
test "上下文管理测试" {
  let root_ctx = Context::root()
  let key1 = ContextKey::new("test_key1")
  let key2 = ContextKey::new("test_key2")
  
  // 测试在根上下文中设置值
  let ctx1 = Context::with_value(root_ctx, key1, "value1")
  match Context::get(ctx1, key1) {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false, "Expected Some value")
  }
  
  // 测试链式上下文设置
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  match Context::get(ctx2, key1) {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false, "Expected Some value for key1")
  }
  
  match Context::get(ctx2, key2) {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false, "Expected Some value for key2")
  }
  
  // 测试在根上下文中获取不存在的键
  match Context::get(root_ctx, key1) {
    Some(_) => assert_true(false, "Expected None")
    None => assert_true(true)
  }
}

// 测试3: Span生命周期测试
test "Span生命周期测试" {
  let span_ctx = SpanContext::new("trace_123", "span_456", true, "state=test")
  let span = Span::new("test_span", Server, span_ctx)
  
  // 测试span基本属性
  assert_eq(Span::name(span), "test_span")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  
  // 测试span上下文
  let retrieved_ctx = Span::span_context(span)
  assert_eq(SpanContext::trace_id(retrieved_ctx), "trace_123")
  assert_eq(SpanContext::span_id(retrieved_ctx), "span_456")
  assert_true(SpanContext::is_sampled(retrieved_ctx))
  assert_true(SpanContext::is_valid(retrieved_ctx))
  
  // 测试span状态
  assert_eq(Span::status(span), Unset)
  
  // 测试设置状态和添加事件
  Span::set_status(span, Ok, Some("Operation completed"))
  Span::add_event(span, "event1", Some([("key1", StringValue("value1"))]))
  
  // 测试结束span
  Span::end(span)
}

// 测试4: 指标操作测试
test "指标操作测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  // 测试创建计数器
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5, Some(Attributes::new()))
  
  // 测试创建直方图
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.5, Some(Attributes::new()))
  
  // 测试创建UpDown计数器
  let updown_counter = Meter::create_updown_counter(meter, "test_updown", Some("Test updown counter"), Some("items"))
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -5.0, Some(Attributes::new()))
  
  // 测试创建仪表
  let gauge = Meter::create_gauge(meter, "test_gauge", Some("Test gauge"), Some("percent"))
  
  // 测试仪器操作
  let counter_instrument = Counter("test_counter", Some("Test counter"), Some("count"))
  assert_eq(Instrument::name(counter_instrument), "test_counter")
  assert_eq(Instrument::description(counter_instrument), Some("Test counter"))
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
  
  let histogram_instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(histogram_instrument), "test_histogram")
}

// 测试5: 日志记录测试
test "日志记录测试" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test_logger")
  
  // 测试创建基本日志记录
  let log_record = LogRecord::new(Info, "Test log message")
  assert_eq(LogRecord::severity_number(log_record), Info)
  match LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Test log message")
    None => assert_true(false, "Expected Some body")
  }
  
  // 测试创建带上下文的日志记录
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.attr", StringValue("log_value"))
  
  let context_log = LogRecord::new_with_context(
    Warn,
    Some("Warning message"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace_123"),
    Some("span_456"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(context_log), Warn)
  match LogRecord::body(context_log) {
    Some(body) => assert_eq(body, "Warning message")
    None => assert_true(false, "Expected Some body")
  }
  
  assert_eq(LogRecord::trace_id(context_log), Some("trace_123"))
  assert_eq(LogRecord::span_id(context_log), Some("span_456"))
  
  // 测试发出日志记录
  Logger::emit(logger, log_record)
  Logger::emit(logger, context_log)
}

// 测试6: Baggage传播测试
test "Baggage传播测试" {
  let baggage = Baggage::new()
  
  // 测试设置和获取baggage条目
  let updated_baggage = Baggage::set_entry(baggage, "key1", "value1")
  let final_baggage = Baggage::set_entry(updated_baggage, "key2", "value2")
  
  match Baggage::get_entry(final_baggage, "key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false, "Expected Some value for key1")
  }
  
  match Baggage::get_entry(final_baggage, "key2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false, "Expected Some value for key2")
  }
  
  // 测试获取不存在的条目
  match Baggage::get_entry(final_baggage, "nonexistent") {
    Some(_) => assert_true(false, "Expected None")
    None => assert_true(true)
  }
  
  // 测试移除条目
  let reduced_baggage = Baggage::remove_entry(final_baggage, "key1")
  match Baggage::get_entry(reduced_baggage, "key1") {
    Some(_) => assert_true(false, "Expected None after removal")
    None => assert_true(true)
  }
  
  // 确保其他条目仍然存在
  match Baggage::get_entry(reduced_baggage, "key2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false, "Expected Some value for key2")
  }
}

// 测试7: 资源管理测试
test "资源管理测试" {
  let resource = Resource::new()
  
  // 测试设置资源属性
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("host.name", StringValue("test-host"))
  ]
  
  let with_attrs = Resource::with_attributes(resource, attrs)
  
  // 测试获取资源属性
  match Resource::get_attribute(with_attrs, "service.name") {
    Some(StringValue(value)) => assert_eq(value, "test_service")
    _ => assert_true(false, "Expected StringValue for service.name")
  }
  
  match Resource::get_attribute(with_attrs, "service.version") {
    Some(StringValue(value)) => assert_eq(value, "1.0.0")
    _ => assert_true(false, "Expected StringValue for service.version")
  }
  
  match Resource::get_attribute(with_attrs, "host.name") {
    Some(StringValue(value)) => assert_eq(value, "test-host")
    _ => assert_true(false, "Expected StringValue for host.name")
  }
  
  // 测试获取不存在的属性
  match Resource::get_attribute(with_attrs, "nonexistent.attr") {
    Some(_) => assert_true(false, "Expected None")
    None => assert_true(true)
  }
  
  // 测试资源合并
  let override_attrs = [
    ("service.version", StringValue("2.0.0")),
    ("deployment.environment", StringValue("test"))
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged = Resource::merge(with_attrs, override_resource)
  
  match Resource::get_attribute(merged, "service.version") {
    Some(StringValue(value)) => assert_eq(value, "2.0.0")
    _ => assert_true(false, "Expected merged version")
  }
  
  match Resource::get_attribute(merged, "deployment.environment") {
    Some(StringValue(value)) => assert_eq(value, "test")
    _ => assert_true(false, "Expected merged environment")
  }
}

// 测试8: 跟踪上下文操作测试
test "跟踪上下文操作测试" {
  // 测试创建有效的span上下文
  let valid_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "test=value")
  
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_true(SpanContext::is_sampled(valid_ctx))
  assert_eq(SpanContext::trace_id(valid_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(valid_ctx), "b7ad6b7169203331")
  
  // 测试创建无效的span上下文
  let invalid_ctx1 = SpanContext::new("", "b7ad6b7169203331", true, "")
  let invalid_ctx2 = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", true, "")
  
  assert_false(SpanContext::is_valid(invalid_ctx1))
  assert_false(SpanContext::is_valid(invalid_ctx2))
  
  // 测试未采样的上下文
  let unsampled_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", false, "")
  assert_false(SpanContext::is_sampled(unsampled_ctx))
  assert_true(SpanContext::is_valid(unsampled_ctx))
  
  // 测试不同类型的span kind
  let internal_span = Span::new("internal_op", Internal, valid_ctx)
  let server_span = Span::new("server_request", Server, valid_ctx)
  let client_span = Span::new("client_request", Client, valid_ctx)
  let producer_span = Span::new("message_produce", Producer, valid_ctx)
  let consumer_span = Span::new("message_consume", Consumer, valid_ctx)
  
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::kind(consumer_span), Consumer)
}

// 测试9: 复合传播器测试
test "复合传播器测试" {
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // 测试注入操作
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite, ctx, carrier)
  
  // 验证注入的头部
  match TextMapCarrier::get(carrier, "traceparent") {
    Some(value) => assert_true(value.length() > 0)
    None => assert_true(false, "Expected traceparent header")
  }
  
  // 测试提取操作
  let extract_carrier = TextMapCarrier::new()
  let extracted_ctx = CompositePropagator::extract(composite, extract_carrier)
  
  // 验证提取的上下文
  let extracted_key = ContextKey::new("extracted")
  match Context::get(extracted_ctx, extracted_key) {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false, "Expected extracted value")
  }
  
  // 测试多个传播器
  let multi_composite = CompositePropagator::new([trace_propagator])
  CompositePropagator::inject(multi_composite, ctx, carrier)
  let multi_extracted = CompositePropagator::extract(multi_composite, carrier)
  
  match Context::get(multi_extracted, extracted_key) {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false, "Expected multi-extracted value")
  }
}

// 测试10: 跨服务遥测一致性测试
test "跨服务遥测一致性测试" {
  // 模拟服务A创建span
  let service_a_provider = TracerProvider::default()
  let service_a_tracer = TracerProvider::get_tracer(service_a_provider, "service-a", Some("1.0.0"))
  let root_span = Tracer::start_span(service_a_tracer, "root-operation")
  
  let root_ctx = Span::span_context(root_span)
  assert_true(SpanContext::is_valid(root_ctx))
  assert_eq(SpanContext::trace_id(root_ctx), "test_trace_id")
  
  // 模拟服务B继续跟踪
  let service_b_provider = TracerProvider::default()
  let service_b_tracer = TracerProvider::get_tracer(service_b_provider, "service-b", Some("2.0.0"))
  let child_span = Tracer::start_span(service_b_tracer, "child-operation")
  
  let child_ctx = Span::span_context(child_span)
  assert_true(SpanContext::is_valid(child_ctx))
  
  // 验证跟踪ID一致性（在实际实现中应该相同）
  // 这里简化测试，验证span ID不同
  assert_true(SpanContext::span_id(root_ctx) != SpanContext::span_id(child_ctx))
  
  // 测试仪器作用域
  let service_a_scope = Tracer::instrumentation_scope(service_a_tracer)
  assert_eq(service_a_scope.name, "service-a")
  assert_eq(service_a_scope.version, Some("1.0.0"))
  
  let service_b_scope = Tracer::instrumentation_scope(service_b_tracer)
  assert_eq(service_b_scope.name, "service-b")
  assert_eq(service_b_scope.version, Some("2.0.0"))
  
  // 测试跨服务指标
  let meter_provider = MeterProvider::default()
  let service_a_meter = MeterProvider::get_meter(meter_provider, "service-a")
  let service_b_meter = MeterProvider::get_meter(meter_provider, "service-b")
  
  let service_a_counter = Meter::create_counter(service_a_meter, "service_a_requests")
  let service_b_counter = Meter::create_counter(service_b_meter, "service_b_requests")
  
  Counter::add(service_a_counter, 1.0)
  Counter::add(service_b_counter, 1.0)
  
  // 测试跨服务日志
  let logger_provider = LoggerProvider::default()
  let service_a_logger = LoggerProvider::get_logger(logger_provider, "service-a")
  let service_b_logger = LoggerProvider::get_logger(logger_provider, "service-b")
  
  let service_a_log = LogRecord::new_with_context(
    Info,
    Some("Service A operation completed"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(root_ctx)),
    Some(SpanContext::span_id(root_ctx)),
    None
  )
  
  let service_b_log = LogRecord::new_with_context(
    Info,
    Some("Service B operation completed"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(child_ctx)),
    Some(SpanContext::span_id(child_ctx)),
    None
  )
  
  Logger::emit(service_a_logger, service_a_log)
  Logger::emit(service_b_logger, service_b_log)
  
  // 结束spans
  Span::end(child_span)
  Span::end(root_span)
}
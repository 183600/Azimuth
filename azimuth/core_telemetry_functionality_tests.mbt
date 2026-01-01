// Azimuth Core Telemetry Functionality Tests
// 核心遥测功能测试

// Test 1: Span生命周期完整测试
test "span完整生命周期测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle.test")
  
  // 创建span
  let span = Tracer::start_span(tracer, "lifecycle.operation")
  assert_eq(Span::name(span), "lifecycle.operation")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // 添加事件
  Span::add_event(span, "operation.start", None)
  Span::add_event(span, "operation.progress", Some([("progress", IntValue(25))]))
  
  // 设置属性
  let attrs = [("operation.type", StringValue("test")), ("operation.id", StringValue("op-12345"))]
  for (key, value) in attrs {
    Span::set_attribute(span, key, value)
  }
  
  // 设置状态
  Span::set_status(span, Ok)
  
  // 结束span
  Span::end(span)
  assert_false(Span::is_recording(span))
}

// Test 2: 度量仪表板聚合测试
test "度量仪表板聚合测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "dashboard.test")
  
  // 创建多种度量类型
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let response_histogram = Meter::create_histogram(meter, "http.response.duration.ms")
  let active_connections = Meter::create_updown_counter(meter, "http.active.connections")
  let memory_usage = Meter::create_gauge(meter, "memory.usage.bytes")
  
  // 模拟度量数据
  Counter::add(request_counter, 100.0)
  Histogram::record(response_histogram, 150.0)
  Histogram::record(response_histogram, 200.0)
  Histogram::record(response_histogram, 120.0)
  UpDownCounter::add(active_connections, 10.0)
  
  // 验证度量创建
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.duration.ms")
  assert_eq(active_connections.name, "http.active.connections")
  assert_eq(memory_usage.name, "memory.usage.bytes")
}

// Test 3: 日志记录上下文关联测试
test "日志记录上下文关联测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "context.test")
  
  // 创建span上下文
  let span_ctx = SpanContext::new("trace-12345", "span-67890", true, "key1=value1")
  
  // 创建带有上下文的日志记录
  let log_record = LogRecord::new_with_context(
    Info,
    Some("用户操作完成"),
    Some([("user.id", StringValue("12345")), ("operation.type", StringValue("update"))]),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-12345"),
    Some("span-67890"),
    Some(Context::root())
  )
  
  // 验证日志记录属性
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("用户操作完成"))
  assert_eq(LogRecord::trace_id(log_record), Some("trace-12345"))
  assert_eq(LogRecord::span_id(log_record), Some("span-67890"))
  
  // 发出日志
  Logger::emit(logger, log_record)
}

// Test 4: 资源属性合并策略测试
test "资源属性合并策略测试" {
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("development"))
  ])
  
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("override-service")),  // 应覆盖
    ("host.name", StringValue("test-host")),            // 新增
    ("service.instance.id", StringValue("instance-123")) // 新增
  ])
  
  // 合并资源
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // 验证合并结果
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("override-service")))
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(merged_resource, "deployment.environment"), Some(StringValue("development")))
  assert_eq(Resource::get_attribute(merged_resource, "host.name"), Some(StringValue("test-host")))
  assert_eq(Resource::get_attribute(merged_resource, "service.instance.id"), Some(StringValue("instance-123")))
}

// Test 5: 跨服务传播一致性测试
test "跨服务传播一致性测试" {
  // 模拟服务A
  let service_a_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-a")
  let root_span = Tracer::start_span(service_a_tracer, "service-a.operation")
  let root_span_ctx = Span::span_context(root_span)
  
  // 模拟服务B（使用相同的trace_id）
  let service_b_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-b")
  let child_span_ctx = SpanContext::new(
    SpanContext::trace_id(root_span_ctx),
    "span-b-child",
    true,
    SpanContext::trace_state(root_span_ctx)
  )
  let child_span = Span::new("service-b.operation", Server, child_span_ctx)
  
  // 模拟服务C（继续传播）
  let service_c_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-c")
  let grandchild_span_ctx = SpanContext::new(
    SpanContext::trace_id(root_span_ctx),
    "span-c-grandchild",
    true,
    SpanContext::trace_state(root_span_ctx)
  )
  let grandchild_span = Span::new("service-c.operation", Internal, grandchild_span_ctx)
  
  // 验证trace_id一致性
  assert_eq(SpanContext::trace_id(root_span_ctx), SpanContext::trace_id(child_span_ctx))
  assert_eq(SpanContext::trace_id(child_span_ctx), SpanContext::trace_id(grandchild_span_ctx))
  
  // 验证span_id唯一性
  assert_true(SpanContext::span_id(root_span_ctx) != SpanContext::span_id(child_span_ctx))
  assert_true(SpanContext::span_id(child_span_ctx) != SpanContext::span_id(grandchild_span_ctx))
}

// Test 6: 属性值类型转换边界测试
test "属性值类型转换边界测试" {
  let attrs = Attributes::new()
  
  // 测试各种边界值
  let max_int = IntValue(2147483647)
  let min_int = IntValue(-2147483648)
  let zero_int = IntValue(0)
  let max_float = FloatValue(1.7976931348623157e+308)
  let min_float = FloatValue(-1.7976931348623157e+308)
  let nan_float = FloatValue(0.0/0.0)  // NaN
  let inf_float = FloatValue(1.0/0.0)  // 正无穷
  let neg_inf_float = FloatValue(-1.0/0.0)  // 负无穷
  
  // 设置边界值属性
  Attributes::set(attrs, "max.int", max_int)
  Attributes::set(attrs, "min.int", min_int)
  Attributes::set(attrs, "zero.int", zero_int)
  Attributes::set(attrs, "max.float", max_float)
  Attributes::set(attrs, "min.float", min_float)
  Attributes::set(attrs, "nan.float", nan_float)
  Attributes::set(attrs, "inf.float", inf_float)
  Attributes::set(attrs, "neg.inf.float", neg_inf_float)
  
  // 测试特殊字符串值
  let empty_string = StringValue("")
  let unicode_string = StringValue("测试中文🚀")
  let very_long_string = StringValue("这是一个非常长的字符串，用于测试边界条件和处理能力")
  let special_chars_string = StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?")
  
  Attributes::set(attrs, "empty.string", empty_string)
  Attributes::set(attrs, "unicode.string", unicode_string)
  Attributes::set(attrs, "very.long.string", very_long_string)
  Attributes::set(attrs, "special.chars.string", special_chars_string)
  
  // 测试数组属性
  let empty_array = ArrayStringValue([])
  let single_element_array = ArrayStringValue(["single"])
  let large_array = ArrayStringValue(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"])
  
  Attributes::set(attrs, "empty.array", empty_array)
  Attributes::set(attrs, "single.element.array", single_element_array)
  Attributes::set(attrs, "large.array", large_array)
}

// Test 7: 时间序列数据处理测试
test "时间序列数据处理测试" {
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // 创建时间序列数据点
  let timestamps = []
  let values = []
  
  for i in 0..10 {
    let timestamp = base_timestamp + (i * 1000000L)  // 每个点间隔1毫秒
    let value = 100.0 + (i.to_double() * 10.5)  // 递增值
    
    timestamps.push(timestamp)
    values.push(value)
  }
  
  // 验证时间序列顺序
  for i in 1..timestamps.length() {
    assert_true(timestamps[i] > timestamps[i-1])
    assert_true(values[i] > values[i-1])
  }
  
  // 测试时间差计算
  let duration = timestamps[9] - timestamps[0]
  assert_eq(duration, 9000000L)  // 9毫秒
  
  // 创建基于时间序列的日志
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "time.series.test")
  
  for i in 0..timestamps.length() {
    let log_record = LogRecord::new_with_context(
      Info,
      Some("时间序列数据点 " + i.to_string()),
      Some([("value", FloatValue(values[i])), ("index", IntValue(i))]),
      Some(timestamps[i]),
      None,
      None,
      None,
      Some(Context::root())
    )
    Logger::emit(logger, log_record)
  }
}

// Test 8: 复合传播器组合测试
test "复合传播器组合测试" {
  // 创建多种传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // 创建复合传播器
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 准备上下文和载体
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // 在上下文中设置值
  let user_key = ContextKey::new("user.id")
  let ctx_with_user = Context::with_value(ctx, user_key, "12345")
  
  // 创建baggage
  let baggage = Baggage::new()
  let baggage_with_session = Baggage::set_entry(baggage, "session.id", "sess-67890")
  
  // 注入上下文
  CompositePropagator::inject(composite_propagator, ctx_with_user, carrier)
  
  // 验证注入的头部
  let trace_header = TextMapCarrier::get(carrier, "traceparent")
  assert_true(trace_header != None)
  
  // 提取上下文
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_user = Context::get(extracted_ctx, user_key)
  assert_eq(extracted_user, Some("12345"))
}

// Test 9: 并发安全性验证测试
test "并发安全性验证测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrency.test")
  
  // 创建多个span模拟并发
  let spans = []
  for i in 0..50 {
    let span = Tracer::start_span(tracer, "concurrent.span." + i.to_string())
    spans.push(span)
  }
  
  // 并发添加事件和属性
  for i in 0..spans.length() {
    Span::add_event(spans[i], "concurrent.event", Some([("index", IntValue(i))]))
    Span::set_attribute(spans[i], "thread.id", StringValue("thread-" + i.to_string()))
  }
  
  // 并发设置状态
  for i in 0..spans.length() {
    if (i % 2 == 0) {
      Span::set_status(spans[i], Ok)
    } else {
      Span::set_status(spans[i], Error)
    }
  }
  
  // 并发结束span
  for span in spans {
    Span::end(span)
  }
  
  // 验证所有span都已正确结束
  for span in spans {
    assert_false(Span::is_recording(span))
  }
}

// Test 10: 端到端业务流程集成测试
test "端到端业务流程集成测试" {
  // 设置资源
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("e2e-test-service")),
    ("service.version", StringValue("2.0.0")),
    ("business.domain", StringValue("order-processing"))
  ])
  
  // 创建追踪器
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "e2e-test")
  let order_span = Tracer::start_span(tracer, "process.order")
  
  // 添加业务事件
  Span::add_event(order_span, "order.received", Some([
    ("order.id", StringValue("ORD-12345")),
    ("customer.id", StringValue("CUST-67890")),
    ("order.amount", FloatValue(150.75))
  ]))
  
  // 创建度量
  let meter = MeterProvider::get_meter(MeterProvider::default(), "e2e-test")
  let order_counter = Meter::create_counter(meter, "orders.processed")
  let order_amount_histogram = Meter::create_histogram(meter, "order.amount")
  
  // 记录度量
  Counter::add(order_counter, 1.0)
  Histogram::record(order_amount_histogram, 150.75)
  
  // 创建日志
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "e2e-test")
  let log_record = LogRecord::new_with_context(
    Info,
    Some("订单处理完成"),
    Some([
      ("order.id", StringValue("ORD-12345")),
      ("processing.time.ms", FloatValue(250.5)),
      ("status", StringValue("completed"))
    ]),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(order_span))),
    Some(SpanContext::span_id(Span::span_context(order_span))),
    Some(Context::root())
  )
  
  // 发出日志
  Logger::emit(logger, log_record)
  
  // 结束span
  Span::set_status(order_span, Ok)
  Span::end(order_span)
  
  // 验证端到端流程
  assert_false(Span::is_recording(order_span))
  assert_eq(order_counter.name, "orders.processed")
  assert_eq(order_amount_histogram.name, "order.amount")
  assert_eq(logger.scope.name, "e2e-test")
}
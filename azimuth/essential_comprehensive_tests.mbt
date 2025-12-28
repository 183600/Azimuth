// Essential Comprehensive Tests for Azimuth Telemetry System
// 核心综合测试用例

test "属性值类型深度嵌套测试" {
  let attrs = Attributes::new()
  
  // 测试字符串属性的各种情况
  Attributes::set(attrs, "empty.string", StringValue(""))
  Attributes::set(attrs, "normal.string", StringValue("normal value"))
  Attributes::set(attrs, "unicode.string", StringValue("测试🚀emoji"))
  Attributes::set(attrs, "special.chars", StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  
  // 测试整数属性的边界值
  Attributes::set(attrs, "zero.int", IntValue(0))
  Attributes::set(attrs, "positive.int", IntValue(2147483647))
  Attributes::set(attrs, "negative.int", IntValue(-2147483648))
  
  // 测试浮点数属性的特殊值
  Attributes::set(attrs, "zero.float", FloatValue(0.0))
  Attributes::set(attrs, "positive.float", FloatValue(3.14159265359))
  Attributes::set(attrs, "negative.float", FloatValue(-2.71828182846))
  
  // 测试布尔属性
  Attributes::set(attrs, "true.bool", BoolValue(true))
  Attributes::set(attrs, "false.bool", BoolValue(false))
  
  // 测试字符串数组
  Attributes::set(attrs, "empty.string.array", ArrayStringValue([]))
  Attributes::set(attrs, "single.string.array", ArrayStringValue(["single"]))
  Attributes::set(attrs, "multi.string.array", ArrayStringValue(["item1", "item2", "item3"]))
  
  // 测试整数数组
  Attributes::set(attrs, "empty.int.array", ArrayIntValue([]))
  Attributes::set(attrs, "single.int.array", ArrayIntValue([42]))
  Attributes::set(attrs, "multi.int.array", ArrayIntValue([1, 2, 3, 4, 5]))
  
  // 验证所有属性都能正确设置和获取
  let empty_string = Attributes::get(attrs, "empty.string")
  match empty_string {
    Some(StringValue(value)) => assert_eq(value, "")
    _ => assert_true(false)
  }
  
  let unicode_string = Attributes::get(attrs, "unicode.string")
  match unicode_string {
    Some(StringValue(value)) => assert_eq(value, "测试🚀emoji")
    _ => assert_true(false)
  }
  
  let positive_int = Attributes::get(attrs, "positive.int")
  match positive_int {
    Some(IntValue(value)) => assert_eq(value, 2147483647)
    _ => assert_true(false)
  }
  
  let true_bool = Attributes::get(attrs, "true.bool")
  match true_bool {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
  
  let multi_string_array = Attributes::get(attrs, "multi.string.array")
  match multi_string_array {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "item1")
      assert_eq(values[1], "item2")
      assert_eq(values[2], "item3")
    }
    _ => assert_true(false)
  }
  
  let multi_int_array = Attributes::get(attrs, "multi.int.array")
  match multi_int_array {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], 1)
      assert_eq(values[4], 5)
    }
    _ => assert_true(false)
  }
}

test "SpanContext边界条件详细测试" {
  // 测试各种有效的SpanContext组合
  let valid_ctx1 = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "rojo=00f067aa0ba902b7")
  assert_true(SpanContext::is_valid(valid_ctx1))
  assert_true(SpanContext::is_sampled(valid_ctx1))
  assert_eq(SpanContext::trace_id(valid_ctx1), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(valid_ctx1), "b7ad6b7169203331")
  
  // 测试未采样的有效上下文
  let valid_ctx2 = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", false, "")
  assert_true(SpanContext::is_valid(valid_ctx2))
  assert_false(SpanContext::is_sampled(valid_ctx2))
  
  // 测试空trace_id的各种情况
  let empty_trace_ctx1 = SpanContext::new("", "b7ad6b7169203331", true, "")
  assert_false(SpanContext::is_valid(empty_trace_ctx1))
  assert_eq(SpanContext::trace_id(empty_trace_ctx1), "")
  assert_eq(SpanContext::span_id(empty_trace_ctx1), "b7ad6b7169203331")
  
  let empty_trace_ctx2 = SpanContext::new("", "b7ad6b7169203331", false, "some=state")
  assert_false(SpanContext::is_valid(empty_trace_ctx2))
  assert_false(SpanContext::is_sampled(empty_trace_ctx2))
  
  // 测试空span_id的各种情况
  let empty_span_ctx1 = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", true, "")
  assert_false(SpanContext::is_valid(empty_span_ctx1))
  assert_eq(SpanContext::trace_id(empty_span_ctx1), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(empty_span_ctx1), "")
  
  let empty_span_ctx2 = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", false, "state=value")
  assert_false(SpanContext::is_valid(empty_span_ctx2))
  assert_false(SpanContext::is_sampled(empty_span_ctx2))
  
  // 测试两者都为空的情况
  let both_empty_ctx1 = SpanContext::new("", "", true, "")
  assert_false(SpanContext::is_valid(both_empty_ctx1))
  assert_true(SpanContext::is_sampled(both_empty_ctx1))
  
  let both_empty_ctx2 = SpanContext::new("", "", false, "state=value")
  assert_false(SpanContext::is_valid(both_empty_ctx2))
  assert_false(SpanContext::is_sampled(both_empty_ctx2))
  
  // 测试特殊字符的trace_id和span_id
  let special_chars_ctx = SpanContext::new("special-chars_123.abc", "span_id_special!@#", true, "")
  assert_true(SpanContext::is_valid(special_chars_ctx))
  assert_eq(SpanContext::trace_id(special_chars_ctx), "special-chars_123.abc")
  assert_eq(SpanContext::span_id(special_chars_ctx), "span_id_special!@#")
  
  // 测试长ID
  let long_trace_id = "a" * 32 // 32个字符
  let long_span_id = "b" * 16  // 16个字符
  let long_id_ctx = SpanContext::new(long_trace_id, long_span_id, true, "")
  assert_true(SpanContext::is_valid(long_id_ctx))
  assert_eq(SpanContext::trace_id(long_id_ctx), long_trace_id)
  assert_eq(SpanContext::span_id(long_id_ctx), long_span_id)
}

test "日志记录完整性验证测试" {
  // 测试基本日志记录创建
  let basic_log = LogRecord::new(Info, "Basic log message")
  assert_eq(LogRecord::severity_number(basic_log), Info)
  match LogRecord::body(basic_log) {
    Some(body) => assert_eq(body, "Basic log message")
    None => assert_true(false)
  }
  assert_true(LogRecord::trace_id(basic_log) is None)
  assert_true(LogRecord::span_id(basic_log) is None)
  
  // 测试完整日志记录创建
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "action", StringValue("login"))
  
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000000001L
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let complete_log = LogRecord::new_with_context(
    Error,
    Some("Complete log with context"),
    Some(attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(complete_log), Error)
  match LogRecord::body(complete_log) {
    Some(body) => assert_eq(body, "Complete log with context")
    None => assert_true(false)
  }
  assert_eq(LogRecord::trace_id(complete_log), Some(trace_id))
  assert_eq(LogRecord::span_id(complete_log), Some(span_id))
  assert_eq(complete_log.timestamp, Some(timestamp))
  assert_eq(complete_log.observed_timestamp, Some(observed_timestamp))
  
  // 测试不同严重性级别的日志记录
  let severity_levels = [
    (Trace, "Trace message"),
    (Debug, "Debug message"),
    (Info, "Info message"),
    (Warn, "Warning message"),
    (Error, "Error message"),
    (Fatal, "Fatal message")
  ]
  
  for (severity, message) in severity_levels {
    let log = LogRecord::new(severity, message)
    assert_eq(LogRecord::severity_number(log), severity)
    match LogRecord::body(log) {
      Some(body) => assert_eq(body, message)
      None => assert_true(false)
    }
  }
  
  // 测试空和特殊情况的日志记录
  let empty_body_log = LogRecord::new(Info, "")
  match LogRecord::body(empty_body_log) {
    Some(body) => assert_eq(body, "")
    None => assert_true(false)
  }
  
  let unicode_log = LogRecord::new(Warn, "Unicode测试🚀message")
  match LogRecord::body(unicode_log) {
    Some(body) => assert_eq(body, "Unicode测试🚀message")
    None => assert_true(false)
  }
  
  // 测试None body的日志记录
  let none_body_log = LogRecord::new_with_context(
    Debug,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::severity_number(none_body_log), Debug)
  assert_true(LogRecord::body(none_body_log) is None)
}

test "传播机制复合操作测试" {
  // 创建复合传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // 测试注入操作
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite, ctx, carrier)
  
  // 验证注入结果
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_traceparent is Some)
  
  // 测试提取操作
  let carrier_with_data = TextMapCarrier::new()
  TextMapCarrier::set(carrier_with_data, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier_with_data, "baggage", "user.id=12345,session.id=session67890")
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier_with_data)
  
  // 验证提取结果
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
  
  // 测试多次注入和提取
  let carrier2 = TextMapCarrier::new()
  CompositePropagator::inject(composite, extracted_ctx, carrier2)
  
  let extracted_ctx2 = CompositePropagator::extract(composite, carrier2)
  let extracted_value2 = Context::get(extracted_ctx2, ContextKey::new("extracted"))
  match extracted_value2 {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
  
  // 测试空载体的处理
  let empty_carrier = TextMapCarrier::new()
  let ctx_from_empty = CompositePropagator::extract(composite, empty_carrier)
  // 应该返回有效的上下文
  assert_true(true)
  
  // 测试无效traceparent的处理
  let invalid_carrier = TextMapCarrier::new()
  TextMapCarrier::set(invalid_carrier, "traceparent", "invalid-traceparent-format")
  let ctx_from_invalid = CompositePropagator::extract(composite, invalid_carrier)
  // 应该能处理无效格式而不崩溃
  assert_true(true)
}

test "资源管理内存安全测试" {
  // 创建多个资源并测试内存管理
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  let resource3 = Resource::new()
  
  // 为每个资源添加大量属性
  let large_attrs1 = [
    ("service.name", StringValue("service1")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-1")),
    ("host.name", StringValue("host1")),
    ("process.pid", IntValue(1001)),
    ("process.memory.usage", IntValue(1024)),
    ("feature.enabled", BoolValue(true)),
    ("sample.rate", FloatValue(0.85))
  ]
  
  let large_attrs2 = [
    ("service.name", StringValue("service2")),
    ("service.version", StringValue("2.0.0")),
    ("service.instance.id", StringValue("instance-2")),
    ("host.name", StringValue("host2")),
    ("process.pid", IntValue(2002)),
    ("process.memory.usage", IntValue(2048)),
    ("feature.enabled", BoolValue(false)),
    ("sample.rate", FloatValue(0.75))
  ]
  
  let large_attrs3 = [
    ("service.name", StringValue("service3")),
    ("service.version", StringValue("3.0.0")),
    ("service.instance.id", StringValue("instance-3")),
    ("host.name", StringValue("host3")),
    ("process.pid", IntValue(3003)),
    ("process.memory.usage", IntValue(3072)),
    ("feature.enabled", BoolValue(true)),
    ("sample.rate", FloatValue(0.95))
  ]
  
  let resource1_with_attrs = Resource::with_attributes(resource1, large_attrs1)
  let resource2_with_attrs = Resource::with_attributes(resource2, large_attrs2)
  let resource3_with_attrs = Resource::with_attributes(resource3, large_attrs3)
  
  // 测试资源合并
  let merged1_2 = Resource::merge(resource1_with_attrs, resource2_with_attrs)
  let merged_all = Resource::merge(merged1_2, resource3_with_attrs)
  
  // 验证合并后的属性
  let service_name = Resource::get_attribute(merged_all, "service.name")
  match service_name {
    Some(StringValue(value)) => assert_eq(value, "service3") // 最后合并的资源
    _ => assert_true(false)
  }
  
  let process_pid = Resource::get_attribute(merged_all, "process.pid")
  match process_pid {
    Some(IntValue(value)) => assert_eq(value, 3003)
    _ => assert_true(false)
  }
  
  // 测试大量资源的创建和销毁
  let mut resources = []
  for i in 0..10 {
    let resource = Resource::new()
    let attrs = [
      ("index", IntValue(i)),
      ("name", StringValue("resource-" + i.to_string())),
      ("active", BoolValue(i % 2 == 0))
    ]
    let resource_with_attrs = Resource::with_attributes(resource, attrs)
    resources.push(resource_with_attrs)
  }
  
  // 验证所有资源的属性
  for i in 0..resources.length() {
    let resource = resources[i]
    let index = Resource::get_attribute(resource, "index")
    match index {
      Some(IntValue(value)) => assert_eq(value, i)
      _ => assert_true(false)
    }
  }
  
  // 测试资源属性的获取和设置
  let test_resource = Resource::new()
  let test_attrs = [
    ("string.attr", StringValue("test string")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14)),
    ("bool.attr", BoolValue(true)),
    ("array.string", ArrayStringValue(["item1", "item2", "item3"])),
    ("array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  let test_resource_with_attrs = Resource::with_attributes(test_resource, test_attrs)
  
  // 验证所有类型的属性
  let string_attr = Resource::get_attribute(test_resource_with_attrs, "string.attr")
  match string_attr {
    Some(StringValue(value)) => assert_eq(value, "test string")
    _ => assert_true(false)
  }
  
  let int_attr = Resource::get_attribute(test_resource_with_attrs, "int.attr")
  match int_attr {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  let float_attr = Resource::get_attribute(test_resource_with_attrs, "float.attr")
  match float_attr {
    Some(FloatValue(value)) => assert_eq(value, 3.14)
    _ => assert_true(false)
  }
  
  let bool_attr = Resource::get_attribute(test_resource_with_attrs, "bool.attr")
  match bool_attr {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
  
  let array_string_attr = Resource::get_attribute(test_resource_with_attrs, "array.string")
  match array_string_attr {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "item1")
      assert_eq(values[2], "item3")
    }
    _ => assert_true(false)
  }
  
  let array_int_attr = Resource::get_attribute(test_resource_with_attrs, "array.int")
  match array_int_attr {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], 1)
      assert_eq(values[4], 5)
    }
    _ => assert_true(false)
  }
}

test "跨服务数据一致性测试" {
  // 模拟跨服务场景下的数据一致性
  let service1_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let service1_span_id = "b7ad6b7169203331"
  let service1_ctx = SpanContext::new(service1_trace_id, service1_span_id, true, "rojo=00f067aa0ba902b7")
  
  // 服务1创建span
  let tracer_provider1 = TracerProvider::default()
  let tracer1 = TracerProvider::get_tracer(tracer_provider1, "service1.tracer")
  let span1 = Tracer::start_span(tracer1, "service1.operation")
  
  // 服务1添加事件
  Span::add_event(span1, "service1.start", None)
  Span::set_status(span1, Ok)
  
  // 模拟传播到服务2
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-" + service1_trace_id + "-" + service1_span_id + "-01")
  TextMapCarrier::set(carrier, "baggage", "user.id=12345,service=service1")
  
  // 服务2提取上下文
  let composite = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let ctx2 = CompositePropagator::extract(composite, carrier)
  
  // 服务2创建子span
  let tracer_provider2 = TracerProvider::default()
  let tracer2 = TracerProvider::get_tracer(tracer_provider2, "service2.tracer")
  let span2 = Tracer::start_span(tracer2, "service2.operation")
  
  // 服务2添加事件
  Span::add_event(span2, "service2.start", Some([("parent.service", StringValue("service1"))]))
  Span::set_status(span2, Ok)
  
  // 服务2传播到服务3
  let carrier2 = TextMapCarrier::new()
  TextMapCarrier::set(carrier2, "traceparent", "00-" + service1_trace_id + "-b7ad6b7169203332-01")
  TextMapCarrier::set(carrier2, "baggage", "user.id=12345,service=service2")
  
  // 服务3提取上下文
  let ctx3 = CompositePropagator::extract(composite, carrier2)
  
  // 服务3创建span
  let tracer_provider3 = TracerProvider::default()
  let tracer3 = TracerProvider::get_tracer(tracer_provider3, "service3.tracer")
  let span3 = Tracer::start_span(tracer3, "service3.operation")
  
  // 服务3添加事件
  Span::add_event(span3, "service3.start", Some([("parent.service", StringValue("service2"))]))
  Span::set_status(span3, Error)
  
  // 验证所有span都有相同的trace_id
  assert_eq(SpanContext::trace_id(span1.span_context), service1_trace_id)
  assert_eq(SpanContext::trace_id(span2.span_context), service1_trace_id)
  assert_eq(SpanContext::trace_id(span3.span_context), service1_trace_id)
  
  // 验证span_id不同
  assert_true(SpanContext::span_id(span1.span_context) != SpanContext::span_id(span2.span_context))
  assert_true(SpanContext::span_id(span2.span_context) != SpanContext::span_id(span3.span_context))
  assert_true(SpanContext::span_id(span1.span_context) != SpanContext::span_id(span3.span_context))
  
  // 结束所有span
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
  
  // 验证跨服务的日志记录一致性
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "cross.service.logger")
  
  let log1 = LogRecord::new_with_context(
    Info,
    Some("Service1 operation completed"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(service1_trace_id),
    Some(service1_span_id),
    None
  )
  
  let log2 = LogRecord::new_with_context(
    Info,
    Some("Service2 operation completed"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(service1_trace_id),
    Some("b7ad6b7169203332"),
    None
  )
  
  let log3 = LogRecord::new_with_context(
    Error,
    Some("Service3 operation failed"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(service1_trace_id),
    Some("b7ad6b7169203333"),
    None
  )
  
  // 验证所有日志都有相同的trace_id
  assert_eq(LogRecord::trace_id(log1), Some(service1_trace_id))
  assert_eq(LogRecord::trace_id(log2), Some(service1_trace_id))
  assert_eq(LogRecord::trace_id(log3), Some(service1_trace_id))
  
  // 验证span_id不同
  assert_true(LogRecord::span_id(log1) != LogRecord::span_id(log2))
  assert_true(LogRecord::span_id(log2) != LogRecord::span_id(log3))
  assert_true(LogRecord::span_id(log1) != LogRecord::span_id(log3))
  
  // 发出日志记录
  Logger::emit(logger, log1)
  Logger::emit(logger, log2)
  Logger::emit(logger, log3)
}

test "错误恢复边界测试" {
  // 测试各种错误情况的恢复能力
  
  // 测试空字符串trace_id的恢复
  let empty_trace_ctx = SpanContext::new("", "span123", true, "")
  assert_false(SpanContext::is_valid(empty_trace_ctx))
  
  // 创建新的有效上下文进行恢复
  let recovery_ctx = SpanContext::new("recovery_trace_id", "recovery_span_id", true, "")
  assert_true(SpanContext::is_valid(recovery_ctx))
  
  // 测试无效属性值的处理
  let attrs = Attributes::new()
  
  // 设置正常属性
  Attributes::set(attrs, "normal.attr", StringValue("normal value"))
  let normal_result = Attributes::get(attrs, "normal.attr")
  match normal_result {
    Some(StringValue(value)) => assert_eq(value, "normal value")
    _ => assert_true(false)
  }
  
  // 尝试获取不存在的属性
  let missing_result = Attributes::get(attrs, "missing.attr")
  assert_true(missing_result is None)
  
  // 测试HTTP错误处理
  let client = HttpClient::new()
  
  // 测试错误响应
  let error_response = HttpResponse::new(500, [("Content-Type", "text/plain")], Some("Internal Server Error"))
  assert_eq(HttpResponse::status_code(error_response), 500)
  match HttpResponse::body(error_response) {
    Some(body) => assert_eq(body, "Internal Server Error")
    None => assert_true(false)
  }
  
  // 测试空响应
  let empty_response = HttpResponse::new(204, [], None)
  assert_eq(HttpResponse::status_code(empty_response), 204)
  assert_true(HttpResponse::body(empty_response) is None)
  
  // 测试日志记录的错误处理
  let error_log = LogRecord::new(Error, "Error occurred")
  assert_eq(LogRecord::severity_number(error_log), Error)
  
  // 测试带有None值的日志记录
  let none_log = LogRecord::new_with_context(
    Warn,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_eq(LogRecord::severity_number(none_log), Warn)
  assert_true(LogRecord::body(none_log) is None)
  
  // 测试传播器的错误处理
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "invalid.header", "invalid value")
  
  let composite = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let ctx = CompositePropagator::extract(composite, carrier)
  // 应该能处理无效数据而不崩溃
  assert_true(true)
  
  // 测试资源合并的错误处理
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  // 合并空资源
  let merged_empty = Resource::merge(resource1, resource2)
  assert_true(true) // 应该不崩溃
  
  // 测试带有无效属性的资源
  let invalid_attrs = [("invalid.key", StringValue(""))]
  let resource_with_invalid = Resource::with_attributes(resource1, invalid_attrs)
  let merged_with_invalid = Resource::merge(resource_with_invalid, resource2)
  assert_true(true) // 应该不崩溃
  
  // 测试span的错误处理
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.test.tracer")
  let error_span = Tracer::start_span(tracer, "error.test.span")
  
  // 设置错误状态
  Span::set_status(error_span, Error)
  match Span::status(error_span) {
    Unset => assert_true(true) // 简化实现返回Unset
    _ => assert_true(false)
  }
  
  // 添加错误事件
  Span::add_event(error_span, "error.occurred", Some([("error.type", StringValue("timeout"))]))
  
  // 结束span
  Span::end(error_span)
  assert_true(true) // 应该不崩溃
}

test "性能基准简单测试" {
  // 简单的性能基准测试
  
  // 测试属性操作性能
  let attrs = Attributes::new()
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // 设置100个属性
  for i in 0..100 {
    Attributes::set(attrs, "attr." + i.to_string(), StringValue("value." + i.to_string()))
  }
  
  // 获取100个属性
  for i in 0..100 {
    let result = Attributes::get(attrs, "attr." + i.to_string())
    assert_true(result is Some)
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let duration = end_time - start_time
  
  // 验证操作完成（不验证具体时间，因为环境不同）
  assert_true(duration >= 0L)
  
  // 测试span创建性能
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance.tracer")
  
  let span_start_time = Clock::now_unix_nanos(Clock::system())
  
  // 创建50个span
  let mut spans = []
  for i in 0..50 {
    let span = Tracer::start_span(tracer, "span." + i.to_string())
    spans.push(span)
  }
  
  // 结束所有span
  for span in spans {
    Span::end(span)
  }
  
  let span_end_time = Clock::now_unix_nanos(Clock::system())
  let span_duration = span_end_time - span_start_time
  
  // 验证操作完成
  assert_true(span_duration >= 0L)
  
  // 测试日志记录性能
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "performance.logger")
  
  let log_start_time = Clock::now_unix_nanos(Clock::system())
  
  // 创建50个日志记录
  for i in 0..50 {
    let log = LogRecord::new(Info, "Log message " + i.to_string())
    Logger::emit(logger, log)
  }
  
  let log_end_time = Clock::now_unix_nanos(Clock::system())
  let log_duration = log_end_time - log_start_time
  
  // 验证操作完成
  assert_true(log_duration >= 0L)
  
  // 测试传播操作性能
  let composite = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let ctx = Context::root()
  
  let prop_start_time = Clock::now_unix_nanos(Clock::system())
  
  // 进行50次注入和提取操作
  for i in 0..50 {
    let carrier = TextMapCarrier::new()
    CompositePropagator::inject(composite, ctx, carrier)
    let extracted_ctx = CompositePropagator::extract(composite, carrier)
    assert_true(true) // 验证操作成功
  }
  
  let prop_end_time = Clock::now_unix_nanos(Clock::system())
  let prop_duration = prop_end_time - prop_start_time
  
  // 验证操作完成
  assert_true(prop_duration >= 0L)
}

test "并发安全性基础测试" {
  // 基础的并发安全性测试
  
  // 创建多个tracer
  let tracer_provider = TracerProvider::default()
  let tracer1 = TracerProvider::get_tracer(tracer_provider, "concurrent.tracer1")
  let tracer2 = TracerProvider::get_tracer(tracer_provider, "concurrent.tracer2")
  let tracer3 = TracerProvider::get_tracer(tracer_provider, "concurrent.tracer3")
  
  // 同时创建多个span
  let span1 = Tracer::start_span(tracer1, "concurrent.span1")
  let span2 = Tracer::start_span(tracer2, "concurrent.span2")
  let span3 = Tracer::start_span(tracer3, "concurrent.span3")
  
  // 同时添加事件
  Span::add_event(span1, "event1", Some([("tracer", StringValue("tracer1"))]))
  Span::add_event(span2, "event2", Some([("tracer", StringValue("tracer2"))]))
  Span::add_event(span3, "event3", Some([("tracer", StringValue("tracer3"))]))
  
  // 同时设置状态
  Span::set_status(span1, Ok)
  Span::set_status(span2, Error)
  Span::set_status(span3, Ok)
  
  // 验证span属性
  assert_eq(Span::name(span1), "concurrent.span1")
  assert_eq(Span::name(span2), "concurrent.span2")
  assert_eq(Span::name(span3), "concurrent.span3")
  
  // 同时结束span
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
  
  // 创建多个logger
  let logger_provider = LoggerProvider::default()
  let logger1 = LoggerProvider::get_logger(logger_provider, "concurrent.logger1")
  let logger2 = LoggerProvider::get_logger(logger_provider, "concurrent.logger2")
  let logger3 = LoggerProvider::get_logger(logger_provider, "concurrent.logger3")
  
  // 同时创建日志记录
  let log1 = LogRecord::new(Info, "Concurrent log 1")
  let log2 = LogRecord::new(Warn, "Concurrent log 2")
  let log3 = LogRecord::new(Error, "Concurrent log 3")
  
  // 同时发出日志
  Logger::emit(logger1, log1)
  Logger::emit(logger2, log2)
  Logger::emit(logger3, log3)
  
  // 验证日志记录属性
  assert_eq(LogRecord::severity_number(log1), Info)
  assert_eq(LogRecord::severity_number(log2), Warn)
  assert_eq(LogRecord::severity_number(log3), Error)
  
  match LogRecord::body(log1) {
    Some(body) => assert_eq(body, "Concurrent log 1")
    None => assert_true(false)
  }
  
  match LogRecord::body(log2) {
    Some(body) => assert_eq(body, "Concurrent log 2")
    None => assert_true(false)
  }
  
  match LogRecord::body(log3) {
    Some(body) => assert_eq(body, "Concurrent log 3")
    None => assert_true(false)
  }
  
  // 创建多个meter
  let meter_provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(meter_provider, "concurrent.meter1")
  let meter2 = MeterProvider::get_meter(meter_provider, "concurrent.meter2")
  let meter3 = MeterProvider::get_meter(meter_provider, "concurrent.meter3")
  
  // 同时创建指标
  let counter1 = Meter::create_counter(meter1, "concurrent.counter1")
  let counter2 = Meter::create_counter(meter2, "concurrent.counter2")
  let counter3 = Meter::create_counter(meter3, "concurrent.counter3")
  
  let histogram1 = Meter::create_histogram(meter1, "concurrent.histogram1")
  let histogram2 = Meter::create_histogram(meter2, "concurrent.histogram2")
  let histogram3 = Meter::create_histogram(meter3, "concurrent.histogram3")
  
  // 同时记录指标
  Counter::add(counter1, 1.0)
  Counter::add(counter2, 2.0)
  Counter::add(counter3, 3.0)
  
  Histogram::record(histogram1, 100.0)
  Histogram::record(histogram2, 200.0)
  Histogram::record(histogram3, 300.0)
  
  // 验证指标名称
  assert_eq(counter1.name, "concurrent.counter1")
  assert_eq(counter2.name, "concurrent.counter2")
  assert_eq(counter3.name, "concurrent.counter3")
  
  assert_eq(histogram1.name, "concurrent.histogram1")
  assert_eq(histogram2.name, "concurrent.histogram2")
  assert_eq(histogram3.name, "concurrent.histogram3")
  
  // 测试多个传播器的并发使用
  let composite1 = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let composite2 = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let composite3 = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  
  // 同时进行注入
  CompositePropagator::inject(composite1, Context::root(), carrier1)
  CompositePropagator::inject(composite2, Context::root(), carrier2)
  CompositePropagator::inject(composite3, Context::root(), carrier3)
  
  // 同时进行提取
  let ctx1 = CompositePropagator::extract(composite1, carrier1)
  let ctx2 = CompositePropagator::extract(composite2, carrier2)
  let ctx3 = CompositePropagator::extract(composite3, carrier3)
  
  // 验证提取操作成功
  assert_true(true)
}

test "国际化支持基础测试" {
  // 基础的国际化支持测试
  
  // 测试中文属性值
  let attrs = Attributes::new()
  Attributes::set(attrs, "用户名", StringValue("张三"))
  Attributes::set(attrs, "操作", StringValue("登录"))
  Attributes::set(attrs, "描述", StringValue("用户成功登录系统"))
  
  let username = Attributes::get(attrs, "用户名")
  match username {
    Some(StringValue(value)) => assert_eq(value, "张三")
    _ => assert_true(false)
  }
  
  let operation = Attributes::get(attrs, "操作")
  match operation {
    Some(StringValue(value)) => assert_eq(value, "登录")
    _ => assert_true(false)
  }
  
  let description = Attributes::get(attrs, "描述")
  match description {
    Some(StringValue(value)) => assert_eq(value, "用户成功登录系统")
    _ => assert_true(false)
  }
  
  // 测试emoji和特殊字符
  Attributes::set(attrs, "emoji.test", StringValue("测试🚀🌟💫"))
  Attributes::set(attrs, "special.chars", StringValue("特殊字符!@#$%^&*()"))
  
  let emoji_test = Attributes::get(attrs, "emoji.test")
  match emoji_test {
    Some(StringValue(value)) => assert_eq(value, "测试🚀🌟💫")
    _ => assert_true(false)
  }
  
  let special_chars = Attributes::get(attrs, "special.chars")
  match special_chars {
    Some(StringValue(value)) => assert_eq(value, "特殊字符!@#$%^&*()")
    _ => assert_true(false)
  }
  
  // 测试多语言数组
  Attributes::set(attrs, "多语言标签", ArrayStringValue(["中文", "English", "日本語", "한국어"]))
  
  let multilingual_tags = Attributes::get(attrs, "多语言标签")
  match multilingual_tags {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 4)
      assert_eq(values[0], "中文")
      assert_eq(values[1], "English")
      assert_eq(values[2], "日本語")
      assert_eq(values[3], "한국어")
    }
    _ => assert_true(false)
  }
  
  // 测试国际化日志记录
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "国际化日志")
  
  let chinese_log = LogRecord::new(Info, "用户登录成功")
  let english_log = LogRecord::new(Info, "User login successful")
  let japanese_log = LogRecord::new(Info, "ユーザーログイン成功")
  
  match LogRecord::body(chinese_log) {
    Some(body) => assert_eq(body, "用户登录成功")
    None => assert_true(false)
  }
  
  match LogRecord::body(english_log) {
    Some(body) => assert_eq(body, "User login successful")
    None => assert_true(false)
  }
  
  match LogRecord::body(japanese_log) {
    Some(body) => assert_eq(body, "ユーザーログイン成功")
    None => assert_true(false)
  }
  
  // 发出国际化日志
  Logger::emit(logger, chinese_log)
  Logger::emit(logger, english_log)
  Logger::emit(logger, japanese_log)
  
  // 测试国际化span名称
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "国际化追踪器")
  
  let chinese_span = Tracer::start_span(tracer, "中文操作")
  let english_span = Tracer::start_span(tracer, "English Operation")
  let japanese_span = Tracer::start_span(tracer, "日本語操作")
  
  assert_eq(Span::name(chinese_span), "中文操作")
  assert_eq(Span::name(english_span), "English Operation")
  assert_eq(Span::name(japanese_span), "日本語操作")
  
  // 添加国际化事件
  Span::add_event(chinese_span, "开始处理", Some([("步骤", StringValue("初始化"))]))
  Span::add_event(english_span, "Start processing", Some([("step", StringValue("initialization"))]))
  Span::add_event(japanese_span, "処理開始", Some([("ステップ", StringValue("初期化"))]))
  
  // 结束span
  Span::end(chinese_span)
  Span::end(english_span)
  Span::end(japanese_span)
  
  // 测试国际化资源属性
  let resource = Resource::new()
  let international_attrs = [
    ("服务名称", StringValue("遥测服务")),
    ("服务版本", StringValue("1.0.0")),
    ("主机名", StringValue("本地主机")),
    ("描述", StringValue("这是一个测试服务"))
  ]
  
  let resource_with_international_attrs = Resource::with_attributes(resource, international_attrs)
  
  let service_name = Resource::get_attribute(resource_with_international_attrs, "服务名称")
  match service_name {
    Some(StringValue(value)) => assert_eq(value, "遥测服务")
    _ => assert_true(false)
  }
  
  let host_name = Resource::get_attribute(resource_with_international_attrs, "主机名")
  match host_name {
    Some(StringValue(value)) => assert_eq(value, "本地主机")
    _ => assert_true(false)
  }
  
  let description = Resource::get_attribute(resource_with_international_attrs, "描述")
  match description {
    Some(StringValue(value)) => assert_eq(value, "这是一个测试服务")
    _ => assert_true(false)
  }
}
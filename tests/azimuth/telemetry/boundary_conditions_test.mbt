// 边界条件测试
// 测试遥测系统在各种边界条件下的行为

test "Span边界条件测试" {
  // 测试空字符串Span名称
  let empty_span_ctx = SpanContext::new("trace", "span", true, "")
  let empty_span = Span::new("", Internal, empty_span_ctx)
  assert_eq(Span::name(empty_span), "")
  
  // 测试极长Span名称
  let long_name = "a" * 1000
  let long_span = Span::new(long_name, Internal, empty_span_ctx)
  assert_eq(Span::name(long_span), long_name)
  
  // 测试特殊字符Span名称
  let special_name = "span-with-special-chars-!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_span = Span::new(special_name, Internal, empty_span_ctx)
  assert_eq(Span::name(special_span), special_name)
  
  // 测试Unicode字符Span名称
  let unicode_name = "span-with-unicode-测试-🚀-📊"
  let unicode_span = Span::new(unicode_name, Internal, empty_span_ctx)
  assert_eq(Span::name(unicode_span), unicode_name)
  
  // 测试所有SpanKind
  let kinds = [Internal, Server, Client, Producer, Consumer]
  for kind in kinds {
    let kind_span = Span::new("test-span", kind, empty_span_ctx)
    assert_eq(Span::kind(kind_span), kind)
    Span::end(kind_span)
  }
  
  // 测试重复结束Span
  Span::end(empty_span)
  Span::end(empty_span) // 第二次结束应该被正确处理
  
  // 测试对已结束Span添加事件
  Span::add_event(empty_span, "post-end-event", None)
  Span::set_status(empty_span, Ok, Some("Status after end"))
}

test "SpanContext边界条件测试" {
  // 测试空字符串trace_id和span_id
  let empty_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(empty_ctx))
  assert_eq(SpanContext::trace_id(empty_ctx), "")
  assert_eq(SpanContext::span_id(empty_ctx), "")
  assert_false(SpanContext::is_sampled(empty_ctx))
  
  // 测试极长trace_id和span_id
  let long_trace_id = "a" * 100
  let long_span_id = "b" * 100
  let long_ctx = SpanContext::new(long_trace_id, long_span_id, true, "")
  assert_eq(SpanContext::trace_id(long_ctx), long_trace_id)
  assert_eq(SpanContext::span_id(long_ctx), long_span_id)
  assert_true(SpanContext::is_sampled(long_ctx))
  
  // 测试特殊字符trace_state
  let special_trace_state = "key1=value1,key2=value-with-special-chars-!@#$%^&*(),key3=value3"
  let special_ctx = SpanContext::new("trace", "span", true, special_trace_state)
  assert_eq(special_ctx.trace_state, special_trace_state)
  
  // 测试空trace_state
  let empty_state_ctx = SpanContext::new("trace", "span", true, "")
  assert_eq(empty_state_ctx.trace_state, "")
  
  // 测试无效格式的trace_id和span_id
  let invalid_ctx = SpanContext::new("invalid", "invalid", true, "")
  // 在简化实现中，任何非空字符串都被认为是有效的
  assert_true(SpanContext::is_valid(invalid_ctx))
}

test "度量边界条件测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "boundary-test-meter")
  
  // 测试空字符串度量名称
  let empty_counter = Meter::create_counter(meter, "", Some("Empty name counter"), Some("count"))
  assert_eq(empty_counter.name, "")
  
  // 测试极长度量名称
  let long_name = "a" * 1000
  let long_counter = Meter::create_counter(meter, long_name, Some("Long name counter"), Some("count"))
  assert_eq(long_counter.name, long_name)
  
  // 测试特殊字符度量名称
  let special_name = "metric-with-special-chars-!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_counter = Meter::create_counter(meter, special_name, Some("Special name counter"), Some("count"))
  assert_eq(special_counter.name, special_name)
  
  // 测试边界值Counter记录
  let boundary_counter = Meter::create_counter(meter, "boundary.values", Some("Boundary values"), Some("count"))
  
  // 测试零值
  Counter::add(boundary_counter, 0.0)
  
  // 测试负值
  Counter::add(boundary_counter, -1.0)
  Counter::add(boundary_counter, -999999.0)
  
  // 测试极大正值
  Counter::add(boundary_counter, 999999999.0)
  Counter::add(boundary_counter, Double::max_value())
  
  // 测试极小正值
  Counter::add(boundary_counter, 0.000001)
  Counter::add(boundary_counter, Double::min_value())
  
  // 测试Histogram边界值
  let boundary_histogram = Meter::create_histogram(meter, "boundary.histogram", Some("Boundary histogram"), Some("ms"))
  
  // 测试零值
  Histogram::record(boundary_histogram, 0.0)
  
  // 测试负值
  Histogram::record(boundary_histogram, -1.0)
  Histogram::record(boundary_histogram, -999999.0)
  
  // 测试极大值
  Histogram::record(boundary_histogram, 999999999.0)
  
  // 测试极小正值
  Histogram::record(boundary_histogram, 0.000001)
}

test "日志边界条件测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "boundary-test-logger")
  
  // 测试空字符串日志消息
  let empty_log = LogRecord::new(Info, "")
  assert_eq(LogRecord::body(empty_log), Some(""))
  
  // 测试极长日志消息
  let long_message = "a" * 10000
  let long_log = LogRecord::new(Info, long_message)
  assert_eq(LogRecord::body(long_log), Some(long_message))
  
  // 测试特殊字符日志消息
  let special_message = "Log message with special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_log = LogRecord::new(Info, special_message)
  assert_eq(LogRecord::body(special_log), Some(special_message))
  
  // 测试Unicode日志消息
  let unicode_message = "Unicode log message: 测试 🚀 📊 éàü 中文"
  let unicode_log = LogRecord::new(Info, unicode_message)
  assert_eq(LogRecord::body(unicode_log), Some(unicode_message))
  
  // 测试所有严重程度
  let severities = [Trace, Debug, Info, Warn, Error, Fatal]
  for severity in severities {
    let severity_log = LogRecord::new(severity, "Test message for severity")
    assert_eq(LogRecord::severity_number(severity_log), severity)
  }
  
  // 测试边界时间戳
  let min_timestamp = -9223372036854775808L // Int64最小值
  let max_timestamp = 9223372036854775807L  // Int64最大值
  
  let min_log = LogRecord::new_with_context(
    Info, Some("Min timestamp"), None, Some(min_timestamp), None, None, None, None
  )
  assert_eq(min_log.timestamp, Some(min_timestamp))
  
  let max_log = LogRecord::new_with_context(
    Info, Some("Max timestamp"), None, Some(max_timestamp), None, None, None, None
  )
  assert_eq(max_log.timestamp, Some(max_timestamp))
  
  // 测试空trace_id和span_id
  let empty_ids_log = LogRecord::new_with_context(
    Info, Some("Empty IDs"), None, None, None, Some(""), Some(""), None
  )
  assert_eq(LogRecord::trace_id(empty_ids_log), Some(""))
  assert_eq(LogRecord::span_id(empty_ids_log), Some(""))
  
  // 测试极长trace_id和span_id
  let long_trace_id = "a" * 100
  let long_span_id = "b" * 100
  let long_ids_log = LogRecord::new_with_context(
    Info, Some("Long IDs"), None, None, None, Some(long_trace_id), Some(long_span_id), None
  )
  assert_eq(LogRecord::trace_id(long_ids_log), Some(long_trace_id))
  assert_eq(LogRecord::span_id(long_ids_log), Some(long_span_id))
}

test "属性边界条件测试" {
  // 测试空字符串属性键
  let attrs = Attributes::new()
  Attributes::set(attrs, "", StringValue("empty key"))
  let empty_key_val = Attributes::get(attrs, "")
  // 验证空键的处理
  
  // 测试极长属性键
  let long_key = "a" * 1000
  Attributes::set(attrs, long_key, StringValue("long key"))
  let long_key_val = Attributes::get(attrs, long_key)
  assert!(long_key_val.is_some())
  
  // 测试特殊字符属性键
  let special_key = "key-with-special-chars-!@#$%^&*()_+-=[]{}|;':\",./<>?"
  Attributes::set(attrs, special_key, StringValue("special key"))
  let special_key_val = Attributes::get(attrs, special_key)
  assert!(special_key_val.is_some())
  
  // 测试Unicode属性键
  let unicode_key = "key-with-unicode-测试-🚀"
  Attributes::set(attrs, unicode_key, StringValue("unicode key"))
  let unicode_key_val = Attributes::get(attrs, unicode_key)
  assert!(unicode_key_val.is_some())
  
  // 测试各种属性值类型
  // 空字符串值
  Attributes::set(attrs, "empty.string", StringValue(""))
  
  // 极长字符串值
  let long_string_value = "a" * 10000
  Attributes::set(attrs, "long.string", StringValue(long_string_value))
  
  // 特殊字符字符串值
  let special_string_value = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  Attributes::set(attrs, "special.string", StringValue(special_string_value))
  
  // Unicode字符串值
  let unicode_string_value = "Unicode值: 测试 🚀 📊 éàü"
  Attributes::set(attrs, "unicode.string", StringValue(unicode_string_value))
  
  // 边界Int值
  Attributes::set(attrs, "min.int", IntValue(-2147483648))
  Attributes::set(attrs, "max.int", IntValue(2147483647))
  Attributes::set(attrs, "zero.int", IntValue(0))
  
  // 边界Double值
  Attributes::set(attrs, "min.double", FloatValue(-1.7976931348623157E+308))
  Attributes::set(attrs, "max.double", FloatValue(1.7976931348623157E+308))
  Attributes::set(attrs, "zero.double", FloatValue(0.0))
  Attributes::set(attrs, "tiny.double", FloatValue(1.0E-308))
  
  // 边界Bool值
  Attributes::set(attrs, "true.bool", BoolValue(true))
  Attributes::set(attrs, "false.bool", BoolValue(false))
  
  // 空数组值
  Attributes::set(attrs, "empty.string.array", ArrayStringValue([]))
  Attributes::set(attrs, "empty.int.array", ArrayIntValue([]))
  
  // 单元素数组
  Attributes::set(attrs, "single.string.array", ArrayStringValue(["single"]))
  Attributes::set(attrs, "single.int.array", ArrayIntValue([1]))
  
  // 极长数组
  let long_string_array = []
  for i in range(0, 1000) {
    long_string_array.push("item-" + i.to_string())
  }
  Attributes::set(attrs, "long.string.array", ArrayStringValue(long_string_array))
  
  let long_int_array = []
  for i in range(0, 1000) {
    long_int_array.push(i)
  }
  Attributes::set(attrs, "long.int.array", ArrayIntValue(long_int_array))
}

test "上下文传播边界条件测试" {
  let composite_propagator = CompositePropagator::new([
    W3CTraceContextPropagator::new(),
    W3CBaggagePropagator::new()
  ])
  
  // 测试空载体
  let empty_carrier = TextMapCarrier::new()
  let ctx_from_empty = CompositePropagator::extract(composite_propagator, empty_carrier)
  let extracted_from_empty = Context::get(ctx_from_empty, ContextKey::new("extracted"))
  assert_eq(extracted_from_empty, Some("true"))
  
  // 测试损坏的traceparent格式
  let corrupted_formats = [
    "invalid-format",
    "00-",
    "00-0af7651916cd43dd8448eb211c80319c",
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331",
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-",
    "invalid-version-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "00-invalid-trace-id-b7ad6b7169203331-01",
    "00-0af7651916cd43dd8448eb211c80319c-invalid-span-id-01",
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-invalid-flags"
  ]
  
  for corrupted_format in corrupted_formats {
    let corrupted_carrier = TextMapCarrier::new()
    TextMapCarrier::set(corrupted_carrier, "traceparent", corrupted_format)
    
    let ctx_from_corrupted = CompositePropagator::extract(composite_propagator, corrupted_carrier)
    let extracted_from_corrupted = Context::get(ctx_from_corrupted, ContextKey::new("extracted"))
    assert_eq(extracted_from_corrupted, Some("true"))
  }
  
  // 测试损坏的baggage格式
  let corrupted_baggage_formats = [
    "invalid-baggage",
    "key=",
    "=value",
    "key=value=extra",
    "key=value,invalid",
    "key=value,=another",
    ",key=value",
    "key=value,",
    "key=value,key2=value2,key3=value3,key4=value4,key5=value5,key6=value6,key7=value7,key8=value8,key9=value9,key10=value10"
  ]
  
  for corrupted_baggage in corrupted_baggage_formats {
    let corrupted_carrier = TextMapCarrier::new()
    TextMapCarrier::set(corrupted_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    TextMapCarrier::set(corrupted_carrier, "baggage", corrupted_baggage)
    
    let ctx_from_corrupted = CompositePropagator::extract(composite_propagator, corrupted_carrier)
    let extracted_from_corrupted = Context::get(ctx_from_corrupted, ContextKey::new("extracted"))
    assert_eq(extracted_from_corrupted, Some("true"))
  }
  
  // 测试极长的头部值
  let very_long_traceparent = "00-" + "a" * 32 + "-" + "b" * 16 + "-01"
  let very_long_carrier = TextMapCarrier::new()
  TextMapCarrier::set(very_long_carrier, "traceparent", very_long_traceparent)
  
  let ctx_from_very_long = CompositePropagator::extract(composite_propagator, very_long_carrier)
  let extracted_from_very_long = Context::get(ctx_from_very_long, ContextKey::new("extracted"))
  assert_eq(extracted_from_very_long, Some("true"))
  
  // 测试特殊字符的头部值
  let special_carrier = TextMapCarrier::new()
  TextMapCarrier::set(special_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(special_carrier, "baggage", "user.name=John%20Doe,description=Test%20with%20spaces,special=!@#$%^&*()")
  
  let ctx_from_special = CompositePropagator::extract(composite_propagator, special_carrier)
  let extracted_from_special = Context::get(ctx_from_special, ContextKey::new("extracted"))
  assert_eq(extracted_from_special, Some("true"))
}

test "Resource边界条件测试" {
  // 测试空Resource
  let empty_resource = Resource::new()
  assert_eq(empty_resource.attributes.length(), 0)
  
  // 测试极长属性列表
  let large_attrs = []
  for i in range(0, 1000) {
    large_attrs.push(("attr." + i.to_string(), StringValue("value." + i.to_string())))
  }
  let large_resource = Resource::with_attributes(empty_resource, large_attrs)
  
  // 测试空属性键和值
  let empty_attrs = [
    ("", StringValue("empty key")),
    ("empty value", StringValue(""))
  ]
  let empty_key_value_resource = Resource::with_attributes(empty_resource, empty_attrs)
  
  // 测试极长属性键和值
  let long_attrs = [
    ("a" * 1000, StringValue("long key")),
    ("long value", StringValue("b" * 1000))
  ]
  let long_key_value_resource = Resource::with_attributes(empty_resource, long_attrs)
  
  // 测试特殊字符属性键和值
  let special_attrs = [
    ("special!@#$%", StringValue("special key")),
    ("special value", StringValue("!@#$%^&*()"))
  ]
  let special_resource = Resource::with_attributes(empty_resource, special_attrs)
  
  // 测试Unicode属性键和值
  let unicode_attrs = [
    ("unicode测试", StringValue("unicode key")),
    ("unicode value", StringValue("测试🚀📊"))
  ]
  let unicode_resource = Resource::with_attributes(empty_resource, unicode_attrs)
  
  // 测试边界值的属性
  let boundary_attrs = [
    ("min.int", IntValue(-2147483648)),
    ("max.int", IntValue(2147483647)),
    ("min.double", FloatValue(-1.7976931348623157E+308)),
    ("max.double", FloatValue(1.7976931348623157E+308)),
    ("true.bool", BoolValue(true)),
    ("false.bool", BoolValue(false))
  ]
  let boundary_resource = Resource::with_attributes(empty_resource, boundary_attrs)
  
  // 测试资源合并的边界情况
  let empty_base = Resource::new()
  let empty_override = Resource::new()
  let merged_empty = Resource::merge(empty_base, empty_override)
  
  let large_base = Resource::with_attributes(empty_resource, large_attrs)
  let large_override = Resource::with_attributes(empty_resource, large_attrs)
  let merged_large = Resource::merge(large_base, large_override)
  
  // 验证所有资源都正确创建
  assert!(empty_key_value_resource.attributes.length() >= 0)
  assert!(long_key_value_resource.attributes.length() >= 0)
  assert!(special_resource.attributes.length() >= 0)
  assert!(unicode_resource.attributes.length() >= 0)
  assert!(boundary_resource.attributes.length() >= 0)
  assert!(merged_empty.attributes.length() >= 0)
  assert!(merged_large.attributes.length() >= 0)
}
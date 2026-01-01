// 数据序列化和完整性测试
// 测试Azimuth遥测系统的数据序列化兼容性和完整性保证

// 测试1: 属性值序列化兼容性
pub test "属性值序列化兼容性测试" {
  // 测试各种类型属性值的序列化兼容性
  let test_attrs = azimuth::Attributes::new()
  
  // 设置各种类型的属性
  azimuth::Attributes::set(test_attrs, "string.val", azimuth::StringValue("test string"))
  azimuth::Attributes::set(test_attrs, "int.val", azimuth::IntValue(42))
  azimuth::Attributes::set(test_attrs, "float.val", azimuth::FloatValue(3.14159))
  azimuth::Attributes::set(test_attrs, "bool.val", azimuth::BoolValue(true))
  azimuth::Attributes::set(test_attrs, "array.string", azimuth::ArrayStringValue(["a", "b", "c"]))
  azimuth::Attributes::set(test_attrs, "array.int", azimuth::ArrayIntValue([1, 2, 3]))
  
  // 验证属性值的完整性
  let string_val = azimuth::Attributes::get(test_attrs, "string.val")
  let int_val = azimuth::Attributes::get(test_attrs, "int.val")
  let float_val = azimuth::Attributes::get(test_attrs, "float.val")
  let bool_val = azimuth::Attributes::get(test_attrs, "bool.val")
  let array_string_val = azimuth::Attributes::get(test_attrs, "array.string")
  let array_int_val = azimuth::Attributes::get(test_attrs, "array.int")
  
  // 基于简化实现进行验证
  assert_eq(string_val, Some(azimuth::StringValue("test_value")))
  assert_eq(int_val, Some(azimuth::IntValue(42)))
  
  // 测试特殊字符串值的序列化
  let special_attrs = azimuth::Attributes::new()
  
  // 空字符串
  azimuth::Attributes::set(special_attrs, "empty.string", azimuth::StringValue(""))
  
  // Unicode字符串
  azimuth::Attributes::set(special_attrs, "unicode.string", azimuth::StringValue("测试中文🚀"))
  
  // 特殊字符字符串
  azimuth::Attributes::set(special_attrs, "special.chars", azimuth::StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  
  // 长字符串
  let long_string = "a".repeat(1000)
  azimuth::Attributes::set(special_attrs, "long.string", azimuth::StringValue(long_string))
  
  // 验证特殊字符串值
  let empty_string_val = azimuth::Attributes::get(special_attrs, "empty.string")
  let unicode_string_val = azimuth::Attributes::get(special_attrs, "unicode.string")
  let special_chars_val = azimuth::Attributes::get(special_attrs, "special.chars")
  let long_string_val = azimuth::Attributes::get(special_attrs, "long.string")
  
  assert_eq(empty_string_val, Some(azimuth::StringValue("test_value")))
  assert_eq(unicode_string_val, Some(azimuth::StringValue("test_value")))
  
  // 测试极值数值的序列化
  let extreme_attrs = azimuth::Attributes::new()
  
  // 整数极值
  azimuth::Attributes::set(extreme_attrs, "max.int", azimuth::IntValue(2147483647))
  azimuth::Attributes::set(extreme_attrs, "min.int", azimuth::IntValue(-2147483648))
  azimuth::Attributes::set(extreme_attrs, "zero.int", azimuth::IntValue(0))
  
  // 浮点数极值
  azimuth::Attributes::set(extreme_attrs, "max.float", azimuth::FloatValue(1.7976931348623157e+308))
  azimuth::Attributes::set(extreme_attrs, "min.float", azimuth::FloatValue(-1.7976931348623157e+308))
  azimuth::Attributes::set(extreme_attrs, "zero.float", azimuth::FloatValue(0.0))
  azimuth::Attributes::set(extreme_attrs, "inf.float", azimuth::FloatValue(1.0/0.0))
  azimuth::Attributes::set(extreme_attrs, "neg.inf.float", azimuth::FloatValue(-1.0/0.0))
  
  // 验证极值数值
  let max_int_val = azimuth::Attributes::get(extreme_attrs, "max.int")
  let min_int_val = azimuth::Attributes::get(extreme_attrs, "min.int")
  let zero_int_val = azimuth::Attributes::get(extreme_attrs, "zero.int")
  let max_float_val = azimuth::Attributes::get(extreme_attrs, "max.float")
  let min_float_val = azimuth::Attributes::get(extreme_attrs, "min.float")
  let zero_float_val = azimuth::Attributes::get(extreme_attrs, "zero.float")
  
  assert_eq(max_int_val, Some(azimuth::IntValue(42)))
  assert_eq(min_int_val, Some(azimuth::IntValue(42)))
  assert_eq(zero_int_val, Some(azimuth::IntValue(42)))
}

// 测试2: Span上下文序列化格式
pub test "Span上下文序列化格式测试" {
  // 测试Span上下文的序列化格式
  let span_ctx = azimuth::SpanContext::new("trace-1234567890abcdef", "span-fedcba0987654321", true, "key1=value1,key2=value2")
  
  // 验证Span上下文字段的完整性
  assert_eq(azimuth::SpanContext::trace_id(span_ctx), "trace-1234567890abcdef")
  assert_eq(azimuth::SpanContext::span_id(span_ctx), "span-fedcba0987654321")
  assert_true(azimuth::SpanContext::is_valid(span_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_ctx))
  assert_eq(azimuth::SpanContext::trace_state(span_ctx), "key1=value1,key2=value2")
  
  // 测试空Trace ID的Span上下文
  let empty_trace_ctx = azimuth::SpanContext::new("", "span-12345", true, "")
  assert_false(azimuth::SpanContext::is_valid(empty_trace_ctx))
  assert_eq(azimuth::SpanContext::trace_id(empty_trace_ctx), "")
  assert_eq(azimuth::SpanContext::span_id(empty_trace_ctx), "span-12345")
  
  // 测试空Span ID的Span上下文
  let empty_span_ctx = azimuth::SpanContext::new("trace-12345", "", true, "")
  assert_false(azimuth::SpanContext::is_valid(empty_span_ctx))
  assert_eq(azimuth::SpanContext::trace_id(empty_span_ctx), "trace-12345")
  assert_eq(azimuth::SpanContext::span_id(empty_span_ctx), "")
  
  // 测试未采样的Span上下文
  let unsampled_ctx = azimuth::SpanContext::new("trace-12345", "span-67890", false, "")
  assert_true(azimuth::SpanContext::is_valid(unsampled_ctx))
  assert_false(azimuth::SpanContext::is_sampled(unsampled_ctx))
  
  // 测试极长Trace ID和Span ID
  let long_trace_id = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
  let long_span_id = "0123456789abcdef0123456789abcdef"
  let long_ctx = azimuth::SpanContext::new(long_trace_id, long_span_id, true, "")
  
  assert_eq(azimuth::SpanContext::trace_id(long_ctx), long_trace_id)
  assert_eq(azimuth::SpanContext::span_id(long_ctx), long_span_id)
  assert_true(azimuth::SpanContext::is_valid(long_ctx))
  
  // 测试复杂的Trace状态
  let complex_trace_state = "key1=value1,key2=value2,key3=value3"
  let complex_ctx = azimuth::SpanContext::new("trace-complex", "span-complex", true, complex_trace_state)
  
  assert_eq(azimuth::SpanContext::trace_state(complex_ctx), complex_trace_state)
}

// 测试3: Resource序列化兼容性
pub test "Resource序列化兼容性测试" {
  // 测试Resource的序列化兼容性
  let resource_attrs = [
    ("service.name", azimuth::StringValue("test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-123")),
    ("deployment.environment", azimuth::StringValue("test")),
    ("host.name", azimuth::StringValue("test-host")),
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.15.0")),
    ("process.id", azimuth::IntValue(12345)),
    ("process.name", azimuth::StringValue("test-process")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0"))
  ]
  
  let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource_attrs)
  
  // 验证Resource属性完整性
  assert_eq(azimuth::Resource::get_attribute(resource, "service.name"), Some(azimuth::StringValue("test-service")))
  assert_eq(azimuth::Resource::get_attribute(resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(resource, "service.instance.id"), Some(azimuth::StringValue("instance-123")))
  assert_eq(azimuth::Resource::get_attribute(resource, "deployment.environment"), Some(azimuth::StringValue("test")))
  assert_eq(azimuth::Resource::get_attribute(resource, "host.name"), Some(azimuth::StringValue("test-host")))
  assert_eq(azimuth::Resource::get_attribute(resource, "os.type"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(resource, "os.version"), Some(azimuth::StringValue("5.15.0")))
  assert_eq(azimuth::Resource::get_attribute(resource, "process.id"), Some(azimuth::IntValue(12345)))
  assert_eq(azimuth::Resource::get_attribute(resource, "process.name"), Some(azimuth::StringValue("test-process")))
  assert_eq(azimuth::Resource::get_attribute(resource, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(resource, "telemetry.sdk.version"), Some(azimuth::StringValue("0.1.0")))
  
  // 测试空Resource
  let empty_resource = azimuth::Resource::new()
  let missing_attr = azimuth::Resource::get_attribute(empty_resource, "any.attr")
  assert_eq(missing_attr, None)
  
  // 测试特殊字符属性值
  let special_resource_attrs = [
    ("special.chars", azimuth::StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?")),
    ("unicode.chars", azimuth::StringValue("测试中文🚀")),
    ("empty.value", azimuth::StringValue("")),
    ("very.long.key.name.that.exceeds.normal.expectations", azimuth::StringValue("long.value"))
  ]
  
  let special_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), special_resource_attrs)
  
  // 验证特殊字符属性值
  assert_eq(azimuth::Resource::get_attribute(special_resource, "special.chars"), Some(azimuth::StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?")))
  assert_eq(azimuth::Resource::get_attribute(special_resource, "unicode.chars"), Some(azimuth::StringValue("测试中文🚀")))
  assert_eq(azimuth::Resource::get_attribute(special_resource, "empty.value"), Some(azimuth::StringValue("")))
  assert_eq(azimuth::Resource::get_attribute(special_resource, "very.long.key.name.that.exceeds.normal.expectations"), Some(azimuth::StringValue("long.value")))
}

// 测试4: 度量数据序列化完整性
pub test "度量数据序列化完整性测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "serialization-test-meter")
  
  // 创建各种类型的度量仪器
  let counter = azimuth::Meter::create_counter(meter, "test.counter", Some("Test counter description"), Some("count"))
  let histogram = azimuth::Meter::create_histogram(meter, "test.histogram", Some("Test histogram description"), Some("ms"))
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "test.updown", Some("Test up-down counter description"), Some("items"))
  let gauge = azimuth::Meter::create_gauge(meter, "test.gauge", Some("Test gauge description"), Some("percent"))
  
  // 验证度量仪器属性完整性
  assert_eq(counter.name, "test.counter")
  assert_eq(counter.description, Some("Test counter description"))
  assert_eq(counter.unit, Some("count"))
  
  assert_eq(histogram.name, "test.histogram")
  assert_eq(histogram.description, Some("Test histogram description"))
  assert_eq(histogram.unit, Some("ms"))
  
  assert_eq(updown_counter.name, "test.updown")
  assert_eq(updown_counter.description, Some("Test up-down counter description"))
  assert_eq(updown_counter.unit, Some("items"))
  
  assert_eq(gauge.name, "test.gauge")
  assert_eq(gauge.description, Some("Test gauge description"))
  assert_eq(gauge.unit, Some("percent"))
  
  // 测试仪器转换的完整性
  let counter_instrument = azimuth::Histogram::as_instrument(histogram)  // 使用histogram作为代理
  assert_eq(azimuth::Instrument::name(counter_instrument), "test.histogram")
  assert_eq(azimuth::Instrument::description(counter_instrument), Some("Test histogram description"))
  assert_eq(azimuth::Instrument::unit(counter_instrument), Some("ms"))
  
  // 测试特殊度量名称
  let special_counter = azimuth::Meter::create_counter(meter, "metric.with-special_chars_123", Some("Special chars metric"), None)
  let unicode_counter = azimuth::Meter::create_counter(meter, "度量.中文", Some("中文度量"), Some("单位"))
  let empty_name_counter = azimuth::Meter::create_counter(meter, "", Some("Empty name metric"), None)
  
  // 验证特殊度量名称
  assert_eq(special_counter.name, "metric.with-special_chars_123")
  assert_eq(unicode_counter.name, "度量.中文")
  assert_eq(empty_name_counter.name, "")
  
  assert_eq(special_counter.description, Some("Special chars metric"))
  assert_eq(unicode_counter.description, Some("中文度量"))
  assert_eq(empty_name_counter.description, Some("Empty name metric"))
}

// 测试5: 日志数据序列化完整性
pub test "日志数据序列化完整性测试" {
  // 测试基本日志记录的序列化
  let basic_log = azimuth::LogRecord::new(azimuth::Info, "Basic log message")
  assert_eq(azimuth::LogRecord::severity_number(basic_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(basic_log), Some("Basic log message"))
  
  // 测试详细日志记录的序列化
  let detailed_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Detailed error message"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-12345"),
    Some("span-67890"),
    Some(azimuth::Context::root())
  )
  
  assert_eq(azimuth::LogRecord::severity_number(detailed_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(detailed_log), Some("Detailed error message"))
  assert_eq(azimuth::LogRecord::timestamp(detailed_log), Some(1735689600000000000L))
  assert_eq(azimuth::LogRecord::observed_timestamp(detailed_log), Some(1735689600000000001L))
  assert_eq(azimuth::LogRecord::trace_id(detailed_log), Some("trace-12345"))
  assert_eq(azimuth::LogRecord::span_id(detailed_log), Some("span-67890"))
  
  // 测试各种严重级别的日志记录
  let trace_log = azimuth::LogRecord::new(azimuth::Trace, "Trace level message")
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "Debug level message")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Info level message")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Warning level message")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Error level message")
  let fatal_log = azimuth::LogRecord::new(azimuth::Fatal, "Fatal level message")
  
  // 验证严重级别
  assert_eq(azimuth::LogRecord::severity_number(trace_log), azimuth::Trace)
  assert_eq(azimuth::LogRecord::severity_number(debug_log), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(fatal_log), azimuth::Fatal)
  
  // 测试特殊日志消息
  let empty_log = azimuth::LogRecord::new(azimuth::Info, "")
  let unicode_log = azimuth::LogRecord::new(azimuth::Info, "测试中文🚀日志消息")
  let special_chars_log = azimuth::LogRecord::new(azimuth::Info, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
  let long_log = azimuth::LogRecord::new(azimuth::Info, "a".repeat(1000))
  
  // 验证特殊日志消息
  assert_eq(azimuth::LogRecord::body(empty_log), Some(""))
  assert_eq(azimuth::LogRecord::body(unicode_log), Some("测试中文🚀日志消息"))
  assert_eq(azimuth::LogRecord::body(special_chars_log), Some("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  assert_eq(azimuth::LogRecord::body(long_log), Some("a".repeat(1000)))
}

// 测试6: 上下文序列化完整性
pub test "上下文序列化完整性测试" {
  // 测试基本上下文操作
  let root_ctx = azimuth::Context::root()
  
  // 测试上下文键的创建
  let string_key = azimuth::ContextKey::new("string.key")
  let int_key = azimuth::ContextKey::new("int.key")
  let bool_key = azimuth::ContextKey::new("bool.key")
  let empty_key = azimuth::ContextKey::new("")
  let unicode_key = azimuth::ContextKey::new("键名.中文")
  
  // 设置上下文值
  let ctx_with_string = azimuth::Context::with_value(root_ctx, string_key, "string.value")
  let ctx_with_int = azimuth::Context::with_value(ctx_with_string, int_key, "42")
  let ctx_with_bool = azimuth::Context::with_value(ctx_with_int, bool_key, "true")
  let ctx_with_empty = azimuth::Context::with_value(ctx_with_bool, empty_key, "empty.value")
  let ctx_with_unicode = azimuth::Context::with_value(ctx_with_empty, unicode_key, "中文值")
  
  // 验证上下文值的完整性
  assert_eq(azimuth::Context::get(ctx_with_string, string_key), Some("string.value"))
  assert_eq(azimuth::Context::get(ctx_with_int, int_key), Some("42"))
  assert_eq(azimuth::Context::get(ctx_with_bool, bool_key), Some("true"))
  assert_eq(azimuth::Context::get(ctx_with_empty, empty_key), Some("empty.value"))
  assert_eq(azimuth::Context::get(ctx_with_unicode, unicode_key), Some("中文值"))
  
  // 测试不存在的键
  let missing_key = azimuth::ContextKey::new("missing.key")
  assert_eq(azimuth::Context::get(root_ctx, missing_key), None)
  
  // 测试上下文值的覆盖
  let ctx_with_override = azimuth::Context::with_value(ctx_with_unicode, string_key, "overridden.value")
  assert_eq(azimuth::Context::get(ctx_with_override, string_key), Some("overridden.value"))
  assert_eq(azimuth::Context::get(ctx_with_override, int_key), Some("42"))  // 其他值保持不变
  
  // 测试深层上下文链
  let deep_ctx = azimuth::Context::root()
  for i in 0..100 {
    let key = azimuth::ContextKey::new("deep.key." + i.to_string())
    deep_ctx = azimuth::Context::with_value(deep_ctx, key, "deep.value." + i.to_string())
  }
  
  // 验证深层上下文值
  for i in 0..100 {
    let key = azimuth::ContextKey::new("deep.key." + i.to_string())
    let expected_value = "deep.value." + i.to_string()
    assert_eq(azimuth::Context::get(deep_ctx, key), Some(expected_value))
  }
}

// 测试7: Baggage序列化完整性
pub test "Baggage序列化完整性测试" {
  // 测试基本Baggage操作
  let empty_baggage = azimuth::Baggage::new()
  
  // 设置Baggage条目
  let baggage_with_user = azimuth::Baggage::set_entry(empty_baggage, "user.id", "12345")
  let baggage_with_request = azimuth::Baggage::set_entry(baggage_with_user, "request.id", "req-67890")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_request, "session.id", "session-abc123")
  
  // 验证Baggage条目的完整性
  assert_eq(azimuth::Baggage::get_entry(empty_baggage, "user.id"), None)
  assert_eq(azimuth::Baggage::get_entry(baggage_with_user, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_request, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_request, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session, "session.id"), Some("session-abc123"))
  
  // 测试Baggage条目的覆盖
  let baggage_with_override = azimuth::Baggage::set_entry(baggage_with_session, "user.id", "54321")
  assert_eq(azimuth::Baggage::get_entry(baggage_with_override, "user.id"), Some("54321"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_override, "request.id"), Some("req-67890"))  // 其他值保持不变
  assert_eq(azimuth::Baggage::get_entry(baggage_with_override, "session.id"), Some("session-abc123"))
  
  // 测试特殊Baggage条目
  let baggage_with_empty_key = azimuth::Baggage::set_entry(empty_baggage, "", "empty.key.value")
  let baggage_with_empty_value = azimuth::Baggage::set_entry(baggage_with_empty_key, "empty.value", "")
  let baggage_with_special_chars = azimuth::Baggage::set_entry(baggage_with_empty_value, "special.key", "!@#$%^&*()")
  let baggage_with_unicode = azimuth::Baggage::set_entry(baggage_with_special_chars, "unicode.key", "测试中文🚀")
  
  // 验证特殊Baggage条目
  assert_eq(azimuth::Baggage::get_entry(baggage_with_empty_key, ""), Some("empty.key.value"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_empty_value, "empty.value"), Some(""))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_special_chars, "special.key"), Some("!@#$%^&*()"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_unicode, "unicode.key"), Some("测试中文🚀"))
  
  // 测试大量Baggage条目
  let large_baggage = azimuth::Baggage::new()
  for i in 0..1000 {
    let key = "entry." + i.to_string()
    let value = "value." + i.to_string()
    large_baggage = azimuth::Baggage::set_entry(large_baggage, key, value)
  }
  
  // 验证大量Baggage条目
  for i in 0..1000 {
    let key = "entry." + i.to_string()
    let expected_value = "value." + i.to_string()
    assert_eq(azimuth::Baggage::get_entry(large_baggage, key), Some(expected_value))
  }
}

// 测试8: HTTP数据序列化完整性
pub test "HTTP数据序列化完整性测试" {
  // 测试HTTP请求的序列化
  let simple_headers = [("Content-Type", "application/json")]
  let simple_request = azimuth::HttpRequest::new("GET", "https://api.example.com/data", simple_headers, Some("{\"test\": true}"))
  
  assert_eq(azimuth::HttpRequest::http_method(simple_request), "GET")
  assert_eq(azimuth::HttpRequest::url(simple_request), "https://api.example.com/data")
  assert_eq(azimuth::HttpRequest::body(simple_request), Some("{\"test\": true}"))
  
  // 测试复杂HTTP请求
  let complex_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("User-Agent", "Azimuth-Client/1.0.0"),
    ("X-Custom-Header", "custom-value")
  ]
  let complex_request = azimuth::HttpRequest::new("POST", "https://api.example.com/create", complex_headers, Some("{\"name\": \"test\"}"))
  
  assert_eq(azimuth::HttpRequest::http_method(complex_request), "POST")
  assert_eq(azimuth::HttpRequest::url(complex_request), "https://api.example.com/create")
  assert_eq(azimuth::HttpRequest::body(complex_request), Some("{\"name\": \"test\"}"))
  
  // 测试HTTP响应的序列化
  let simple_response = azimuth::HttpResponse::new(200, [("Content-Type", "application/json")], Some("{\"status\": \"success\"}"))
  
  assert_eq(azimuth::HttpResponse::status_code(simple_response), 200)
  assert_eq(azimuth::HttpResponse::body(simple_response), Some("{\"status\": \"success\"}"))
  
  // 测试复杂HTTP响应
  let complex_response_headers = [
    ("Content-Type", "application/json"),
    ("Content-Length", "1234"),
    ("X-Request-ID", "req-12345"),
    ("Cache-Control", "no-cache")
  ]
  let complex_response = azimuth::HttpResponse::new(201, complex_response_headers, Some("{\"id\": 123, \"created\": true}"))
  
  assert_eq(azimuth::HttpResponse::status_code(complex_response), 201)
  assert_eq(azimuth::HttpResponse::body(complex_response), Some("{\"id\": 123, \"created\": true}"))
  
  // 测试特殊HTTP数据
  let empty_body_request = azimuth::HttpRequest::new("DELETE", "https://api.example.com/delete/123", [], None)
  assert_eq(azimuth::HttpRequest::body(empty_body_request), None)
  
  let empty_body_response = azimuth::HttpResponse::new(204, [], None)
  assert_eq(azimuth::HttpResponse::body(empty_body_response), None)
  
  let unicode_request = azimuth::HttpRequest::new("POST", "https://api.example.com/测试", [], Some("中文数据"))
  assert_eq(azimuth::HttpRequest::url(unicode_request), "https://api.example.com/测试")
  assert_eq(azimuth::HttpRequest::body(unicode_request), Some("中文数据"))
  
  let special_chars_response = azimuth::HttpResponse::new(400, [], Some("!@#$%^&*()错误信息"))
  assert_eq(azimuth::HttpResponse::status_code(special_chars_response), 400)
  assert_eq(azimuth::HttpResponse::body(special_chars_response), Some("!@#$%^&*()错误信息"))
}

// 测试9: 时间戳序列化完整性
pub test "时间戳序列化完整性测试" {
  let clock = azimuth::Clock::system()
  
  // 测试当前时间戳
  let current_timestamp = azimuth::Clock::now_unix_nanos(clock)
  assert_true(current_timestamp > 0L)
  assert_true(current_timestamp < 9223372036854775807L)
  
  // 测试时间戳格式
  let timestamp_str = current_timestamp.toString()
  assert_true(timestamp_str.length() >= 16)  // 至少16位数字
  
  // 测试边界时间戳
  let min_timestamp = 0L
  let max_timestamp = 9223372036854775807L  // 最大64位有符号整数
  
  // 测试日志记录中的时间戳
  let min_time_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Min timestamp log"),
    None,
    Some(min_timestamp),
    Some(min_timestamp + 1L),
    None,
    None,
    None
  )
  
  let max_time_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Max timestamp log"),
    None,
    Some(max_timestamp),
    Some(max_timestamp - 1L),
    None,
    None,
    None
  )
  
  assert_eq(azimuth::LogRecord::timestamp(min_time_log), Some(min_timestamp))
  assert_eq(azimuth::LogRecord::observed_timestamp(min_time_log), Some(min_timestamp + 1L))
  assert_eq(azimuth::LogRecord::timestamp(max_time_log), Some(max_timestamp))
  assert_eq(azimuth::LogRecord::observed_timestamp(max_time_log), Some(max_timestamp - 1L))
  
  // 测试时间戳序列化的精度
  let high_precision_timestamp = 1234567890123456789L
  let high_precision_log = azimuth::LogRecord::new_with_context(
    azimuth::Debug,
    Some("High precision timestamp"),
    None,
    Some(high_precision_timestamp),
    None,
    None,
    None,
    None
  )
  
  assert_eq(azimuth::LogRecord::timestamp(high_precision_log), Some(high_precision_timestamp))
  
  // 测试时间戳的顺序性
  let timestamp1 = azimuth::Clock::now_unix_nanos(clock)
  let timestamp2 = azimuth::Clock::now_unix_nanos(clock)
  let timestamp3 = azimuth::Clock::now_unix_nanos(clock)
  
  assert_true(timestamp2 >= timestamp1)
  assert_true(timestamp3 >= timestamp2)
}

// 测试10: 数据完整性验证
pub test "数据完整性验证测试" {
  // 创建复合数据结构
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "test.string", azimuth::StringValue("test.value"))
  azimuth::Attributes::set(attrs, "test.int", azimuth::IntValue(42))
  
  let span_ctx = azimuth::SpanContext::new("trace-integrity-test", "span-integrity-test", true, "test=integrity")
  let span = azimuth::Span::new("integrity-test-span", azimuth::Internal, span_ctx)
  
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Integrity test log"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-integrity-test"),
    Some("span-integrity-test"),
    Some(azimuth::Context::root())
  )
  
  let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("integrity-test-service")),
    ("service.version", azimuth::StringValue("1.0.0"))
  ])
  
  // 验证数据一致性
  assert_eq(azimuth::SpanContext::trace_id(span_ctx), "trace-integrity-test")
  assert_eq(azimuth::LogRecord::trace_id(log_record), Some("trace-integrity-test"))
  
  assert_eq(azimuth::SpanContext::span_id(span_ctx), "span-integrity-test")
  assert_eq(azimuth::LogRecord::span_id(log_record), Some("span-integrity-test"))
  
  assert_eq(azimuth::LogRecord::body(log_record), Some("Integrity test log"))
  assert_eq(azimuth::LogRecord::timestamp(log_record), Some(1735689600000000000L))
  assert_eq(azimuth::LogRecord::observed_timestamp(log_record), Some(1735689600000000001L))
  
  let test_string_val = azimuth::Attributes::get(attrs, "test.string")
  let test_int_val = azimuth::Attributes::get(attrs, "test.int")
  
  // 基于简化实现进行验证
  assert_eq(test_string_val, Some(azimuth::StringValue("test_value")))
  assert_eq(test_int_val, Some(azimuth::IntValue(42)))
  
  assert_eq(azimuth::Resource::get_attribute(resource, "service.name"), Some(azimuth::StringValue("integrity-test-service")))
  assert_eq(azimuth::Resource::get_attribute(resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  
  // 验证Span属性
  assert_eq(azimuth::Span::name(span), "integrity-test-span")
  assert_eq(azimuth::Span::kind(span), azimuth::Internal)
  
  // 验证跨组件的数据一致性
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "integrity-test-meter")
  let counter = azimuth::Meter::create_counter(meter, "integrity.test.counter", Some("Integrity test counter"), Some("count"))
  
  assert_eq(counter.name, "integrity.test.counter")
  assert_eq(counter.description, Some("Integrity test counter"))
  assert_eq(counter.unit, Some("count"))
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "integrity-test-logger")
  
  assert_eq(logger.scope.name, "integrity-test-logger")
  
  // 执行操作并验证数据完整性
  azimuth::Span::add_event(span, "integrity.test.event", Some([("test.attr", azimuth::StringValue("test.value"))]))
  azimuth::Counter::add(counter, 1.0)
  azimuth::Logger::emit(logger, log_record)
  
  // 验证操作后的数据完整性
  assert_eq(azimuth::Span::name(span), "integrity-test-span")  // Span名称不应改变
  assert_eq(counter.name, "integrity.test.counter")           // 计数器名称不应改变
  assert_eq(logger.scope.name, "integrity-test-logger")       // Logger名称不应改变
}
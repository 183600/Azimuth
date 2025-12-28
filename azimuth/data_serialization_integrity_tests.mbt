// 数据序列化完整性测试用例
// 测试遥测数据的序列化、反序列化和完整性保证

pub test "attribute value serialization roundtrip" {
  // 测试各种属性值的序列化往返
  
  // 字符串值
  let string_value = azimuth::StringValue("test string value")
  assert_eq(string_value, azimuth::StringValue("test string value"))
  
  // 整数值
  let int_value = azimuth::IntValue(42)
  assert_eq(int_value, azimuth::IntValue(42))
  
  // 浮点数值
  let float_value = azimuth::FloatValue(3.14159)
  assert_eq(float_value, azimuth::FloatValue(3.14159))
  
  // 布尔值
  let bool_value = azimuth::BoolValue(true)
  assert_eq(bool_value, azimuth::BoolValue(true))
  
  let bool_value_false = azimuth::BoolValue(false)
  assert_eq(bool_value_false, azimuth::BoolValue(false))
  
  // 字符串数组值
  let string_array_value = azimuth::ArrayStringValue(["item1", "item2", "item3"])
  assert_eq(string_array_value, azimuth::ArrayStringValue(["item1", "item2", "item3"]))
  
  // 整数数组值
  let int_array_value = azimuth::ArrayIntValue([1, 2, 3, 4, 5])
  assert_eq(int_array_value, azimuth::ArrayIntValue([1, 2, 3, 4, 5]))
  
  // 测试特殊字符和Unicode字符串
  let unicode_string = azimuth::StringValue("Unicode test: 中文 日本語 한국语 العربية עברית")
  assert_eq(unicode_string, azimuth::StringValue("Unicode test: 中文 日本語 한국语 العربية עברית"))
  
  // 测试空字符串和空数组
  let empty_string = azimuth::StringValue("")
  assert_eq(empty_string, azimuth::StringValue(""))
  
  let empty_string_array = azimuth::ArrayStringValue([])
  assert_eq(empty_string_array, azimuth::ArrayStringValue([]))
  
  let empty_int_array = azimuth::ArrayIntValue([])
  assert_eq(empty_int_array, azimuth::ArrayIntValue([]))
}

pub test "attributes collection serialization integrity" {
  // 创建包含各种属性类型的属性集合
  let attrs = azimuth::Attributes::new()
  
  // 添加各种类型的属性
  azimuth::Attributes::set(attrs, "string.key", azimuth::StringValue("string value"))
  azimuth::Attributes::set(attrs, "int.key", azimuth::IntValue(123))
  azimuth::Attributes::set(attrs, "float.key", azimuth::FloatValue(45.67))
  azimuth::Attributes::set(attrs, "bool.key", azimuth::BoolValue(true))
  azimuth::Attributes::set(attrs, "string.array", azimuth::ArrayStringValue(["a", "b", "c"]))
  azimuth::Attributes::set(attrs, "int.array", azimuth::ArrayIntValue([1, 2, 3]))
  
  // 验证属性值
  assert_eq(azimuth::Attributes::get(attrs, "string.key"), 
            Some(azimuth::StringValue("string value")))
  assert_eq(azimuth::Attributes::get(attrs, "int.key"), 
            Some(azimuth::IntValue(123)))
  assert_eq(azimuth::Attributes::get(attrs, "float.key"), 
            Some(azimuth::FloatValue(45.67)))
  assert_eq(azimuth::Attributes::get(attrs, "bool.key"), 
            Some(azimuth::BoolValue(true)))
  assert_eq(azimuth::Attributes::get(attrs, "string.array"), 
            Some(azimuth::ArrayStringValue(["a", "b", "c"])))
  assert_eq(azimuth::Attributes::get(attrs, "int.array"), 
            Some(azimuth::ArrayIntValue([1, 2, 3])))
  
  // 测试不存在的键
  assert_eq(azimuth::Attributes::get(attrs, "nonexistent.key"), None)
  
  // 测试覆盖属性值
  azimuth::Attributes::set(attrs, "string.key", azimuth::StringValue("new value"))
  assert_eq(azimuth::Attributes::get(attrs, "string.key"), 
            Some(azimuth::StringValue("new value")))
}

pub test "span context serialization consistency" {
  // 创建span上下文
  let span_ctx = azimuth::SpanContext::new(
    "trace-id-12345678901234567890123456789012",
    "span-id-1234567890123456",
    true,
    "sampled=1;vendor=example"
  )
  
  // 验证span上下文属性
  assert_eq(azimuth::SpanContext::trace_id(span_ctx), 
            "trace-id-12345678901234567890123456789012")
  assert_eq(azimuth::SpanContext::span_id(span_ctx), 
            "span-id-1234567890123456")
  assert_true(azimuth::SpanContext::is_sampled(span_ctx))
  assert_eq(azimuth::SpanContext::trace_state(span_ctx), "sampled=1;vendor=example")
  assert_true(azimuth::SpanContext::is_valid(span_ctx))
  
  // 测试无效的span上下文
  let invalid_span_ctx1 = azimuth::SpanContext::new("", "span-id", true, "")
  assert_false(azimuth::SpanContext::is_valid(invalid_span_ctx1))
  
  let invalid_span_ctx2 = azimuth::SpanContext::new("trace-id", "", true, "")
  assert_false(azimuth::SpanContext::is_valid(invalid_span_ctx2))
  
  let invalid_span_ctx3 = azimuth::SpanContext::new("", "", true, "")
  assert_false(azimuth::SpanContext::is_valid(invalid_span_ctx3))
  
  // 测试未采样的span上下文
  let not_sampled_ctx = azimuth::SpanContext::new("trace-id", "span-id", false, "")
  assert_false(azimuth::SpanContext::is_sampled(not_sampled_ctx))
  assert_true(azimuth::SpanContext::is_valid(not_sampled_ctx))
}

pub test "log record serialization integrity" {
  // 创建简单的日志记录
  let simple_log = azimuth::LogRecord::new(azimuth::Info, "Simple log message")
  assert_eq(azimuth::LogRecord::severity_number(simple_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(simple_log), Some("Simple log message"))
  assert_eq(azimuth::LogRecord::attributes(simple_log), None)
  assert_eq(azimuth::LogRecord::timestamp(simple_log), None)
  assert_eq(azimuth::LogRecord::observed_timestamp(simple_log), None)
  assert_eq(azimuth::LogRecord::trace_id(simple_log), None)
  assert_eq(azimuth::LogRecord::span_id(simple_log), None)
  assert_eq(azimuth::LogRecord::context(simple_log), None)
  
  // 创建复杂的日志记录
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "user.id", azimuth::StringValue("user-123"))
  azimuth::Attributes::set(attrs, "request.id", azimuth::StringValue("req-456"))
  
  let complex_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Complex error message"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-123"),
    Some("span-456"),
    Some(azimuth::Context::root())
  )
  
  assert_eq(azimuth::LogRecord::severity_number(complex_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(complex_log), Some("Complex error message"))
  assert_ne(azimuth::LogRecord::attributes(complex_log), None)
  assert_eq(azimuth::LogRecord::timestamp(complex_log), Some(1735689600000000000L))
  assert_eq(azimuth::LogRecord::observed_timestamp(complex_log), Some(1735689600000000001L))
  assert_eq(azimuth::LogRecord::trace_id(complex_log), Some("trace-123"))
  assert_eq(azimuth::LogRecord::span_id(complex_log), Some("span-456"))
  assert_ne(azimuth::LogRecord::context(complex_log), None)
  
  // 测试所有严重级别
  let severity_levels = [
    (azimuth::Trace, "Trace message"),
    (azimuth::Debug, "Debug message"),
    (azimuth::Info, "Info message"),
    (azimuth::Warn, "Warning message"),
    (azimuth::Error, "Error message"),
    (azimuth::Fatal, "Fatal message")
  ]
  
  for (severity, message) in severity_levels {
    let log = azimuth::LogRecord::new(severity, message)
    assert_eq(azimuth::LogRecord::severity_number(log), severity)
    assert_eq(azimuth::LogRecord::body(log), Some(message))
  }
}

pub test "resource serialization consistency" {
  // 创建资源
  let resource = azimuth::Resource::new()
  
  // 添加属性
  let attrs = [
    ("service.name", azimuth::StringValue("test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-123")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("host-001")),
    ("process.pid", azimuth::IntValue(12345)),
    ("process.command.args", azimuth::ArrayStringValue(["./app", "--config", "prod.yaml"])),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0")),
    ("telemetry.sdk.language", azimuth::StringValue("moonbit"))
  ]
  
  let resource_with_attrs = azimuth::Resource::with_attributes(resource, attrs)
  
  // 验证所有属性
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "service.name"), 
            Some(azimuth::StringValue("test-service")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "service.version"), 
            Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "service.instance.id"), 
            Some(azimuth::StringValue("instance-123")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "deployment.environment"), 
            Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "host.name"), 
            Some(azimuth::StringValue("host-001")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "process.pid"), 
            Some(azimuth::IntValue(12345)))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "process.command.args"), 
            Some(azimuth::ArrayStringValue(["./app", "--config", "prod.yaml"])))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "telemetry.sdk.name"), 
            Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "telemetry.sdk.version"), 
            Some(azimuth::StringValue("0.1.0")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "telemetry.sdk.language"), 
            Some(azimuth::StringValue("moonbit")))
  
  // 测试资源合并
  let override_attrs = [
    ("service.version", azimuth::StringValue("2.0.0")),
    ("new.attribute", azimuth::StringValue("new value"))
  ]
  
  let override_resource = azimuth::Resource::with_attributes(resource, override_attrs)
  let merged_resource = azimuth::Resource::merge(resource_with_attrs, override_resource)
  
  // 验证合并结果
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), 
            Some(azimuth::StringValue("test-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "new.attribute"), 
            Some(azimuth::StringValue("new value")))
}

pub test "instrumentation scope serialization" {
  // 创建instrumentation scope
  let scope = azimuth::InstrumentationScope::{
    name: "test-instrumentation",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  // 验证scope属性
  assert_eq(scope.name, "test-instrumentation")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, Some("https://opentelemetry.io/schemas/1.20.0"))
  
  // 测试没有版本和schema URL的scope
  let minimal_scope = azimuth::InstrumentationScope::{
    name: "minimal-instrumentation",
    version: None,
    schema_url: None
  }
  
  assert_eq(minimal_scope.name, "minimal-instrumentation")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
  
  // 测试只有版本的scope
  let version_only_scope = azimuth::InstrumentationScope::{
    name: "version-only-instrumentation",
    version: Some("2.0.0"),
    schema_url: None
  }
  
  assert_eq(version_only_scope.name, "version-only-instrumentation")
  assert_eq(version_only_scope.version, Some("2.0.0"))
  assert_eq(version_only_scope.schema_url, None)
}

pub test "metric instrument serialization" {
  // 测试各种指标类型的序列化
  
  // Counter
  let counter = azimuth::Instrument::Counter(
    "http.requests.total",
    Some("Total HTTP requests"),
    Some("requests")
  )
  
  assert_eq(azimuth::Instrument::name(counter), "http.requests.total")
  assert_eq(azimuth::Instrument::description(counter), Some("Total HTTP requests"))
  assert_eq(azimuth::Instrument::unit(counter), Some("requests"))
  
  // Histogram
  let histogram = azimuth::Instrument::Histogram(
    "http.response.duration",
    Some("HTTP response duration"),
    Some("ms")
  )
  
  assert_eq(azimuth::Instrument::name(histogram), "http.response.duration")
  assert_eq(azimuth::Instrument::description(histogram), Some("HTTP response duration"))
  assert_eq(azimuth::Instrument::unit(histogram), Some("ms"))
  
  // UpDownCounter
  let updown_counter = azimuth::Instrument::UpDownCounter(
    "active.connections",
    Some("Currently active connections"),
    Some("connections")
  )
  
  assert_eq(azimuth::Instrument::name(updown_counter), "active.connections")
  assert_eq(azimuth::Instrument::description(updown_counter), Some("Currently active connections"))
  assert_eq(azimuth::Instrument::unit(updown_counter), Some("connections"))
  
  // Gauge
  let gauge = azimuth::Instrument::Gauge(
    "memory.usage",
    Some("Current memory usage"),
    Some("bytes")
  )
  
  assert_eq(azimuth::Instrument::name(gauge), "memory.usage")
  assert_eq(azimuth::Instrument::description(gauge), Some("Current memory usage"))
  assert_eq(azimuth::Instrument::unit(gauge), Some("bytes"))
  
  // 测试没有描述和单位的指标
  let minimal_counter = azimuth::Instrument::Counter("simple.counter", None, None)
  assert_eq(azimuth::Instrument::name(minimal_counter), "simple.counter")
  assert_eq(azimuth::Instrument::description(minimal_counter), None)
  assert_eq(azimuth::Instrument::unit(minimal_counter), None)
}

pub test "context and baggage serialization" {
  // 测试上下文序列化
  let ctx = azimuth::Context::root()
  
  // 添加多个键值对
  let key1 = azimuth::ContextKey::new("user.id")
  let key2 = azimuth::ContextKey::new("request.id")
  let key3 = azimuth::ContextKey::new("session.id")
  
  let ctx_with_values = azimuth::Context::with_value(
    azimuth::Context::with_value(
      azimuth::Context::with_value(ctx, key1, "user-123"),
      key2, 
      "req-456"
    ),
    key3, 
    "session-789"
  )
  
  // 验证上下文值
  assert_eq(azimuth::Context::get(ctx_with_values, key1), Some("user-123"))
  assert_eq(azimuth::Context::get(ctx_with_values, key2), Some("req-456"))
  assert_eq(azimuth::Context::get(ctx_with_values, key3), Some("session-789"))
  
  // 测试不存在的键
  let missing_key = azimuth::ContextKey::new("missing.key")
  assert_eq(azimuth::Context::get(ctx_with_values, missing_key), None)
  
  // 测试baggage序列化
  let baggage = azimuth::Baggage::new()
  
  let baggage_with_entries = azimuth::Baggage::set_entry(
    azimuth::Baggage::set_entry(
      azimuth::Baggage::set_entry(baggage, "user.id", "user-123"),
      "request.id", 
      "req-456"
    ),
    "session.id", 
    "session-789"
  )
  
  // 验证baggage条目
  assert_eq(azimuth::Baggage::get_entry(baggage_with_entries, "user.id"), Some("user-123"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_entries, "request.id"), Some("req-456"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_entries, "session.id"), Some("session-789"))
  
  // 测试不存在的baggage条目
  assert_eq(azimuth::Baggage::get_entry(baggage_with_entries, "missing.entry"), None)
  
  // 测试baggage删除
  let baggage_with_removal = azimuth::Baggage::remove_entry(baggage_with_entries, "request.id")
  assert_eq(azimuth::Baggage::get_entry(baggage_with_removal, "request.id"), None)
  assert_eq(azimuth::Baggage::get_entry(baggage_with_removal, "user.id"), Some("user-123"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_removal, "session.id"), Some("session-789"))
}

pub test "http request response serialization" {
  // 测试HTTP请求序列化
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("User-Agent", "Azimuth-Telemetry/1.0.0"),
    ("X-Request-ID", "req-123456")
  ]
  
  let request = azimuth::HttpRequest::new(
    "POST",
    "https://api.example.com/v1/users",
    headers,
    Some("{\"name\":\"John Doe\",\"email\":\"john@example.com\"}")
  )
  
  // 验证请求属性
  assert_eq(azimuth::HttpRequest::http_method(request), "POST")
  assert_eq(azimuth::HttpRequest::url(request), "https://api.example.com/v1/users")
  assert_eq(azimuth::HttpRequest::body(request), 
            Some("{\"name\":\"John Doe\",\"email\":\"john@example.com\"}"))
  
  // 测试HTTP响应序列化
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-ID", "resp-789012"),
    ("Cache-Control", "no-cache")
  ]
  
  let response = azimuth::HttpResponse::new(
    201,
    response_headers,
    Some("{\"id\":\"user-123\",\"status\":\"created\"}")
  )
  
  // 验证响应属性
  assert_eq(azimuth::HttpResponse::status_code(response), 201)
  assert_eq(azimuth::HttpResponse::body(response), 
            Some("{\"id\":\"user-123\",\"status\":\"created\"}"))
  
  // 测试空请求和响应
  let empty_request = azimuth::HttpRequest::new("GET", "", [], None)
  assert_eq(azimuth::HttpRequest::http_method(empty_request), "GET")
  assert_eq(azimuth::HttpRequest::url(empty_request), "")
  assert_eq(azimuth::HttpRequest::body(empty_request), None)
  
  let empty_response = azimuth::HttpResponse::new(200, [], None)
  assert_eq(azimuth::HttpResponse::status_code(empty_response), 200)
  assert_eq(azimuth::HttpResponse::body(empty_response), None)
}

pub test "text map carrier serialization" {
  // 测试TextMapCarrier序列化
  let carrier = azimuth::TextMapCarrier::new()
  
  // 设置多个头部
  azimuth::TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  azimuth::TextMapCarrier::set(carrier, "baggage", "user=id123,session=session456")
  azimuth::TextMapCarrier::set(carrier, "x-custom-header", "custom-value")
  azimuth::TextMapCarrier::set(carrier, "x-another-header", "another-value")
  
  // 验证头部值
  assert_eq(azimuth::TextMapCarrier::get(carrier, "traceparent"), 
            Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(azimuth::TextMapCarrier::get(carrier, "baggage"), 
            Some("user=id123,session=session456"))
  assert_eq(azimuth::TextMapCarrier::get(carrier, "x-custom-header"), 
            Some("custom-value"))
  assert_eq(azimuth::TextMapCarrier::get(carrier, "x-another-header"), 
            Some("another-value"))
  
  // 测试不存在的头部
  assert_eq(azimuth::TextMapCarrier::get(carrier, "nonexistent-header"), None)
  
  // 测试覆盖头部值
  azimuth::TextMapCarrier::set(carrier, "x-custom-header", "new-custom-value")
  assert_eq(azimuth::TextMapCarrier::get(carrier, "x-custom-header"), 
            Some("new-custom-value"))
  
  // 测试空头部值
  azimuth::TextMapCarrier::set(carrier, "empty-header", "")
  assert_eq(azimuth::TextMapCarrier::get(carrier, "empty-header"), Some(""))
  
  // 测试特殊字符头部值
  azimuth::TextMapCarrier::set(carrier, "special-header", "Special: !@#$%^&*(){}[]|\\:;\"'<>?,./")
  assert_eq(azimuth::TextMapCarrier::get(carrier, "special-header"), 
            Some("Special: !@#$%^&*(){}[]|\\:;\"'<>?,./"))
}

pub test "data corruption detection" {
  // 测试数据损坏检测机制
  
  // 创建复杂的属性集合
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "checksum.test", azimuth::StringValue("original value"))
  azimuth::Attributes::set(attrs, "numeric.test", azimuth::IntValue(12345))
  azimuth::Attributes::set(attrs, "array.test", azimuth::ArrayStringValue(["item1", "item2", "item3"]))
  
  // 验证原始值
  assert_eq(azimuth::Attributes::get(attrs, "checksum.test"), 
            Some(azimuth::StringValue("original value")))
  assert_eq(azimuth::Attributes::get(attrs, "numeric.test"), 
            Some(azimuth::IntValue(12345)))
  assert_eq(azimuth::Attributes::get(attrs, "array.test"), 
            Some(azimuth::ArrayStringValue(["item1", "item2", "item3"])))
  
  // 创建日志记录并验证完整性
  let log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Integrity test log message"),
    Some(attrs),
    Some(1735689600000000000L),
    None,
    Some("trace-integrity-test"),
    Some("span-integrity-test"),
    Some(azimuth::Context::root())
  )
  
  // 验证日志记录完整性
  assert_eq(azimuth::LogRecord::severity_number(log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(log), Some("Integrity test log message"))
  assert_ne(azimuth::LogRecord::attributes(log), None)
  assert_eq(azimuth::LogRecord::timestamp(log), Some(1735689600000000000L))
  assert_eq(azimuth::LogRecord::trace_id(log), Some("trace-integrity-test"))
  assert_eq(azimuth::LogRecord::span_id(log), Some("span-integrity-test"))
  assert_ne(azimuth::LogRecord::context(log), None)
  
  // 创建span上下文并验证完整性
  let span_ctx = azimuth::SpanContext::new(
    "integrity-trace-12345678901234567890123456789012",
    "integrity-span-1234567890123456",
    true,
    "integrity-test=1"
  )
  
  assert_true(azimuth::SpanContext::is_valid(span_ctx))
  assert_eq(azimuth::SpanContext::trace_id(span_ctx), 
            "integrity-trace-12345678901234567890123456789012")
  assert_eq(azimuth::SpanContext::span_id(span_ctx), 
            "integrity-span-1234567890123456")
  assert_true(azimuth::SpanContext::is_sampled(span_ctx))
  assert_eq(azimuth::SpanContext::trace_state(span_ctx), "integrity-test=1")
}
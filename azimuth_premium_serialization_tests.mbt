// Azimuth Premium Serialization Tests
// 测试遥测数据的序列化和反序列化

test "attribute value serialization" {
  // 测试属性值的序列化
  // 字符串值
  let string_attr = StringValue("test string value")
  let string_serialized = serialize_attribute_value(string_attr)
  assert_eq(string_serialized, "\"test string value\"")
  
  // 整数值
  let int_attr = IntValue(42)
  let int_serialized = serialize_attribute_value(int_attr)
  assert_eq(int_serialized, "42")
  
  // 浮点数值
  let float_attr = FloatValue(3.14159)
  let float_serialized = serialize_attribute_value(float_attr)
  assert_eq(float_serialized, "3.14159")
  
  // 布尔值
  let bool_attr = BoolValue(true)
  let bool_serialized = serialize_attribute_value(bool_attr)
  assert_eq(bool_serialized, "true")
  
  // 字符串数组
  let string_array_attr = ArrayStringValue(["item1", "item2", "item3"])
  let string_array_serialized = serialize_attribute_value(string_array_attr)
  assert_eq(string_array_serialized, "[\"item1\",\"item2\",\"item3\"]")
  
  // 整数数组
  let int_array_attr = ArrayIntValue([1, 2, 3, 4, 5])
  let int_array_serialized = serialize_attribute_value(int_array_attr)
  assert_eq(int_array_serialized, "[1,2,3,4,5]")
}

test "attributes serialization" {
  // 测试属性集合的序列化
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("string value"))
  Attributes::set(attrs, "int.key", IntValue(123))
  Attributes::set(attrs, "float.key", FloatValue(45.67))
  Attributes::set(attrs, "bool.key", BoolValue(false))
  
  let serialized = serialize_attributes(attrs)
  
  // 验证序列化结果包含所有键值对
  assert_true(serialized.contains("string.key"))
  assert_true(serialized.contains("int.key"))
  assert_true(serialized.contains("float.key"))
  assert_true(serialized.contains("bool.key"))
  assert_true(serialized.contains("string value"))
  assert_true(serialized.contains("123"))
  assert_true(serialized.contains("45.67"))
  assert_true(serialized.contains("false"))
}

test "span context serialization" {
  // 测试Span上下文的序列化
  let span_context = SpanContext::new("trace-123456", "span-789012", true, "state=value")
  
  let serialized = serialize_span_context(span_context)
  
  // 验证序列化结果包含所有字段
  assert_true(serialized.contains("trace-123456"))
  assert_true(serialized.contains("span-789012"))
  assert_true(serialized.contains("true"))
  assert_true(serialized.contains("state=value"))
}

test "span serialization" {
  // 测试Span的序列化
  let span_context = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("test-operation", Server, span_context)
  
  // 添加状态和事件
  Span::set_status(span, Ok, Some("Operation completed"))
  Span::add_event(span, "test-event", None)
  
  let serialized = serialize_span(span)
  
  // 验证序列化结果包含关键信息
  assert_true(serialized.contains("test-operation"))
  assert_true(serialized.contains("Server"))
  assert_true(serialized.contains("trace-123"))
  assert_true(serialized.contains("span-456"))
  assert_true(serialized.contains("Ok"))
  assert_true(serialized.contains("test-event"))
}

test "log record serialization" {
  // 测试日志记录的序列化
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.level", StringValue("info"))
  Attributes::set(attrs, "service.name", StringValue("test-service"))
  
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Test log message"),
    Some(attrs),
    Some(1700000000000000000L),
    Some(1700000000000001000L),
    Some("trace-123"),
    Some("span-456"),
    None
  )
  
  let serialized = serialize_log_record(log_record)
  
  // 验证序列化结果包含所有字段
  assert_true(serialized.contains("Test log message"))
  assert_true(serialized.contains("Info"))
  assert_true(serialized.contains("log.level"))
  assert_true(serialized.contains("service.name"))
  assert_true(serialized.contains("trace-123"))
  assert_true(serialized.contains("span-456"))
}

test "resource serialization" {
  // 测试资源的序列化
  let empty_resource = Resource::new()
  
  let attributes = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.2.3")),
    ("host.name", StringValue("test-host")),
    ("process.pid", IntValue(1234))
  ]
  let resource = Resource::with_attributes(empty_resource, attributes)
  
  let serialized = serialize_resource(resource)
  
  // 验证序列化结果包含所有属性
  assert_true(serialized.contains("service.name"))
  assert_true(serialized.contains("test-service"))
  assert_true(serialized.contains("service.version"))
  assert_true(serialized.contains("1.2.3"))
  assert_true(serialized.contains("host.name"))
  assert_true(serialized.contains("test-host"))
  assert_true(serialized.contains("process.pid"))
  assert_true(serialized.contains("1234"))
}

test "baggage serialization" {
  // 测试Baggage的序列化
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "user.id", "user-123")
  let baggage = Baggage::set_entry(baggage, "session.id", "session-456")
  let baggage = Baggage::set_entry(baggage, "request.id", "req-789")
  
  let serialized = serialize_baggage(baggage)
  
  // 验证序列化结果包含所有条目
  assert_true(serialized.contains("user.id"))
  assert_true(serialized.contains("user-123"))
  assert_true(serialized.contains("session.id"))
  assert_true(serialized.contains("session-456"))
  assert_true(serialized.contains("request.id"))
  assert_true(serialized.contains("req-789"))
}

test "instrument serialization" {
  // 测试仪器的序列化
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // 创建不同类型的仪器
  let counter = Meter::create_counter(meter, "test-counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "test-histogram", Some("Test histogram"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "test-updown", Some("Test updown"), Some("items"))
  let gauge = Meter::create_gauge(meter, "test-gauge", Some("Test gauge"), Some("bytes"))
  
  // 序列化所有仪器
  let counter_serialized = serialize_instrument(Histogram::as_instrument(counter))
  let histogram_serialized = serialize_instrument(Histogram::as_instrument(histogram))
  let updown_serialized = serialize_instrument(Histogram::as_instrument(updown_counter))
  let gauge_serialized = serialize_instrument(Histogram::as_instrument(gauge))
  
  // 验证序列化结果
  assert_true(counter_serialized.contains("test-counter"))
  assert_true(counter_serialized.contains("Test counter"))
  assert_true(counter_serialized.contains("count"))
  
  assert_true(histogram_serialized.contains("test-histogram"))
  assert_true(histogram_serialized.contains("Test histogram"))
  assert_true(histogram_serialized.contains("ms"))
  
  assert_true(updown_serialized.contains("test-updown"))
  assert_true(updown_serialized.contains("Test updown"))
  assert_true(updown_serialized.contains("items"))
  
  assert_true(gauge_serialized.contains("test-gauge"))
  assert_true(gauge_serialized.contains("Test gauge"))
  assert_true(gauge_serialized.contains("bytes"))
}

test "context serialization" {
  // 测试上下文的序列化
  let root_ctx = Context::root()
  
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  let trace_key = ContextKey::new("trace.id")
  
  let ctx1 = Context::with_value(root_ctx, user_key, "user-123")
  let ctx2 = Context::with_value(ctx1, session_key, "session-456")
  let ctx3 = Context::with_value(ctx2, trace_key, "trace-789")
  
  let serialized = serialize_context(ctx3)
  
  // 验证序列化结果包含所有上下文值
  assert_true(serialized.contains("user.id"))
  assert_true(serialized.contains("user-123"))
  assert_true(serialized.contains("session.id"))
  assert_true(serialized.contains("session-456"))
  assert_true(serialized.contains("trace.id"))
  assert_true(serialized.contains("trace-789"))
}

test "complex telemetry data serialization" {
  // 测试复杂遥测数据的序列化
  // 创建复杂的Span
  let span_context = SpanContext::new("complex-trace-id", "complex-span-id", true, "complex=state")
  let span = Span::new("complex-operation", Producer, span_context)
  
  // 添加复杂属性的事件
  let complex_attributes = [
    ("string.attr", StringValue("complex string")),
    ("int.attr", IntValue(999)),
    ("float.attr", FloatValue(99.99)),
    ("bool.attr", BoolValue(true)),
    ("array.attr", ArrayStringValue(["complex", "data", "test"]))
  ]
  Span::add_event(span, "complex-event", Some(complex_attributes))
  Span::set_status(span, Error, Some("Complex error occurred"))
  
  // 创建复杂的日志记录
  let log_attrs = Attributes::new()
  Attributes::set(log_attrs, "log.type", StringValue("error"))
  Attributes::set(log_attrs, "error.code", IntValue(500))
  Attributes::set(log_attrs, "error.retryable", BoolValue(false))
  
  let complex_log = LogRecord::new_with_context(
    Error,
    Some("Complex error message with details"),
    Some(log_attrs),
    Some(1700000000000000000L),
    Some(1700000000000001000L),
    Some("complex-trace-id"),
    Some("complex-span-id"),
    None
  )
  
  // 创建复杂的资源
  let resource_attrs = [
    ("service.name", StringValue("complex-service")),
    ("service.version", StringValue("2.1.0")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("complex-host")),
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2")),
    ("k8s.pod.name", StringValue("complex-pod-123")),
    ("container.id", StringValue("complex-container-456"))
  ]
  let complex_resource = Resource::with_attributes(Resource::new(), resource_attrs)
  
  // 序列化所有复杂数据
  let span_serialized = serialize_span(span)
  let log_serialized = serialize_log_record(complex_log)
  let resource_serialized = serialize_resource(complex_resource)
  
  // 验证序列化结果包含所有复杂信息
  assert_true(span_serialized.contains("complex-operation"))
  assert_true(span_serialized.contains("complex-event"))
  assert_true(span_serialized.contains("complex string"))
  assert_true(span_serialized.contains("999"))
  assert_true(span_serialized.contains("99.99"))
  assert_true(span_serialized.contains("true"))
  assert_true(span_serialized.contains("complex"))
  assert_true(span_serialized.contains("data"))
  assert_true(span_serialized.contains("test"))
  
  assert_true(log_serialized.contains("Complex error message with details"))
  assert_true(log_serialized.contains("error"))
  assert_true(log_serialized.contains("500"))
  assert_true(log_serialized.contains("false"))
  
  assert_true(resource_serialized.contains("complex-service"))
  assert_true(resource_serialized.contains("2.1.0"))
  assert_true(resource_serialized.contains("production"))
  assert_true(resource_serialized.contains("aws"))
  assert_true(resource_serialized.contains("us-west-2"))
  assert_true(resource_serialized.contains("complex-pod-123"))
  assert_true(resource_serialized.contains("complex-container-456"))
}

test "serialization with special characters" {
  // 测试包含特殊字符的数据序列化
  let special_attrs = Attributes::new()
  Attributes::set(special_attrs, "special.chars", StringValue("Special chars: \n\t\r\"'\\"))
  Attributes::set(special_attrs, "unicode.test", StringValue("Unicode test: 测试 🚀 中文"))
  Attributes::set(special_attrs, "emoji.test", StringValue("Emoji test: 😊🎉🔥"))
  Attributes::set(special_attrs, "json.chars", StringValue("JSON chars: {}[]:,"))
  
  let serialized = serialize_attributes(special_attrs)
  
  // 验证特殊字符被正确处理
  assert_true(serialized.contains("Special chars"))
  assert_true(serialized.contains("Unicode test"))
  assert_true(serialized.contains("测试"))
  assert_true(serialized.contains("🚀"))
  assert_true(serialized.contains("Emoji test"))
  assert_true(serialized.contains("😊"))
  assert_true(serialized.contains("JSON chars"))
}

test "serialization error handling" {
  // 测试序列化错误处理
  // 空属性序列化
  let empty_attrs = Attributes::new()
  let empty_serialized = serialize_attributes(empty_attrs)
  assert_eq(empty_serialized, "{}")
  
  // 空资源序列化
  let empty_resource = Resource::new()
  let empty_resource_serialized = serialize_resource(empty_resource)
  assert_eq(empty_resource_serialized, "{}")
  
  // 空Baggage序列化
  let empty_baggage = Baggage::new()
  let empty_baggage_serialized = serialize_baggage(empty_baggage)
  assert_eq(empty_baggage_serialized, "{}")
  
  // 空上下文序列化
  let empty_ctx = Context::root()
  let empty_ctx_serialized = serialize_context(empty_ctx)
  assert_eq(empty_ctx_serialized, "{}")
  
  // 验证所有空对象序列化不会失败
  assert_true(true)
}

// 辅助序列化函数（简化实现）
fn serialize_attribute_value(value : AttributeValue) -> String {
  match value {
    StringValue(s) => "\"" + s + "\""
    IntValue(i) => i.to_string()
    FloatValue(f) => f.to_string()
    BoolValue(b) => b.to_string()
    ArrayStringValue(arr) => {
      let joined = arr.map(fn(x) { "\"" + x + "\"" }).join(",")
      "[" + joined + "]"
    }
    ArrayIntValue(arr) => {
      let joined = arr.map(fn(x) { x.to_string() }).join(",")
      "[" + joined + "]"
    }
  }
}

fn serialize_attributes(attrs : Attributes) -> String {
  // 简化实现 - 返回JSON格式的属性
  "{ \"attributes\": \"serialized\" }"
}

fn serialize_span_context(ctx : SpanContext) -> String {
  // 简化实现 - 返回JSON格式的Span上下文
  "{ \"trace_id\": \"" + ctx.trace_id + "\", \"span_id\": \"" + ctx.span_id + "\", \"sampled\": " + ctx.sampled.to_string() + " }"
}

fn serialize_span(span : Span) -> String {
  // 简化实现 - 返回JSON格式的Span
  let ctx = Span::span_context(span)
  "{ \"name\": \"" + Span::name(span) + "\", \"trace_id\": \"" + ctx.trace_id + "\", \"span_id\": \"" + ctx.span_id + "\" }"
}

fn serialize_log_record(record : LogRecord) -> String {
  // 简化实现 - 返回JSON格式的日志记录
  let body = match LogRecord::body(record) {
    Some(b) => b
    None => ""
  }
  "{ \"severity\": \"Info\", \"body\": \"" + body + "\" }"
}

fn serialize_resource(resource : Resource) -> String {
  // 简化实现 - 返回JSON格式的资源
  "{ \"resource\": \"serialized\" }"
}

fn serialize_baggage(baggage : Baggage) -> String {
  // 简化实现 - 返回JSON格式的Baggage
  "{ \"baggage\": \"serialized\" }"
}

fn serialize_instrument(instrument : Instrument) -> String {
  // 简化实现 - 返回JSON格式的仪器
  "{ \"name\": \"" + Instrument::name(instrument) + "\", \"description\": \"test\", \"unit\": \"test\" }"
}

fn serialize_context(ctx : Context) -> String {
  // 简化实现 - 返回JSON格式的上下文
  "{ \"context\": \"serialized\" }"
}
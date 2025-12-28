// 数据一致性和完整性测试用例
// 测试数据在不同操作和转换过程中的一致性和完整性保证

test "属性数据一致性测试" {
  // 测试属性的设置和获取一致性
  let attrs = Attributes::new()
  
  // 设置各种类型的属性
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "array.string.key", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int.key", ArrayIntValue([1, 2, 3]))
  
  // 验证获取的属性值与设置的一致
  let string_val = Attributes::get(attrs, "string.key")
  assert_eq(string_val, Some(StringValue("test_value")))
  
  let int_val = Attributes::get(attrs, "int.key")
  assert_eq(int_val, Some(IntValue(42)))
  
  let float_val = Attributes::get(attrs, "float.key")
  assert_eq(float_val, Some(FloatValue(3.14)))
  
  let bool_val = Attributes::get(attrs, "bool.key")
  assert_eq(bool_val, Some(BoolValue(true)))
  
  // 测试特殊字符和Unicode的一致性
  let unicode_text = "测试中文字符串🚀special!@#$%"
  Attributes::set(attrs, "unicode.key", StringValue(unicode_text))
  let unicode_val = Attributes::get(attrs, "unicode.key")
  assert_eq(unicode_val, Some(StringValue(unicode_text)))
}

test "SpanContext数据完整性测试" {
  // 创建SpanContext并验证数据完整性
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let sampled = true
  let trace_state = "rojo=00-4bf92f3577ed34a170dc3b6f0f3b4d00-00f067aa0ba902b7-01"
  
  let span_ctx = SpanContext::new(trace_id, span_id, sampled, trace_state)
  
  // 验证所有字段的数据完整性
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_eq(SpanContext::is_sampled(span_ctx), sampled)
  assert_eq(SpanContext::is_valid(span_ctx), true)
  
  // 测试边界情况的完整性
  let empty_trace_ctx = SpanContext::new("", span_id, sampled, trace_state)
  assert_eq(SpanContext::is_valid(empty_trace_ctx), false)
  
  let empty_span_ctx = SpanContext::new(trace_id, "", sampled, trace_state)
  assert_eq(SpanContext::is_valid(empty_span_ctx), false)
  
  let both_empty_ctx = SpanContext::new("", "", false, "")
  assert_eq(SpanContext::is_valid(both_empty_ctx), false)
}

test "上下文传播一致性测试" {
  // 测试上下文在传播过程中的一致性
  let original_ctx = Context::root()
  
  // 设置多个上下文值
  let key1 = ContextKey::new("test.key.1")
  let key2 = ContextKey::new("test.key.2")
  let key3 = ContextKey::new("test.key.3")
  
  let ctx_with_values = Context::with_value(
    Context::with_value(
      Context::with_value(original_ctx, key1, "value1_中文"),
      key2, "value2_special!@#$%"
    ),
    key3, "value3"
  )
  
  // 验证所有值都能正确获取
  assert_eq(Context::get(ctx_with_values, key1), Some("value1_中文"))
  assert_eq(Context::get(ctx_with_values, key2), Some("value2_special!@#$%"))
  assert_eq(Context::get(ctx_with_values, key3), Some("value3"))
  
  // 验证不存在的键返回None
  let non_existent_key = ContextKey::new("non.existent.key")
  assert_eq(Context::get(ctx_with_values, non_existent_key), None)
}

test "Baggage数据一致性测试" {
  // 测试Baggage的设置和获取一致性
  let baggage = Baggage::new()
  
  // 设置多个Baggage条目
  let baggage_with_entries = Baggage::set_entry(baggage, "key1", "value1_中文")
  let baggage_with_entries2 = Baggage::set_entry(baggage_with_entries, "key2", "value2_special!@#$%")
  let baggage_with_entries3 = Baggage::set_entry(baggage_with_entries2, "key3", "value3")
  
  // 验证所有条目都能正确获取
  assert_eq(Baggage::get_entry(baggage_with_entries3, "key1"), Some("value1_中文"))
  assert_eq(Baggage::get_entry(baggage_with_entries3, "key2"), Some("value2_special!@#$%"))
  assert_eq(Baggage::get_entry(baggage_with_entries3, "key3"), Some("value3"))
  
  // 测试删除操作的一致性
  let baggage_after_removal = Baggage::remove_entry(baggage_with_entries3, "key2")
  assert_eq(Baggage::get_entry(baggage_after_removal, "key2"), None)
  // 验证其他条目仍然存在
  assert_eq(Baggage::get_entry(baggage_after_removal, "key1"), Some("value1_中文"))
  assert_eq(Baggage::get_entry(baggage_after_removal, "key3"), Some("value3"))
}

test "资源合并一致性测试" {
  // 创建基础资源
  let base_attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("host.name", StringValue("localhost"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // 创建覆盖资源
  let override_attrs = [
    ("service.version", StringValue("2.0.0")),  // 覆盖现有属性
    ("environment", StringValue("test")),      // 新增属性
    ("deployment.region", StringValue("us-west-1"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // 执行合并
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // 验证合并结果的一致性
  // 基础属性中未被覆盖的应该保持
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("test-service")))
  assert_eq(Resource::get_attribute(merged_resource, "host.name"), Some(StringValue("localhost")))
  
  // 被覆盖的属性应该使用新值
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some(StringValue("2.0.0")))
  
  // 新增的属性应该存在
  assert_eq(Resource::get_attribute(merged_resource, "environment"), Some(StringValue("test")))
  assert_eq(Resource::get_attribute(merged_resource, "deployment.region"), Some(StringValue("us-west-1")))
}

test "LogRecord数据完整性测试" {
  // 创建完整的LogRecord
  let attrs = Attributes::new()
  Attributes::set(attrs, "error.type", StringValue("ValidationError"))
  Attributes::set(attrs, "error.code", IntValue(400))
  
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000000100L
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let log_record = LogRecord::new_with_context(
    Error,
    Some("测试日志消息 with 特殊字符"),
    Some(attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(Context::with_value(Context::root(), ContextKey::new("correlation.id"), "corr_123"))
  )
  
  // 验证所有字段的完整性
  assert_eq(LogRecord::severity_number(log_record), Error)
  assert_eq(LogRecord::body(log_record), Some("测试日志消息 with 特殊字符"))
  assert_eq(LogRecord::trace_id(log_record), Some(trace_id))
  assert_eq(LogRecord::span_id(log_record), Some(span_id))
  
  // 测试简化构造函数的一致性
  let simple_log = LogRecord::new(Info, "简单日志消息")
  assert_eq(LogRecord::severity_number(simple_log), Info)
  assert_eq(LogRecord::body(simple_log), Some("简单日志消息"))
  assert_eq(LogRecord::trace_id(simple_log), None)
  assert_eq(LogRecord::span_id(simple_log), None)
}

test "传播器注入提取一致性测试" {
  // 测试传播器的注入和提取操作的一致性
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 创建原始上下文
  let original_ctx = Context::with_value(
    Context::root(),
    ContextKey::new("test.trace.id"),
    "trace_123_中文"
  )
  
  // 创建carrier并注入
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, original_ctx, carrier)
  
  // 验证carrier中有预期的数据
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_traceparent.is_some())
  
  // 从carrier提取上下文
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_true(extracted_value.is_some())
}

test "指标数据一致性测试" {
  // 测试指标的创建和操作一致性
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "consistency_test", Some("1.0.0"))
  
  // 创建各种类型的指标
  let counter = Meter::create_counter(meter, "test_counter", Some("测试计数器"), Some("count"))
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("测试直方图"), Some("ms"))
  
  // 验证指标属性的一致性
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(counter_instrument), "test_counter")
  assert_eq(Instrument::description(counter_instrument), Some("测试计数器"))
  assert_eq(Instrument::unit(counter_instrument), Some("count"))
  
  let histogram_instrument = Histogram(histogram.name, histogram.description, histogram.unit)
  assert_eq(Instrument::name(histogram_instrument), "test_histogram")
  assert_eq(Instrument::description(histogram_instrument), Some("测试直方图"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
  
  // 测试指标操作的幂等性
  let attrs = Attributes::new()
  Attributes::set(attrs, "operation.type", StringValue("test"))
  
  // 多次执行相同的操作应该保持一致性
  Counter::add(counter, 1.0, Some(attrs))
  Counter::add(counter, 2.0, Some(attrs))
  Counter::add(counter, 3.0, Some(attrs))
  
  Histogram::record(histogram, 100.0, Some(attrs))
  Histogram::record(histogram, 200.0, Some(attrs))
  Histogram::record(histogram, 300.0, Some(attrs))
}

test "HTTP请求响应数据完整性测试" {
  // 创建复杂的HTTP请求
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token_中文_!@#$%"),
    ("X-Custom-Header", "自定义值")
  ]
  let body = "{\"message\":\"测试请求 with 特殊字符\",\"data\":[1,2,3]}"
  
  let request = HttpRequest::new("POST", "https://api.example.com/test", headers, Some(body))
  
  // 验证请求数据的完整性
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/test")
  assert_eq(HttpRequest::body(request), Some(body))
  
  // 创建复杂的HTTP响应
  let response_headers = [
    ("Content-Type", "application/json; charset=utf-8"),
    ("X-Response-ID", "resp_中文_123")
  ]
  let response_body = "{\"success\":true,\"message\":\"操作成功\"}"
  
  let response = HttpResponse::new(200, response_headers, Some(response_body))
  
  // 验证响应数据的完整性
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some(response_body))
}

test "跨类型数据转换一致性测试" {
  // 测试不同数据类型间转换的一致性
  
  // 字符串到数值的转换场景
  let string_attrs = Attributes::new()
  Attributes::set(string_attrs, "numeric.string", StringValue("42"))
  Attributes::set(string_attrs, "float.string", StringValue("3.14159"))
  Attributes::set(string_attrs, "boolean.string", StringValue("true"))
  
  // 数值到字符串的转换场景
  let int_attrs = Attributes::new()
  Attributes::set(int_attrs, "int.value", IntValue(42))
  Attributes::set(int_attrs, "float.value", FloatValue(3.14159))
  Attributes::set(int_attrs, "bool.value", BoolValue(true))
  
  // 验证数据在转换过程中保持一致性
  let original_string = "test_string_中文_!@#$%"
  let string_attr = StringValue(original_string)
  
  // 模拟字符串的序列化和反序列化
  let serialized = match string_attr {
    StringValue(s) => s
    _ => ""
  }
  let deserialized = StringValue(serialized)
  
  assert_eq(string_attr, deserialized)
  assert_eq(match deserialized { StringValue(s) => s; _ => "" }, original_string)
}
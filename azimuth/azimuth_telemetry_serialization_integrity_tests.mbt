// Azimuth Telemetry Serialization Integrity Tests
// 测试遥测数据序列化和反序列化的完整性

test "telemetry_span_serialization_integrity" {
  // 创建一个复杂的 Span 对象
  let span_ctx = SpanContext::new("trace123456789", "span987654321", true, "test-state")
  let span = Span::new("test-operation", Server, span_ctx)
  
  // 验证 Span 基本属性
  @assertion.assert_eq(Span::name(span), "test-operation")?
  @assertion.assert_eq(Span::is_recording(span), true)?
  
  // 验证 SpanContext 属性
  let ctx = Span::span_context(span)
  @assertion.assert_eq(SpanContext::trace_id(ctx), "trace123456789")?
  @assertion.assert_eq(SpanContext::span_id(ctx), "span987654321")?
  @assertion.assert_eq(SpanContext::is_sampled(ctx), true)?
  @assertion.assert_eq(SpanContext::is_valid(ctx), true)?
}

test "telemetry_log_record_serialization_integrity" {
  // 创建带有各种属性的 LogRecord
  let attributes = Attributes::new()
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Test error message"),
    Some(attributes),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace123456789"),
    Some("span987654321"),
    Some(Context::root())
  )
  
  // 验证 LogRecord 属性
  @assertion.assert_eq(LogRecord::severity_number(log_record), Error)?
  @assertion.assert_eq(LogRecord::body(log_record), Some("Test error message"))?
  @assertion.assert_eq(LogRecord::trace_id(log_record), Some("trace123456789"))?
  @assertion.assert_eq(LogRecord::span_id(log_record), Some("span987654321"))?
}

test "telemetry_attributes_serialization_integrity" {
  // 测试不同类型的属性值序列化
  let attrs = Attributes::new()
  
  // 设置不同类型的属性值
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "array.string.key", ArrayStringValue(["value1", "value2"]))
  Attributes::set(attrs, "array.int.key", ArrayIntValue([1, 2, 3]))
  
  // 验证属性值获取
  @assertion.assert_eq(Attributes::get(attrs, "string.key"), Some(StringValue("test_value")))?
  @assertion.assert_eq(Attributes::get(attrs, "int.key"), Some(IntValue(42)))?
  @assertion.assert_eq(Attributes::get(attrs, "nonexistent.key"), None)?
}

test "telemetry_context_serialization_integrity" {
  // 测试 Context 的序列化和反序列化
  let ctx = Context::root()
  let key = ContextKey::new("test_key")
  let ctx_with_value = Context::with_value(ctx, key, "test_value")
  
  // 验证 Context 值的获取
  @assertion.assert_eq(Context::get(ctx_with_value, key), Some("test_value"))?
  
  // 验证不存在的键
  let nonexistent_key = ContextKey::new("nonexistent")
  @assertion.assert_eq(Context::get(ctx_with_value, nonexistent_key), None)?
}

test "telemetry_baggage_serialization_integrity" {
  // 测试 Baggage 的序列化和反序列化
  let baggage = Baggage::new()
  
  // 添加条目
  let baggage_with_entry = Baggage::set_entry(baggage, "key1", "value1")
  let baggage_with_more_entries = Baggage::set_entry(baggage_with_entry, "key2", "value2")
  
  // 验证条目获取
  @assertion.assert_eq(Baggage::get_entry(baggage_with_more_entries, "key1"), Some("value1"))?
  @assertion.assert_eq(Baggage::get_entry(baggage_with_more_entries, "key2"), Some("value2"))?
  @assertion.assert_eq(Baggage::get_entry(baggage_with_more_entries, "nonexistent"), None)?
  
  // 测试条目移除
  let baggage_after_removal = Baggage::remove_entry(baggage_with_more_entries, "key1")
  @assertion.assert_eq(Baggage::get_entry(baggage_after_removal, "key1"), None)?
}

test "telemetry_resource_serialization_integrity" {
  // 测试 Resource 的序列化和反序列化
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // 验证属性获取
  @assertion.assert_eq(Resource::get_attribute(resource_with_attrs, "service.name"), Some(StringValue("test-service")))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_attrs, "service.version"), Some(StringValue("1.0.0")))?
  @assertion.assert_eq(Resource::get_attribute(resource_with_attrs, "nonexistent"), None)?
  
  // 测试资源合并
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("environment", StringValue("test")),
    ("service.name", StringValue("overridden-service"))
  ])
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  @assertion.assert_eq(Resource::get_attribute(merged_resource, "environment"), Some(StringValue("test")))?
}

test "telemetry_instrument_serialization_integrity" {
  // 测试各种 Instrument 类型的序列化
  let counter = Counter("test.counter", Some("Test counter"), Some("count"))
  let histogram = Histogram("test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter = UpDownCounter("test.updown", Some("Test updown counter"), Some("count"))
  let gauge = Gauge("test.gauge", Some("Test gauge"), Some("percent"))
  
  // 验证 Instrument 名称
  @assertion.assert_eq(Instrument::name(Counter("test.counter", None, None)), "test.counter")?
  @assertion.assert_eq(Instrument::name(Histogram("test.histogram", None, None)), "test.histogram")?
  @assertion.assert_eq(Instrument::name(UpDownCounter("test.updown", None, None)), "test.updown")?
  @assertion.assert_eq(Instrument::name(Gauge("test.gauge", None, None)), "test.gauge")?
  
  // 验证 Instrument 描述和单位
  @assertion.assert_eq(Instrument::description(counter), Some("Test counter"))?
  @assertion.assert_eq(Instrument::unit(counter), Some("count"))?
  @assertion.assert_eq(Instrument::description(histogram), Some("Test histogram"))?
  @assertion.assert_eq(Instrument::unit(histogram), Some("ms"))?
}

test "telemetry_propagation_serialization_integrity" {
  // 测试传播机制的序列化完整性
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 注入上下文
  CompositePropagator::inject(propagator, ctx, carrier)
  
  // 提取上下文
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  let extract_key = ContextKey::new("extracted")
  @assertion.assert_eq(Context::get(extracted_ctx, extract_key), Some("true"))?
  
  // 验证载体中的 traceparent
  @assertion.assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-test-trace-id-test-span-id-01"))?
}
// 数据完整性验证测试用例
// 专注于遥测数据的完整性、一致性和准确性验证

test "Span数据完整性验证测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "integrity.tracer")
  
  // 创建具有完整数据的Span
  let span = Tracer::start_span(tracer, "integrity.test.span")
  let span_ctx = Span::span_context(span)
  
  // 验证Span基本数据完整性
  assert_eq(Span::name(span), "integrity.test.span")
  assert_true(Span::is_recording(span))
  assert_true(SpanContext::is_valid(span_ctx))
  assert_true(SpanContext::span_id(span_ctx) != "")
  assert_true(SpanContext::trace_id(span_ctx) != "")
  
  // 设置状态和事件
  Span::set_status(span, Ok, Some("Integrity test successful"))
  Span::add_event(span, "integrity.start.event", Some([
    ("test.type", StringValue("data.integrity")),
    ("test.timestamp", IntValue(1735689600)),
    ("test.version", StringValue("1.0.0"))
  ]))
  
  // 验证Span状态完整性
  assert_true(Span::is_recording(span))
  assert_eq(Span::kind(span), Internal)
  
  // 添加更多事件验证数据完整性
  for i = 0; i < 10; i = i + 1 {
    Span::add_event(span, "integrity.event." + i.to_string(), Some([
      ("event.sequence", IntValue(i)),
      ("event.data", StringValue("test.data." + i.to_string())),
      ("event.timestamp", IntValue(1735689600 + i))
    ]))
  }
  
  // 结束Span并验证最终状态
  Span::end(span)
  assert_false(Span::is_recording(span))
  
  // 验证Span上下文数据完整性
  let final_ctx = Span::span_context(span)
  assert_eq(SpanContext::trace_id(final_ctx), SpanContext::trace_id(span_ctx))
  assert_eq(SpanContext::span_id(final_ctx), SpanContext::span_id(span_ctx))
}

test "指标数据完整性验证测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "integrity.meter")
  
  // 创建各种类型的指标仪器
  let counter = Meter::create_counter(meter, "integrity.counter")
  let histogram = Meter::create_histogram(meter, "integrity.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "integrity.updown")
  let gauge = Meter::create_gauge(meter, "integrity.gauge")
  
  // 验证仪器元数据完整性
  assert_eq(counter.name, "integrity.counter")
  assert_eq(histogram.name, "integrity.histogram")
  assert_eq(updown_counter.name, "integrity.updown")
  assert_eq(gauge.name, "integrity.gauge")
  
  // 记录已知值并验证数据完整性
  let test_values = [0.0, 1.0, 42.5, 100.0, -50.0, 3.14159]
  let expected_sum = 0.0
  
  for value in test_values {
    Counter::add(counter, value)
    Histogram::record(histogram, value)
    UpDownCounter::add(updown_counter, value)
    
    // 验证每次操作后仪器状态完整性
    assert_eq(counter.name, "integrity.counter")
    assert_eq(histogram.name, "integrity.histogram")
    assert_eq(updown_counter.name, "integrity.updown")
  }
  
  // 测试边界值数据完整性
  let boundary_values = [
    0.0,                    // 零值
    1.7976931348623157e+308, // 最大Double
    -1.7976931348623157e+308, // 最小Double
    1.23e-10,               // 极小正值
    -1.23e-10,              // 极小负值
    1.0 / 0.0,              // 正无穷
    -1.0 / 0.0,             // 负无穷
    0.0 / 0.0               // NaN
  ]
  
  for boundary_value in boundary_values {
    Counter::add(counter, boundary_value)
    Histogram::record(histogram, boundary_value)
    UpDownCounter::add(updown_counter, boundary_value)
    
    // 验证边界值处理后仪器完整性
    assert_eq(counter.name, "integrity.counter")
    assert_eq(histogram.name, "integrity.histogram")
    assert_eq(updown_counter.name, "integrity.updown")
  }
  
  // 验证仪器类型完整性
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  let histogram_instrument = Histogram(histogram.name, histogram.description, histogram.unit)
  
  assert_eq(Instrument::name(counter_instrument), "integrity.counter")
  assert_eq(Instrument::name(histogram_instrument), "integrity.histogram")
}

test "日志数据完整性验证测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "integrity.logger")
  
  // 测试不同严重性级别的日志数据完整性
  let severities = [Trace, Debug, Info, Warn, Error, Fatal]
  let test_messages = [
    "Trace level integrity test",
    "Debug level integrity test",
    "Info level integrity test",
    "Warning level integrity test",
    "Error level integrity test",
    "Fatal level integrity test"
  ]
  
  for i = 0; i < severities.length(); i = i + 1 {
    let record = LogRecord::new(severities[i], test_messages[i])
    
    // 验证日志记录基本完整性
    assert_eq(LogRecord::severity_number(record), severities[i])
    assert_eq(LogRecord::body(record), Some(test_messages[i]))
    
    Logger::emit(logger, record)
  }
  
  // 测试带完整上下文的日志记录
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.test.type", StringValue("data.integrity"))
  Attributes::set(attrs, "log.test.version", StringValue("1.0.0"))
  Attributes::set(attrs, "log.test.sequence", IntValue(1))
  
  let contextual_record = LogRecord::new_with_context(
    Error,
    Some("Contextual integrity test log"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("integrity-trace-12345"),
    Some("integrity-span-67890"),
    Some(Context::with_value(Context::root(), ContextKey::new("test.context"), "integrity.value"))
  )
  
  // 验证完整上下文日志记录
  assert_eq(LogRecord::severity_number(contextual_record), Error)
  assert_eq(LogRecord::body(contextual_record), Some("Contextual integrity test log"))
  assert_eq(LogRecord::trace_id(contextual_record), Some("integrity-trace-12345"))
  assert_eq(LogRecord::span_id(contextual_record), Some("integrity-span-67890"))
  assert_eq(LogRecord::trace_id(contextual_record), Some("integrity-trace-12345"))
  
  Logger::emit(logger, contextual_record)
  
  // 测试特殊字符和Unicode日志完整性
  let special_messages = [
    "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?",
    "Unicode test: 测试🚀Тестالعربية",
    "Mixed: Hello 测试 🚀 World 123 !@#",
    "Newlines:\nLine1\nLine2\nLine3",
    "Tabs:\tCol1\tCol2\tCol3"
  ]
  
  for special_msg in special_messages {
    let special_record = LogRecord::new(Info, special_msg)
    assert_eq(LogRecord::body(special_record), Some(special_msg))
    Logger::emit(logger, special_record)
  }
}

test "属性数据完整性验证测试" {
  // 测试各种属性类型的完整性
  let attrs = Attributes::new()
  
  // 字符串属性完整性测试
  let string_values = [
    "",
    "hello",
    "hello world",
    "1234567890",
    "special!@#$%^&*()",
    "unicode测试🚀",
    "a" * 1000  // 长字符串
  ]
  
  for i = 0; i < string_values.length(); i = i + 1 {
    let key = "string.attr." + i.to_string()
    let value = StringValue(string_values[i])
    Attributes::set(attrs, key, value)
    
    // 验证设置和获取一致性
    let retrieved = Attributes::get(attrs, key)
    assert_true(retrieved is Some)
    match retrieved {
      Some(StringValue(retrieved_string)) => assert_eq(retrieved_string, string_values[i])
      _ => assert_true(false)
    }
  }
  
  // 整数属性完整性测试
  let int_values = [0, 1, -1, 42, -42, 2147483647, -2147483648]
  
  for i = 0; i < int_values.length(); i = i + 1 {
    let key = "int.attr." + i.to_string()
    let value = IntValue(int_values[i])
    Attributes::set(attrs, key, value)
    
    let retrieved = Attributes::get(attrs, key)
    assert_true(retrieved is Some)
    match retrieved {
      Some(IntValue(retrieved_int)) => assert_eq(retrieved_int, int_values[i])
      _ => assert_true(false)
    }
  }
  
  // 浮点数属性完整性测试
  let float_values = [0.0, 1.0, -1.0, 3.14159, -3.14159, 1.7976931348623157e+308, -1.7976931348623157e+308]
  
  for i = 0; i < float_values.length(); i = i + 1 {
    let key = "float.attr." + i.to_string()
    let value = FloatValue(float_values[i])
    Attributes::set(attrs, key, value)
    
    let retrieved = Attributes::get(attrs, key)
    assert_true(retrieved is Some)
    match retrieved {
      Some(FloatValue(retrieved_float)) => {
        // 浮点数比较使用近似相等
        let diff = retrieved_float - float_values[i]
        assert_true(diff.abs() < 0.000001 || diff.is_nan() && float_values[i].is_nan())
      }
      _ => assert_true(false)
    }
  }
  
  // 布尔属性完整性测试
  let bool_values = [true, false]
  
  for i = 0; i < bool_values.length(); i = i + 1 {
    let key = "bool.attr." + i.to_string()
    let value = BoolValue(bool_values[i])
    Attributes::set(attrs, key, value)
    
    let retrieved = Attributes::get(attrs, key)
    assert_true(retrieved is Some)
    match retrieved {
      Some(BoolValue(retrieved_bool)) => assert_eq(retrieved_bool, bool_values[i])
      _ => assert_true(false)
    }
  }
  
  // 数组属性完整性测试
  let string_array_value = ArrayStringValue(["item1", "item2", "item3", "测试", "🚀"])
  let int_array_value = ArrayIntValue([1, 2, 3, 42, -42])
  
  Attributes::set(attrs, "string.array.attr", string_array_value)
  Attributes::set(attrs, "int.array.attr", int_array_value)
  
  let retrieved_string_array = Attributes::get(attrs, "string.array.attr")
  let retrieved_int_array = Attributes::get(attrs, "int.array.attr")
  
  assert_true(retrieved_string_array is Some)
  assert_true(retrieved_int_array is Some)
}

test "上下文传播数据完整性验证测试" {
  // 测试上下文数据完整性
  let root_ctx = Context::root()
  
  // 设置多个上下文值
  let ctx1 = Context::with_value(root_ctx, ContextKey::new("test.key.1"), "test.value.1")
  let ctx2 = Context::with_value(ctx1, ContextKey::new("test.key.2"), "test.value.2")
  let ctx3 = Context::with_value(ctx2, ContextKey::new("test.key.3"), "test.value.3")
  
  // 验证上下文值获取完整性
  let value1 = Context::get(ctx3, ContextKey::new("test.key.1"))
  let value2 = Context::get(ctx3, ContextKey::new("test.key.2"))
  let value3 = Context::get(ctx3, ContextKey::new("test.key.3"))
  
  assert_eq(value1, Some("test.value.1"))
  assert_eq(value2, Some("test.value.2"))
  assert_eq(value3, Some("test.value.3"))
  
  // 测试传播器数据完整性
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let carrier = TextMapCarrier::new()
  
  // 注入上下文
  CompositePropagator::inject(propagator, ctx3, carrier)
  
  // 设置额外头部
  TextMapCarrier::set(carrier, "custom.header.1", "custom.value.1")
  TextMapCarrier::set(carrier, "custom.header.2", "custom.value.2")
  
  // 验证载体数据完整性
  let trace_value = TextMapCarrier::get(carrier, "traceparent")
  let custom1 = TextMapCarrier::get(carrier, "custom.header.1")
  let custom2 = TextMapCarrier::get(carrier, "custom.header.2")
  
  assert_true(trace_value is Some)
  assert_eq(custom1, Some("custom.value.1"))
  assert_eq(custom2, Some("custom.value.2"))
  
  // 提取上下文并验证完整性
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  assert_true(extracted_value is Some)
  
  // 测试Baggage数据完整性
  let baggage = Baggage::new()
  
  // 设置多个Baggage条目
  let baggage1 = Baggage::set_entry(baggage, "baggage.key.1", "baggage.value.1")
  let baggage2 = Baggage::set_entry(baggage1, "baggage.key.2", "baggage.value.2")
  let baggage3 = Baggage::set_entry(baggage2, "baggage.key.3", "baggage.value.3")
  
  // 验证Baggage条目完整性
  let bag_value1 = Baggage::get_entry(baggage3, "baggage.key.1")
  let bag_value2 = Baggage::get_entry(baggage3, "baggage.key.2")
  let bag_value3 = Baggage::get_entry(baggage3, "baggage.key.3")
  
  assert_eq(bag_value1, Some("baggage.value.1"))
  assert_eq(bag_value2, Some("baggage.value.2"))
  assert_eq(bag_value3, Some("baggage.value.3"))
  
  // 测试不存在的键
  let missing_bag_value = Baggage::get_entry(baggage3, "nonexistent.key")
  assert_eq(missing_bag_value, None)
}

test "资源数据完整性验证测试" {
  // 测试资源属性完整性
  let resource = Resource::new()
  
  // 设置各种类型的资源属性
  let resource_attrs = [
    ("service.name", StringValue("integrity-test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("integrity-instance-123")),
    ("deployment.environment", StringValue("test")),
    ("host.name", StringValue("integrity-test-host")),
    ("process.id", IntValue(12345)),
    ("process.start.time", IntValue(1735689600)),
    ("cpu.count", IntValue(4)),
    ("memory.total", IntValue(8589934592)),
    ("service.uptime", FloatValue(3600.5)),
    ("service.debug.enabled", BoolValue(true))
  ]
  
  let enriched_resource = Resource::with_attributes(resource, resource_attrs)
  
  // 验证每个资源属性完整性
  for (key, expected_value) in resource_attrs {
    let actual_value = Resource::get_attribute(enriched_resource, key)
    assert_true(actual_value is Some)
    
    match expected_value {
      StringValue(expected_str) => {
        match actual_value {
          Some(StringValue(actual_str)) => assert_eq(actual_str, expected_str)
          _ => assert_true(false)
        }
      }
      IntValue(expected_int) => {
        match actual_value {
          Some(IntValue(actual_int)) => assert_eq(actual_int, expected_int)
          _ => assert_true(false)
        }
      }
      FloatValue(expected_float) => {
        match actual_value {
          Some(FloatValue(actual_float)) => {
            let diff = actual_float - expected_float
            assert_true(diff.abs() < 0.000001)
          }
          _ => assert_true(false)
        }
      }
      BoolValue(expected_bool) => {
        match actual_value {
          Some(BoolValue(actual_bool)) => assert_eq(actual_bool, expected_bool)
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 测试资源合并完整性
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("base.attr.1", StringValue("base.value.1")),
    ("base.attr.2", StringValue("base.value.2"))
  ])
  
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("override.attr.1", StringValue("override.value.1")),
    ("override.attr.2", StringValue("override.value.2"))
  ])
  
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // 验证合并结果完整性
  let override1 = Resource::get_attribute(merged_resource, "override.attr.1")
  let override2 = Resource::get_attribute(merged_resource, "override.attr.2")
  
  assert_true(override1 is Some)
  assert_true(override2 is Some)
  
  match override1 {
    Some(StringValue(value)) => assert_eq(value, "override.value.1")
    _ => assert_true(false)
  }
  
  match override2 {
    Some(StringValue(value)) => assert_eq(value, "override.value.2")
    _ => assert_true(false)
  }
}
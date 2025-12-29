// Azimuth Telemetry - Cross Module Edge Cases Tests
// 测试跨模块的边界情况和复杂交互

test "cross_module_context_trace_logs_integration_edge_cases" {
  // 测试Context、Trace和Logs模块集成的边界情况
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  
  // 测试使用极长键名的Context
  let very_long_key_name = "a".repeat(1000)
  let long_key = @azimuth.telemetry.api.context.create_key(very_long_key_name)
  let context_with_long_key = base_context.with_value(long_key, "long.key.value")
  
  // 创建带有长键名的Span
  let (span_context, span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    context_with_long_key,
    "span-with-long-context-key",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("long.key.name", @azimuth.telemetry.api.common.AttributeValue::string(very_long_key_name)),
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("edge-case-test"))
    ])
  )
  
  // 创建带有长键名的LogRecord
  let log_record = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Log with long context key")
    .with_attribute("long.key.name", @azimuth.telemetry.api.common.AttributeValue::string(very_long_key_name))
    .with_attribute("context.value", @azimuth.telemetry.api.common.AttributeValue::string("long.key.value"))
    .build()
  
  // 验证所有操作都能正常处理长键名
  assert_eq(span.name, "span-with-long-context-key")
  assert_eq(log_record.body, Some("Log with long context key"))
  assert_eq(log_record.attributes.length(), 2)
}

test "cross_module_special_characters_handling" {
  // 测试跨模块特殊字符处理
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  
  // 测试包含特殊字符的Context键和值
  let special_key = @azimuth.telemetry.api.context.create_key("key.with.special@chars#123")
  let special_value = "value.with.special@chars#456"
  let context_with_special = base_context.with_value(special_key, special_value)
  
  // 测试Unicode字符
  let unicode_key = @azimuth.telemetry.api.context.create_key("键名中文测试")
  let unicode_value = "值中文测试"
  let context_with_unicode = context_with_special.with_value(unicode_key, unicode_value)
  
  // 创建带有特殊字符的Span
  let (span_context, span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    context_with_unicode,
    "span.with.special.chars",
    Some(@azimuth.telemetry.api.trace.Client),
    Some([
      ("special.key", @azimuth.telemetry.api.common.AttributeValue::string("key.with.special@chars#123")),
      ("special.value", @azimuth.telemetry.api.common.AttributeValue::string("value.with.special@chars#456")),
      ("unicode.key", @azimuth.telemetry.api.common.AttributeValue::string("键名中文测试")),
      ("unicode.value", @azimuth.telemetry.api.common.AttributeValue::string("值中文测试")),
      ("emoji.test", @azimuth.telemetry.api.common.AttributeValue::string("🔥 🚀 ✨"))
    ])
  )
  
  // 创建带有特殊字符的LogRecord
  let log_record = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Warn)
    .body("Special characters test: \n\t\r\"'<>{}[]|&;`")
    .with_attribute("special.chars", @azimuth.telemetry.api.common.AttributeValue::string("key.with.special@chars#123"))
    .with_attribute("unicode.chars", @azimuth.telemetry.api.common.AttributeValue::string("键名中文测试"))
    .with_attribute("emoji.chars", @azimuth.telemetry.api.common.AttributeValue::string("🔥 🚀 ✨"))
    .with_attribute("control.chars", @azimuth.telemetry.api.common.AttributeValue::string("\n\t\r"))
    .build()
  
  // 验证特殊字符处理
  assert_eq(span.name, "span.with.special.chars")
  assert_eq(span.attributes.length(), 5)
  assert_eq(log_record.body, Some("Special characters test: \n\t\r\"'<>{}[]|&;`"))
  assert_eq(log_record.attributes.length(), 4)
}

test "cross_module_extreme_values_handling" {
  // 测试跨模块极值处理
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  
  // 测试极值时间戳
  let min_time = -9223372036854775808L
  let max_time = 9223372036854775807L
  
  // 创建带有极值时间戳的Span
  let (min_time_context, min_time_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    base_context,
    "min-time-span",
    Some(@azimuth.telemetry.api.trace.Internal),
    Some([
      ("time.type", @azimuth.telemetry.api.common.AttributeValue::string("minimum")),
      ("time.value", @azimuth.telemetry.api.common.AttributeValue::int(min_time))
    ]),
    Some(min_time)
  )
  
  let (max_time_context, max_time_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    base_context,
    "max-time-span",
    Some(@azimuth.telemetry.api.trace.Producer),
    Some([
      ("time.type", @azimuth.telemetry.api.common.AttributeValue::string("maximum")),
      ("time.value", @azimuth.telemetry.api.common.AttributeValue::int(max_time))
    ]),
    Some(max_time)
  )
  
  // 创建带有极值时间戳的LogRecord
  let min_time_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(min_time)
    .severity(@azimuth.telemetry.api.logs.Error)
    .body("Minimum time log")
    .with_attribute("time.type", @azimuth.telemetry.api.common.AttributeValue::string("minimum"))
    .build()
  
  let max_time_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(max_time)
    .severity(@azimuth.telemetry.api.logs.Fatal)
    .body("Maximum time log")
    .with_attribute("time.type", @azimuth.telemetry.api.common.AttributeValue::string("maximum"))
    .build()
  
  // 测试极值数值
  let min_int = -9223372036854775808L
  let max_int = 9223372036854775807L
  let min_float = -1.7976931348623157e+308
  let max_float = 1.7976931348623157e+308
  let nan_float = 0.0/0.0
  let inf_float = 1.0/0.0
  
  let extreme_values_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Extreme values test")
    .with_attribute("min.int", @azimuth.telemetry.api.common.AttributeValue::int(min_int))
    .with_attribute("max.int", @azimuth.telemetry.api.common.AttributeValue::int(max_int))
    .with_attribute("min.float", @azimuth.telemetry.api.common.AttributeValue::float(min_float))
    .with_attribute("max.float", @azimuth.telemetry.api.common.AttributeValue::float(max_float))
    .with_attribute("nan.float", @azimuth.telemetry.api.common.AttributeValue::float(nan_float))
    .with_attribute("inf.float", @azimuth.telemetry.api.common.AttributeValue::float(inf_float))
    .build()
  
  // 验证极值处理
  assert_eq(min_time_span.start_time_unix_nanos, min_time)
  assert_eq(max_time_span.start_time_unix_nanos, max_time)
  assert_eq(min_time_log.timestamp_unix_nanos, min_time)
  assert_eq(max_time_log.timestamp_unix_nanos, max_time)
  assert_eq(extreme_values_log.attributes.length(), 6)
  
  // 验证NaN和无穷值
  match extreme_values_log.attributes[4] {
    ("nan.float", @azimuth.telemetry.api.common.AttributeValue::FloatValue(nan_val)) => {
      assert_eq(nan_val/nan_val != nan_val/nan_val, true) // NaN检查
    }
    _ => @test.fail("Expected NaN float value")
  }
  
  match extreme_values_log.attributes[5] {
    ("inf.float", @azimuth.telemetry.api.common.AttributeValue::FloatValue(inf_val)) => {
      assert_eq(inf_val > 0.0, true) // 正无穷检查
    }
    _ => @test.fail("Expected infinity float value")
  }
}

test "cross_module_empty_and_null_values_handling" {
  // 测试跨模块空值和null值处理
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  
  // 测试空字符串键和值
  let empty_key = @azimuth.telemetry.api.context.create_key("")
  let context_with_empty_key = base_context.with_value(empty_key, "empty.key.value")
  let context_with_empty_value = base_context.with_value(@azimuth.telemetry.api.context.create_key("empty.value"), "")
  
  // 创建带有空值的Span
  let (empty_context, empty_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    context_with_empty_key,
    "",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("", @azimuth.telemetry.api.common.AttributeValue::string("empty.key")),
      ("empty.value", @azimuth.telemetry.api.common.AttributeValue::string("")),
      ("normal.key", @azimuth.telemetry.api.common.AttributeValue::string("normal.value"))
    ])
  )
  
  // 创建带有空值的LogRecord
  let empty_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("")
    .with_attribute("", @azimuth.telemetry.api.common.AttributeValue::string("empty.key"))
    .with_attribute("empty.value", @azimuth.telemetry.api.common.AttributeValue::string(""))
    .with_attribute("normal.attribute", @azimuth.telemetry.api.common.AttributeValue::string("normal.value"))
    .build()
  
  // 测试空数组
  let empty_array_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Debug)
    .body("Empty arrays test")
    .with_attribute("empty.string.array", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue([]))
    .with_attribute("empty.int.array", @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue([]))
    .with_attribute("empty.float.array", @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue([]))
    .with_attribute("empty.bool.array", @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue([]))
    .build()
  
  // 验证空值处理
  assert_eq(empty_span.name, "")
  assert_eq(empty_span.attributes.length(), 3)
  assert_eq(empty_log.body, Some(""))
  assert_eq(empty_log.attributes.length(), 3)
  assert_eq(empty_array_log.attributes.length(), 4)
}

test "cross_module_metrics_with_edge_cases" {
  // 测试跨模块指标的边界情况
  let meter = @azimuth.telemetry.api.metrics.NoopMeter
  
  // 创建各种仪器
  let counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("edge.case.counter", "1", "Edge case counter")
  let histogram = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("edge.case.histogram", "ms", "Edge case histogram")
  let up_down_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_up_down_counter("edge.case.up.down", "1", "Edge case up-down counter")
  let gauge = @azimuth.telemetry.api.metrics.NoopMeter::create_gauge("edge.case.gauge", "%", "Edge case gauge")
  
  // 测试带有边界属性的指标操作
  let edge_case_attrs = [
    ("", @azimuth.telemetry.api.common.AttributeValue::string("empty.key")),
    ("empty.value", @azimuth.telemetry.api.common.AttributeValue::string("")),
    ("long.key", @azimuth.telemetry.api.common.AttributeValue::string("a".repeat(1000))),
    ("special.chars", @azimuth.telemetry.api.common.AttributeValue::string("key.with.special@chars#123")),
    ("unicode.key", @azimuth.telemetry.api.common.AttributeValue::string("键名中文测试")),
    ("emoji.key", @azimuth.telemetry.api.common.AttributeValue::string("🔥🚀✨")),
    ("control.chars", @azimuth.telemetry.api.common.AttributeValue::string("\n\t\r")),
    ("json.value", @azimuth.telemetry.api.common.AttributeValue::string("{\"key\": \"value\"}")),
    ("url.value", @azimuth.telemetry.api.common.AttributeValue::string("https://example.com/path?query=value#fragment")),
    ("array.value", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(["a", "b", "c"]))
  ]
  
  // 测试极值指标操作
  @azimuth.telemetry.api.metrics.NoopCounter::add(0L, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopCounter::add(-1L, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopCounter::add(9223372036854775807L, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopCounter::add(-9223372036854775808L, Some(edge_case_attrs))
  
  @azimuth.telemetry.api.metrics.NoopHistogram::record(0.0, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopHistogram::record(1.7976931348623157e+308, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopHistogram::record(-1.7976931348623157e+308, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopHistogram::record(0.0/0.0, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopHistogram::record(1.0/0.0, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopHistogram::record(-1.0/0.0, Some(edge_case_attrs))
  
  @azimuth.telemetry.api.metrics.NoopUpDownCounter::add(0L, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopUpDownCounter::add(1000000L, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopUpDownCounter::add(-1000000L, Some(edge_case_attrs))
  
  @azimuth.telemetry.api.metrics.NoopGauge::record(0.0, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopGauge::record(100.0, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopGauge::record(-100.0, Some(edge_case_attrs))
  @azimuth.telemetry.api.metrics.NoopGauge::record(1.7976931348623157e+308, Some(edge_case_attrs))
  
  // 验证所有操作都能正常执行
  assert_eq(true, true)
}

test "cross_module_propagation_edge_cases" {
  // 测试跨模块传播的边界情况
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  
  // 创建包含边界情况的Carrier
  let edge_case_data = [
    (@azimuth.telemetry.api.propagation.TRACE_PARENT_HEADER, ""),
    (@azimuth.telemetry.api.propagation.BAGGAGE_HEADER, ""),
    ("invalid.header", "invalid.value"),
    (@azimuth.telemetry.api.propagation.TRACE_PARENT_HEADER, "00-" + "a".repeat(32) + "-" + "b".repeat(16) + "-01"),
    (@azimuth.telemetry.api.propagation.BAGGAGE_HEADER, "key1=" + "a".repeat(1000) + ",key2=" + "b".repeat(1000)),
    (@azimuth.telemetry.api.propagation.BAGGAGE_HEADER, "中文键=中文值,emoji=🔥🚀,mixed=中mixed文"),
    ("x-custom-header", "custom.value.with.special@chars#123")
  ]
  
  let edge_case_carrier = @azimuth.telemetry.api.propagation.MapCarrier::from_map(edge_case_data)
  
  // 测试传播器处理边界情况
  let trace_ctx = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::extract(base_context, edge_case_carrier)
  let baggage_ctx = @azimuth.telemetry.api.propagation.W3CBaggagePropagator::extract(base_context, edge_case_carrier)
  
  // 测试复合传播器处理边界情况
  let composite = @azimuth.telemetry.api.propagation.CompositePropagator::new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator,
    @azimuth.telemetry.api.propagation.W3CBaggagePropagator
  ])
  
  let composite_ctx = @azimuth.telemetry.api.propagation.CompositePropagator::extract(base_context, edge_case_carrier)
  
  // 测试注入边界情况
  let empty_carrier = @azimuth.telemetry.api.propagation.MapCarrier::new()
  @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::inject(base_context, empty_carrier)
  @azimuth.telemetry.api.propagation.W3CBaggagePropagator::inject(base_context, empty_carrier)
  
  // 验证所有操作都能正常执行
  assert_eq(true, true)
}

test "cross_module_complex_attribute_combinations" {
  // 测试跨模块复杂属性组合
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  
  // 创建包含所有类型属性的Span
  let complex_attrs = [
    ("string.attr", @azimuth.telemetry.api.common.AttributeValue::string("string.value")),
    ("int.attr", @azimuth.telemetry.api.common.AttributeValue::int(42L)),
    ("float.attr", @azimuth.telemetry.api.common.AttributeValue::float(3.14)),
    ("bool.attr", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
    ("string.array", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(["a", "b", "c"])),
    ("int.array", @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue([1L, 2L, 3L])),
    ("float.array", @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue([1.1, 2.2, 3.3])),
    ("bool.array", @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue([true, false, true])),
    ("empty.string", @azimuth.telemetry.api.common.AttributeValue::string("")),
    ("zero.int", @azimuth.telemetry.api.common.AttributeValue::int(0L)),
    ("zero.float", @azimuth.telemetry.api.common.AttributeValue::float(0.0)),
    ("false.bool", @azimuth.telemetry.api.common.AttributeValue::bool(false)),
    ("empty.array", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue([])),
    ("unicode.string", @azimuth.telemetry.api.common.AttributeValue::string("Unicode测试: 🔥 🚀 ✨")),
    ("special.chars", @azimuth.telemetry.api.common.AttributeValue::string("Special: \n\t\r\"'<>{}[]|&;`")),
    ("json.string", @azimuth.telemetry.api.common.AttributeValue::string("{\"key\": \"value\", \"array\": [1, 2, 3]}")),
    ("url.string", @azimuth.telemetry.api.common.AttributeValue::string("https://example.com/path?query=value#fragment"))
  ]
  
  let (_, complex_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    base_context,
    "complex-attributes-span",
    Some(@azimuth.telemetry.api.trace.Server),
    Some(complex_attrs)
  )
  
  // 创建包含所有类型属性的LogRecord
  let complex_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Complex attributes log")
    .with_attribute("string.attr", @azimuth.telemetry.api.common.AttributeValue::string("string.value"))
    .with_attribute("int.attr", @azimuth.telemetry.api.common.AttributeValue::int(42L))
    .with_attribute("float.attr", @azimuth.telemetry.api.common.AttributeValue::float(3.14))
    .with_attribute("bool.attr", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    .with_attribute("string.array", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(["a", "b", "c"]))
    .with_attribute("int.array", @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue([1L, 2L, 3L]))
    .with_attribute("float.array", @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue([1.1, 2.2, 3.3]))
    .with_attribute("bool.array", @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue([true, false, true]))
    .build()
  
  // 创建包含所有类型属性的指标
  let meter = @azimuth.telemetry.api.metrics.NoopMeter
  let counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("complex.counter", "1", "Complex counter")
  
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some(complex_attrs))
  
  // 验证复杂属性组合
  assert_eq(complex_span.name, "complex-attributes-span")
  assert_eq(complex_span.attributes.length(), 17)
  assert_eq(complex_log.body, Some("Complex attributes log"))
  assert_eq(complex_log.attributes.length(), 8)
}
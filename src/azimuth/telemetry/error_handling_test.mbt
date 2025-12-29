// Azimuth Telemetry - Error Handling Tests
// 错误处理和异常情况测试

test "error_handling_context_invalid_operations" {
  // 测试Context模块的错误处理
  let empty_context = @azimuth.telemetry.api.context.Context::empty()
  
  // 测试使用不存在的键
  let missing_key = @azimuth.telemetry.api.context.create_key("nonexistent.key")
  let result = empty_context.get(missing_key)
  assert_eq(result, None) // 应该返回None而不是崩溃
  
  // 测试使用空字符串键
  let empty_key = @azimuth.telemetry.api.context.create_key("")
  let context_with_empty = empty_context.with_value(empty_key, "empty.key.value")
  let empty_result = context_with_empty.get(empty_key)
  assert_eq(empty_result, Some("empty.key.value"))
  
  // 测试使用非常长的键名
  let long_key_name = "a".repeat(10000)
  let long_key = @azimuth.telemetry.api.context.create_key(long_key_name)
  let context_with_long = empty_context.with_value(long_key, "long.key.value")
  let long_result = context_with_long.get(long_key)
  assert_eq(long_result, Some("long.key.value"))
  
  // 测试使用特殊字符键
  let special_key = @azimuth.telemetry.api.context.create_key("key.with.special@chars#123")
  let context_with_special = empty_context.with_value(special_key, "special.key.value")
  let special_result = context_with_special.get(special_key)
  assert_eq(special_result, Some("special.key.value"))
}

test "error_handling_baggage_invalid_operations" {
  // 测试Baggage模块的错误处理
  let empty_baggage = @azimuth.telemetry.api.context.Baggage::empty()
  
  // 测试使用不存在的键
  let missing_result = empty_baggage.get("nonexistent.key")
  assert_eq(missing_result, None) // 应该返回None而不是崩溃
  
  // 测试使用空字符串键
  let baggage_with_empty = empty_baggage.with_entry("", "empty.key.value")
  let empty_result = baggage_with_empty.get("")
  assert_eq(empty_result, Some("empty.key.value"))
  
  // 测试使用非常长的键名和值
  let long_key_name = "a".repeat(10000)
  let long_value = "b".repeat(10000)
  let baggage_with_long = empty_baggage.with_entry(long_key_name, long_value)
  let long_result = baggage_with_long.get(long_key_name)
  assert_eq(long_result, Some(long_value))
  
  // 测试使用特殊字符键和值
  let special_key = "key.with.special@chars#123"
  let special_value = "value.with.special@chars#456"
  let baggage_with_special = empty_baggage.with_entry(special_key, special_value)
  let special_result = baggage_with_special.get(special_key)
  assert_eq(special_result, Some(special_value))
  
  // 测试使用Unicode字符
  let unicode_key = "键名中文测试"
  let unicode_value = "值中文测试"
  let baggage_with_unicode = empty_baggage.with_entry(unicode_key, unicode_value)
  let unicode_result = baggage_with_unicode.get(unicode_key)
  assert_eq(unicode_result, Some(unicode_value))
}

test "error_handling_logs_invalid_operations" {
  // 测试Logs模块的错误处理
  let base_time = 1640995200000000000L
  
  // 测试创建带有无效参数的LogRecord
  let log_with_empty_body = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(base_time)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("")
    .build()
  
  assert_eq(log_with_empty_body.body, Some(""))
  
  // 测试创建带有极值时间戳的LogRecord
  let log_with_min_time = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(-9223372036854775808L) // Int64最小值
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Min time test")
    .build()
  
  assert_eq(log_with_min_time.timestamp_unix_nanos, -9223372036854775808L)
  
  let log_with_max_time = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(9223372036854775807L) // Int64最大值
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Max time test")
    .build()
  
  assert_eq(log_with_max_time.timestamp_unix_nanos, 9223372036854775807L)
  
  // 测试创建带有无效属性的LogRecord
  let log_with_invalid_attrs = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(base_time)
    .severity(@azimuth.telemetry.api.logs.Error)
    .body("Invalid attributes test")
    .with_attribute("", @azimuth.telemetry.api.common.AttributeValue::string("empty.key"))
    .with_attribute("valid.key", @azimuth.telemetry.api.common.AttributeValue::string(""))
    .with_attribute("long.key", @azimuth.telemetry.api.common.AttributeValue::string("a".repeat(10000)))
    .build()
  
  assert_eq(log_with_invalid_attrs.attributes.length(), 3)
  
  // 测试使用NoopLogger处理各种日志级别
  let logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger("error-test")
  
  // 测试使用空消息体
  logger.debug("", None)
  logger.info("", Some([("test", @azimuth.telemetry.api.common.AttributeValue::bool(true))]))
  
  // 测试使用非常长的消息体
  let long_message = "a".repeat(100000)
  logger.warn(long_message, None)
  
  // 测试使用包含特殊字符的消息体
  let special_message = "Special chars: \n\t\r\"'<>{}[]|&;`"
  logger.error(special_message, None)
  
  // 测试使用Unicode消息体
  let unicode_message = "Unicode测试: 🔥 🚀 ✨ 中文测试"
  logger.fatal(unicode_message, None)
  
  // 验证所有操作都能正常执行（no-op实现不会抛出异常）
  assert_eq(true, true)
}

test "error_handling_metrics_invalid_operations" {
  // 测试Metrics模块的错误处理
  let meter = @azimuth.telemetry.api.metrics.NoopMeter
  
  // 测试创建带有无效名称的仪器
  let counter_with_empty_name = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("", "1", "Empty name counter")
  let counter_with_long_name = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("a".repeat(10000), "1", "Long name counter")
  let counter_with_special_chars = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("counter.with.special@chars#123", "1", "Special chars counter")
  
  // 测试创建带有无效单位和描述的仪器
  let histogram_with_empty_unit = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("test.histogram", "", "Empty unit histogram")
  let histogram_with_long_unit = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("test.histogram", "a".repeat(1000), "Long unit histogram")
  let histogram_with_special_unit = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("test.histogram", "unit.with.special@chars", "Special unit histogram")
  
  // 测试使用极值进行指标操作
  let test_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("test.counter", "1", "Test counter")
  
  // 测试Int64边界值
  @azimuth.telemetry.api.metrics.NoopCounter::add(0L, None)
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, None)
  @azimuth.telemetry.api.metrics.NoopCounter::add(-1L, None)
  @azimuth.telemetry.api.metrics.NoopCounter::add(9223372036854775807L, None) // Int64最大值
  @azimuth.telemetry.api.metrics.NoopCounter::add(-9223372036854775808L, None) // Int64最小值
  
  let test_histogram = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("test.histogram", "ms", "Test histogram")
  
  // 测试Double边界值
  @azimuth.telemetry.api.metrics.NoopHistogram::record(0.0, None)
  @azimuth.telemetry.api.metrics.NoopHistogram::record(1.7976931348623157e+308, None) // Double最大值
  @azimuth.telemetry.api.metrics.NoopHistogram::record(-1.7976931348623157e+308, None) // Double最小值
  @azimuth.telemetry.api.metrics.NoopHistogram::record(0.0/0.0, None) // NaN
  @azimuth.telemetry.api.metrics.NoopHistogram::record(1.0/0.0, None) // 正无穷
  @azimuth.telemetry.api.metrics.NoopHistogram::record(-1.0/0.0, None) // 负无穷
  
  let test_up_down_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_up_down_counter("test.up_down", "1", "Test up-down counter")
  
  // 测试UpDownCounter的正负值
  @azimuth.telemetry.api.metrics.NoopUpDownCounter::add(1000000L, None)
  @azimuth.telemetry.api.metrics.NoopUpDownCounter::add(-1000000L, None)
  
  // 测试使用无效属性
  let invalid_attrs = [
    ("", @azimuth.telemetry.api.common.AttributeValue::string("empty.key")),
    ("valid.key", @azimuth.telemetry.api.common.AttributeValue::string("")),
    ("long.key", @azimuth.telemetry.api.common.AttributeValue::string("a".repeat(10000))),
    ("special.key", @azimuth.telemetry.api.common.AttributeValue::string("key.with.special@chars#123")),
    ("unicode.key", @azimuth.telemetry.api.common.AttributeValue::string("键名中文测试"))
  ]
  
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some(invalid_attrs))
  @azimuth.telemetry.api.metrics.NoopHistogram::record(100.0, Some(invalid_attrs))
  @azimuth.telemetry.api.metrics.NoopUpDownCounter::add(-1L, Some(invalid_attrs))
  @azimuth.telemetry.api.metrics.NoopGauge::record(50.0, Some(invalid_attrs))
  
  // 验证所有操作都能正常执行（no-op实现不会抛出异常）
  assert_eq(true, true)
}

test "error_handling_trace_invalid_operations" {
  // 测试Trace模块的错误处理
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  
  // 测试创建带有无效名称的Span
  let (_, span_with_empty_name) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    base_context,
    "",
    Some(@azimuth.telemetry.api.trace.Server)
  )
  
  assert_eq(span_with_empty_name.name, "")
  
  let (_, span_with_long_name) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    base_context,
    "a".repeat(10000),
    Some(@azimuth.telemetry.api.trace.Client)
  )
  
  assert_eq(span_with_long_name.name.length(), 10000)
  
  let (_, span_with_special_chars) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    base_context,
    "span.with.special@chars#123",
    Some(@azimuth.telemetry.api.trace.Internal)
  )
  
  assert_eq(span_with_special_chars.name, "span.with.special@chars#123")
  
  // 测试创建带有无效时间戳的Span
  let (_, span_with_min_time) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    base_context,
    "min-time-span",
    Some(@azimuth.telemetry.api.trace.Producer),
    Some([("test", @azimuth.telemetry.api.common.AttributeValue::bool(true))]),
    Some(-9223372036854775808L)
  )
  
  assert_eq(span_with_min_time.start_time_unix_nanos, -9223372036854775808L)
  
  let (_, span_with_max_time) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    base_context,
    "max-time-span",
    Some(@azimuth.telemetry.api.trace.Consumer),
    Some([("test", @azimuth.telemetry.api.common.AttributeValue::bool(true))]),
    Some(9223372036854775807L)
  )
  
  assert_eq(span_with_max_time.start_time_unix_nanos, 9223372036854775807L)
  
  // 测试创建带有无效属性的Span
  let invalid_span_attrs = [
    ("", @azimuth.telemetry.api.common.AttributeValue::string("empty.key")),
    ("valid.key", @azimuth.telemetry.api.common.AttributeValue::string("")),
    ("long.key", @azimuth.telemetry.api.common.AttributeValue::string("a".repeat(10000))),
    ("special.key", @azimuth.telemetry.api.common.AttributeValue::string("key.with.special@chars#123")),
    ("unicode.key", @azimuth.telemetry.api.common.AttributeValue::string("键名中文测试"))
  ]
  
  let (_, span_with_invalid_attrs) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    base_context,
    "invalid-attrs-span",
    Some(@azimuth.telemetry.api.trace.Server),
    Some(invalid_span_attrs)
  )
  
  assert_eq(span_with_invalid_attrs.attributes.length(), 5)
  
  // 测试创建带有无效事件的Span
  let invalid_event = @azimuth.telemetry.api.trace.SpanEvent::{
    name: "",
    timestamp_unix_nanos: -9223372036854775808L,
    attributes: [
      ("", @azimuth.telemetry.api.common.AttributeValue::string("empty.key")),
      ("valid.key", @azimuth.telemetry.api.common.AttributeValue::string(""))
    ]
  }
  
  // 测试创建带有无效链接的Span
  let invalid_trace_id = Array::make(16, 0_byte)
  let invalid_span_id = Array::make(8, 0_byte)
  let invalid_context = @azimuth.telemetry.api.trace.SpanContext::{
    trace_id: invalid_trace_id,
    span_id: invalid_span_id,
    trace_flags: 255_byte, // 无效的trace flags
    trace_state: ""
  }
  
  let invalid_link = @azimuth.telemetry.api.trace.SpanLink::{
    context: invalid_context,
    attributes: [
      ("", @azimuth.telemetry.api.common.AttributeValue::string("empty.key")),
      ("valid.key", @azimuth.telemetry.api.common.AttributeValue::string(""))
    ]
  }
  
  // 验证所有操作都能正常执行（no-op实现不会抛出异常）
  assert_eq(true, true)
}

test "error_handling_propagation_invalid_operations" {
  // 测试Propagation模块的错误处理
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  
  // 测试使用空的Carrier
  let empty_carrier = @azimuth.telemetry.api.propagation.MapCarrier::new()
  let result1 = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::extract(base_context, empty_carrier)
  let result2 = @azimuth.telemetry.api.propagation.W3CBaggagePropagator::extract(base_context, empty_carrier)
  
  // 应该能够处理空carrier而不崩溃
  assert_eq(true, true)
  
  // 测试使用包含无效数据的Carrier
  let mut invalid_data = []
  invalid_data = invalid_data.push((@azimuth.telemetry.api.propagation.TRACE_PARENT_HEADER, ""))
  invalid_data = invalid_data.push((@azimuth.telemetry.api.propagation.BAGGAGE_HEADER, ""))
  invalid_data = invalid_data.push(("invalid.header", "invalid.value"))
  invalid_data = invalid_data.push(("", "empty.key.value"))
  invalid_data = invalid_data.push(("valid.key", ""))
  
  let invalid_carrier = @azimuth.telemetry.api.propagation.MapCarrier::from_map(invalid_data)
  
  // 应该能够处理无效数据而不崩溃
  let result3 = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::extract(base_context, invalid_carrier)
  let result4 = @azimuth.telemetry.api.propagation.W3CBaggagePropagator::extract(base_context, invalid_carrier)
  
  assert_eq(true, true)
  
  // 测试使用包含超长数据的Carrier
  let very_long_traceparent = "00-" + "a".repeat(32) + "-" + "b".repeat(16) + "-01"
  let very_long_baggage = "key1=" + "a".repeat(100000) + ",key2=" + "b".repeat(100000)
  
  let mut long_data = []
  long_data = long_data.push((@azimuth.telemetry.api.propagation.TRACE_PARENT_HEADER, very_long_traceparent))
  long_data = long_data.push((@azimuth.telemetry.api.propagation.BAGGAGE_HEADER, very_long_baggage))
  
  let long_carrier = @azimuth.telemetry.api.propagation.MapCarrier::from_map(long_data)
  
  // 应该能够处理超长数据而不崩溃
  let result5 = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::extract(base_context, long_carrier)
  let result6 = @azimuth.telemetry.api.propagation.W3CBaggagePropagator::extract(base_context, long_carrier)
  
  assert_eq(true, true)
  
  // 测试使用包含特殊字符的Carrier
  let special_traceparent = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  let special_baggage = "key=value with spaces,key2=value,with,commas,key3=value;with;semicolons,key4=value\\with\\backslashes"
  
  let mut special_data = []
  special_data = special_data.push((@azimuth.telemetry.api.propagation.TRACE_PARENT_HEADER, special_traceparent))
  special_data = special_data.push((@azimuth.telemetry.api.propagation.BAGGAGE_HEADER, special_baggage))
  
  let special_carrier = @azimuth.telemetry.api.propagation.MapCarrier::from_map(special_data)
  
  // 应该能够处理特殊字符而不崩溃
  let result7 = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::extract(base_context, special_carrier)
  let result8 = @azimuth.telemetry.api.propagation.W3CBaggagePropagator::extract(base_context, special_carrier)
  
  assert_eq(true, true)
  
  // 测试使用包含Unicode字符的Carrier
  let unicode_baggage = "中文键=中文值,emoji=🔥🚀,mixed=中mixed文"
  
  let mut unicode_data = []
  unicode_data = unicode_data.push((@azimuth.telemetry.api.propagation.BAGGAGE_HEADER, unicode_baggage))
  
  let unicode_carrier = @azimuth.telemetry.api.propagation.MapCarrier::from_map(unicode_data)
  
  // 应该能够处理Unicode字符而不崩溃
  let result9 = @azimuth.telemetry.api.propagation.W3CBaggagePropagator::extract(base_context, unicode_carrier)
  
  assert_eq(true, true)
}

test "error_handling_resource_and_instrumentation_scope" {
  // 测试Resource和InstrumentationScope的错误处理
  
  // 测试创建带有无效服务名的Resource
  let resource_with_empty_name = @azimuth.telemetry.api.common.Resource::default("")
  assert_eq(resource_with_empty_name.service_name, "")
  
  let resource_with_long_name = @azimuth.telemetry.api.common.Resource::default("a".repeat(10000))
  assert_eq(resource_with_long_name.service_name.length(), 10000)
  
  let resource_with_special_chars = @azimuth.telemetry.api.common.Resource::default("service.with.special@chars#123")
  assert_eq(resource_with_special_chars.service_name, "service.with.special@chars#123")
  
  let resource_with_unicode = @azimuth.telemetry.api.common.Resource::default("服务名称中文测试")
  assert_eq(resource_with_unicode.service_name, "服务名称中文测试")
  
  // 测试创建带有无效字段的InstrumentationScope
  let scope_with_empty_name = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "",
    version: Some(""),
    schema_url: Some("")
  }
  
  assert_eq(scope_with_empty_name.name, "")
  assert_eq(scope_with_empty_name.version?, "")
  assert_eq(scope_with_empty_name.schema_url?, "")
  
  let scope_with_long_fields = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "a".repeat(10000),
    version: Some("a".repeat(1000)),
    schema_url: Some("a".repeat(5000))
  }
  
  assert_eq(scope_with_long_fields.name.length(), 10000)
  assert_eq(scope_with_long_fields.version?.length(), 1000)
  assert_eq(scope_with_long_fields.schema_url?.length(), 5000)
  
  let scope_with_special_chars = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "scope.with.special@chars#123",
    version: Some("version_1.0.0-beta+build.123"),
    schema_url: Some("https://example.com/path?query=value#fragment")
  }
  
  assert_eq(scope_with_special_chars.name, "scope.with.special@chars#123")
  assert_eq(scope_with_special_chars.version?, "version_1.0.0-beta+build.123")
  assert_eq(scope_with_special_chars.schema_url?, "https://example.com/path?query=value#fragment")
  
  let scope_with_unicode = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "范围名称中文测试",
    version: Some("版本1.0.0"),
    schema_url: Some("https://例子.com/路径")
  }
  
  assert_eq(scope_with_unicode.name, "范围名称中文测试")
  assert_eq(scope_with_unicode.version?, "版本1.0.0")
  assert_eq(scope_with_unicode.schema_url?, "https://例子.com/路径")
}

test "error_handling_attribute_value_edge_cases" {
  // 测试AttributeValue的边界情况和错误处理
  
  // 测试空字符串值
  let empty_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("")
  match empty_string_attr {
    @azimuth.telemetry.api.common.AttributeValue::StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试超长字符串值
  let long_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("a".repeat(100000))
  match long_string_attr {
    @azimuth.telemetry.api.common.AttributeValue::StringValue(s) => assert_eq(s.length(), 100000)
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试包含特殊字符的字符串值
  let special_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("Special chars: \n\t\r\"'<>{}[]|&;`")
  match special_string_attr {
    @azimuth.telemetry.api.common.AttributeValue::StringValue(s) => assert_eq(s, "Special chars: \n\t\r\"'<>{}[]|&;`")
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试Unicode字符串值
  let unicode_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("Unicode测试: 🔥 🚀 ✨ 中文测试")
  match unicode_string_attr {
    @azimuth.telemetry.api.common.AttributeValue::StringValue(s) => assert_eq(s, "Unicode测试: 🔥 🚀 ✨ 中文测试")
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试Int64边界值
  let min_int_attr = @azimuth.telemetry.api.common.AttributeValue::int(-9223372036854775808L)
  match min_int_attr {
    @azimuth.telemetry.api.common.AttributeValue::IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Expected IntValue")
  }
  
  let max_int_attr = @azimuth.telemetry.api.common.AttributeValue::int(9223372036854775807L)
  match max_int_attr {
    @azimuth.telemetry.api.common.AttributeValue::IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Expected IntValue")
  }
  
  // 测试Double边界值
  let min_float_attr = @azimuth.telemetry.api.common.AttributeValue::float(-1.7976931348623157e+308)
  match min_float_attr {
    @azimuth.telemetry.api.common.AttributeValue::FloatValue(f) => assert_eq(f, -1.7976931348623157e+308)
    _ => @test.fail("Expected FloatValue")
  }
  
  let max_float_attr = @azimuth.telemetry.api.common.AttributeValue::float(1.7976931348623157e+308)
  match max_float_attr {
    @azimuth.telemetry.api.common.AttributeValue::FloatValue(f) => assert_eq(f, 1.7976931348623157e+308)
    _ => @test.fail("Expected FloatValue")
  }
  
  let nan_float_attr = @azimuth.telemetry.api.common.AttributeValue::float(0.0/0.0)
  match nan_float_attr {
    @azimuth.telemetry.api.common.AttributeValue::FloatValue(f) => assert_eq(f/f != f/f, true) // NaN检查
    _ => @test.fail("Expected FloatValue")
  }
  
  let inf_float_attr = @azimuth.telemetry.api.common.AttributeValue::float(1.0/0.0)
  match inf_float_attr {
    @azimuth.telemetry.api.common.AttributeValue::FloatValue(f) => assert_eq(f > 0.0, true) // 正无穷检查
    _ => @test.fail("Expected FloatValue")
  }
  
  // 测试空数组值
  let empty_string_array_attr = @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue([])
  match empty_string_array_attr {
    @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 测试包含特殊字符的数组值
  let special_string_array_attr = @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue([
    "Special chars: \n\t\r\"'<>{}[]|&;`",
    "Unicode测试: 🔥 🚀 ✨",
    "Empty string: ",
    "Normal string"
  ])
  
  match special_string_array_attr {
    @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], "Special chars: \n\t\r\"'<>{}[]|&;`")
      assert_eq(arr[1], "Unicode测试: 🔥 🚀 ✨")
      assert_eq(arr[2], "Empty string: ")
      assert_eq(arr[3], "Normal string")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
}
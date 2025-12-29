// 遥测错误处理和异常情况测试用例
// 测试各种错误处理和异常情况的处理能力

test "attribute_value_error_handling" {
  // 测试AttributeValue的错误处理
  
  // 测试极值处理
  let max_int_attr = AttributeValue::int(9223372036854775807L)
  let min_int_attr = AttributeValue::int(-9223372036854775808L)
  let max_float_attr = AttributeValue::float(1.7976931348623157e+308)
  let min_float_attr = AttributeValue::float(-1.7976931348623157e+308)
  
  // 验证极值能够正确创建和访问
  match max_int_attr {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Expected IntValue")
  }
  
  match min_int_attr {
    IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Expected IntValue")
  }
  
  match max_float_attr {
    FloatValue(f) => assert_eq(f, 1.7976931348623157e+308)
    _ => @test.fail("Expected FloatValue")
  }
  
  match min_float_attr {
    FloatValue(f) => assert_eq(f, -1.7976931348623157e+308)
    _ => @test.fail("Expected FloatValue")
  }
  
  // 测试特殊浮点值处理
  let infinity_attr = AttributeValue::float(1.0/0.0)
  let neg_infinity_attr = AttributeValue::float(-1.0/0.0)
  let nan_attr = AttributeValue::float(0.0/0.0)
  
  match infinity_attr {
    FloatValue(f) => assert_eq(f, 1.0/0.0)
    _ => @test.fail("Expected FloatValue")
  }
  
  match neg_infinity_attr {
    FloatValue(f) => assert_eq(f, -1.0/0.0)
    _ => @test.fail("Expected FloatValue")
  }
  
  match nan_attr {
    FloatValue(f) => assert_eq(f, 0.0/0.0)
    _ => @test.fail("Expected FloatValue")
  }
  
  // 测试空数组和极长数组
  let empty_string_array = AttributeValue::array_string([])
  let empty_int_array = AttributeValue::array_int([])
  let empty_float_array = AttributeValue::array_float([])
  let empty_bool_array = AttributeValue::array_bool([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match empty_float_array {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match empty_bool_array {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayBoolValue")
  }
}

test "context_error_handling" {
  // 测试Context的错误处理
  
  // 测试空键和空值
  let empty_key = create_key("")
  let ctx = context::Context::empty()
  
  let ctx_with_empty_key = ctx.with_value(empty_key, "empty.key.value")
  match ctx_with_empty_key.get(empty_key) {
    Some(v) => assert_eq(v, "empty.key.value")
    None => @test.fail("Expected empty.key.value")
  }
  
  let normal_key = create_key("normal.key")
  let ctx_with_empty_value = ctx.with_value(normal_key, "")
  match ctx_with_empty_value.get(normal_key) {
    Some(v) => assert_eq(v, "")
    None => @test.fail("Expected empty string")
  }
  
  // 测试特殊字符键
  let special_key = create_key("!@#$%^&*()_+-=[]{}|;':\",./<>?")
  let ctx_with_special_key = ctx.with_value(special_key, "special.value")
  match ctx_with_special_key.get(special_key) {
    Some(v) => assert_eq(v, "special.value")
    None => @test.fail("Expected special.value")
  }
  
  // 测试Unicode键
  let unicode_key = create_key("测试键🚀")
  let ctx_with_unicode_key = ctx.with_value(unicode_key, "测试值🌟")
  match ctx_with_unicode_key.get(unicode_key) {
    Some(v) => assert_eq(v, "测试值🌟")
    None => @test.fail("Expected unicode value")
  }
  
  // 测试不存在的键
  let nonexistent_key = create_key("nonexistent.key")
  match ctx.get(nonexistent_key) {
    Some(_) => @test.fail("Expected None")
    None => assert_eq(true, true)
  }
  
  // 测试长键和值
  let long_key_name = "this.is.a.very.long.key.name.with.many.components.and.subdomains.that.might.cause.issues.in.some.systems"
  let long_key = create_key(long_key_name)
  let long_value = "this.is.a.very.long.value.with.many.words.and.characters.that.might.be.used.in.real.world.scenarios.to.test.the.system.robustness"
  let ctx_with_long = ctx.with_value(long_key, long_value)
  
  match ctx_with_long.get(long_key) {
    Some(v) => assert_eq(v, long_value)
    None => @test.fail("Expected long value")
  }
}

test "baggage_error_handling" {
  // 测试Baggage的错误处理
  
  // 测试空键和空值
  let baggage = Baggage::empty()
  
  let baggage_with_empty_key = baggage.with_entry("", "empty.key.value")
  match baggage_with_empty_key.get("") {
    Some(v) => assert_eq(v, "empty.key.value")
    None => @test.fail("Expected empty.key.value")
  }
  
  let baggage_with_empty_value = baggage.with_entry("empty.value", "")
  match baggage_with_empty_value.get("empty.value") {
    Some(v) => assert_eq(v, "")
    None => @test.fail("Expected empty string")
  }
  
  // 测试特殊字符键
  let baggage_with_special_key = baggage.with_entry("!@#$%^&*()", "special.value")
  match baggage_with_special_key.get("!@#$%^&*()") {
    Some(v) => assert_eq(v, "special.value")
    None => @test.fail("Expected special.value")
  }
  
  // 测试Unicode键
  let baggage_with_unicode_key = baggage.with_entry("测试键🚀", "测试值🌟")
  match baggage_with_unicode_key.get("测试键🚀") {
    Some(v) => assert_eq(v, "测试值🌟")
    None => @test.fail("Expected unicode value")
  }
  
  // 测试不存在的键
  match baggage.get("nonexistent.key") {
    Some(_) => @test.fail("Expected None")
    None => assert_eq(true, true)
  }
  
  // 测试长键和值
  let long_key_name = "this.is.a.very.long.baggage.key.name.with.many.components"
  let long_value = "this.is.a.very.long.baggage.value.with.many.words.and.characters"
  let baggage_with_long = baggage.with_entry(long_key_name, long_value)
  
  match baggage_with_long.get(long_key_name) {
    Some(v) => assert_eq(v, long_value)
    None => @test.fail("Expected long value")
  }
}

test "span_error_handling" {
  // 测试Span的错误处理
  
  let ctx = context::Context::empty()
  let tracer = NoopTracer::{}
  
  // 测试极值时间戳
  let max_timestamp = 9223372036854775807L
  let min_timestamp = -9223372036854775808L
  let zero_timestamp = 0L
  
  let (_, span_max_time) = tracer.start_span(ctx, "max-time-span", Some(Internal), [], Some(max_timestamp))
  let (_, span_min_time) = tracer.start_span(ctx, "min-time-span", Some(Internal), [], Some(min_timestamp))
  let (_, span_zero_time) = tracer.start_span(ctx, "zero-time-span", Some(Internal), [], Some(zero_timestamp))
  
  assert_eq(span_max_time.start_time_unix_nanos, max_timestamp)
  assert_eq(span_min_time.start_time_unix_nanos, min_timestamp)
  assert_eq(span_zero_time.start_time_unix_nanos, zero_timestamp)
  
  // 测试空名称和特殊字符名称
  let (_, span_empty_name) = tracer.start_span(ctx, "")
  let (_, span_special_name) = tracer.start_span(ctx, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
  let (_, span_unicode_name) = tracer.start_span(ctx, "测试Span名称🚀")
  let (_, span_long_name) = tracer.start_span(ctx, "this.is.a.very.long.span.name.with.many.components.and.subdomains")
  
  assert_eq(span_empty_name.name, "")
  assert_eq(span_special_name.name, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
  assert_eq(span_unicode_name.name, "测试Span名称🚀")
  assert_eq(span_long_name.name, "this.is.a.very.long.span.name.with.many.components.and.subdomains")
  
  // 测试复杂属性
  let complex_attributes = [
    ("", AttributeValue::string("empty.key")),
    ("special.chars", AttributeValue::string("!@#$%^&*()")),
    ("unicode", AttributeValue::string("测试🚀")),
    ("very.long.key.name.that.exceeds.normal.limits", AttributeValue::string("long.value")),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("max.float", AttributeValue::float(1.7976931348623157e+308)),
    ("min.float", AttributeValue::float(-1.7976931348623157e+308)),
    ("infinity", AttributeValue::float(1.0/0.0)),
    ("neg.infinity", AttributeValue::float(-1.0/0.0)),
    ("nan", AttributeValue::float(0.0/0.0)),
    ("empty.array", AttributeValue::array_string([])),
    ("complex.array", AttributeValue::array_string(["", " ", "!@#$%", "测试", "🚀"]))
  ]
  
  let (_, span_complex_attrs) = tracer.start_span(ctx, "complex-attrs-span", Some(Internal), Some(complex_attributes))
  assert_eq(span_complex_attrs.attributes.length(), 13)
}

test "log_record_error_handling" {
  // 测试LogRecord的错误处理
  
  // 测试极值时间戳
  let max_timestamp_log = LogRecord::builder()
    .timestamp(9223372036854775807L)
    .severity(Fatal)
    .body("Max timestamp log")
    .build()
  
  let min_timestamp_log = LogRecord::builder()
    .timestamp(-9223372036854775808L)
    .severity(Fatal)
    .body("Min timestamp log")
    .build()
  
  let zero_timestamp_log = LogRecord::builder()
    .timestamp(0L)
    .severity(Fatal)
    .body("Zero timestamp log")
    .build()
  
  assert_eq(max_timestamp_log.timestamp_unix_nanos, 9223372036854775807L)
  assert_eq(min_timestamp_log.timestamp_unix_nanos, -9223372036854775808L)
  assert_eq(zero_timestamp_log.timestamp_unix_nanos, 0L)
  
  // 测试空消息体和特殊字符消息体
  let empty_body_log = LogRecord::builder()
    .severity(Info)
    .body("")
    .build()
  
  let special_body_log = LogRecord::builder()
    .severity(Info)
    .body("!@#$%^&*()_+-=[]{}|;':\",./<>?")
    .build()
  
  let unicode_body_log = LogRecord::builder()
    .severity(Info)
    .body("测试日志消息🚀🌟💫")
    .build()
  
  let long_body_log = LogRecord::builder()
    .severity(Info)
    .body("this.is.a.very.long.log.message.with.many.words.and.characters.that.might.be.used.in.real.world.scenarios.to.test.the.system.robustness.and.ability.to.handle.various.types.of.input")
    .build()
  
  assert_eq(empty_body_log.body, Some(""))
  assert_eq(special_body_log.body, Some("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  assert_eq(unicode_body_log.body, Some("测试日志消息🚀🌟💫"))
  assert_eq(long_body_log.body, Some("this.is.a.very.long.log.message.with.many.words.and.characters.that.might.be.used.in.real.world.scenarios.to.test.the.system.robustness.and.ability.to.handle.various.types.of.input"))
  
  // 测试复杂属性
  let complex_log = LogRecord::builder()
    .severity(Error)
    .body("Complex error log")
    .with_attribute("", AttributeValue::string("empty.key"))
    .with_attribute("special.chars", AttributeValue::string("!@#$%^&*()"))
    .with_attribute("unicode", AttributeValue::string("测试🚀"))
    .with_attribute("very.long.key.name.that.exceeds.normal.limits", AttributeValue::string("long.value"))
    .with_attribute("max.int", AttributeValue::int(9223372036854775807L))
    .with_attribute("min.int", AttributeValue::int(-9223372036854775808L))
    .with_attribute("max.float", AttributeValue::float(1.7976931348623157e+308))
    .with_attribute("min.float", AttributeValue::float(-1.7976931348623157e+308))
    .with_attribute("infinity", AttributeValue::float(1.0/0.0))
    .with_attribute("neg.infinity", AttributeValue::float(-1.0/0.0))
    .with_attribute("nan", AttributeValue::float(0.0/0.0))
    .with_attribute("empty.array", AttributeValue::array_string([]))
    .with_attribute("complex.array", AttributeValue::array_string(["", " ", "!@#$%", "测试", "🚀"]))
    .build()
  
  assert_eq(complex_log.attributes.length(), 13)
}

test "metrics_error_handling" {
  // 测试Metrics的错误处理
  
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("error-test-meter", Some("1.0.0"))
  
  // 创建各种类型的指标
  let counter = meter.create_counter("error.counter", Some("ops"), Some("Error counter"))
  let histogram = meter.create_histogram("error.histogram", Some("ms"), Some("Error histogram"))
  let up_down_counter = meter.create_up_down_counter("error.up.down.counter", Some("units"), Some("Error up-down counter"))
  let gauge = meter.create_gauge("error.gauge", Some("units"), Some("Error gauge"))
  
  // 测试极值
  counter.add(9223372036854775807L, Some([("value.type", AttributeValue::string("max"))]))
  counter.add(-9223372036854775808L, Some([("value.type", AttributeValue::string("min"))]))
  counter.add(0L, Some([("value.type", AttributeValue::string("zero"))]))
  
  histogram.record(1.7976931348623157e+308, Some([("value.type", AttributeValue::string("max.float"))]))
  histogram.record(-1.7976931348623157e+308, Some([("value.type", AttributeValue::string("min.float"))]))
  histogram.record(0.0, Some([("value.type", AttributeValue::string("zero.float"))]))
  histogram.record(1.0/0.0, Some([("value.type", AttributeValue::string("infinity"))]))
  histogram.record(-1.0/0.0, Some([("value.type", AttributeValue::string("neg.infinity"))]))
  histogram.record(0.0/0.0, Some([("value.type", AttributeValue::string("nan"))]))
  
  up_down_counter.add(9223372036854775807L, Some([("value.type", AttributeValue::string("updown.max"))]))
  up_down_counter.add(-9223372036854775808L, Some([("value.type", AttributeValue::string("updown.min"))]))
  up_down_counter.add(0L, Some([("value.type", AttributeValue::string("updown.zero"))]))
  
  gauge.record(1.7976931348623157e+308, Some([("value.type", AttributeValue::string("gauge.max"))]))
  gauge.record(-1.7976931348623157e+308, Some([("value.type", AttributeValue::string("gauge.min"))]))
  gauge.record(0.0, Some([("value.type", AttributeValue::string("gauge.zero"))]))
  gauge.record(1.0/0.0, Some([("value.type", AttributeValue::string("gauge.infinity"))]))
  gauge.record(-1.0/0.0, Some([("value.type", AttributeValue::string("gauge.neg.infinity"))]))
  gauge.record(0.0/0.0, Some([("value.type", AttributeValue::string("gauge.nan"))]))
  
  // 测试复杂属性
  let complex_attributes = [
    ("", AttributeValue::string("empty.key")),
    ("special.chars", AttributeValue::string("!@#$%^&*()")),
    ("unicode", AttributeValue::string("测试🚀")),
    ("very.long.key.name.that.exceeds.normal.limits", AttributeValue::string("long.value")),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("max.float", AttributeValue::float(1.7976931348623157e+308)),
    ("min.float", AttributeValue::float(-1.7976931348623157e+308)),
    ("infinity", AttributeValue::float(1.0/0.0)),
    ("neg.infinity", AttributeValue::float(-1.0/0.0)),
    ("nan", AttributeValue::float(0.0/0.0)),
    ("empty.array", AttributeValue::array_string([])),
    ("complex.array", AttributeValue::array_string(["", " ", "!@#$%", "测试", "🚀"]))
  ]
  
  counter.add(1L, Some(complex_attributes))
  histogram.record(100.0, Some(complex_attributes))
  up_down_counter.add(1L, Some(complex_attributes))
  gauge.record(50.0, Some(complex_attributes))
  
  // 测试空属性
  counter.add(1L, None)
  histogram.record(100.0, None)
  up_down_counter.add(1L, None)
  gauge.record(50.0, None)
}

test "propagation_error_handling" {
  // 测试传播的错误处理
  
  let w3c_trace_propagator = W3CTraceContextPropagator::{}
  let w3c_baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([w3c_trace_propagator, w3c_baggage_propagator])
  
  let ctx = context::Context::empty()
  
  // 测试无效的carrier数据
  let invalid_data = [
    ("traceparent", "invalid-traceparent-format"),
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01-extra-data"),
    ("traceparent", ""),
    ("baggage", "invalid=baggage=format=with=too=many=equals"),
    ("baggage", "key-without-value"),
    ("baggage", ""),
    ("", "empty.key"),
    ("!@#$%^&*()", "special.chars.key")
  ]
  
  let invalid_carrier = MapCarrier::from_map(invalid_data)
  
  // 测试从无效数据提取（应该优雅地处理错误）
  let extracted_ctx = composite_propagator.extract(ctx, invalid_carrier)
  
  // 验证即使有无效数据，提取也能正常工作
  assert_eq(extracted_ctx.values.length(), ctx.values.length())
  
  // 测试注入到正常carrier
  let normal_carrier = MapCarrier::new()
  composite_propagator.inject(ctx, normal_carrier)
  
  // 验证注入成功
  let keys = normal_carrier.keys()
  assert_eq(keys.length() > 0, true)
  
  // 测试空carrier
  let empty_carrier = MapCarrier::new()
  let extracted_from_empty = composite_propagator.extract(ctx, empty_carrier)
  assert_eq(extracted_from_empty.values.length(), ctx.values.length())
  
  // 测试空传播器列表
  let empty_propagators = []
  let empty_composite = CompositePropagator::new(empty_propagators)
  
  let empty_result_carrier = MapCarrier::new()
  empty_composite.inject(ctx, empty_result_carrier)
  
  let empty_result_ctx = empty_composite.extract(ctx, empty_result_carrier)
  assert_eq(empty_result_ctx.values.length(), ctx.values.length())
}

test "resource_error_handling" {
  // 测试Resource的错误处理
  
  // 测试空服务名称
  let empty_resource = Resource::default("")
  assert_eq(empty_resource.service_name, "")
  assert_eq(empty_resource.telemetry_sdk_name, "azimuth")
  assert_eq(empty_resource.telemetry_sdk_version, "0.1.0")
  
  // 测试特殊字符服务名称
  let special_resource = Resource::default("!@#$%^&*()_+-=[]{}|;':\",./<>?")
  assert_eq(special_resource.service_name, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
  
  // 测试Unicode服务名称
  let unicode_resource = Resource::default("测试服务🚀")
  assert_eq(unicode_resource.service_name, "测试服务🚀")
  
  // 测试长服务名称
  let long_name = "this.is.a.very.long.service.name.with.many.components.and.subdomains.that.might.cause.issues.in.some.systems"
  let long_resource = Resource::default(long_name)
  assert_eq(long_resource.service_name, long_name)
  
  // 测试包含空格的服务名称
  let space_resource = Resource::default("service with spaces")
  assert_eq(space_resource.service_name, "service with spaces")
  
  // 测试复杂属性
  let complex_attrs : Attributes = [
    ("", AttributeValue::string("empty.key")),
    ("special.chars", AttributeValue::string("!@#$%^&*()")),
    ("unicode", AttributeValue::string("测试🚀")),
    ("very.long.key.name.that.exceeds.normal.limits", AttributeValue::string("long.value")),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("max.float", AttributeValue::float(1.7976931348623157e+308)),
    ("min.float", AttributeValue::float(-1.7976931348623157e+308)),
    ("infinity", AttributeValue::float(1.0/0.0)),
    ("neg.infinity", AttributeValue::float(-1.0/0.0)),
    ("nan", AttributeValue::float(0.0/0.0)),
    ("empty.array", AttributeValue::array_string([])),
    ("complex.array", AttributeValue::array_string(["", " ", "!@#$%", "测试", "🚀"]))
  ]
  
  assert_eq(complex_attrs.length(), 13)
}

test "comprehensive_error_scenarios" {
  // 测试综合错误场景
  
  let ctx = context::Context::empty()
  let tracer = NoopTracer::{}
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("comprehensive-error-meter", Some("1.0.0"))
  
  // 创建带有错误场景的复杂上下文
  let error_key = create_key("error.scenario")
  let ctx_with_errors = ctx.with_value(error_key, "comprehensive.test")
  
  // 创建带有错误属性的Span
  let error_attributes = [
    ("error.type", AttributeValue::string("ComprehensiveError")),
    ("error.message", AttributeValue::string("Testing comprehensive error scenarios")),
    ("error.code", AttributeValue::string("COMP_ERR_001")),
    ("error.severity", AttributeValue::string("high")),
    ("error.stack", AttributeValue::string("at comprehensive_test() at line 123")),
    ("error.timestamp", AttributeValue::int(1640995200000000000L)),
    ("error.retry.count", AttributeValue::int(3L)),
    ("error.recovery.action", AttributeValue::string("restart.service")),
    ("error.impact", AttributeValue::string("service.degraded")),
    ("error.escalation", AttributeValue::bool(true))
  ]
  
  let (_, error_span) = tracer.start_span(
    ctx_with_errors,
    "comprehensive-error-span",
    Some(Internal),
    Some(error_attributes),
    Some(1640995200000000000L)
  )
  
  // 创建错误日志
  let error_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .body("Comprehensive error scenario test")
    .with_attribute("error.scenario", AttributeValue::string("comprehensive"))
    .with_attribute("error.phase", AttributeValue::string("testing"))
    .with_attribute("error.component", AttributeValue::string("telemetry"))
    .with_attribute("error.handler", AttributeValue::string("error_handling_test"))
    .build()
  
  // 创建错误指标
  let error_counter = meter.create_counter("errors.total", Some("errors"), Some("Total errors"))
  let error_histogram = meter.create_histogram("error.duration", Some("ms"), Some("Error handling duration"))
  let error_gauge = meter.create_gauge("error.rate", Some("errors_per_second"), Some("Error rate"))
  
  error_counter.add(1L, Some([
    ("error.type", AttributeValue::string("ComprehensiveError")),
    ("error.severity", AttributeValue::string("high")),
    ("error.component", AttributeValue::string("telemetry"))
  ]))
  
  error_histogram.record(1000.0, Some([
    ("error.type", AttributeValue::string("ComprehensiveError")),
    ("error.phase", AttributeValue::string("handling"))
  ]))
  
  error_gauge.record(0.1, Some([
    ("error.type", AttributeValue::string("ComprehensiveError")),
    ("error.window", AttributeValue::string("1m"))
  ]))
  
  // 验证所有错误场景都能正常处理
  assert_eq(error_span.name, "comprehensive-error-span")
  assert_eq(error_span.attributes.length(), 10)
  assert_eq(error_log.severity_number, Error)
  assert_eq(error_log.attributes.length(), 4)
  
  // 验证错误上下文
  match ctx_with_errors.get(error_key) {
    Some(v) => assert_eq(v, "comprehensive.test")
    None => @test.fail("Expected comprehensive.test")
  }
}
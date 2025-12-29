// 兼容性测试 - 测试不同配置和环境下的兼容性

test "attribute_value_compatibility_across_versions" {
  // 测试AttributeValue在不同版本间的兼容性
  
  // 1. 基础类型兼容性测试
  let string_attr_v1 = AttributeValue::string("test_string")
  let int_attr_v1 = AttributeValue::int(42L)
  let float_attr_v1 = AttributeValue::float(3.14)
  let bool_attr_v1 = AttributeValue::bool(true)
  
  // 验证基础类型可以正确创建和匹配
  match string_attr_v1 {
    StringValue(s) => assert_eq(s, "test_string")
    _ => @test.fail("String attribute compatibility test failed")
  }
  
  match int_attr_v1 {
    IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Int attribute compatibility test failed")
  }
  
  match float_attr_v1 {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Float attribute compatibility test failed")
  }
  
  match bool_attr_v1 {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Bool attribute compatibility test failed")
  }
  
  // 2. 数组类型兼容性测试
  let string_array_v1 = AttributeValue::array_string(["a", "b", "c"])
  let int_array_v1 = AttributeValue::array_int([1L, 2L, 3L])
  let float_array_v1 = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_v1 = AttributeValue::array_bool([true, false, true])
  
  // 验证数组类型兼容性
  match string_array_v1 {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => @test.fail("String array attribute compatibility test failed")
  }
  
  match int_array_v1 {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1L)
      assert_eq(arr[1], 2L)
      assert_eq(arr[2], 3L)
    }
    _ => @test.fail("Int array attribute compatibility test failed")
  }
  
  // 3. 边界值兼容性测试
  let max_int_v1 = AttributeValue::int(9223372036854775807L)
  let min_int_v1 = AttributeValue::int(-9223372036854775808L)
  let zero_int_v1 = AttributeValue::int(0L)
  
  match max_int_v1 {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Max int compatibility test failed")
  }
  
  match min_int_v1 {
    IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Min int compatibility test failed")
  }
  
  // 4. 特殊浮点值兼容性测试
  let infinity_v1 = AttributeValue::float(1.0/0.0)
  let neg_infinity_v1 = AttributeValue::float(-1.0/0.0)
  let nan_v1 = AttributeValue::float(0.0/0.0)
  
  match infinity_v1 {
    FloatValue(f) => assert_eq(f > 1.0e308, true)
    _ => @test.fail("Infinity compatibility test failed")
  }
  
  // 5. 空值和空数组兼容性测试
  let empty_string_v1 = AttributeValue::string("")
  let empty_string_array_v1 = AttributeValue::array_string([])
  let empty_int_array_v1 = AttributeValue::array_int([])
  
  match empty_string_v1 {
    StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Empty string compatibility test failed")
  }
  
  match empty_string_array_v1 {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Empty string array compatibility test failed")
  }
}

test "span_context_backward_compatibility" {
  // 测试SpanContext的向后兼容性
  
  // 1. 基础SpanContext兼容性
  let basic_trace_id = [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88]
  let basic_span_id = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88]
  
  let basic_span_context = SpanContext::{
    trace_id: basic_trace_id,
    span_id: basic_span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  assert_eq(basic_span_context.trace_id.length(), 16)
  assert_eq(basic_span_context.span_id.length(), 8)
  assert_eq(basic_span_context.trace_flags, 1_byte)
  assert_eq(basic_span_context.trace_state, "")
  
  // 2. 复杂trace_state兼容性
  let complex_trace_state = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  let complex_span_context = SpanContext::{
    trace_id: basic_trace_id,
    span_id: basic_span_id,
    trace_flags: 1_byte,
    trace_state: complex_trace_state
  }
  
  assert_eq(complex_span_context.trace_state, complex_trace_state)
  
  // 3. 不同trace_flags值兼容性
  let sampled_context = SpanContext::{
    trace_id: basic_trace_id,
    span_id: basic_span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let not_sampled_context = SpanContext::{
    trace_id: basic_trace_id,
    span_id: basic_span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  assert_eq(sampled_context.trace_flags, 1_byte)
  assert_eq(not_sampled_context.trace_flags, 0_byte)
  
  // 4. 边界值trace_id和span_id兼容性
  let zero_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })
  let max_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xff_byte })
  let zero_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })
  let max_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xff_byte })
  
  let zero_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let max_context = SpanContext::{
    trace_id: max_trace_id,
    span_id: max_span_id,
    trace_flags: 0xff_byte,
    trace_state: ""
  }
  
  // 验证零值上下文
  let mut i = 0
  while i < 16 {
    assert_eq(zero_context.trace_id[i], 0_byte)
    i = i + 1
  }
  
  i = 0
  while i < 8 {
    assert_eq(zero_context.span_id[i], 0_byte)
    i = i + 1
  }
  
  // 验证最大值上下文
  i = 0
  while i < 16 {
    assert_eq(max_context.trace_id[i], 0xff_byte)
    i = i + 1
  }
  
  i = 0
  while i < 8 {
    assert_eq(max_context.span_id[i], 0xff_byte)
    i = i + 1
  }
}

test "resource_configuration_compatibility" {
  // 测试Resource配置的兼容性
  
  // 1. 基础Resource创建兼容性
  let basic_resource = Resource::default("test-service")
  
  assert_eq(basic_resource.service_name, "test-service")
  assert_eq(basic_resource.service_version, None)
  assert_eq(basic_resource.telemetry_sdk_name, "azimuth")
  assert_eq(basic_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(basic_resource.attributes.length(), 0)
  
  // 2. 空服务名称兼容性
  let empty_service_resource = Resource::default("")
  
  assert_eq(empty_service_resource.service_name, "")
  assert_eq(empty_service_resource.telemetry_sdk_name, "azimuth")
  
  // 3. 特殊字符服务名称兼容性
  let special_service_resource = Resource::default("service!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
  
  assert_eq(special_service_resource.service_name, "service!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
  assert_eq(special_service_resource.telemetry_sdk_name, "azimuth")
  
  // 4. Unicode服务名称兼容性
  let unicode_service_resource = Resource::default("服务测试🚀")
  
  assert_eq(unicode_service_resource.service_name, "服务测试🚀")
  assert_eq(unicode_service_resource.telemetry_sdk_name, "azimuth")
  
  // 5. 长服务名称兼容性
  let long_service_name = "a" * 1000
  let long_service_resource = Resource::default(long_service_name)
  
  assert_eq(long_service_resource.service_name.length(), 1000)
  assert_eq(long_service_resource.telemetry_sdk_name, "azimuth")
}

test "instrumentation_scope_version_compatibility" {
  // 测试InstrumentationScope版本兼容性
  
  // 1. 基础InstrumentationScope兼容性
  let basic_scope = InstrumentationScope::{
    name: "test-instrumentation",
    version: None,
    schema_url: None
  }
  
  assert_eq(basic_scope.name, "test-instrumentation")
  match basic_scope.version {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  match basic_scope.schema_url {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 2. 带版本的InstrumentationScope兼容性
  let versioned_scope = InstrumentationScope::{
    name: "versioned-instrumentation",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  assert_eq(versioned_scope.name, "versioned-instrumentation")
  match versioned_scope.version {
    Some(version) => assert_eq(version, "1.0.0")
    None => @test.fail("Test failed")
  }
  
  // 3. 带schema_url的InstrumentationScope兼容性
  let schema_scope = InstrumentationScope::{
    name: "schema-instrumentation",
    version: None,
    schema_url: Some("https://example.com/schema")
  }
  
  assert_eq(schema_scope.name, "schema-instrumentation")
  match schema_scope.schema_url {
    Some(schema_url) => assert_eq(schema_url, "https://example.com/schema")
    None => @test.fail("Test failed")
  }
  
  // 4. 完整配置的InstrumentationScope兼容性
  let full_scope = InstrumentationScope::{
    name: "full-instrumentation",
    version: Some("2.1.0"),
    schema_url: Some("https://example.com/full-schema")
  }
  
  assert_eq(full_scope.name, "full-instrumentation")
  match full_scope.version {
    Some(version) => assert_eq(version, "2.1.0")
    None => @test.fail("Test failed")
  }
  match full_scope.schema_url {
    Some(schema_url) => assert_eq(schema_url, "https://example.com/full-schema")
    None => @test.fail("Test failed")
  }
  
  // 5. 特殊字符名称兼容性
  let special_scope = InstrumentationScope::{
    name: "special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./",
    version: Some("special-version-1.0"),
    schema_url: Some("https://special.example.com/schema")
  }
  
  assert_eq(special_scope.name, "special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
  match special_scope.version {
    Some(version) => assert_eq(version, "special-version-1.0")
    None => @test.fail("Test failed")
  }
}

test "log_record_severity_compatibility" {
  // 测试LogRecord严重性级别的兼容性
  
  // 1. 所有严重性级别兼容性测试
  let trace_severity = Trace
  let debug_severity = Debug
  let info_severity = Info
  let warn_severity = Warn
  let error_severity = Error
  let fatal_severity = Fatal
  
  // 验证所有严重性级别都能正确创建
  let trace_log = LogRecord::builder()
    .severity(trace_severity)
    .body("Trace message")
    .build()
  
  let debug_log = LogRecord::builder()
    .severity(debug_severity)
    .body("Debug message")
    .build()
  
  let info_log = LogRecord::builder()
    .severity(info_severity)
    .body("Info message")
    .build()
  
  let warn_log = LogRecord::builder()
    .severity(warn_severity)
    .body("Warning message")
    .build()
  
  let error_log = LogRecord::builder()
    .severity(error_severity)
    .body("Error message")
    .build()
  
  let fatal_log = LogRecord::builder()
    .severity(fatal_severity)
    .body("Fatal message")
    .build()
  
  // 验证严重性级别
  assert_eq(trace_log.severity_number, Trace)
  assert_eq(debug_log.severity_number, Debug)
  assert_eq(info_log.severity_number, Info)
  assert_eq(warn_log.severity_number, Warn)
  assert_eq(error_log.severity_number, Error)
  assert_eq(fatal_log.severity_number, Fatal)
  
  // 2. 带自定义严重性文本的兼容性
  let custom_error_log = LogRecord::builder()
    .severity(Error)
    .severity_text("CUSTOM_ERROR")
    .body("Custom error message")
    .build()
  
  assert_eq(custom_error_log.severity_number, Error)
  match custom_error_log.severity_text {
    Some(text) => assert_eq(text, "CUSTOM_ERROR")
    None => @test.fail("Test failed")
  }
  
  // 3. 无严重性文本的兼容性
  let no_text_log = LogRecord::builder()
    .severity(Info)
    .body("Message without severity text")
    .build()
  
  assert_eq(no_text_log.severity_number, Info)
  match no_text_log.severity_text {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 4. 空严重性文本的兼容性
  let empty_text_log = LogRecord::builder()
    .severity(Warn)
    .severity_text("")
    .body("Message with empty severity text")
    .build()
  
  assert_eq(empty_text_log.severity_number, Warn)
  match empty_text_log.severity_text {
    Some(text) => assert_eq(text, "")
    None => @test.fail("Test failed")
  }
}

test "metrics_instrument_type_compatibility" {
  // 测试Metrics仪器类型的兼容性
  
  // 1. 所有仪器类型兼容性测试
  let counter_type = Counter
  let histogram_type = Histogram
  let up_down_counter_type = UpDownCounter
  let gauge_type = Gauge
  let observable_counter_type = ObservableCounter
  let observable_gauge_type = ObservableGauge
  let observable_up_down_counter_type = ObservableUpDownCounter
  
  // 验证所有仪器类型都能正确创建
  let meter = NoopMeter::{}
  
  let counter = meter.create_counter("test_counter", Some("count"), Some("Test counter"))
  let histogram = meter.create_histogram("test_histogram", Some("value"), Some("Test histogram"))
  let up_down_counter = meter.create_up_down_counter("test_up_down", Some("count"), Some("Test up-down counter"))
  let gauge = meter.create_gauge("test_gauge", Some("value"), Some("Test gauge"))
  
  // 2. 仪器操作兼容性测试
  let test_attributes = [
    ("test.key", AttributeValue::string("test.value")),
    ("test.number", AttributeValue::int(42L))
  ]
  
  // Counter操作兼容性
  counter.add(1L, Some(test_attributes))
  counter.add(0L, None)
  counter.add(-1L, Some([]))
  
  // Histogram操作兼容性
  histogram.record(1.5, Some(test_attributes))
  histogram.record(0.0, None)
  histogram.record(-1.5, Some([]))
  
  // UpDownCounter操作兼容性
  up_down_counter.add(1L, Some(test_attributes))
  up_down_counter.add(0L, None)
  up_down_counter.add(-1L, Some([]))
  
  // Gauge操作兼容性
  gauge.record(1.5, Some(test_attributes))
  gauge.record(0.0, None)
  gauge.record(-1.5, Some([]))
  
  // 3. 边界值操作兼容性
  let max_int64 = 9223372036854775807L
  let min_int64 = -9223372036854775808L
  let max_double = 1.7976931348623157e308
  let min_double = -1.7976931348623157e308
  
  counter.add(max_int64, Some(test_attributes))
  counter.add(min_int64, None)
  
  histogram.record(max_double, Some(test_attributes))
  histogram.record(min_double, None)
  
  up_down_counter.add(max_int64, Some(test_attributes))
  up_down_counter.add(min_int64, None)
  
  gauge.record(max_double, Some(test_attributes))
  gauge.record(min_double, None)
  
  // 4. 特殊值操作兼容性
  let infinity = 1.0/0.0
  let neg_infinity = -1.0/0.0
  let nan = 0.0/0.0
  
  histogram.record(infinity, Some(test_attributes))
  histogram.record(neg_infinity, None)
  histogram.record(nan, Some([]))
  
  gauge.record(infinity, Some(test_attributes))
  gauge.record(neg_infinity, None)
  gauge.record(nan, Some([]))
}

test "propagation_format_compatibility" {
  // 测试传播格式的兼容性
  
  // 1. W3C TraceContext格式兼容性
  let w3c_propagator = W3CTraceContextPropagator::{}
  let carrier = MapCarrier::new()
  let ctx = Context::empty()
  
  // 测试注入操作
  w3c_propagator.inject(ctx, carrier)
  
  // 验证traceparent格式
  match carrier.get("traceparent") {
    Some(trace_parent) => {
      // 验证格式: version-trace_id-span_id-trace_flags
      assert_eq(trace_parent.length(), 55)  // "00-" + 32 + "-" + 16 + "-01"
      assert_eq(trace_parent[0:2], "00")
      assert_eq(trace_parent[51:53], "01")
      assert_eq(trace_parent[2], '-')
      assert_eq(trace_parent[35], '-')
      assert_eq(trace_parent[52], '-')
    }
    None => @test.fail("W3C TraceContext injection failed")
  }
  
  // 2. W3C Baggage格式兼容性
  let baggage_propagator = W3CBaggagePropagator::{}
  let baggage_carrier = MapCarrier::new()
  
  baggage_propagator.inject(ctx, baggage_carrier)
  
  // 验证baggage格式
  match baggage_carrier.get("baggage") {
    Some(baggage_value) => {
      // 验证格式: key1=value1,key2=value2
      assert_eq(baggage_value, "key1=value1,key2=value2")
      assert_eq(baggage_value.contains("="), true)
      assert_eq(baggage_value.contains(","), true)
    }
    None => @test.fail("W3C Baggage injection failed")
  }
  
  // 3. 复合传播器兼容性
  let composite_propagator = CompositePropagator::new([w3c_propagator, baggage_propagator])
  let composite_carrier = MapCarrier::new()
  
  composite_propagator.inject(ctx, composite_carrier)
  
  // 验证两种header都被设置
  match composite_carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Composite propagator traceparent injection failed")
  }
  
  match composite_carrier.get("baggage") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Composite propagator baggage injection failed")
  }
  
  // 4. 提取操作兼容性
  let extract_carrier = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "user_id=12345,session_id=abcdef"),
    ("other-header", "other-value")
  ])
  
  let extracted_ctx = composite_propagator.extract(ctx, extract_carrier)
  
  // 提取操作不应该崩溃
  assert_eq(true, true)
  
  // 5. 无效格式处理兼容性
  let invalid_carrier = MapCarrier::from_map([
    ("traceparent", "invalid-format"),
    ("baggage", "invalid=baggage=format=with=too=many=equals"),
    ("other-header", "other-value")
  ])
  
  let invalid_extracted_ctx = composite_propagator.extract(ctx, invalid_carrier)
  
  // 即使格式无效，也不应该崩溃
  assert_eq(true, true)
}

test "context_key_naming_compatibility" {
  // 测试Context键命名的兼容性
  
  // 1. 基础键命名兼容性
  let basic_key = create_key("basic.key")
  let ctx = Context::empty()
  let ctx_with_basic = ctx.with_value(basic_key, "basic_value")
  
  match ctx_with_basic.get(basic_key) {
    Some(value) => assert_eq(value, "basic_value")
    None => @test.fail("Basic key compatibility test failed")
  }
  
  // 2. 特殊字符键命名兼容性
  let special_key = create_key("!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
  let ctx_with_special = ctx_with_basic.with_value(special_key, "special_value")
  
  match ctx_with_special.get(special_key) {
    Some(value) => assert_eq(value, "special_value")
    None => @test.fail("Special key compatibility test failed")
  }
  
  // 3. Unicode键命名兼容性
  let unicode_key = create_key("键名测试🚀")
  let ctx_with_unicode = ctx_with_special.with_value(unicode_key, "unicode_value")
  
  match ctx_with_unicode.get(unicode_key) {
    Some(value) => assert_eq(value, "unicode_value")
    None => @test.fail("Unicode key compatibility test failed")
  }
  
  // 4. 空键命名兼容性
  let empty_key = create_key("")
  let ctx_with_empty = ctx_with_unicode.with_value(empty_key, "empty_key_value")
  
  match ctx_with_empty.get(empty_key) {
    Some(value) => assert_eq(value, "empty_key_value")
    None => @test.fail("Empty key compatibility test failed")
  }
  
  // 5. 长键命名兼容性
  let long_key_name = "a" * 1000
  let long_key = create_key(long_key_name)
  let ctx_with_long = ctx_with_empty.with_value(long_key, "long_key_value")
  
  match ctx_with_long.get(long_key) {
    Some(value) => assert_eq(value, "long_key_value")
    None => @test.fail("Long key compatibility test failed")
  }
  
  // 6. 点号分隔键命名兼容性
  let dotted_key = create_key("section.subsection.item")
  let ctx_with_dotted = ctx_with_long.with_value(dotted_key, "dotted_value")
  
  match ctx_with_dotted.get(dotted_key) {
    Some(value) => assert_eq(value, "dotted_value")
    None => @test.fail("Dotted key compatibility test failed")
  }
  
  // 7. 空格键命名兼容性
  let space_key = create_key("key with spaces")
  let ctx_with_space = ctx_with_dotted.with_value(space_key, "space_value")
  
  match ctx_with_space.get(space_key) {
    Some(value) => assert_eq(value, "space_value")
    None => @test.fail("Space key compatibility test failed")
  }
  
  // 8. 验证所有键都存在
  match ctx_with_space.get(basic_key) {
    Some(value) => assert_eq(value, "basic_value")
    None => @test.fail("Basic key lost during operations")
  }
  
  match ctx_with_space.get(special_key) {
    Some(value) => assert_eq(value, "special_value")
    None => @test.fail("Special key lost during operations")
  }
  
  match ctx_with_space.get(unicode_key) {
    Some(value) => assert_eq(value, "unicode_value")
    None => @test.fail("Unicode key lost during operations")
  }
  
  match ctx_with_space.get(empty_key) {
    Some(value) => assert_eq(value, "empty_key_value")
    None => @test.fail("Empty key lost during operations")
  }
  
  match ctx_with_space.get(long_key) {
    Some(value) => assert_eq(value, "long_key_value")
    None => @test.fail("Long key lost during operations")
  }
  
  match ctx_with_space.get(dotted_key) {
    Some(value) => assert_eq(value, "dotted_value")
    None => @test.fail("Dotted key lost during operations")
  }
  
  match ctx_with_space.get(space_key) {
    Some(value) => assert_eq(value, "space_value")
    None => @test.fail("Space key lost during operations")
  }
}
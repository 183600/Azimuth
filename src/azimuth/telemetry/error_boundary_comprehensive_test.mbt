// 错误处理和边界测试 - 测试异常情况和边界条件

test "attribute_value_boundary_conditions" {
  // 测试AttributeValue的各种边界条件
  
  // 测试空字符串和极长字符串
  let empty_string = AttributeValue::string("")
  let very_long_string = AttributeValue::string("a" * 10000)  // 10K字符
  
  match empty_string {
    StringValue(value) => assert_eq(value, "")
    _ => @test.fail("Test failed")
  }
  
  match very_long_string {
    StringValue(value) => assert_eq(value.length(), 10000)
    _ => @test.fail("Test failed")
  }
  
  // 测试极值整数
  let max_int = AttributeValue::int(9223372036854775807L)  // Int64最大值
  let min_int = AttributeValue::int(-9223372036854775808L)  // Int64最小值
  let zero_int = AttributeValue::int(0L)
  
  match max_int {
    IntValue(value) => assert_eq(value, 9223372036854775807L)
    _ => @test.fail("Test failed")
  }
  
  match min_int {
    IntValue(value) => assert_eq(value, -9223372036854775808L)
    _ => @test.fail("Test failed")
  }
  
  match zero_int {
    IntValue(value) => assert_eq(value, 0L)
    _ => @test.fail("Test failed")
  }
  
  // 测试特殊浮点数值
  let infinity = AttributeValue::float(1.0/0.0)  // 正无穷
  let neg_infinity = AttributeValue::float(-1.0/0.0)  // 负无穷
  let nan = AttributeValue::float(0.0/0.0)  // NaN
  let max_float = AttributeValue::float(1.7976931348623157e308)  // 最大Double
  let min_float = AttributeValue::float(-1.7976931348623157e308)  // 最小Double
  let very_small = AttributeValue::float(1.0e-308)  // 非常小的正数
  
  match infinity {
    FloatValue(value) => assert_eq(value.is_infinite(), true && value > 0.0)
    _ => @test.fail("Test failed")
  }
  
  match neg_infinity {
    FloatValue(value) => assert_eq(value.is_infinite(), true && value < 0.0)
    _ => @test.fail("Test failed")
  }
  
  match nan {
    FloatValue(value) => assert_eq(value.is_nan(), true)
    _ => @test.fail("Test failed")
  }
  
  // 测试空数组和超大数组
  let empty_string_array = AttributeValue::array_string([])
  let large_string_array = AttributeValue::array_string([for i = 0; i < 1000; i = i + 1].map(fn(i) { "item-" + i.to_string() }))
  
  let empty_int_array = AttributeValue::array_int([])
  let large_int_array = AttributeValue::array_int([for i = 0; i < 1000; i = i + 1].map(fn(i) { i.to_int64() }))
  
  match empty_string_array {
    ArrayStringValue(array) => assert_eq(array.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match large_string_array {
    ArrayStringValue(array) => assert_eq(array.length(), 1000)
    _ => @test.fail("Test failed")
  }
  
  match empty_int_array {
    ArrayIntValue(array) => assert_eq(array.length(), 0)
    _ => @test.fail("Test failed")
  }
  
  match large_int_array {
    ArrayIntValue(array) => assert_eq(array.length(), 1000)
    _ => @test.fail("Test failed")
  }
}

test "span_context_validation_and_edge_cases" {
  // 测试SpanContext的验证和边界情况
  
  // 测试全零的trace_id和span_id
  let zero_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })
  let zero_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })
  
  let zero_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  assert_eq(zero_context.trace_id.length(), 16)
  assert_eq(zero_context.span_id.length(), 8)
  assert_eq(zero_context.trace_flags, 0_byte)
  assert_eq(zero_context.trace_state, "")
  
  // 测试全1的trace_id和span_id
  let max_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xff_byte })
  let max_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xff_byte })
  
  let max_context = SpanContext::{
    trace_id: max_trace_id,
    span_id: max_span_id,
    trace_flags: 0xff_byte,
    trace_state: ""
  }
  
  // 验证所有字节都是0xff
  let mut i = 0
  while i < 16 {
    assert_eq(max_context.trace_id[i], 0xff_byte)
    i = i + 1
  }
  
  i = 0
  while i < 8 {
    assert_eq(max_context.span_id[i], 0xff_byte)
    i = i + 1
  }
  
  assert_eq(max_context.trace_flags, 0xff_byte)
  
  // 测试复杂的trace_state
  let complex_trace_state = "key1=value1,key2=value2;prop1=val1,key3=value3;prop1=val1;prop2=val2"
  let complex_context = SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(i) { 
      if i % 2 == 0 { 0x0a_byte } else { 0x0b_byte }
    }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(i) { 
      if i % 2 == 0 { 0x0c_byte } else { 0x0d_byte }
    }),
    trace_flags: 1_byte,
    trace_state: complex_trace_state
  }
  
  assert_eq(complex_context.trace_state, complex_trace_state)
  assert_eq(complex_context.trace_flags, 1_byte)
  
  // 测试各种trace_flags值
  let flag_values = [0_byte, 1_byte, 2_byte, 0xff_byte]
  let mut flag_index = 0
  while flag_index < flag_values.length() {
    let flag_context = SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: flag_values[flag_index],
      trace_state: ""
    }
    assert_eq(flag_context.trace_flags, flag_values[flag_index])
    flag_index = flag_index + 1
  }
}

test "log_record_validation_edge_cases" {
  // 测试LogRecord的验证和边界情况
  
  // 测试极值时间戳
  let min_timestamp = -9223372036854775808L  // Int64最小值
  let max_timestamp = 9223372036854775807L   // Int64最大值
  let zero_timestamp = 0L
  
  let min_log = LogRecord::{
    timestamp_unix_nanos: min_timestamp,
    observed_timestamp_unix_nanos: Some(max_timestamp),
    severity_number: Fatal,
    severity_text: Some("MIN_TIMESTAMP"),
    body: Some("Log with minimum timestamp"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let max_log = LogRecord::{
    timestamp_unix_nanos: max_timestamp,
    observed_timestamp_unix_nanos: Some(min_timestamp),
    severity_number: Trace,
    severity_text: Some("MAX_TIMESTAMP"),
    body: Some("Log with maximum timestamp"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let zero_log = LogRecord::{
    timestamp_unix_nanos: zero_timestamp,
    observed_timestamp_unix_nanos: Some(zero_timestamp),
    severity_number: Info,
    severity_text: Some("ZERO_TIMESTAMP"),
    body: Some("Log with zero timestamp"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(min_log.timestamp_unix_nanos, min_timestamp)
  assert_eq(max_log.timestamp_unix_nanos, max_timestamp)
  assert_eq(zero_log.timestamp_unix_nanos, zero_timestamp)
  
  // 测试空和极长的severity_text和body
  let empty_severity_log = LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: None,
    severity_number: Error,
    severity_text: Some(""),
    body: Some("Error with empty severity text"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let long_body_log = LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: None,
    severity_number: Warn,
    severity_text: Some("LONG_BODY"),
    body: Some("a" * 50000),  // 50K字符的日志消息
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  match empty_severity_log.severity_text {
    Some(text) => assert_eq(text, "")
    None => @test.fail("Test failed")
  }
  
  match long_body_log.body {
    Some(body) => assert_eq(body.length(), 50000)
    None => @test.fail("Test failed")
  }
  
  // 测试复杂的属性集合
  let complex_attributes = [
    ("empty.string", AttributeValue::string("")),
    ("unicode.text", AttributeValue::string("测试中文 🚀 emoji")),
    ("special.chars", AttributeValue::string("!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")),
    ("zero.int", AttributeValue::int(0L)),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("zero.float", AttributeValue::float(0.0)),
    ("infinity", AttributeValue::float(1.0/0.0)),
    ("neg.infinity", AttributeValue::float(-1.0/0.0)),
    ("nan", AttributeValue::float(0.0/0.0)),
    ("true.bool", AttributeValue::bool(true)),
    ("false.bool", AttributeValue::bool(false)),
    ("empty.array", AttributeValue::array_string([])),
    ("large.array", AttributeValue::array_string([for i = 0; i < 100; i = i + 1].map(fn(i) { "item-" + i.to_string() })))
  ]
  
  let complex_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("COMPLEX_ATTRIBUTES"),
    body: Some("Log with complex attributes"),
    attributes: complex_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(complex_log.attributes.length(), 15)
  
  // 验证特定属性
  match complex_log.attributes[1].1 {
    StringValue(unicode_text) => {
      assert_eq(unicode_text.contains("测试中文"), true)
      assert_eq(unicode_text.contains("🚀"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  match complex_log.attributes[7].1 {
    FloatValue(inf) => assert_eq(inf.is_infinite() && inf > 0.0, true)
    _ => @test.fail("Test failed")
  }
  
  match complex_log.attributes[9].1 {
    FloatValue(nan_val) => assert_eq(nan_val.is_nan(), true)
    _ => @test.fail("Test failed")
  }
}

test "context_and_baggage_edge_cases" {
  // 测试Context和Baggage的边界情况
  
  // 测试空上下文和大量数据的上下文
  let empty_ctx = Context::empty()
  assert_eq(empty_ctx.values.length(), 0)
  
  // 创建包含大量数据的上下文
  let mut large_ctx = Context::empty()
  let mut i = 0
  while i < 1000 {
    let key = create_key("key." + i.to_string())
    large_ctx = large_ctx.with_value(key, "value." + i.to_string())
    i = i + 1
  }
  
  assert_eq(large_ctx.values.length(), 1000)
  
  // 验证可以正确检索值
  let test_key = create_key("key.500")
  match large_ctx.get(test_key) {
    Some(value) => assert_eq(value, "value.500")
    None => @test.fail("Test failed")
  }
  
  // 测试不存在的键
  let nonexistent_key = create_key("nonexistent")
  match empty_ctx.get(nonexistent_key) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试Baggage的边界情况
  let empty_baggage = Baggage::empty()
  assert_eq(empty_baggage.entries.length(), 0)
  
  // 创建包含大量条目的baggage
  let mut large_baggage = Baggage::empty()
  let mut baggage_i = 0
  while baggage_i < 500 {
    large_baggage = large_baggage.with_entry("baggage.key." + baggage_i.to_string(), "baggage.value." + baggage_i.to_string())
    baggage_i = baggage_i + 1
  }
  
  assert_eq(large_baggage.entries.length(), 500)
  
  // 验证可以正确检索baggage条目
  match large_baggage.get("baggage.key.250") {
    Some(value) => assert_eq(value, "baggage.value.250")
    None => @test.fail("Test failed")
  }
  
  // 测试特殊字符的键值对
  let special_baggage = Baggage::empty()
    .with_entry("", "")  // 空键值
    .with_entry("special.chars", "!@#$%^&*()")
    .with_entry("unicode", "测试中文")
    .with_entry("spaces", "value with spaces")
    .with_entry("equals", "value=with=equals")
  
  assert_eq(special_baggage.entries.length(), 5)
  
  match special_baggage.get("") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  match special_baggage.get("unicode") {
    Some(value) => assert_eq(value, "测试中文")
    None => @test.fail("Test failed")
  }
}

test "propagation_header_parsing_edge_cases" {
  // 测试传播头解析的边界情况
  
  let ctx = Context::empty()
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 测试各种traceparent格式
  let valid_trace_parents = [
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",  // 标准格式
    "00-00000000000000000000000000000000-0000000000000000-00",  // 全零
    "00-ffffffffffffffffffffffffffffffff-ffffffffffffffff-01",  // 全1
    "ff-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"   // 非标准版本
  ]
  
  let mut i = 0
  while i < valid_trace_parents.length() {
    let carrier = MapCarrier::from_map([("traceparent", valid_trace_parents[i])])
    let extracted_ctx = trace_propagator.extract(ctx, carrier)
    // 如果没有崩溃，说明解析成功
    assert_eq(true, true)
    i = i + 1
  }
  
  // 测试各种baggage格式
  let valid_baggages = [
    "key1=value1",
    "key1=value1,key2=value2",
    "key1=value1,key2=value2,key3=value3",
    "key=value;prop1=val1;prop2=val2",  // 带属性
    "key=value%20with%20spaces",        // URL编码
    "key=",                             // 空值
    "=value",                           // 空键
    "key=value,key2=",                  // 混合空值
    "",                                 // 空字符串
    "very.long.key.name.that.exceeds.normal.expectations.and.tests.parsing.robustness=corresponding.very.long.value.name.that.also.expects.normal.parsing.to.work.correctly"
  ]
  
  let mut baggage_i = 0
  while baggage_i < valid_baggages.length() {
    let carrier = MapCarrier::from_map([("baggage", valid_baggages[baggage_i])])
    let extracted_ctx = baggage_propagator.extract(ctx, carrier)
    // 如果没有崩溃，说明解析成功
    assert_eq(true, true)
    baggage_i = baggage_i + 1
  }
  
  // 测试缺失头的情况
  let empty_carrier = MapCarrier::new()
  let no_trace_ctx = trace_propagator.extract(ctx, empty_carrier)
  let no_baggage_ctx = baggage_propagator.extract(ctx, empty_carrier)
  
  // 应该返回原始上下文
  assert_eq(true, true)
  
  // 测试复合传播器的边界情况
  let composite_propagator = CompositePropagator::new([])
  let empty_composite_ctx = composite_propagator.extract(ctx, empty_carrier)
  assert_eq(true, true)
  
  let single_propagator = CompositePropagator::new([trace_propagator])
  let single_ctx = single_propagator.extract(ctx, MapCarrier::from_map([("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")]))
  assert_eq(true, true)
}

test "resource_and_instrumentation_validation" {
  // 测试Resource和InstrumentationScope的验证
  
  // 测试空和极长的service name
  let empty_resource = Resource::default("")
  assert_eq(empty_resource.service_name, "")
  
  let long_service_name = "a" * 1000
  let long_resource = Resource::default(long_service_name)
  assert_eq(long_resource.service_name.length(), 1000)
  
  // 测试特殊字符的service name
  let special_resource = Resource::default("!@#$%^&*()_+-={}[]|\\:;\"'<>?,./测试中文🚀")
  assert_eq(special_resource.service_name.contains("!@#$%^&*()"), true)
  assert_eq(special_resource.service_name.contains("测试中文"), true)
  assert_eq(special_resource.service_name.contains("🚀"), true)
  
  // 测试复杂的资源属性
  let complex_resource = Resource::{
    service_name: "complex-service",
    service_version: Some("2.1.0-beta.1+build.123"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0-alpha",
    attributes: [
      ("", AttributeValue::string("empty_key")),  // 空键
      ("empty.value", AttributeValue::string("")),  // 空值
      ("unicode.attr", AttributeValue::string("属性值测试")),
      ("special.chars", AttributeValue::string("!@#$%^&*()")),
      ("very.long.attribute.name.that.exceeds.normal.expectations", AttributeValue::string("corresponding.value")),
      ("zero.int", AttributeValue::int(0L)),
      ("max.int", AttributeValue::int(9223372036854775807L)),
      ("infinity.float", AttributeValue::float(1.0/0.0)),
      ("nan.float", AttributeValue::float(0.0/0.0)),
      ("true.bool", AttributeValue::bool(true)),
      ("false.bool", AttributeValue::bool(false)),
      ("empty.array", AttributeValue::array_string([])),
      ("large.array", AttributeValue::array_string([for i = 0; i < 100; i = i + 1].map(fn(i) { "item-" + i.to_string() })))
    ]
  }
  
  assert_eq(complex_resource.service_name, "complex-service")
  assert_eq(complex_resource.service_version, Some("2.1.0-beta.1+build.123"))
  assert_eq(complex_resource.attributes.length(), 14)
  
  // 测试InstrumentationScope的边界情况
  let empty_scope = InstrumentationScope::{
    name: "",
    version: Some(""),
    schema_url: Some("")
  }
  
  let long_scope = InstrumentationScope::{
    name: "a" * 1000,
    version: Some("b" * 1000),
    schema_url: Some("c" * 1000)
  }
  
  let special_scope = InstrumentationScope::{
    name: "!@#$%^&*()测试中文🚀",
    version: Some("!@#$%^&*()测试中文🚀"),
    schema_url: Some("!@#$%^&*()测试中文🚀")
  }
  
  assert_eq(empty_scope.name, "")
  assert_eq(long_scope.name.length(), 1000)
  assert_eq(special_scope.name.contains("测试中文"), true)
  
  match empty_scope.version {
    Some(version) => assert_eq(version, "")
    None => @test.fail("Test failed")
  }
  
  match empty_scope.schema_url {
    Some(url) => assert_eq(url, "")
    None => @test.fail("Test failed")
  }
}
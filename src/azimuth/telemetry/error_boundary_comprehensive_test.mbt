// 错误处理和边界情况综合测试
test "attribute_value_boundary_cases" {
  // 测试AttributeValue的各种边界情况
  
  // 测试极长字符串
  let very_long_string = "a".repeat(1000000)
  let long_string_attr = AttributeValue::string(very_long_string)
  match long_string_attr {
    StringValue(s) => assert_eq(s.length(), 1000000)
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试极值整数
  let max_int_attr = AttributeValue::int(9223372036854775807L)
  let min_int_attr = AttributeValue::int(-9223372036854775808L)
  
  match max_int_attr {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Expected IntValue")
  }
  
  match min_int_attr {
    IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Expected IntValue")
  }
  
  // 测试特殊浮点值
  let infinity_attr = AttributeValue::float(1.0/0.0)
  let neg_infinity_attr = AttributeValue::float(-1.0/0.0)
  let nan_attr = AttributeValue::float(0.0/0.0)
  
  match infinity_attr {
    FloatValue(f) => assert_eq(f > 0.0 && f.is_infinite(), true)
    _ => @test.fail("Expected FloatValue")
  }
  
  match neg_infinity_attr {
    FloatValue(f) => assert_eq(f < 0.0 && f.is_infinite(), true)
    _ => @test.fail("Expected FloatValue")
  }
  
  match nan_attr {
    FloatValue(f) => assert_eq(f.is_nan(), true)
    _ => @test.fail("Expected FloatValue")
  }
  
  // 测试极小和极大浮点数
  let smallest_float = AttributeValue::float(1.7976931348623157e-308)
  let largest_float = AttributeValue::float(1.7976931348623157e+308)
  
  match smallest_float {
    FloatValue(f) => assert_eq(f > 0.0 && f < 1.0e-300, true)
    _ => @test.fail("Expected FloatValue")
  }
  
  match largest_float {
    FloatValue(f) => assert_eq(f > 1.0e+300, true)
    _ => @test.fail("Expected FloatValue")
  }
}

test "span_context_boundary_cases" {
  // 测试SpanContext的边界情况
  
  // 测试全零trace_id和span_id
  let zero_trace_id = Array::make(16, 0x00_byte)
  let zero_span_id = Array::make(8, 0x00_byte)
  
  let zero_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0x00_byte,
    trace_state: ""
  }
  
  assert_eq(zero_context.trace_id.length(), 16)
  assert_eq(zero_context.span_id.length(), 8)
  assert_eq(zero_context.trace_flags, 0x00_byte)
  assert_eq(zero_context.trace_state, "")
  
  // 测试全F的trace_id和span_id
  let max_trace_id = Array::make(16, 0xFF_byte)
  let max_span_id = Array::make(8, 0xFF_byte)
  
  let max_context = SpanContext::{
    trace_id: max_trace_id,
    span_id: max_span_id,
    trace_flags: 0xFF_byte,
    trace_state: "very-long-trace-state-with-lots-of-data-and-information"
  }
  
  assert_eq(max_context.trace_id.length(), 16)
  assert_eq(max_context.span_id.length(), 8)
  assert_eq(max_context.trace_flags, 0xFF_byte)
  assert_eq(max_context.trace_state.length(), 58)
  
  // 测试交替的trace_id和span_id
  let alternating_trace_id = Array::make(16, 0x00_byte)
  let alternating_span_id = Array::make(8, 0x00_byte)
  let mut i = 0
  while i < 16 {
    alternating_trace_id[i] = if i % 2 == 0 { 0xAA_byte } else { 0x55_byte }
    i = i + 1
  }
  let mut j = 0
  while j < 8 {
    alternating_span_id[j] = if j % 2 == 0 { 0xAA_byte } else { 0x55_byte }
    j = j + 1
  }
  
  let alternating_context = SpanContext::{
    trace_id: alternating_trace_id,
    span_id: alternating_span_id,
    trace_flags: 0x55_byte,
    trace_state: "custom=value1,another=value2"
  }
  
  assert_eq(alternating_context.trace_id[0], 0xAA_byte)
  assert_eq(alternating_context.trace_id[1], 0x55_byte)
  assert_eq(alternating_context.span_id[0], 0xAA_byte)
  assert_eq(alternating_context.span_id[1], 0x55_byte)
  assert_eq(alternating_context.trace_flags, 0x55_byte)
}

test "log_record_builder_boundary_cases" {
  // 测试LogRecordBuilder的边界情况
  
  // 测试极长的时间戳
  let max_timestamp_record = LogRecord::builder()
    .timestamp(9223372036854775807L)
    .severity(Info)
    .body("Max timestamp test")
    .build()
  
  assert_eq(max_timestamp_record.timestamp_unix_nanos, 9223372036854775807L)
  
  // 测试负时间戳
  let negative_timestamp_record = LogRecord::builder()
    .timestamp(-9223372036854775808L)
    .severity(Debug)
    .body("Negative timestamp test")
    .build()
  
  assert_eq(negative_timestamp_record.timestamp_unix_nanos, -9223372036854775808L)
  
  // 测试零时间戳
  let zero_timestamp_record = LogRecord::builder()
    .timestamp(0L)
    .severity(Warn)
    .body("Zero timestamp test")
    .build()
  
  assert_eq(zero_timestamp_record.timestamp_unix_nanos, 0L)
  
  // 测试极长的body
  let very_long_body = "x".repeat(1000000)
  let long_body_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Error)
    .body(very_long_body)
    .build()
  
  match long_body_record.body {
    Some(message) => assert_eq(message.length(), 1000000)
    None => @test.fail("Expected body message")
  }
  
  // 测试空body
  let empty_body_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Fatal)
    .body("")
    .build()
  
  match empty_body_record.body {
    Some(message) => assert_eq(message, "")
    None => @test.fail("Expected empty body message")
  }
  
  // 测试包含特殊字符的body
  let special_body_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Trace)
    .body("Special chars: !@#$%^&*(){}[]|\\:;\"'<>?,./ Unicode: 测试🚀\n\t\r")
    .build()
  
  match special_body_record.body {
    Some(message) => {
      assert_eq(message.contains("!@#$%^&*()"), true)
      assert_eq(message.contains("测试🚀"), true)
      assert_eq(message.contains("\n\t\r"), true)
    }
    None => @test.fail("Expected special chars body")
  }
}

test "context_and_baggage_boundary_cases" {
  // 测试Context和Baggage的边界情况
  
  // 测试极长的键和值
  let very_long_key = "k".repeat(10000)
  let very_long_value = "v".repeat(10000)
  
  let context = context::Context::empty()
  let long_key = create_key(very_long_key)
  let context_with_long = context.with_value(long_key, very_long_value)
  
  match context_with_long.get(long_key) {
    Some(value) => assert_eq(value, very_long_value)
    None => @test.fail("Expected to find long key")
  }
  
  // 测试包含特殊字符的键和值
  let special_key = create_key("special!@#$%^&*(){}[]|\\:;\"'<>?,./Unicode键测试🚀")
  let special_value = "special!@#$%^&*(){}[]|\\:;\"'<>?,./Unicode值测试🚀"
  
  let context_with_special = context.with_value(special_key, special_value)
  match context_with_special.get(special_key) {
    Some(value) => assert_eq(value, special_value)
    None => @test.fail("Expected to find special key")
  }
  
  // 测试Baggage的边界情况
  let baggage = Baggage::empty()
    .with_entry(very_long_key, very_long_value)
    .with_entry("special!@#$%", "special chars value")
    .with_entry("", "empty key value")
    .with_entry("empty value", "")
  
  match baggage.get(very_long_key) {
    Some(value) => assert_eq(value, very_long_value)
    None => @test.fail("Expected to find long baggage key")
  }
  
  match baggage.get("special!@#$%") {
    Some(value) => assert_eq(value, "special chars value")
    None => @test.fail("Expected to find special baggage key")
  }
  
  match baggage.get("") {
    Some(value) => assert_eq(value, "empty key value")
    None => @test.fail("Expected to find empty baggage key")
  }
  
  match baggage.get("empty value") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected to find empty baggage value")
  }
}

test "propagation_boundary_cases" {
  // 测试Propagation的边界情况
  
  // 测试包含特殊字符的头部值
  let special_data = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "key1=value!@#$%,key2=value测试🚀,key3=value with spaces"),
    ("custom-header", "special!@#$%^&*(){}[]|\\:;\"'<>?,./Unicode测试🚀"),
    ("empty-header", ""),
    ("very-long-header", "x".repeat(10000))
  ]
  let carrier = MapCarrier::from_map(special_data)
  
  // 测试从特殊字符carrier提取
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let ctx = context::Context::empty()
  
  let extracted_ctx1 = trace_propagator.extract(ctx, carrier)
  let extracted_ctx2 = baggage_propagator.extract(ctx, carrier)
  
  assert_eq(true, true) // 验证不会出错
  
  // 测试向空carrier注入
  let empty_carrier = MapCarrier::new()
  trace_propagator.inject(ctx, empty_carrier)
  baggage_propagator.inject(ctx, empty_carrier)
  
  // 验证注入的头部
  match empty_carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent header")
  }
  
  match empty_carrier.get("baggage") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected baggage header")
  }
  
  // 测试复合传播器与空传播器数组
  let empty_propagators = []
  let empty_composite = CompositePropagator::new(empty_propagators)
  
  empty_composite.inject(ctx, empty_carrier)
  let extracted_ctx3 = empty_composite.extract(ctx, carrier)
  
  assert_eq(true, true) // 验证不会出错
}

test "metrics_boundary_cases" {
  // 测试Metrics的边界情况
  
  let meter = NoopMeter::{}
  
  // 测试极值指标操作
  let counter = meter.create_counter("boundary_counter", None, None)
  let histogram = meter.create_histogram("boundary_histogram", None, None)
  let up_down_counter = meter.create_up_down_counter("boundary_up_down", None, None)
  let gauge = meter.create_gauge("boundary_gauge", None, None)
  
  // 测试极值counter操作
  counter.add(9223372036854775807L, Some([
    ("max.value", AttributeValue::int(9223372036854775807L)),
    ("very.long.key", AttributeValue::string("k".repeat(1000))),
    ("special.chars", AttributeValue::string("!@#$%^&*()测试🚀"))
  ]))
  
  counter.add(-9223372036854775808L, Some([
    ("min.value", AttributeValue::int(-9223372036854775808L)),
    ("empty.value", AttributeValue::string(""))
  ]))
  
  // 测试极值histogram操作
  histogram.record(1.7976931348623157e+308, Some([
    ("max.float", AttributeValue::float(1.7976931348623157e+308)),
    ("infinity", AttributeValue::float(1.0/0.0)),
    ("neg.infinity", AttributeValue::float(-1.0/0.0)),
    ("nan", AttributeValue::float(0.0/0.0))
  ]))
  
  histogram.record(-1.7976931348623157e+308, Some([
    ("min.float", AttributeValue::float(-1.7976931348623157e+308))
  ]))
  
  // 测试极值up_down_counter操作
  up_down_counter.add(9223372036854775807L, Some([("positive.max", AttributeValue::string("test"))]))
  up_down_counter.add(-9223372036854775808L, Some([("negative.max", AttributeValue::string("test"))]))
  
  // 测试极值gauge操作
  gauge.record(1.7976931348623157e+308, Some([("float.max", AttributeValue::string("test"))]))
  gauge.record(-1.7976931348623157e+308, Some([("float.min", AttributeValue::string("test"))]))
  
  assert_eq(true, true) // 验证所有操作不会出错
}

test "resource_boundary_cases" {
  // 测试Resource的边界情况
  
  // 测试极长的服务名称
  let very_long_service_name = "service-".repeat(10000)
  let long_resource = Resource::default(very_long_service_name)
  
  assert_eq(long_resource.service_name, very_long_service_name)
  assert_eq(long_resource.service_name.length(), 80000) // "service-" is 8 chars, repeated 10000 times
  
  // 测试包含特殊字符的服务名称
  let special_service_name = "special!@#$%^&*(){}[]|\\:;\"'<>?,./Unicode服务测试🚀"
  let special_resource = Resource::default(special_service_name)
  
  assert_eq(special_resource.service_name, special_service_name)
  
  // 测试空服务名称
  let empty_resource = Resource::default("")
  assert_eq(empty_resource.service_name, "")
  
  // 测试只包含空格的服务名称
  let space_resource = Resource::default("   ")
  assert_eq(space_resource.service_name, "   ")
  
  // 测试Unicode服务名称
  let unicode_resource = Resource::default(" Unicode服务测试🚀 with spaces and 特殊字符!@#$% ")
  assert_eq(unicode_resource.service_name, " Unicode服务测试🚀 with spaces and 特殊字符!@#$% ")
  
  // 验证所有资源的SDK信息
  assert_eq(long_resource.telemetry_sdk_name, "azimuth")
  assert_eq(special_resource.telemetry_sdk_name, "azimuth")
  assert_eq(empty_resource.telemetry_sdk_name, "azimuth")
  assert_eq(space_resource.telemetry_sdk_name, "azimuth")
  assert_eq(unicode_resource.telemetry_sdk_name, "azimuth")
  
  assert_eq(long_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(special_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(empty_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(space_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(unicode_resource.telemetry_sdk_version, "0.1.0")
}

test "array_boundary_cases" {
  // 测试各种数组的边界情况
  
  // 测试空数组
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
  
  // 测试单元素数组
  let single_string_array = AttributeValue::array_string(["single"])
  let single_int_array = AttributeValue::array_int([42L])
  let single_float_array = AttributeValue::array_float([3.14])
  let single_bool_array = AttributeValue::array_bool([true])
  
  match single_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "single")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 测试包含特殊值的数组
  let special_string_array = AttributeValue::array_string([
    "", " ", "!@#$%", "测试🚀", "very long string".repeat(1000)
  ])
  
  let special_int_array = AttributeValue::array_int([
    0L, -1L, 1L, 9223372036854775807L, -9223372036854775808L
  ])
  
  let special_float_array = AttributeValue::array_float([
    0.0, -0.0, 1.0/0.0, -1.0/0.0, 0.0/0.0, 1.7976931348623157e+308
  ])
  
  let special_bool_array = AttributeValue::array_bool([true, false])
  
  match special_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], "")
      assert_eq(arr[1], " ")
      assert_eq(arr[2], "!@#$%")
      assert_eq(arr[3], "测试🚀")
      assert_eq(arr[4].length(), 16000) // "very long string" is 16 chars, repeated 1000 times
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match special_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 0L)
      assert_eq(arr[1], -1L)
      assert_eq(arr[2], 1L)
      assert_eq(arr[3], 9223372036854775807L)
      assert_eq(arr[4], -9223372036854775808L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
}
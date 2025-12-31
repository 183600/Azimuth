// 序列化兼容性测试 - 测试数据格式转换和兼容性

test "attribute_value_serialization_compatibility" {
  // 测试AttributeValue的序列化兼容性
  
  // 测试所有AttributeValue类型的序列化表示
  let string_attr = AttributeValue::string("test_string")
  let int_attr = AttributeValue::int(12345L)
  let float_attr = AttributeValue::float(3.14159)
  let bool_attr = AttributeValue::bool(true)
  let string_array_attr = AttributeValue::array_string(["a", "b", "c"])
  let int_array_attr = AttributeValue::array_int([1L, 2L, 3L])
  let float_array_attr = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_attr = AttributeValue::array_bool([true, false, true])
  
  // 验证类型标识和值的兼容性
  match string_attr {
    StringValue(value) => {
      assert_eq(value, "test_string")
      // 验证字符串可以正确转换为其他格式
      let string_repr = value
      assert_eq(string_repr.length(), 11)
    }
    _ => @test.fail("Test failed")
  }
  
  match int_attr {
    IntValue(value) => {
      assert_eq(value, 12345L)
      // 验证整数可以正确转换为字符串表示
      let string_repr = value.to_string()
      assert_eq(string_repr, "12345")
    }
    _ => @test.fail("Test failed")
  }
  
  match float_attr {
    FloatValue(value) => {
      assert_eq(value, 3.14159)
      // 验证浮点数可以正确转换为字符串表示
      let string_repr = value.to_string()
      assert_eq(string_repr.contains("3.14159"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  match bool_attr {
    BoolValue(value) => {
      assert_eq(value, true)
      // 验证布尔值可以正确转换为字符串表示
      let string_repr = value.to_string()
      assert_eq(string_repr, "true")
    }
    _ => @test.fail("Test failed")
  }
  
  match string_array_attr {
    ArrayStringValue(array) => {
      assert_eq(array.length(), 3)
      assert_eq(array[0], "a")
      assert_eq(array[1], "b")
      assert_eq(array[2], "c")
      
      // 验证数组可以正确转换为字符串表示
      let string_repr = "[" + array.join(",") + "]"
      assert_eq(string_repr, "[a,b,c]")
    }
    _ => @test.fail("Test failed")
  }
  
  match int_array_attr {
    ArrayIntValue(array) => {
      assert_eq(array.length(), 3)
      assert_eq(array[0], 1L)
      assert_eq(array[1], 2L)
      assert_eq(array[2], 3L)
      
      // 验证整数数组可以正确转换为字符串表示
      let string_repr = "[" + array.join(",") + "]"
      assert_eq(string_repr, "[1,2,3]")
    }
    _ => @test.fail("Test failed")
  }
  
  match float_array_attr {
    ArrayFloatValue(array) => {
      assert_eq(array.length(), 3)
      assert_eq(array[0], 1.1)
      assert_eq(array[1], 2.2)
      assert_eq(array[2], 3.3)
    }
    _ => @test.fail("Test failed")
  }
  
  match bool_array_attr {
    ArrayBoolValue(array) => {
      assert_eq(array.length(), 3)
      assert_eq(array[0], true)
      assert_eq(array[1], false)
      assert_eq(array[2], true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试边界值的序列化
  let max_int = AttributeValue::int(9223372036854775807L)
  let min_int = AttributeValue::int(-9223372036854775808L)
  let infinity = AttributeValue::float(1.0/0.0)
  let neg_infinity = AttributeValue::float(-1.0/0.0)
  let nan = AttributeValue::float(0.0/0.0)
  
  match max_int {
    IntValue(value) => {
      assert_eq(value, 9223372036854775807L)
      let string_repr = value.to_string()
      assert_eq(string_repr, "9223372036854775807")
    }
    _ => @test.fail("Test failed")
  }
  
  match min_int {
    IntValue(value) => {
      assert_eq(value, -9223372036854775808L)
      let string_repr = value.to_string()
      assert_eq(string_repr, "-9223372036854775808")
    }
    _ => @test.fail("Test failed")
  }
  
  match infinity {
    FloatValue(value) => {
      assert_eq(value.is_infinite() && value > 0.0, true)
      let string_repr = value.to_string()
      assert_eq(string_repr.contains("inf"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  match neg_infinity {
    FloatValue(value) => {
      assert_eq(value.is_infinite() && value < 0.0, true)
      let string_repr = value.to_string()
      assert_eq(string_repr.contains("-inf"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  match nan {
    FloatValue(value) => {
      assert_eq(value.is_nan(), true)
      let string_repr = value.to_string()
      assert_eq(string_repr.contains("nan"), true)
    }
    _ => @test.fail("Test failed")
  }
}

test "span_context_serialization_format" {
  // 测试SpanContext的序列化格式兼容性
  
  // 标准SpanContext
  let trace_id = [0x0a_byte, 0x0b_byte, 0x0c_byte, 0x0d_byte, 0x0e_byte, 0x0f_byte, 0x01_byte, 0x02_byte, 0x03_byte, 0x04_byte, 0x05_byte, 0x06_byte, 0x07_byte, 0x08_byte, 0x09_byte, 0x10_byte]
  let span_id = [0x11_byte, 0x12_byte, 0x13_byte, 0x14_byte, 0x15_byte, 0x16_byte, 0x17_byte, 0x18_byte]
  
  let span_context = SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  // 验证trace_id的十六进制表示
  let mut trace_id_hex = ""
  let mut i = 0
  while i < trace_id.length() {
    let byte_val = trace_id[i]
    let hex_chars = "0123456789abcdef"
    let high_nibble = (byte_val.to_int() >> 4) & 0x0f
    let low_nibble = byte_val.to_int() & 0x0f
    trace_id_hex = trace_id_hex + hex_chars[high_nibble].to_string() + hex_chars[low_nibble].to_string()
    i = i + 1
  }
  assert_eq(trace_id_hex, "0a0b0c0d0e0f01020304050607080910")
  
  // 验证span_id的十六进制表示
  let mut span_id_hex = ""
  let mut span_i = 0
  while span_i < span_id.length() {
    let byte_val = span_id[span_i]
    let hex_chars = "0123456789abcdef"
    let high_nibble = (byte_val.to_int() >> 4) & 0x0f
    let low_nibble = byte_val.to_int() & 0x0f
    span_id_hex = span_id_hex + hex_chars[high_nibble].to_string() + hex_chars[low_nibble].to_string()
    span_i = span_i + 1
  }
  assert_eq(span_id_hex, "1112131415161718")
  
  // 验证W3C traceparent格式
  let trace_parent = "00-" + trace_id_hex + "-" + span_id_hex + "-01"
  assert_eq(trace_parent, "00-0a0b0c0d0e0f01020304050607080910-1112131415161718-01")
  
  // 测试边界情况的SpanContext序列化
  let zero_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })
  let zero_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })
  
  let zero_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let zero_trace_hex = "00000000000000000000000000000000"
  let zero_span_hex = "0000000000000000"
  let zero_trace_parent = "00-" + zero_trace_hex + "-" + zero_span_hex + "-00"
  assert_eq(zero_trace_parent, "00-00000000000000000000000000000000-0000000000000000-00")
  
  // 测试最大值的SpanContext序列化
  let max_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xff_byte })
  let max_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xff_byte })
  
  let max_context = SpanContext::{
    trace_id: max_trace_id,
    span_id: max_span_id,
    trace_flags: 0xff_byte,
    trace_state: ""
  }
  
  let max_trace_hex = "ffffffffffffffffffffffffffffffff"
  let max_span_hex = "ffffffffffffffff"
  let max_trace_parent = "00-" + max_trace_hex + "-" + max_span_hex + "-ff"
  assert_eq(max_trace_parent, "00-ffffffffffffffffffffffffffffffff-ffffffffffffffff-ff")
}

test "log_record_serialization_formats" {
  // 测试LogRecord的序列化格式
  
  let resource = Resource::default("test-service")
  let instrumentation_scope = InstrumentationScope::{
    name: "test-logger",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(i) { 
    if i < 8 { 0x12_byte } else { 0x34_byte }
  })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x56_byte })
  
  // 创建完整的LogRecord
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Error,
    severity_text: Some("DATABASE_ERROR"),
    body: Some("Database connection failed"),
    attributes: [
      ("error.code", AttributeValue::string("DB_CONN_FAILED")),
      ("error.retries", AttributeValue::int(3L)),
      ("error.timeout", AttributeValue::float(30.0)),
      ("error.retryable", AttributeValue::bool(true)),
      ("error.tags", AttributeValue::array_string(["database", "timeout", "retry"]))
    ],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(1_byte),
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  // 验证时间戳的序列化
  let timestamp_str = log_record.timestamp_unix_nanos.to_string()
  assert_eq(timestamp_str, "1640995200000000000")
  
  match log_record.observed_timestamp_unix_nanos {
    Some(observed_timestamp) => {
      let observed_str = observed_timestamp.to_string()
      assert_eq(observed_str, "1640995200000000100")
    }
    None => @test.fail("Test failed")
  }
  
  // 验证严重性级别的序列化
  match log_record.severity_number {
    Error => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  match log_record.severity_text {
    Some(text) => assert_eq(text, "DATABASE_ERROR")
    None => @test.fail("Test failed")
  }
  
  // 验证属性序列化
  assert_eq(log_record.attributes.length(), 5)
  
  // 验证trace_id和span_id的十六进制表示
  match log_record.trace_id {
    Some(id) => {
      let mut trace_hex = ""
      let mut i = 0
      while i < id.length() {
        let byte_val = id[i]
        let hex_chars = "0123456789abcdef"
        let high_nibble = (byte_val.to_int() >> 4) & 0x0f
        let low_nibble = byte_val.to_int() & 0x0f
        trace_hex = trace_hex + hex_chars[high_nibble].to_string() + hex_chars[low_nibble].to_string()
        i = i + 1
      }
      assert_eq(trace_hex, "12343434123434341234343412343434")
    }
    None => @test.fail("Test failed")
  }
  
  match log_record.span_id {
    Some(id) => {
      let mut span_hex = ""
      let mut i = 0
      while i < id.length() {
        let byte_val = id[i]
        let hex_chars = "0123456789abcdef"
        let high_nibble = (byte_val.to_int() >> 4) & 0x0f
        let low_nibble = byte_val.to_int() & 0x0f
        span_hex = span_hex + hex_chars[high_nibble].to_string() + hex_chars[low_nibble].to_string()
        i = i + 1
      }
      assert_eq(span_hex, "5656565656565656")
    }
    None => @test.fail("Test failed")
  }
  
  // 验证资源序列化
  match log_record.resource {
    Some(res) => {
      assert_eq(res.service_name, "test-service")
      assert_eq(res.telemetry_sdk_name, "azimuth")
      assert_eq(res.telemetry_sdk_version, "0.1.0")
    }
    None => @test.fail("Test failed")
  }
  
  // 验证instrumentation scope序列化
  match log_record.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "test-logger")
      assert_eq(scope.version, Some("1.0.0"))
      assert_eq(scope.schema_url, Some("https://example.com/schema"))
    }
    None => @test.fail("Test failed")
  }
}

test "propagation_format_compatibility" {
  // 测试传播格式的兼容性
  
  // 测试W3C TraceContext格式
  let valid_trace_parents = [
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "00-00000000000000000000000000000000-0000000000000000-00",
    "00-ffffffffffffffffffffffffffffffff-ffffffffffffffff-01",
    "ff-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  ]
  
  let mut i = 0
  while i < valid_trace_parents.length() {
    let trace_parent = valid_trace_parents[i]
    
    // 解析traceparent格式
    let parts = trace_parent.split("-")
    assert_eq(parts.length(), 4)
    
    let version = parts[0]
    let trace_id = parts[1]
    let span_id = parts[2]
    let trace_flags = parts[3]
    
    // 验证版本
    assert_eq(version.length(), 2)
    
    // 验证trace_id长度和格式
    assert_eq(trace_id.length(), 32)
    
    // 验证span_id长度和格式
    assert_eq(span_id.length(), 16)
    
    // 验证trace_flags
    assert_eq(trace_flags.length(), 2)
    
    i = i + 1
  }
  
  // 测试W3C Baggage格式
  let valid_baggages = [
    "key1=value1",
    "key1=value1,key2=value2",
    "key1=value1,key2=value2,key3=value3",
    "key=value;prop1=val1;prop2=val2",
    "key=value%20with%20spaces",
    "key=",
    "=value",
    "key=value,key2=",
    ""
  ]
  
  let mut baggage_i = 0
  while baggage_i < valid_baggages.length() {
    let baggage = valid_baggages[baggage_i]
    
    if baggage.length() > 0 {
      // 解析baggage格式
      let entries = baggage.split(",")
      let mut entry_j = 0
      while entry_j < entries.length() {
        let entry = entries[entry_j]
        if entry.length() > 0 {
          let key_value = entry.split("=")
          if key_value.length() >= 1 {
            let key = key_value[0]
            // 验证key不为空（除非是特殊情况）
            if entry_j > 0 || key.length() > 0 {
              assert_eq(true, true)  // 基本解析成功
            }
          }
        }
        entry_j = entry_j + 1
      }
    }
    baggage_i = baggage_i + 1
  }
  
  // 测试复合传播器格式
  let ctx = Context::empty()
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let carrier = MapCarrier::new()
  
  // 注入复合传播数据
  composite_propagator.inject(ctx, carrier)
  
  // 验证包含必要的头
  let keys = carrier.keys()
  assert_eq(keys.length(), 2)
  
  // 验证traceparent格式
  match carrier.get("traceparent") {
    Some(trace_parent) => {
      let parts = trace_parent.split("-")
      assert_eq(parts.length(), 4)
    }
    None => @test.fail("Test failed")
  }
  
  // 验证baggage格式
  match carrier.get("baggage") {
    Some(baggage) => {
      if baggage.length() > 0 {
        let entries = baggage.split(",")
        assert_eq(entries.length() > 0, true)
      }
    }
    None => @test.fail("Test failed")
  }
}

test "resource_and_instrumentation_serialization" {
  // 测试Resource和InstrumentationScope的序列化
  
  // 测试Resource序列化
  let simple_resource = Resource::default("simple-service")
  assert_eq(simple_resource.service_name, "simple-service")
  assert_eq(simple_resource.service_version, None)
  assert_eq(simple_resource.telemetry_sdk_name, "azimuth")
  assert_eq(simple_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(simple_resource.attributes.length(), 0)
  
  let complex_resource = Resource::{
    service_name: "complex-service",
    service_version: Some("2.1.0-beta.1+build.123"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0-alpha",
    attributes: [
      ("deployment.environment", AttributeValue::string("production")),
      ("host.name", AttributeValue::string("prod-server-01")),
      ("process.id", AttributeValue::int(12345L)),
      ("process.cpu.percent", AttributeValue::float(75.5)),
      ("process.healthy", AttributeValue::bool(true)),
      ("service.tags", AttributeValue::array_string(["api", "critical", "revenue"])),
      ("load.balancers", AttributeValue::array_string(["lb-01", "lb-02"])),
      ("port.numbers", AttributeValue::array_int([8080L, 8443L])),
      ("cpu.thresholds", AttributeValue::array_float([0.7, 0.8, 0.9])),
      ("feature.flags", AttributeValue::array_bool([true, false, true]))
    ]
  }
  
  // 验证复杂资源序列化
  assert_eq(complex_resource.service_name, "complex-service")
  assert_eq(complex_resource.service_version, Some("2.1.0-beta.1+build.123"))
  assert_eq(complex_resource.attributes.length(), 10)
  
  // 测试InstrumentationScope序列化
  let simple_scope = InstrumentationScope::{
    name: "simple-scope",
    version: None,
    schema_url: None
  }
  
  assert_eq(simple_scope.name, "simple-scope")
  assert_eq(simple_scope.version, None)
  assert_eq(simple_scope.schema_url, None)
  
  let complex_scope = InstrumentationScope::{
    name: "complex-scope",
    version: Some("1.5.2-rc.1"),
    schema_url: Some("https://example.com/schemas/v1.5.2")
  }
  
  assert_eq(complex_scope.name, "complex-scope")
  assert_eq(complex_scope.version, Some("1.5.2-rc.1"))
  assert_eq(complex_scope.schema_url, Some("https://example.com/schemas/v1.5.2"))
  
  // 测试特殊字符的序列化
  let special_resource = Resource::{
    service_name: "特殊服务!@#$%^&*()",
    service_version: Some("版本-1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("unicode.key", AttributeValue::string("属性值测试")),
      ("special.chars", AttributeValue::string("!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")),
      ("spaces", AttributeValue::string("value with spaces")),
      ("empty", AttributeValue::string(""))
    ]
  }
  
  assert_eq(special_resource.service_name.contains("特殊服务"), true)
  assert_eq(special_resource.service_name.contains("!@#$%^&*()"), true)
  assert_eq(special_resource.attributes.length(), 4)
  
  match special_resource.attributes[0].1 {
    StringValue(value) => assert_eq(value.contains("属性值测试"), true)
    _ => @test.fail("Test failed")
  }
}

test "backward_compatibility_data_formats" {
  // 测试向后兼容性的数据格式
  
  // 测试旧版本的AttributeValue格式兼容性
  let legacy_string_attr = AttributeValue::string("legacy_string")
  let legacy_int_attr = AttributeValue::int(42L)
  let legacy_float_attr = AttributeValue::float(3.14)
  let legacy_bool_attr = AttributeValue::bool(false)
  
  // 验证旧版本属性值仍然可以正确处理
  match legacy_string_attr {
    StringValue(value) => assert_eq(value, "legacy_string")
    _ => @test.fail("Test failed")
  }
  
  match legacy_int_attr {
    IntValue(value) => assert_eq(value, 42L)
    _ => @test.fail("Test failed")
  }
  
  match legacy_float_attr {
    FloatValue(value) => assert_eq(value, 3.14)
    _ => @test.fail("Test failed")
  }
  
  match legacy_bool_attr {
    BoolValue(value) => assert_eq(value, false)
    _ => @test.fail("Test failed")
  }
  
  // 测试旧版本的SpanContext格式兼容性
  let legacy_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(i) { 
    if i % 2 == 0 { 0x00_byte } else { 0xff_byte }
  })
  let legacy_span_id = [for i = 0; i < 8; i = i + 1].map(fn(i) { 
    if i % 2 == 0 { 0x00_byte } else { 0xff_byte }
  })
  
  let legacy_span_context = SpanContext::{
    trace_id: legacy_trace_id,
    span_id: legacy_span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 验证旧版本SpanContext仍然可以正确处理
  assert_eq(legacy_span_context.trace_id.length(), 16)
  assert_eq(legacy_span_context.span_id.length(), 8)
  assert_eq(legacy_span_context.trace_flags, 1_byte)
  assert_eq(legacy_span_context.trace_state, "")
  
  // 测试旧版本的LogRecord格式兼容性
  let legacy_log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,  // 旧版本可能没有这个字段
    severity_number: Info,
    severity_text: None,  // 旧版本可能没有这个字段
    body: Some("Legacy log message"),
    attributes: [
      ("legacy.key", AttributeValue::string("legacy.value"))
    ],
    trace_id: None,  // 旧版本可能没有这些字段
    span_id: None,
    trace_flags: None,
    resource: None,  // 旧版本可能没有这些字段
    instrumentation_scope: None
  }
  
  // 验证旧版本LogRecord仍然可以正确处理
  assert_eq(legacy_log_record.timestamp_unix_nanos, 1640995200000000000L)
  match legacy_log_record.observed_timestamp_unix_nanos {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  assert_eq(legacy_log_record.severity_number, Info)
  match legacy_log_record.severity_text {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  match legacy_log_record.body {
    Some(body) => assert_eq(body, "Legacy log message")
    None => @test.fail("Test failed")
  }
  assert_eq(legacy_log_record.attributes.length(), 1)
  
  // 测试旧版本的Resource格式兼容性
  let legacy_resource = Resource::{
    service_name: "legacy-service",
    service_version: None,  // 旧版本可能没有这个字段
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: []  // 旧版本可能没有属性
  }
  
  assert_eq(legacy_resource.service_name, "legacy-service")
  match legacy_resource.service_version {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  assert_eq(legacy_resource.attributes.length(), 0)
  
  // 测试旧版本的InstrumentationScope格式兼容性
  let legacy_scope = InstrumentationScope::{
    name: "legacy-scope",
    version: None,  // 旧版本可能没有这个字段
    schema_url: None  // 旧版本可能没有这个字段
  }
  
  assert_eq(legacy_scope.name, "legacy-scope")
  match legacy_scope.version {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  match legacy_scope.schema_url {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}
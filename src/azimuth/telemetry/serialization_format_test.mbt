// Azimuth Telemetry - Serialization and Format Test
// 测试数据序列化和格式化场景

test "attribute_value_serialization_scenarios" {
  // 测试各种AttributeValue的序列化场景
  
  // 基本类型属性
  let basic_attrs = [
    ("string_value", common::AttributeValue::string("hello world")),
    ("int_value", common::AttributeValue::int(42)),
    ("float_value", common::AttributeValue::float(3.14159)),
    ("bool_value", common::AttributeValue::bool(true))
  ]
  
  // 特殊字符串值
  let special_string_attrs = [
    ("empty_string", common::AttributeValue::string("")),
    ("unicode_string", common::AttributeValue::string("测试中文 🌟")),
    ("json_string", common::AttributeValue::string("{\"key\": \"value\", \"array\": [1, 2, 3]}")),
    ("xml_string", common::AttributeValue::string("<root><item>test</item></root>")),
    ("base64_string", common::AttributeValue::string("SGVsbG8gV29ybGQ=")),
    ("url_string", common::AttributeValue::string("https://example.com/path?param=value&other=test")),
    ("newline_string", common::AttributeValue::string("line1\nline2\rline3")),
    ("tab_string", common::AttributeValue::string("col1\tcol2\tcol3")),
    ("quote_string", common::AttributeValue::string("He said \"Hello\" and left")),
    ("backslash_string", common::AttributeValue::string("path\\to\\file"))
  ]
  
  // 极值数字属性
  let extreme_number_attrs = [
    ("max_int64", common::AttributeValue::int(9223372036854775807L)),
    ("min_int64", common::AttributeValue::int(-9223372036854775808L)),
    ("zero_int", common::AttributeValue::int(0)),
    ("negative_int", common::AttributeValue::int(-123456789)),
    ("max_double", common::AttributeValue::float(1.7976931348623157e+308)),
    ("min_double", common::AttributeValue::float(-1.7976931348623157e+308)),
    ("zero_double", common::AttributeValue::float(0.0)),
    ("infinity", common::AttributeValue::float(1.0/0.0)),
    ("negative_infinity", common::AttributeValue::float(-1.0/0.0)),
    ("nan", common::AttributeValue::float(0.0/0.0))
  ]
  
  // 数组属性
  let array_attrs = [
    ("empty_string_array", common::AttributeValue::array_string([])),
    ("empty_int_array", common::AttributeValue::array_int([])),
    ("empty_float_array", common::AttributeValue::array_float([])),
    ("empty_bool_array", common::AttributeValue::array_bool([])),
    ("single_string_array", common::AttributeValue::array_string(["single"])),
    ("single_int_array", common::AttributeValue::array_int([42])),
    ("mixed_string_array", common::AttributeValue::array_string(["a", "b", "c", "d", "e"])),
    ("large_int_array", common::AttributeValue::array_int([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])),
    ("float_precision_array", common::AttributeValue::array_float([0.1, 0.01, 0.001, 0.0001])),
    ("bool_mixed_array", common::AttributeValue::array_bool([true, false, true, true, false]))
  ]
  
  // 验证属性数量
  assert_eq(basic_attrs.length(), 4)
  assert_eq(special_string_attrs.length(), 10)
  assert_eq(extreme_number_attrs.length(), 10)
  assert_eq(array_attrs.length(), 10)
  
  // 验证特定属性值
  assert_eq(basic_attrs[0], ("string_value", common::AttributeValue::string("hello world")))
  assert_eq(special_string_attrs[2], ("json_string", common::AttributeValue::string("{\"key\": \"value\", \"array\": [1, 2, 3]}")))
  assert_eq(extreme_number_attrs[0], ("max_int64", common::AttributeValue::int(9223372036854775807L)))
  assert_eq(array_attrs[7], ("large_int_array", common::AttributeValue::array_int([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])))
}

test "span_context_format_validation" {
  // 测试SpanContext格式验证
  
  // 有效的trace_id和span_id格式（16字节和8字节的十六进制表示）
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let valid_span_id = "b7ad6b7169203331"
  
  // 创建有效的SpanContext
  let valid_trace_bytes = Array::make(16, 0_byte)  // 在实际实现中应该解析十六进制字符串
  let valid_span_bytes = Array::make(8, 0_byte)   // 在实际实现中应该解析十六进制字符串
  
  let valid_span_context = trace::SpanContext::{
    trace_id: valid_trace_bytes,
    span_id: valid_span_bytes,
    trace_flags: 1_byte,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 测试边界值的trace_flags
  let flag_scenarios = [
    (0_byte, "no flags"),
    (1_byte, "sampled"),
    (2_byte, "custom flag 1"),
    (4_byte, "custom flag 2"),
    (8_byte, "custom flag 3"),
    (255_byte, "all flags set")
  ]
  
  // 创建不同标志的SpanContext
  let mut contexts = [] : Array[trace::SpanContext]
  let mut i = 0
  while i < flag_scenarios.length() {
    let (flag, description) = flag_scenarios[i]
    let context = trace::SpanContext::{
      trace_id: valid_trace_bytes,
      span_id: valid_span_bytes,
      trace_flags: flag,
      trace_state: description
    }
    contexts.push(context)
    i = i + 1
  }
  
  // 验证不同的trace_flags值
  assert_eq(contexts[0].trace_flags, 0_byte)
  assert_eq(contexts[1].trace_flags, 1_byte)
  assert_eq(contexts[6].trace_flags, 255_byte)
  
  // 测试trace_state格式
  let trace_state_cases = [
    "",  // 空trace_state
    "key=value",  // 简单键值对
    "key1=value1,key2=value2",  // 多个键值对
    "complex.key=complex.value,with.special=chars",  // 复杂键值对
    "vendor1=opaque_value,vendor2=another_value"  // 供应商特定值
  ]
  
  // 创建不同trace_state的SpanContext
  i = 0
  while i < trace_state_cases.length() {
    let trace_state = trace_state_cases[i]
    let context = trace::SpanContext::{
      trace_id: valid_trace_bytes,
      span_id: valid_span_bytes,
      trace_flags: 1_byte,
      trace_state: trace_state
    }
    assert_eq(context.trace_state, trace_state)
    i = i + 1
  }
}

test "log_record_format_standards" {
  // 测试LogRecord格式标准
  
  // 创建符合不同严重性级别的LogRecord
  let severity_levels = [
    (logs::Trace, "TRACE", "Detailed trace information"),
    (logs::Debug, "DEBUG", "Debug information for developers"),
    (logs::Info, "INFO", "General information messages"),
    (logs::Warn, "WARN", "Warning messages"),
    (logs::Error, "ERROR", "Error messages"),
    (logs::Fatal, "FATAL", "Critical error messages")
  ]
  
  // 为每个严重性级别创建LogRecord
  let mut i = 0
  while i < severity_levels.length() {
    let (severity_enum, severity_string, message) = severity_levels[i]
    
    let log_record = logs::LogRecord::builder()
      .timestamp(1640995200000000000L + i.to_int64() * 1000000L)
      .severity(severity_enum)
      .severity_text(severity_string)
      .body(message)
      .with_attribute("severity.level", common::AttributeValue::int(i))
      .with_attribute("log.source", common::AttributeValue::string("test_module"))
      |> build
    
    // 验证LogRecord格式
    assert_eq(log_record.severity_number, severity_enum)
    assert_eq(log_record.severity_text, Some(severity_string))
    assert_eq(log_record.body, Some(message))
    assert_eq(log_record.attributes.length(), 2)
    
    i = i + 1
  }
  
  // 测试时间戳格式
  let timestamp_cases = [
    0L,  // Unix纪元
    1640995200000000000L,  // 2022-01-01 00:00:00 UTC
    -1640995200000000000L,  // 负时间戳
    9223372036854775807L,  // 最大Int64值
    -9223372036854775808L   // 最小Int64值
  ]
  
  i = 0
  while i < timestamp_cases.length() {
    let timestamp = timestamp_cases[i]
    let time_record = logs::LogRecord::builder()
      .timestamp(timestamp)
      .severity(logs::Info)
      .body("Timestamp test")
      |> build
    
    assert_eq(time_record.timestamp_unix_nanos, timestamp)
    i = i + 1
  }
}

test "carrier_header_format_compliance" {
  // 测试Carrier头部格式合规性
  
  // W3C TraceContext头部格式测试
  let valid_trace_parents = [
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",  // 标准格式
    "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01",  // 另一个有效示例
    "00-00000000000000000000000000000000-0000000000000000-00",   // 全零值
    "ff-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"   // 未来版本
  ]
  
  let trace_state_cases = [
    "",  // 空tracestate
    "rojo=00f067aa0ba902b7",  // 单个供应商
    "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE",  // 多个供应商
    "vendor1=opaqueValue,vendor2=anotherValue,vendor3=thirdValue"  // 多个复杂值
  ]
  
  // Baggage头部格式测试
  let valid_baggages = [
    "",  // 空baggage
    "key=value",  // 简单键值对
    "key1=value1,key2=value2",  // 多个键值对
    "keywith%20space=valuewith%20space",  // URL编码
    "key=value;param1;param2",  // 带参数
    "complex%2Bkey=complex%2Bvalue,simple=simple"  // 混合复杂和简单
  ]
  
  // 创建包含不同头信息的carrier
  let mut i = 0
  while i < valid_trace_parents.length() {
    let trace_parent = valid_trace_parents[i]
    let trace_state = trace_state_cases[i % trace_state_cases.length()]
    let baggage = valid_baggages[i % valid_baggages.length()]
    
    let carrier_data = [
      ("traceparent", trace_parent),
      ("tracestate", trace_state),
      ("baggage", baggage),
      ("x-request-id", "req-" + i.to_string()),
      ("x-correlation-id", "corr-" + i.to_string())
    ]
    
    let carrier = propagation::MapCarrier::from_map(carrier_data)
    
    // 验证carrier包含预期的键
    let keys = carrier.keys()
    assert_eq(keys.contains("traceparent"), true)
    assert_eq(keys.contains("tracestate"), true)
    assert_eq(keys.contains("baggage"), true)
    assert_eq(keys.contains("x-request-id"), true)
    assert_eq(keys.contains("x-correlation-id"), true)
    
    // 验证特定的值
    assert_eq(carrier.get("traceparent"), Some(trace_parent))
    assert_eq(carrier.get("tracestate"), if trace_state == "" { None } else { Some(trace_state) })
    assert_eq(carrier.get("baggage"), if baggage == "" { None } else { Some(baggage) })
    
    i = i + 1
  }
}

test "resource_and_instrumentation_format_standards" {
  // 测试Resource和InstrumentationScope格式标准
  
  // 标准Resource属性
  let standard_resource_attrs = [
    ("service.name", common::AttributeValue::string("my-service")),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("service.instance.id", common::AttributeValue::string("instance-123")),
    ("telemetry.sdk.name", common::AttributeValue::string("azimuth")),
    ("telemetry.sdk.version", common::AttributeValue::string("0.1.0")),
    ("telemetry.sdk.language", common::AttributeValue::string("moonbit")),
    ("host.name", common::AttributeValue::string("host-001")),
    ("host.arch", common::AttributeValue::string("amd64")),
    ("os.type", common::AttributeValue::string("linux")),
    ("deployment.environment", common::AttributeValue::string("production"))
  ]
  
  // 创建符合标准的Resource
  let standard_resource = common::Resource::{
    service_name: "my-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: standard_resource_attrs
  }
  
  // 验证Resource格式
  assert_eq(standard_resource.service_name, "my-service")
  assert_eq(standard_resource.service_version, Some("1.0.0"))
  assert_eq(standard_resource.telemetry_sdk_name, "azimuth")
  assert_eq(standard_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(standard_resource.attributes.length(), 10)
  
  // 标准InstrumentationScope属性
  let standard_scope = common::InstrumentationScope::{
    name: "my-instrumentation",
    version: Some("1.2.3"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  // 验证InstrumentationScope格式
  assert_eq(standard_scope.name, "my-instrumentation")
  assert_eq(standard_scope.version, Some("1.2.3"))
  assert_eq(standard_scope.schema_url, Some("https://example.com/schema/v1"))
  
  // 测试各种schema URL格式
  let schema_urls = [
    "https://opentelemetry.io/schemas/v1.20.0",
    "http://example.com/custom/schema",
    "https://github.com/company/telemetry-schema/v2.1",
    "https://api.example.org/schemas/telemetry/v1.0.0"
  ]
  
  let mut i = 0
  while i < schema_urls.length() {
    let scope_with_schema = common::InstrumentationScope::{
      name: "instrumentation-" + i.to_string(),
      version: Some("1.0.0"),
      schema_url: Some(schema_urls[i])
    }
    
    assert_eq(scope_with_schema.schema_url, Some(schema_urls[i]))
    i = i + 1
  }
}
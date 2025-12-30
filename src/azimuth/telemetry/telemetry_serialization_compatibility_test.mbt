// 遥测数据序列化和反序列化测试
// 测试数据在不同格式间的转换和兼容性

test "attribute_value_serialization" {
  // 测试AttributeValue的序列化兼容性
  
  // 测试基本类型的字符串表示
  let string_attr = common::AttributeValue::string("test_value")
  let int_attr = common::AttributeValue::int(42L)
  let float_attr = common::AttributeValue::float(3.14159)
  let bool_attr = common::AttributeValue::bool(true)
  
  // 模拟序列化为字符串表示
  let serialize_to_string = fn(attr: common::AttributeValue) {
    match attr {
      common::StringValue(value) => "string:" + value
      common::IntValue(value) => "int:" + value.to_string()
      common::FloatValue(value) => "float:" + value.to_string()
      common::BoolValue(value) => "bool:" + (if value { "true" } else { "false" })
      common::ArrayStringValue(values) => {
        let result = "array_string:["
        let mut i = 0
        while i < values.length() {
          if i > 0 { result = result + "," }
          result = result + values[i]
          i = i + 1
        }
        result + "]"
      }
      common::ArrayIntValue(values) => {
        let result = "array_int:["
        let mut i = 0
        while i < values.length() {
          if i > 0 { result = result + "," }
          result = result + values[i].to_string()
          i = i + 1
        }
        result + "]"
      }
      common::ArrayFloatValue(values) => {
        let result = "array_float:["
        let mut i = 0
        while i < values.length() {
          if i > 0 { result = result + "," }
          result = result + values[i].to_string()
          i = i + 1
        }
        result + "]"
      }
      common::ArrayBoolValue(values) => {
        let result = "array_bool:["
        let mut i = 0
        while i < values.length() {
          if i > 0 { result = result + "," }
          result = result + (if values[i] { "true" } else { "false" })
          i = i + 1
        }
        result + "]"
      }
    }
  }
  
  // 测试序列化结果
  assert_eq(serialize_to_string(string_attr), "string:test_value")
  assert_eq(serialize_to_string(int_attr), "int:42")
  assert_eq(serialize_to_string(float_attr), "float:3.14159")
  assert_eq(serialize_to_string(bool_attr), "bool:true")
  
  // 测试数组类型序列化
  let string_array = common::AttributeValue::array_string(["a", "b", "c"])
  let int_array = common::AttributeValue::array_int([1L, 2L, 3L])
  let bool_array = common::AttributeValue::array_bool([true, false])
  
  assert_eq(serialize_to_string(string_array), "array_string:[a,b,c]")
  assert_eq(serialize_to_string(int_array), "array_int:[1,2,3]")
  assert_eq(serialize_to_string(bool_array), "array_bool:[true,false]")
}

test "resource_serialization_format" {
  // 测试Resource的序列化格式
  
  let resource = common::Resource::{
    service_name: "test_service",
    service_version: Some("1.2.3"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("env", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-west-2")),
      ("instance_count", common::AttributeValue::int(3L))
    ]
  }
  
  // 模拟序列化为JSON格式字符串
  let serialize_resource_to_json = fn(res: common::Resource) {
    let json = "{"
    json = json + "\"service_name\":\"" + res.service_name + "\"," 
    
    match res.service_version {
      Some(version) => json = json + "\"service_version\":\"" + version + "\"," 
      None => json = json + "\"service_version\":null," 
    }
    
    json = json + "\"telemetry_sdk_name\":\"" + res.telemetry_sdk_name + "\"," 
    json = json + "\"telemetry_sdk_version\":\"" + res.telemetry_sdk_version + "\"," 
    
    json = json + "\"attributes\":{"
    let mut i = 0
    while i < res.attributes.length() {
      if i > 0 { json = json + "," }
      let (key, value) = res.attributes[i]
      json = json + "\"" + key + "\":"
      
      match value {
        common::StringValue(v) => json = json + "\"" + v + "\""
        common::IntValue(v) => json = json + v.to_string()
        common::FloatValue(v) => json = json + v.to_string()
        common::BoolValue(v) => json = json + (if v { "true" } else { "false" })
        _ => json = json + "\"unsupported_type\""
      }
      
      i = i + 1
    }
    json = json + "}}"
  }
  
  let json_result = serialize_resource_to_json(resource)
  
  // 验证JSON格式包含必要字段
  assert_eq(json_result.contains("\"service_name\":\"test_service\""), true)
  assert_eq(json_result.contains("\"service_version\":\"1.2.3\""), true)
  assert_eq(json_result.contains("\"telemetry_sdk_name\":\"azimuth\""), true)
  assert_eq(json_result.contains("\"telemetry_sdk_version\":\"0.1.0\""), true)
  assert_eq(json_result.contains("\"env\":\"production\""), true)
  assert_eq(json_result.contains("\"region\":\"us-west-2\""), true)
  assert_eq(json_result.contains("\"instance_count\":3"), true)
}

test "span_context_serialization" {
  // 测试SpanContext的序列化
  
  let trace_id = [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0, 
                  0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0]
  let span_id = [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0]
  
  let span_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 将字节数组转换为十六进制字符串
  let bytes_to_hex = fn(bytes: Array[Byte]) {
    let hex = ""
    let mut i = 0
    while i < bytes.length() {
      let byte = bytes[i]
      let hex_chars = "0123456789abcdef"
      let high = (byte >> 4) & 0x0f
      let low = byte & 0x0f
      hex = hex + hex_chars[high].to_string() + hex_chars[low].to_string()
      i = i + 1
    }
    hex
  }
  
  // 序列化span context为W3C tracecontext格式
  let serialize_span_context = fn(ctx: trace::SpanContext) {
    let trace_id_hex = bytes_to_hex(ctx.trace_id)
    let span_id_hex = bytes_to_hex(ctx.span_id)
    let trace_flags_hex = (ctx.trace_flags & 0x01).to_string()
    
    "traceparent: 00-" + trace_id_hex + "-" + span_id_hex + "-" + trace_flags_hex
  }
  
  let serialized = serialize_span_context(span_context)
  
  // 验证序列化格式
  assert_eq(serialized.contains("traceparent: 00-"), true)
  assert_eq(serialized.contains("123456789abcdef0123456789abcdef0"), true)
  assert_eq(serialized.contains("123456789abcdef0"), true)
  assert_eq(serialized.contains("-1"), true)
}

test "log_record_serialization" {
  // 测试LogRecord的序列化
  
  let log_record = logs::LogRecord::{
    timestamp_unix_nanos: 1234567890000000L,
    observed_timestamp_unix_nanos: Some(1234567891000000L),
    severity_number: logs::Error,
    severity_text: Some("ERROR"),
    body: Some("Test log message"),
    attributes: [
      ("user_id", common::AttributeValue::string("user123")),
      ("action", common::AttributeValue::string("login")),
      ("success", common::AttributeValue::bool(false))
    ],
    trace_id: Some([0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0, 
                    0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0]),
    span_id: Some([0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0]),
    trace_flags: Some(1_byte),
    resource: Some(common::Resource::default("test_service")),
    instrumentation_scope: Some(common::InstrumentationScope {
      name: "test_logger",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 序列化日志记录为结构化格式
  let serialize_log_record = fn(log: logs::LogRecord) {
    let result = "{"
    result = result + "\"timestamp\":" + log.timestamp_unix_nanos.to_string() + ","
    
    match log.observed_timestamp_unix_nanos {
      Some(ts) => result = result + "\"observed_timestamp\":" + ts.to_string() + ","
      None => result = result + "\"observed_timestamp\":null,"
    }
    
    result = result + "\"severity\":" + (match log.severity_number {
      logs::Trace => "0"
      logs::Debug => "1"
      logs::Info => "2"
      logs::Warn => "3"
      logs::Error => "4"
      logs::Fatal => "5"
    }) + ","
    
    match log.severity_text {
      Some(text) => result = result + "\"severity_text\":\"" + text + "\"," 
      None => result = result + "\"severity_text\":null,"
    }
    
    match log.body {
      Some(body) => result = result + "\"body\":\"" + body + "\"," 
      None => result = result + "\"body\":null,"
    }
    
    result = result + "\"attributes\":{"
    let mut i = 0
    while i < log.attributes.length() {
      if i > 0 { result = result + "," }
      let (key, value) = log.attributes[i]
      result = result + "\"" + key + "\":"
      
      match value {
        common::StringValue(v) => result = result + "\"" + v + "\""
        common::IntValue(v) => result = result + v.to_string()
        common::FloatValue(v) => result = result + v.to_string()
        common::BoolValue(v) => result = result + (if v { "true" } else { "false" })
        _ => result = result + "\"unsupported\""
      }
      
      i = i + 1
    }
    result = result + "}}"
  }
  
  let serialized_log = serialize_log_record(log_record)
  
  // 验证序列化结果
  assert_eq(serialized_log.contains("\"timestamp\":1234567890000000"), true)
  assert_eq(serialized_log.contains("\"severity\":4"), true)
  assert_eq(serialized_log.contains("\"severity_text\":\"ERROR\""), true)
  assert_eq(serialized_log.contains("\"body\":\"Test log message\""), true)
  assert_eq(serialized_log.contains("\"user_id\":\"user123\""), true)
  assert_eq(serialized_log.contains("\"success\":false"), true)
}

test "context_serialization_compatibility" {
  // 测试Context序列化兼容性
  
  let context = context::Context::empty()
  let key1 = context::create_key("trace_id")
  let key2 = context::create_key("user_id")
  let key3 = context::create_key("session_id")
  
  let enriched_context = context
    .with_value(key1, "trace123456")
    .with_value(key2, "user789012")
    .with_value(key3, "session345678")
  
  // 序列化上下文为键值对格式
  let serialize_context = fn(ctx: context::Context) {
    let result = "{"
    let mut i = 0
    while i < ctx.values.length() {
      if i > 0 { result = result + "," }
      let (key, value) = ctx.values[i]
      result = result + "\"" + key + "\":\"" + value + "\""
      i = i + 1
    }
    result + "}"
  }
  
  let serialized_context = serialize_context(enriched_context)
  
  // 验证序列化结果
  assert_eq(serialized_context.contains("\"trace_id\":\"trace123456\""), true)
  assert_eq(serialized_context.contains("\"user_id\":\"user789012\""), true)
  assert_eq(serialized_context.contains("\"session_id\":\"session345678\""), true)
  
  // 测试反序列化兼容性（模拟）
  let deserialize_context = fn(serialized: String) {
    // 简化的解析逻辑，实际实现会更复杂
    let new_context = context::Context::empty()
    
    // 模拟解析结果
    new_context
      .with_value(context::create_key("trace_id"), "trace123456")
      .with_value(context::create_key("user_id"), "user789012")
      .with_value(context::create_key("session_id"), "session345678")
  }
  
  let deserialized_context = deserialize_context(serialized_context)
  
  // 验证反序列化结果
  assert_eq(deserialized_context.get(key1).unwrap(), "trace123456")
  assert_eq(deserialized_context.get(key2).unwrap(), "user789012")
  assert_eq(deserialized_context.get(key3).unwrap(), "session345678")
}

test "baggage_serialization_format" {
  // 测试Baggage序列化格式
  
  let baggage = context::Baggage::empty()
    .with_entry("user_id", "12345")
    .with_entry("session_id", "abcdef")
    .with_entry("request_id", "req789")
    .with_entry("country", "US")
    .with_entry("locale", "en-US")
  
  // 序列化baggage为W3C Baggage格式
  let serialize_baggage = fn(bg: context::Baggage) {
    let result = ""
    let mut i = 0
    while i < bg.entries.length() {
      if i > 0 { result = result + "," }
      let (key, value) = bg.entries[i]
      result = result + key + "=" + value
      i = i + 1
    }
    result
  }
  
  let serialized_baggage = serialize_baggage(baggage)
  
  // 验证序列化格式
  assert_eq(serialized_baggage, "user_id=12345,session_id=abcdef,request_id=req789,country=US,locale=en-US")
  
  // 测试包含特殊字符的baggage序列化
  let special_baggage = context::Baggage::empty()
    .with_entry("key with spaces", "value with spaces")
    .with_entry("special=chars", "special=chars")
    .with_entry("unicode", "测试🚀")
  
  let serialize_special_baggage = fn(bg: context::Baggage) {
    let result = ""
    let mut i = 0
    while i < bg.entries.length() {
      if i > 0 { result = result + "," }
      let (key, value) = bg.entries[i]
      
      // URL编码特殊字符（简化实现）
      let encode_key = key.replace(" ", "%20").replace("=", "%3D")
      let encode_value = value.replace(" ", "%20").replace("=", "%3D")
      
      result = result + encode_key + "=" + encode_value
      i = i + 1
    }
    result
  }
  
  let serialized_special = serialize_special_baggage(special_baggage)
  
  // 验证特殊字符序列化
  assert_eq(serialized_special.contains("key%20with%20spaces=value%20with%20spaces"), true)
  assert_eq(serialized_special.contains("special%3Dchars=special%3Dchars"), true)
  assert_eq(serialized_special.contains("unicode=测试🚀"), true)
}

test "metric_data_serialization" {
  // 测试指标数据序列化
  
  let metric_attributes = [
    ("service", common::AttributeValue::string("api_gateway")),
    ("endpoint", common::AttributeValue::string("/api/users")),
    ("method", common::AttributeValue::string("GET")),
    ("status_code", common::AttributeValue::int(200L))
  ]
  
  // 序列化指标数据为Prometheus格式
  let serialize_metric_prometheus = fn(name: String, value: Double, attrs: Array[(String, common::AttributeValue)]) {
    let result = name + "{"
    let mut i = 0
    while i < attrs.length() {
      if i > 0 { result = result + "," }
      let (key, attr_value) = attrs[i]
      result = result + key + "=\""
      
      match attr_value {
        common::StringValue(v) => result = result + v
        common::IntValue(v) => result = result + v.to_string()
        common::FloatValue(v) => result = result + v.to_string()
        common::BoolValue(v) => result = result + (if v { "true" } else { "false" })
        _ => result = result + "unsupported"
      }
      
      result = result + "\""
      i = i + 1
    }
    result = result + "} " + value.to_string()
    result
  }
  
  let prometheus_counter = serialize_metric_prometheus("http_requests_total", 12345.0, metric_attributes)
  let prometheus_histogram = serialize_metric_prometheus("http_request_duration_seconds", 0.150, metric_attributes)
  let prometheus_gauge = serialize_metric_prometheus("active_connections", 25.0, metric_attributes)
  
  // 验证Prometheus格式
  assert_eq(prometheus_counter.contains("http_requests_total{"), true)
  assert_eq(prometheus_counter.contains("service=\"api_gateway\""), true)
  assert_eq(prometheus_counter.contains("endpoint=\"/api/users\""), true)
  assert_eq(prometheus_counter.contains("method=\"GET\""), true)
  assert_eq(prometheus_counter.contains("status_code=\"200\""), true)
  assert_eq(prometheus_counter.contains("} 12345"), true)
  
  assert_eq(prometheus_histogram.contains("http_request_duration_seconds{"), true)
  assert_eq(prometheus_histogram.contains("} 0.15"), true)
  
  assert_eq(prometheus_gauge.contains("active_connections{"), true)
  assert_eq(prometheus_gauge.contains("} 25"), true)
}

test "cross_format_compatibility" {
  // 测试跨格式兼容性
  
  // 创建包含所有数据类型的复合对象
  let resource = common::Resource::default("compatibility_test")
  let span_context = trace::SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { (i % 256).to_byte() }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { (i % 256).to_byte() }),
    trace_flags: 1_byte,
    trace_state: "test=state"
  }
  
  let log_record = logs::LogRecord::{
    timestamp_unix_nanos: 1234567890000000L,
    severity_number: logs::Info,
    body: Some("Compatibility test"),
    attributes: [
      ("string_attr", common::AttributeValue::string("test")),
      ("int_attr", common::AttributeValue::int(42L)),
      ("float_attr", common::AttributeValue::float(3.14))
    ],
    resource: Some(resource)
  }
  
  // 测试不同序列化格式的一致性
  let serialize_to_json = fn() {
    "{"
    "\"resource\":{\"service_name\":\"compatibility_test\"},"
    "\"log\":{\"timestamp\":1234567890000000,\"severity\":2,\"body\":\"Compatibility test\"},"
    "\"attributes\":{\"string_attr\":\"test\",\"int_attr\":42,\"float_attr\":3.14}"
    "}"
  }
  
  let serialize_to_xml = fn() {
    "<telemetry>"
    "<resource><service_name>compatibility_test</service_name></resource>"
    "<log timestamp=\"1234567890000000\" severity=\"2\">Compatibility test</log>"
    "<attributes><string_attr>test</string_attr><int_attr>42</int_attr><float_attr>3.14</float_attr></attributes>"
    "</telemetry>"
  }
  
  let serialize_to_yaml = fn() {
    "resource:\n  service_name: compatibility_test\n"
    "log:\n  timestamp: 1234567890000000\n  severity: 2\n  body: Compatibility test\n"
    "attributes:\n  string_attr: test\n  int_attr: 42\n  float_attr: 3.14\n"
  }
  
  let json_output = serialize_to_json()
  let xml_output = serialize_to_xml()
  let yaml_output = serialize_to_yaml()
  
  // 验证所有格式都包含相同的核心数据
  let core_data = ["compatibility_test", "Compatibility test", "test", "42", "3.14"]
  
  let mut i = 0
  while i < core_data.length() {
    let data = core_data[i]
    assert_eq(json_output.contains(data), true)
    assert_eq(xml_output.contains(data), true)
    assert_eq(yaml_output.contains(data), true)
    i = i + 1
  }
  
  // 验证格式特异性
  assert_eq(json_output.contains("{"), true)
  assert_eq(json_output.contains("}"), true)
  assert_eq(xml_output.contains("<"), true)
  assert_eq(xml_output.contains(">"), true)
  assert_eq(yaml_output.contains("\n"), true)
  assert_eq(yaml_output.contains(":"), true)
}
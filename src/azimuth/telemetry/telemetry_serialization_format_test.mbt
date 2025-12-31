// 遥测数据序列化/反序列化测试
// 测试数据在不同格式间的转换，确保数据完整性和一致性

// 辅助函数：将AttributeValue转换为字符串表示
fn attribute_value_to_string(attr : common::AttributeValue) -> String {
  match attr {
    common::StringValue(s) => "string:" + s
    common::IntValue(i) => "int:" + i.to_string()
    common::FloatValue(f) => "float:" + f.to_string()
    common::BoolValue(b) => "bool:" + (if b { "true" } else { "false" })
    common::ArrayStringValue(arr) => "array_string:" + arr.join(",")
    common::ArrayIntValue(arr) => "array_int:" + arr.map(fn(i) { i.to_string() }).join(",")
    common::ArrayFloatValue(arr) => "array_float:" + arr.map(fn(f) { f.to_string() }).join(",")
    common::ArrayBoolValue(arr) => "array_bool:" + arr.map(fn(b) { if b { "true" } else { "false" }}).join(",")
  }
}

// 辅助函数：从字符串表示恢复AttributeValue
fn string_to_attribute_value(s : String) -> common::AttributeValue {
  let parts = s.split(":")
  if parts.length() < 2 {
    common::StringValue(s)
  } else {
    match parts[0] {
      "string" => common::StringValue(parts[1])
      "int" => common::IntValue(parts[1].to_int64())
      "float" => common::FloatValue(parts[1].to_double())
      "bool" => common::BoolValue(parts[1] == "true")
      "array_string" => common::ArrayStringValue(parts[1].split(","))
      "array_int" => common::ArrayIntValue(parts[1].split(",").map(fn(i) { i.to_int64() }))
      "array_float" => common::ArrayFloatValue(parts[1].split(",").map(fn(f) { f.to_double() }))
      "array_bool" => common::ArrayBoolValue(parts[1].split(",").map(fn(b) { b == "true" }))
      _ => common::StringValue(s)
    }
  }
}

test "span_serialization_deserialization" {
  // 测试Span的序列化和反序列化
  
  let original_span = trace::Span::{
    name: "test-span",
    context: trace::SpanContext::{
      trace_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte],
      span_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte],
      trace_flags: 1_byte,
      trace_state: "test=1"
    },
    kind: trace::Server,
    parent_span_id: Some([1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000001000L),
    status: trace::Ok,
    status_description: Some("Success"),
    attributes: [
      ("string.attr", common::AttributeValue::string("test-value")),
      ("int.attr", common::AttributeValue::int(42L)),
      ("float.attr", common::AttributeValue::float(3.14)),
      ("bool.attr", common::AttributeValue::bool(true))
    ],
    events: [
      trace::SpanEvent::{
        name: "test-event",
        timestamp_unix_nanos: 1640995200000000500L,
        attributes: [("event.type", common::AttributeValue::string("test"))]
      }
    ],
    links: [
      trace::SpanLink::{
        context: trace::SpanContext::{
          trace_id: [2_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte],
          span_id: [2_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte],
          trace_flags: 1_byte,
          trace_state: ""
        },
        attributes: [("link.type", common::AttributeValue::string("test"))]
      }
    ]
  }
  
  // 序列化为字符串（简化的序列化格式）
  let serialized = "span:" + original_span.name + "|" + 
    original_span.context.trace_id.map(fn(b) { b.to_string() }).join("") + "|" +
    original_span.context.span_id.map(fn(b) { b.to_string() }).join("") + "|" +
    original_span.start_time_unix_nanos.to_string() + "|" +
    original_span.end_time_unix_nanos?.to_string() + "|" +
    original_span.attributes.map(fn(a) { a.0 + "=" + attribute_value_to_string(a.1) }).join(",")
  
  // 验证序列化结果包含关键信息
  @assertion.assert_true(serialized.includes("test-span"))
  @assertion.assert_true(serialized.includes("string.attr=string:test-value"))
  @assertion.assert_true(serialized.includes("int.attr=int:42"))
  
  // 反序列化（简化的反序列化逻辑）
  let parts = serialized.split("|")
  @assertion.assert_eq(parts.length(), 6)
  @assertion.assert_eq(parts[0], "span:test-span")
  @assertion.assert_eq(parts[4], "1640995200000001000")  // end_time
  
  // 验证时间戳的正确转换
  let deserialized_start_time = parts[3].to_int64()
  let deserialized_end_time = parts[4].to_int64()
  @assertion.assert_eq(deserialized_start_time, original_span.start_time_unix_nanos)
  @assertion.assert_eq(deserialized_end_time, original_span.end_time_unix_nanos?)
}

test "log_record_serialization_deserialization" {
  // 测试LogRecord的序列化和反序列化
  
  let original_log = logs::LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000001L),
    severity_number: logs::Error,
    severity_text: Some("ERROR"),
    body: Some("Test error message"),
    attributes: [
      ("error.code", common::AttributeValue::int(500)),
      ("error.type", common::AttributeValue::string("InternalError")),
      ("retry.count", common::AttributeValue::int(3L))
    ],
    trace_id: Some([1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]),
    span_id: Some([1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]),
    trace_flags: Some(1_byte),
    resource: Some(common::Resource::default("test-service")),
    instrumentation_scope: Some(common::InstrumentationScope::{
      name: "test-logger",
      version: Some("1.0.0"),
      schema_url: Some("https://example.com/schema")
    })
  }
  
  // 序列化为JSON风格的字符串
  let serialized = "{"
    + "\"timestamp\":\"" + original_log.timestamp_unix_nanos.to_string() + "\"," 
    + "\"severity\":\"" + original_log.severity_text? + "\"," 
    + "\"body\":\"" + original_log.body? + "\"," 
    + "\"attributes\":[" + original_log.attributes.map(fn(a) { 
      "\"" + a.0 + "\":\"" + attribute_value_to_string(a.1) + "\""
    }).join(",") + "],"
    + "\"trace_id\":\"" + original_log.trace_id?.map(fn(b) { b.to_string() }).join("") + "\"," 
    + "\"service\":\"" + original_log.resource?.service_name + "\""
    + "}"
  
  // 验证序列化结果
  @assertion.assert_true(serialized.includes("\"severity\":\"ERROR\""))
  @assertion.assert_true(serialized.includes("\"body\":\"Test error message\""))
  @assertion.assert_true(serialized.includes("\"service\":\"test-service\""))
  @assertion.assert_true(serialized.includes("\"error.code\":\"int:500\""))
  
  // 简化的反序列化验证
  @assertion.assert_true(serialized.includes("\"timestamp\":\"1640995200000000000\""))
  @assertion.assert_true(serialized.includes("\"trace_id\":\"0102030405060708090a0b0c0d0e0f10\""))
}

test "metric_measurement_serialization_deserialization" {
  // 测试Measurement的序列化和反序列化
  
  let original_measurement = metrics::Measurement::{
    value: 123.456,
    attributes: [
      ("metric.name", common::AttributeValue::string("test.metric")),
      ("unit", common::AttributeValue::string("milliseconds")),
      ("tags", common::AttributeValue::array_string(["tag1", "tag2", "tag3"])),
      ("numeric.tags", common::AttributeValue::array_int([1L, 2L, 3L])),
      ("float.tags", common::AttributeValue::array_float([1.1, 2.2, 3.3])),
      ("boolean.tags", common::AttributeValue::array_bool([true, false, true]))
    ]
  }
  
  // 序列化为字符串格式
  let serialized = "measurement:" + original_measurement.value.to_string() + "|" +
    original_measurement.attributes.map(fn(a) { 
      a.0 + "=" + attribute_value_to_string(a.1)
    }).join(",")
  
  // 验证序列化结果
  @assertion.assert_true(serialized.includes("measurement:123.456"))
  @assertion.assert_true(serialized.includes("metric.name=string:test.metric"))
  @assertion.assert_true(serialized.includes("unit=string:milliseconds"))
  @assertion.assert_true(serialized.includes("tags=array_string:tag1,tag2,tag3"))
  @assertion.assert_true(serialized.includes("numeric.tags=array_int:1,2,3"))
  @assertion.assert_true(serialized.includes("float.tags=array_float:1.1,2.2,3.3"))
  @assertion.assert_true(serialized.includes("boolean.tags=array_bool:true,false,true"))
  
  // 反序列化验证
  let parts = serialized.split("|")
  @assertion.assert_eq(parts.length(), 2)
  
  let deserialized_value = parts[0].split(":")[1].to_double()
  @assertion.assert_eq(deserialized_value, original_measurement.value)
  
  let attr_parts = parts[1].split(",")
  @assertion.assert_eq(attr_parts.length(), original_measurement.attributes.length())
}

test "attribute_value_round_trip_conversion" {
  // 测试AttributeValue的往返转换
  
  let test_values = [
    common::StringValue("test string with spaces"),
    common::IntValue(-9223372036854775808L),  // Min Int64
    common::IntValue(9223372036854775807L),   // Max Int64
    common::FloatValue(3.14159265359),
    common::FloatValue(-0.00000000001),
    common::BoolValue(true),
    common::BoolValue(false),
    common::ArrayStringValue([]),
    common::ArrayStringValue([""]),
    common::ArrayStringValue(["a", "b", "c"]),
    common::ArrayIntValue([0L, -1L, 9223372036854775807L]),
    common::ArrayFloatValue([0.0, -1.5, 3.14159265359]),
    common::ArrayBoolValue([true, false, true, false])
  ]
  
  // 对每个值进行往返转换测试
  let mut i = 0
  while i < test_values.length() {
    let original = test_values[i]
    let serialized = attribute_value_to_string(original)
    let deserialized = string_to_attribute_value(serialized)
    
    // 验证往返转换的一致性
    match (original, deserialized) {
      (common::StringValue(o), common::StringValue(d)) => @assertion.assert_eq(o, d)
      (common::IntValue(o), common::IntValue(d)) => @assertion.assert_eq(o, d)
      (common::FloatValue(o), common::FloatValue(d)) => @assertion.assert_eq(o, d)
      (common::BoolValue(o), common::BoolValue(d)) => @assertion.assert_eq(o, d)
      (common::ArrayStringValue(o), common::ArrayStringValue(d)) => {
        @assertion.assert_eq(o.length(), d.length())
        let mut j = 0
        while j < o.length() {
          @assertion.assert_eq(o[j], d[j])
          j = j + 1
        }
      }
      (common::ArrayIntValue(o), common::ArrayIntValue(d)) => {
        @assertion.assert_eq(o.length(), d.length())
        let mut j = 0
        while j < o.length() {
          @assertion.assert_eq(o[j], d[j])
          j = j + 1
        }
      }
      (common::ArrayFloatValue(o), common::ArrayFloatValue(d)) => {
        @assertion.assert_eq(o.length(), d.length())
        let mut j = 0
        while j < o.length() {
          @assertion.assert_eq(o[j], d[j])
          j = j + 1
        }
      }
      (common::ArrayBoolValue(o), common::ArrayBoolValue(d)) => {
        @assertion.assert_eq(o.length(), d.length())
        let mut j = 0
        while j < o.length() {
          @assertion.assert_eq(o[j], d[j])
          j = j + 1
        }
      }
      _ => @assertion.assert_fail("Type mismatch during round trip conversion")
    }
    
    i = i + 1
  }
}

test "resource_serialization_deserialization" {
  // 测试Resource的序列化和反序列化
  
  let original_resource = common::Resource::{
    service_name: "test-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-west-2")),
      ("instance.id", common::AttributeValue::string("i-1234567890abcdef0")),
      ("cpu.count", common::AttributeValue::int(4L)),
      ("memory.mb", common::AttributeValue::int(8192L)),
      ("features", common::AttributeValue::array_string(["feature1", "feature2"]))
    ]
  }
  
  // 序列化
  let serialized = "resource:" + original_resource.service_name + "|" +
    original_resource.service_version? + "|" +
    original_resource.telemetry_sdk_name + "|" +
    original_resource.telemetry_sdk_version + "|" +
    original_resource.attributes.map(fn(a) { 
      a.0 + "=" + attribute_value_to_string(a.1)
    }).join(",")
  
  // 验证序列化结果
  @assertion.assert_true(serialized.includes("resource:test-service"))
  @assertion.assert_true(serialized.includes("2.1.0"))
  @assertion.assert_true(serialized.includes("azimuth"))
  @assertion.assert_true(serialized.includes("0.1.0"))
  @assertion.assert_true(serialized.includes("environment=string:production"))
  @assertion.assert_true(serialized.includes("cpu.count=int:4"))
  
  // 反序列化验证
  let parts = serialized.split("|")
  @assertion.assert_eq(parts.length(), 5)
  @assertion.assert_eq(parts[0], "resource:test-service")
  @assertion.assert_eq(parts[1], "2.1.0")
  @assertion.assert_eq(parts[2], "azimuth")
  @assertion.assert_eq(parts[3], "0.1.0")
  
  let attr_parts = parts[4].split(",")
  @assertion.assert_eq(attr_parts.length(), original_resource.attributes.length())
}

test "cross_format_data_consistency" {
  // 测试跨不同格式的数据一致性
  
  // 创建测试数据
  let test_attributes = [
    ("test.id", common::AttributeValue::string("cross-format-test")),
    ("test.value", common::AttributeValue::int(42)),
    ("test.precision", common::AttributeValue::float(3.14159265359)),
    ("test.flag", common::AttributeValue::bool(true))
  ]
  
  let span_context = trace::SpanContext::{
    trace_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte],
    span_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte],
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 格式1：JSON风格
  let json_format = "{"
    + "\"trace_id\":\"" + span_context.trace_id.map(fn(b) { b.to_string() }).join("") + "\"," 
    + "\"attributes\":[" + test_attributes.map(fn(a) { 
      "\"" + a.0 + "\":\"" + attribute_value_to_string(a.1) + "\""
    }).join(",") + "]"
    + "}"
  
  // 格式2：管道分隔
  let pipe_format = span_context.trace_id.map(fn(b) { b.to_string() }).join("") + "|" +
    test_attributes.map(fn(a) { 
      a.0 + "=" + attribute_value_to_string(a.1)
    }).join(",")
  
  // 格式3：键值对
  let kv_format = "trace_id=" + span_context.trace_id.map(fn(b) { b.to_string() }).join("") + ";" +
    test_attributes.map(fn(a) { 
      a.0 + ":" + attribute_value_to_string(a.1)
    }).join(";")
  
  // 验证所有格式都包含相同的关键数据
  let trace_id_str = span_context.trace_id.map(fn(b) { b.to_string() }).join("")
  
  @assertion.assert_true(json_format.includes(trace_id_str))
  @assertion.assert_true(pipe_format.includes(trace_id_str))
  @assertion.assert_true(kv_format.includes(trace_id_str))
  
  @assertion.assert_true(json_format.includes("test.id=string:cross-format-test"))
  @assertion.assert_true(pipe_format.includes("test.id=string:cross-format-test"))
  @assertion.assert_true(kv_format.includes("test.id:string:cross-format-test"))
  
  @assertion.assert_true(json_format.includes("test.value=int:42"))
  @assertion.assert_true(pipe_format.includes("test.value=int:42"))
  @assertion.assert_true(kv_format.includes("test.value:int:42"))
  
  // 验证从不同格式提取的数据一致性
  let json_trace_id = json_format.split("\"trace_id\":\"")[1].split("\"")[0]
  let pipe_trace_id = pipe_format.split("|")[0]
  let kv_trace_id = kv_format.split("trace_id=")[1].split(";")[0]
  
  @assertion.assert_eq(json_trace_id, trace_id_str)
  @assertion.assert_eq(pipe_trace_id, trace_id_str)
  @assertion.assert_eq(kv_trace_id, trace_id_str)
}
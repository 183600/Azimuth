// 序列化兼容性测试
// 测试遥测数据的序列化和反序列化兼容性

use azimuth.telemetry.api.trace
use azimuth.telemetry.api.context
use azimuth.telemetry.api.common
use @lib.json

test "attribute_value_serialization" {
  // 测试属性值的序列化
  let string_attr = common::AttributeValue::string("test-value")
  let int_attr = common::AttributeValue::int(42L)
  let float_attr = common::AttributeValue::float(3.14159)
  let bool_attr = common::AttributeValue::bool(true)
  
  // 序列化
  let string_json = json.serialize(string_attr)
  let int_json = json.serialize(int_attr)
  let float_json = json.serialize(float_attr)
  let bool_json = json.serialize(bool_attr)
  
  // 验证JSON格式
  assert_eq(string_json.contains("test-value"), true)
  assert_eq(int_json.contains("42"), true)
  assert_eq(float_json.contains("3.14159"), true)
  assert_eq(bool_json.contains("true"), true)
  
  // 反序列化验证
  let deserialized_string = json.deserialize(string_json) as common::AttributeValue
  let deserialized_int = json.deserialize(int_json) as common::AttributeValue
  let deserialized_float = json.deserialize(float_json) as common::AttributeValue
  let deserialized_bool = json.deserialize(bool_json) as common::AttributeValue
  
  match deserialized_string {
    common::StringValue(s) => assert_eq(s, "test-value")
    _ => assert_eq(false, true, "Failed to deserialize string")
  }
  
  match deserialized_int {
    common::IntValue(i) => assert_eq(i, 42L)
    _ => assert_eq(false, true, "Failed to deserialize int")
  }
}

test "span_serialization" {
  // 测试Span的序列化
  let span_context = trace::SpanContext::{
    trace_id: [0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte],
    span_id: [0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte],
    trace_flags: 1_byte,
    trace_state: "test-state"
  }
  
  let span = trace::Span::{
    name: "test-span",
    context: span_context,
    kind: trace::Server,
    parent_span_id: Some([0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte]),
    start_time_unix_nanos: 1234567890L,
    end_time_unix_nanos: Some(1234567891L),
    status: trace::Ok,
    status_description: Some("success"),
    attributes: [("key1", common::AttributeValue::string("value1"))],
    events: [
      trace::SpanEvent::{
        name: "test-event",
        timestamp_unix_nanos: 1234567890L,
        attributes: [("event-key", common::AttributeValue::string("event-value"))]
      }
    ],
    links: [
      trace::SpanLink::{
        context: span_context,
        attributes: [("link-key", common::AttributeValue::string("link-value"))]
      }
    ]
  }
  
  // 序列化
  let span_json = json.serialize(span)
  
  // 验证JSON包含必要字段
  assert_eq(span_json.contains("test-span"), true)
  assert_eq(span_json.contains("Server"), true)
  assert_eq(span_json.contains("Ok"), true)
  assert_eq(span_json.contains("success"), true)
  assert_eq(span_json.contains("test-event"), true)
  
  // 反序列化
  let deserialized_span = json.deserialize(span_json) as trace::Span
  assert_eq(deserialized_span.name, "test-span")
  assert_eq(deserialized_span.kind, trace::Server)
  assert_eq(deserialized_span.status, trace::Ok)
  assert_eq(deserialized_span.status_description, Some("success"))
}

test "resource_serialization" {
  // 测试Resource的序列化
  let resource = common::Resource::{
    service_name: "test-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("attr1", common::AttributeValue::string("value1")),
      ("attr2", common::AttributeValue::int(42L)),
      ("attr3", common::AttributeValue::bool(true))
    ]
  }
  
  // 序列化
  let resource_json = json.serialize(resource)
  
  // 验证JSON内容
  assert_eq(resource_json.contains("test-service"), true)
  assert_eq(resource_json.contains("1.0.0"), true)
  assert_eq(resource_json.contains("azimuth"), true)
  assert_eq(resource_json.contains("0.1.0"), true)
  
  // 反序列化
  let deserialized_resource = json.deserialize(resource_json) as common::Resource
  assert_eq(deserialized_resource.service_name, "test-service")
  assert_eq(deserialized_resource.service_version, Some("1.0.0"))
  assert_eq(deserialized_resource.telemetry_sdk_name, "azimuth")
  assert_eq(deserialized_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(deserialized_resource.attributes.length(), 3)
}

test "array_attribute_serialization" {
  // 测试数组属性的序列化
  let string_array = common::AttributeValue::array_string(["a", "b", "c"])
  let int_array = common::AttributeValue::array_int([1L, 2L, 3L])
  let float_array = common::AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array = common::AttributeValue::array_bool([true, false, true])
  
  // 序列化
  let string_array_json = json.serialize(string_array)
  let int_array_json = json.serialize(int_array)
  let float_array_json = json.serialize(float_array)
  let bool_array_json = json.serialize(bool_array)
  
  // 验证数组格式
  assert_eq(string_array_json.contains("[\"a\",\"b\",\"c\"]"), true)
  assert_eq(int_array_json.contains("[1,2,3]"), true)
  assert_eq(float_array_json.contains("[1.1,2.2,3.3]"), true)
  assert_eq(bool_array_json.contains("[true,false,true]"), true)
  
  // 反序列化
  let deserialized_string_array = json.deserialize(string_array_json) as common::AttributeValue
  let deserialized_int_array = json.deserialize(int_array_json) as common::AttributeValue
  let deserialized_float_array = json.deserialize(float_array_json) as common::AttributeValue
  let deserialized_bool_array = json.deserialize(bool_array_json) as common::AttributeValue
  
  match deserialized_string_array {
    common::ArrayStringValue(arr) => assert_eq(arr.length(), 3)
    _ => assert_eq(false, true, "Failed to deserialize string array")
  }
  
  match deserialized_int_array {
    common::ArrayIntValue(arr) => assert_eq(arr.length(), 3)
    _ => assert_eq(false, true, "Failed to deserialize int array")
  }
}

test "backward_compatibility" {
  // 测试向后兼容性
  let old_format_data = "{\"name\":\"legacy-span\",\"kind\":\"Internal\",\"status\":\"Unset\"}"
  
  // 尝试解析旧格式数据
  let legacy_span = json.deserialize(old_format_data) as trace::Span
  assert_eq(legacy_span.name, "legacy-span")
  assert_eq(legacy_span.kind, trace::Internal)
  assert_eq(legacy_span.status, trace::Unset)
  
  // 新格式数据应该包含更多字段
  let new_span = trace::Span::{
    name: "new-span",
    context: trace::SpanContext::{
      trace_id: [0_byte; 16],
      span_id: [0_byte; 8],
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 0L,
    end_time_unix_nanos: None,
    status: trace::Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  let new_format_json = json.serialize(new_span)
  assert_eq(new_format_json.contains("trace_id"), true)
  assert_eq(new_format_json.contains("span_id"), true)
  assert_eq(new_format_json.contains("attributes"), true)
}
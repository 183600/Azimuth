// 数据类型转换和边界条件测试
// 测试属性值转换、边界条件和异常情况处理

use azimuth.telemetry.api.common
use azimuth.telemetry.api.trace
use azimuth.telemetry.api.metrics
use azimuth.telemetry.api.logs

test "attribute_value_type_conversions" {
  // 测试属性值类型转换
  
  // 测试字符串转换
  let string_attr = common::AttributeValue::string("test")
  match string_attr {
    StringValue(s) => {
      assert_eq(s, "test")
      assert_eq(s.length(), 4)
    }
    _ => assert_eq(false, true)
  }
  
  // 测试整数转换
  let int_attr = common::AttributeValue::int(-42L)
  match int_attr {
    IntValue(i) => {
      assert_eq(i, -42L)
      assert_eq(i < 0L, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 测试浮点数转换
  let float_attr = common::AttributeValue::float(-3.14159)
  match float_attr {
    FloatValue(f) => {
      assert_eq(f, -3.14159)
      assert_eq(f < 0.0, true)
      assert_eq(f > -10.0, true)
    }
    _ => assert_eq(false, true)
  }
  
  // 测试布尔值转换
  let bool_attr_true = common::AttributeValue::bool(true)
  let bool_attr_false = common::AttributeValue::bool(false)
  match bool_attr_true {
    BoolValue(b) => assert_eq(b, true)
    _ => assert_eq(false, true)
  }
  match bool_attr_false {
    BoolValue(b) => assert_eq(b, false)
    _ => assert_eq(false, true)
  }
}

test "array_attribute_value_conversions" {
  // 测试数组属性值转换
  
  // 测试字符串数组
  let string_array = ["apple", "banana", "cherry"]
  let string_array_attr = common::AttributeValue::array_string(string_array)
  match string_array_attr {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "apple")
      assert_eq(arr[1], "banana")
      assert_eq(arr[2], "cherry")
    }
    _ => assert_eq(false, true)
  }
  
  // 测试整数数组
  let int_array = [1L, 2L, 3L, 4L, 5L]
  let int_array_attr = common::AttributeValue::array_int(int_array)
  match int_array_attr {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1L)
      assert_eq(arr[4], 5L)
    }
    _ => assert_eq(false, true)
  }
  
  // 测试浮点数数组
  let float_array = [1.1, 2.2, 3.3]
  let float_array_attr = common::AttributeValue::array_float(float_array)
  match float_array_attr {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[2], 3.3)
    }
    _ => assert_eq(false, true)
  }
  
  // 测试布尔数组
  let bool_array = [true, false, true, true]
  let bool_array_attr = common::AttributeValue::array_bool(bool_array)
  match bool_array_attr {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[3], true)
    }
    _ => assert_eq(false, true)
  }
}

test "empty_and_null_boundary_conditions" {
  // 测试空值和null边界条件
  
  // 测试空字符串
  let empty_string_attr = common::AttributeValue::string("")
  match empty_string_attr {
    StringValue(s) => {
      assert_eq(s.length(), 0)
      assert_eq(s, "")
    }
    _ => assert_eq(false, true)
  }
  
  // 测试零值
  let zero_int_attr = common::AttributeValue::int(0L)
  let zero_float_attr = common::AttributeValue::float(0.0)
  match zero_int_attr {
    IntValue(i) => assert_eq(i, 0L)
    _ => assert_eq(false, true)
  }
  match zero_float_attr {
    FloatValue(f) => assert_eq(f, 0.0)
    _ => assert_eq(false, true)
  }
  
  // 测试空数组
  let empty_string_array = [] : Array[String]
  let empty_int_array = [] : Array[Int64]
  let empty_string_array_attr = common::AttributeValue::array_string(empty_string_array)
  let empty_int_array_attr = common::AttributeValue::array_int(empty_int_array)
  
  match empty_string_array_attr {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_eq(false, true)
  }
  match empty_int_array_attr {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_eq(false, true)
  }
  
  // 测试可选类型为None
  let none_string : String? = None
  let none_int : Int64? = None
  assert_eq(none_string, None)
  assert_eq(none_int, None)
}

test "extreme_value_boundary_conditions" {
  // 测试极值边界条件
  
  // 测试极大整数
  let max_int = 9223372036854775807L  // Int64最大值
  let min_int = -9223372036854775808L // Int64最小值
  let max_int_attr = common::AttributeValue::int(max_int)
  let min_int_attr = common::AttributeValue::int(min_int)
  
  match max_int_attr {
    IntValue(i) => assert_eq(i, max_int)
    _ => assert_eq(false, true)
  }
  match min_int_attr {
    IntValue(i) => assert_eq(i, min_int)
    _ => assert_eq(false, true)
  }
  
  // 测试极大浮点数
  let max_float = 1.7976931348623157e+308 // Double最大值
  let min_float = -1.7976931348623157e+308 // Double最小值
  let inf_float = 1.0/0.0 // 无穷大
  let neg_inf_float = -1.0/0.0 // 负无穷大
  let nan_float = 0.0/0.0 // NaN
  
  let max_float_attr = common::AttributeValue::float(max_float)
  let min_float_attr = common::AttributeValue::float(min_float)
  let inf_float_attr = common::AttributeValue::float(inf_float)
  
  match max_float_attr {
    FloatValue(f) => assert_eq(f, max_float)
    _ => assert_eq(false, true)
  }
  match min_float_attr {
    FloatValue(f) => assert_eq(f, min_float)
    _ => assert_eq(false, true)
  }
  match inf_float_attr {
    FloatValue(f) => assert_eq(f.is_infinite(), true)
    _ => assert_eq(false, true)
  }
  
  // 测试极长字符串
  let long_string = "a" * 1000 // 1000个字符的字符串
  let long_string_attr = common::AttributeValue::string(long_string)
  match long_string_attr {
    StringValue(s) => {
      assert_eq(s.length(), 1000)
      assert_eq(s.has_prefix("aaa"), true)
    }
    _ => assert_eq(false, true)
  }
}

test "span_context_boundary_conditions" {
  // 测试Span上下文边界条件
  
  // 测试全零trace_id和span_id
  let zero_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })
  let zero_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })
  let zero_span_context = trace::SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  assert_eq(zero_span_context.trace_id.length(), 16)
  assert_eq(zero_span_context.span_id.length(), 8)
  assert_eq(zero_span_context.trace_flags, 0_byte)
  assert_eq(zero_span_context.trace_state, "")
  
  // 测试最大trace_flags值
  let max_flags_context = trace::SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 255_byte, // 最大字节值
    trace_state: "test-state"
  }
  
  assert_eq(max_flags_context.trace_flags, 255_byte)
  assert_eq(max_flags_context.trace_state, "test-state")
  
  // 测试极长trace_state
  let long_trace_state = "x" * 256
  let long_state_context = trace::SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 1_byte,
    trace_state: long_trace_state
  }
  
  assert_eq(long_state_context.trace_state.length(), 256)
}

test "log_record_boundary_conditions" {
  // 测试日志记录边界条件
  
  // 测试极早和极晚时间戳
  let min_timestamp = -9223372036854775808L // Int64最小值
  let max_timestamp = 9223372036854775807L  // Int64最大值
  
  let min_time_log = logs::LogRecord::{
    timestamp_unix_nanos: min_timestamp,
    observed_timestamp_unix_nanos: Some(min_timestamp),
    severity_number: logs::Info,
    severity_text: None,
    body: None,
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let max_time_log = logs::LogRecord::{
    timestamp_unix_nanos: max_timestamp,
    observed_timestamp_unix_nanos: Some(max_timestamp),
    severity_number: logs::Error,
    severity_text: Some("ERROR"),
    body: Some("Max timestamp log"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(min_time_log.timestamp_unix_nanos, min_timestamp)
  assert_eq(max_time_log.timestamp_unix_nanos, max_timestamp)
  assert_eq(min_time_log.observed_timestamp_unix_nanos.unwrap(), min_timestamp)
  assert_eq(max_time_log.observed_timestamp_unix_nanos.unwrap(), max_timestamp)
  
  // 测试空body和极长body
  let empty_body_log = logs::LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: None,
    severity_number: logs::Debug,
    severity_text: None,
    body: Some(""),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let long_body = "log message " + "x" * 1000
  let long_body_log = logs::LogRecord::{
    timestamp_unix_nanos: 0L,
    observed_timestamp_unix_nanos: None,
    severity_number: logs::Warn,
    severity_text: Some("WARN"),
    body: Some(long_body),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(empty_body_log.body.unwrap(), "")
  assert_eq(long_body_log.body.unwrap().length(), 1000 + 12) // "log message " + 1000个"x"
}

test "resource_boundary_conditions" {
  // 测试资源边界条件
  
  // 测试空服务名
  let empty_service_resource = common::Resource::{
    service_name: "",
    service_version: None,
    telemetry_sdk_name: "",
    telemetry_sdk_version: "",
    attributes: []
  }
  
  assert_eq(empty_service_resource.service_name, "")
  assert_eq(empty_service_resource.telemetry_sdk_name, "")
  assert_eq(empty_service_resource.telemetry_sdk_version, "")
  
  // 测试极长服务名
  let long_service_name = "service-" + "x" * 100
  let long_service_resource = common::Resource::{
    service_name: long_service_name,
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "test-sdk",
    telemetry_sdk_version: "2.0.0",
    attributes: []
  }
  
  assert_eq(long_service_resource.service_name.length(), 107) // "service-" + 100个"x"
  
  // 测试大量属性
  let many_attributes = [] : Array[(String, common::AttributeValue)]
  let mut i = 0
  while i < 100 {
    many_attributes.push(("attr" + i.to_string(), common::AttributeValue::string("value" + i.to_string())))
    i = i + 1
  }
  
  let many_attrs_resource = common::Resource::{
    service_name: "test-service",
    service_version: None,
    telemetry_sdk_name: "test-sdk",
    telemetry_sdk_version: "1.0.0",
    attributes: many_attributes
  }
  
  assert_eq(many_attrs_resource.attributes.length(), 100)
  assert_eq(many_attrs_resource.attributes[0].0, "attr0")
  assert_eq(many_attrs_resource.attributes[0].1, common::AttributeValue::string("value0"))
  assert_eq(many_attrs_resource.attributes[99].0, "attr99")
  assert_eq(many_attrs_resource.attributes[99].1, common::AttributeValue::string("value99"))
}

test "span_events_and_links_boundary_conditions" {
  // 测试Span事件和链接边界条件
  
  // 测试空事件和链接数组
  let empty_events_span = trace::Span::{
    name: "test-span",
    context: trace::SpanContext::{ trace_id: [], span_id: [], trace_flags: 0_byte, trace_state: "" },
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
  
  assert_eq(empty_events_span.events.length(), 0)
  assert_eq(empty_events_span.links.length(), 0)
  
  // 测试大量事件和链接
  let many_events = [] : Array[trace::SpanEvent]
  let many_links = [] : Array[trace::SpanLink]
  let mut i = 0
  while i < 50 {
    many_events.push(trace::SpanEvent::{
      name: "event" + i.to_string(),
      timestamp_unix_nanos: i.to_int64(),
      attributes: [("index", common::AttributeValue::int(i.to_int64()))]
    })
    
    many_links.push(trace::SpanLink::{
      context: trace::SpanContext::{
        trace_id: [for j = 0; j < 16; j = j + 1].map(fn(_) { i.to_byte() }),
        span_id: [for j = 0; j < 8; j = j + 1].map(fn(_) { i.to_byte() }),
        trace_flags: 1_byte,
        trace_state: "link" + i.to_string()
      },
      attributes: [("link_index", common::AttributeValue::int(i.to_int64()))]
    })
    
    i = i + 1
  }
  
  let many_events_links_span = trace::Span::{
    name: "complex-span",
    context: trace::SpanContext::{ trace_id: [], span_id: [], trace_flags: 0_byte, trace_state: "" },
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 0L,
    end_time_unix_nanos: None,
    status: trace::Ok,
    status_description: Some("Success"),
    attributes: [],
    events: many_events,
    links: many_links
  }
  
  assert_eq(many_events_links_span.events.length(), 50)
  assert_eq(many_events_links_span.links.length(), 50)
  assert_eq(many_events_links_span.events[0].name, "event0")
  assert_eq(many_events_links_span.events[49].name, "event49")
  assert_eq(many_events_links_span.links[0].context.trace_state, "link0")
  assert_eq(many_events_links_span.links[49].context.trace_state, "link49")
}

test "measurement_boundary_conditions" {
  // 测试测量边界条件
  
  // 测试极值测量
  let extreme_measurements = [
    metrics::Measurement::{ value: -1.7976931348623157e+308, attributes: [("type", common::AttributeValue::string("min"))] },
    metrics::Measurement::{ value: 0.0, attributes: [("type", common::AttributeValue::string("zero"))] },
    metrics::Measurement::{ value: 1.7976931348623157e+308, attributes: [("type", common::AttributeValue::string("max"))] },
    metrics::Measurement::{ value: 1.0/0.0, attributes: [("type", common::AttributeValue::string("inf"))] },
    metrics::Measurement::{ value: -1.0/0.0, attributes: [("type", common::AttributeValue::string("neg_inf"))] },
    metrics::Measurement::{ value: 0.0/0.0, attributes: [("type", common::AttributeValue::string("nan"))] }
  ]
  
  assert_eq(extreme_measurements.length(), 6)
  assert_eq(extreme_measurements[0].value < 0.0, true)
  assert_eq(extreme_measurements[1].value, 0.0)
  assert_eq(extreme_measurements[2].value > 0.0, true)
  assert_eq(extreme_measurements[3].value.is_infinite(), true)
  assert_eq(extreme_measurements[4].value.is_infinite(), true)
  
  // 测试空属性和大量属性
  let empty_attrs_measurement = metrics::Measurement::{
    value: 42.0,
    attributes: []
  }
  
  let many_attrs = [] : Array[(String, common::AttributeValue)]
  let mut i = 0
  while i < 20 {
    many_attrs.push(("attr" + i.to_string(), common::AttributeValue::float(i.to_double())))
    i = i + 1
  }
  
  let many_attrs_measurement = metrics::Measurement::{
    value: 100.0,
    attributes: many_attrs
  }
  
  assert_eq(empty_attrs_measurement.attributes.length(), 0)
  assert_eq(many_attrs_measurement.attributes.length(), 20)
  assert_eq(many_attrs_measurement.attributes[0].0, "attr0")
  assert_eq(many_attrs_measurement.attributes[19].0, "attr19")
}

test "attribute_key_boundary_conditions" {
  // 测试属性键边界条件
  
  // 测试空键和特殊字符键
  let special_keys = [
    ("", common::AttributeValue::string("empty_key")),
    (" ", common::AttributeValue::string("space_key")),
    ("key with spaces", common::AttributeValue::string("spaces_key")),
    ("key-with-dashes", common::AttributeValue::string("dashes_key")),
    ("key_with_underscores", common::AttributeValue::string("underscores_key")),
    ("key.with.dots", common::AttributeValue::string("dots_key")),
    ("key/with/slashes", common::AttributeValue::string("slashes_key")),
    ("key\\with\\backslashes", common::AttributeValue::string("backslashes_key")),
    ("key@with@symbols", common::AttributeValue::string("symbols_key")),
    ("中文字符键", common::AttributeValue::string("chinese_key"))
  ]
  
  assert_eq(special_keys.length(), 10)
  assert_eq(special_keys[0].0, "")
  assert_eq(special_keys[1].0, " ")
  assert_eq(special_keys[2].0, "key with spaces")
  assert_eq(special_keys[9].0, "中文字符键")
  
  // 测试极长键
  let long_key = "k" * 100
  let long_key_attr = (long_key, common::AttributeValue::string("long_key_value"))
  
  assert_eq(long_key_attr.0.length(), 100)
  assert_eq(long_key_attr.1, common::AttributeValue::string("long_key_value"))
}
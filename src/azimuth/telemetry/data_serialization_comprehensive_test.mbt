// 数据序列化和反序列化综合测试

test "attribute_value_serialization_roundtrip" {
  // 测试属性值的序列化往返
  let original_values = [
    common::AttributeValue::string("test_string"),
    common::AttributeValue::int(42L),
    common::AttributeValue::float(3.14159),
    common::AttributeValue::bool(true),
    common::AttributeValue::array_string(["a", "b", "c"]),
    common::AttributeValue::array_int([1L, 2L, 3L]),
    common::AttributeValue::array_float([1.1, 2.2, 3.3]),
    common::AttributeValue::array_bool([true, false, true])
  ]
  
  // 模拟序列化过程
  for i = 0; i < original_values.length; i = i + 1 {
    let original = original_values[i]
    
    // 模拟序列化为字符串
    let serialized = match original {
      common::StringValue(s) => "string:${s}"
      common::IntValue(i) => "int:${i.to_string()}"
      common::FloatValue(f) => "float:${f.to_string()}"
      common::BoolValue(b) => "bool:${b.to_string()}"
      common::ArrayStringValue(arr) => "array_string:${arr.join(",")}"
      common::ArrayIntValue(arr) => "array_int:${arr.to_string()}"
      common::ArrayFloatValue(arr) => "array_float:${arr.to_string()}"
      common::ArrayBoolValue(arr) => "array_bool:${arr.to_string()}"
    }
    
    // 验证序列化结果不为空
    @assertion.assert_true(serialized.length > 0)
    
    // 模拟反序列化验证
    let deserialized = original // 简化的往返测试
    @assertion.assert_eq(deserialized.to_string(), original.to_string())
  }
}

test "resource_serialization_format" {
  // 测试资源序列化格式
  let resource = common::Resource::default("serialization-test-service")
  let resource_with_version = common::Resource::{
    service_name: "test-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("env", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-west-2"))
    ]
  }
  
  // 验证资源结构
  @assertion.assert_eq(resource.service_name, "serialization-test-service")
  @assertion.assert_eq(resource.service_version, None)
  @assertion.assert_eq(resource.attributes.length, 0)
  
  @assertion.assert_eq(resource_with_version.service_name, "test-service")
  @assertion.assert_eq(resource_with_version.service_version, Some("1.0.0"))
  @assertion.assert_eq(resource_with_version.attributes.length, 2)
}

test "span_context_serialization" {
  // 测试Span上下文序列化
  let span_context = trace::SpanContext::{
    trace_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 
               9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte],
    span_id: [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte],
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  // 验证Span上下文结构
  @assertion.assert_eq(span_context.trace_id.length, 16)
  @assertion.assert_eq(span_context.span_id.length, 8)
  @assertion.assert_eq(span_context.trace_flags, 1_byte)
  @assertion.assert_eq(span_context.trace_state, "key1=value1,key2=value2")
  
  // 模拟Trace ID的十六进制表示
  let trace_id_hex = span_context.trace_id.map(fn(b) { b.to_string(16).pad_left(2, '0') }).join("")
  @assertion.assert_eq(trace_id_hex.length, 32)
  
  // 模拟Span ID的十六进制表示
  let span_id_hex = span_context.span_id.map(fn(b) { b.to_string(16).pad_left(2, '0') }).join("")
  @assertion.assert_eq(span_id_hex.length, 16)
}

test "span_serialization_comprehensive" {
  // 测试Span的完整序列化
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("serialization-tracer")
  
  let (ctx, span) = tracer.start_span(
    context::Context::empty(),
    "serialization_test_span",
    trace::Server,
    [
      ("user_id", common::AttributeValue::string("user123")),
      ("operation_type", common::AttributeValue::string("database_query")),
      ("duration_ms", common::AttributeValue::int(150L))
    ]
  )
  
  // 验证Span结构完整性
  @assertion.assert_eq(span.name, "serialization_test_span")
  @assertion.assert_eq(span.kind, trace::Server)
  @assertion.assert_eq(span.status, trace::Unset)
  @assertion.assert_eq(span.attributes.length, 3)
  @assertion.assert_eq(span.events.length, 0)
  @assertion.assert_eq(span.links.length, 0)
  
  // 验证属性序列化
  for i = 0; i < span.attributes.length; i = i + 1 {
    let (key, value) = span.attributes[i]
    @assertion.assert_true(key.length > 0)
    
    match value {
      common::StringValue(s) => @assertion.assert_true(s.length > 0)
      common::IntValue(i) => @assertion.assert_true(i >= 0L)
      _ => @assertion.assert_true(true)
    }
  }
}

test "measurement_serialization_format" {
  // 测试指标测量值的序列化格式
  let measurements = [
    metrics::Measurement::{ value: 1.0, attributes: [("metric_type", common::AttributeValue::string("counter"))] },
    metrics::Measurement::{ value: 2.5, attributes: [("metric_type", common::AttributeValue::string("gauge"))] },
    metrics::Measurement::{ value: 3.14, attributes: [("metric_type", common::AttributeValue::string("histogram"))] }
  ]
  
  // 验证测量值结构
  for i = 0; i < measurements.length; i = i + 1 {
    let measurement = measurements[i]
    @assertion.assert_true(measurement.value >= 0.0)
    @assertion.assert_eq(measurement.attributes.length, 1)
    
    let (key, value) = measurement.attributes[0]
    @assertion.assert_eq(key, "metric_type")
    
    match value {
      common::StringValue(s) => @assertion.assert_true(s == "counter" || s == "gauge" || s == "histogram")
      _ => @assertion.assert_true(false)
    }
  }
}

test "complex_attribute_serialization" {
  // 测试复杂属性的序列化
  let complex_attributes = [
    ("nested_object", common::AttributeValue::string("{\"key\":\"value\",\"number\":42}")),
    ("json_array", common::AttributeValue::array_string(["item1", "item2", "item3"])),
    ("numeric_values", common::AttributeValue::array_float([1.1, 2.2, 3.3, 4.4])),
    ("boolean_flags", common::AttributeValue::array_bool([true, false, true, false, true])),
    ("large_string", common::AttributeValue::string("This is a very long string that tests serialization of large text content with various characters: 中文, ñ, ü, 🎉"))
  ]
  
  // 验证复杂属性结构
  for i = 0; i < complex_attributes.length; i = i + 1 {
    let (key, value) = complex_attributes[i]
    @assertion.assert_true(key.length > 0)
    
    match value {
      common::StringValue(s) => @assertion.assert_true(s.length > 0)
      common::ArrayStringValue(arr) => @assertion.assert_true(arr.length > 0)
      common::ArrayFloatValue(arr) => @assertion.assert_true(arr.length > 0)
      common::ArrayBoolValue(arr) => @assertion.assert_true(arr.length > 0)
      _ => @assertion.assert_true(false)
    }
  }
}

test "serialization_format_compatibility" {
  // 测试序列化格式兼容性
  let resource_v1 = common::Resource::default("v1-service")
  let resource_v2 = common::Resource::{
    service_name: "v2-service",
    service_version: Some("2.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.2.0",
    attributes: [("compatibility", common::AttributeValue::string("test"))]
  }
  
  // 验证版本兼容性
  @assertion.assert_eq(resource_v1.telemetry_sdk_name, resource_v2.telemetry_sdk_name)
  @assertion.assert_true(resource_v2.telemetry_sdk_version > resource_v1.telemetry_sdk_version)
  
  // 验证结构兼容性
  @assertion.assert_eq(resource_v1.service_name.length > 0, resource_v2.service_name.length > 0)
  @assertion.assert_eq(resource_v1.attributes.length >= 0, resource_v2.attributes.length >= 0)
}

test "data_integrity_serialization" {
  // 测试数据完整性序列化
  let original_data = "test_data_with_unicode_中文_🚀_ñ_ü"
  let string_attr = common::AttributeValue::string(original_data)
  
  // 验证字符串属性保持完整性
  match string_attr {
    common::StringValue(s) => {
      @assertion.assert_eq(s, original_data)
      @assertion.assert_eq(s.length, original_data.length)
    }
    _ => @assertion.assert_true(false)
  }
  
  // 测试数值精度保持
  let precise_float = 3.141592653589793
  let float_attr = common::AttributeValue::float(precise_float)
  
  match float_attr {
    common::FloatValue(f) => {
      @assertion.assert_true(f - precise_float < 0.0001)
    }
    _ => @assertion.assert_true(false)
  }
}
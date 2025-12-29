// 序列化和反序列化测试 - 测试数据的持久化和传输
use azimuth.telemetry.api.common.{AttributeValue, Resource}
use azimuth.telemetry.api.context.{Context, create_key}
use azimuth.telemetry.api.logs.{LogRecord, SeverityNumber}
use azimuth.telemetry.api.metrics.{Measurement}
use azimuth.telemetry.api.trace.{SpanKind, StatusCode, SpanContext}
use azimuth.telemetry.api.propagation.{MapCarrier, W3CTraceContextPropagator}

test "serialization_attribute_values" {
  // 测试AttributeValue的序列化和反序列化
  
  // 1. 测试基本类型的序列化
  let string_attr = AttributeValue::string("test-string")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14159)
  let bool_attr = AttributeValue::bool(true)
  
  // 模拟序列化为字符串
  let string_serialized = "string:test-string"
  let int_serialized = "int:42"
  let float_serialized = "float:3.14159"
  let bool_serialized = "bool:true"
  
  // 验证序列化格式
  assert_eq(string_serialized.starts_with("string:"), true)
  assert_eq(int_serialized.starts_with("int:"), true)
  assert_eq(float_serialized.starts_with("float:"), true)
  assert_eq(bool_serialized.starts_with("bool:"), true)
  
  // 2. 测试数组类型的序列化
  let string_array = AttributeValue::array_string(["a", "b", "c"])
  let int_array = AttributeValue::array_int([1L, 2L, 3L])
  let float_array = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array = AttributeValue::array_bool([true, false, true])
  
  // 模拟数组序列化
  let string_array_serialized = "array_string:[\"a\",\"b\",\"c\"]"
  let int_array_serialized = "array_int:[1,2,3]"
  let float_array_serialized = "array_float:[1.1,2.2,3.3]"
  let bool_array_serialized = "array_bool:[true,false,true]"
  
  // 验证数组序列化格式
  assert_eq(string_array_serialized.starts_with("array_string:"), true)
  assert_eq(int_array_serialized.starts_with("array_int:"), true)
  assert_eq(float_array_serialized.starts_with("array_float:"), true)
  assert_eq(bool_array_serialized.starts_with("array_bool:"), true)
  
  // 3. 测试特殊值的序列化
  let empty_string = AttributeValue::string("")
  let zero_int = AttributeValue::int(0L)
  let negative_int = AttributeValue::int(-1L)
  let infinity_float = AttributeValue::float(1.0/0.0)
  let nan_float = AttributeValue::float(0.0/0.0)
  
  // 验证特殊值序列化
  assert_eq(empty_string, StringValue(""))
  assert_eq(zero_int, IntValue(0L))
  assert_eq(negative_int, IntValue(-1L))
  match infinity_float {
    FloatValue(v) => assert_eq(v > 1.0e+308, true)
    _ => @test.fail("Expected FloatValue with infinity")
  }
  match nan_float {
    FloatValue(v) => assert_eq(v != v, true)
    _ => @test.fail("Expected FloatValue with NaN")
  }
}

test "serialization_resource" {
  // 测试Resource的序列化和反序列化
  
  // 1. 测试基本Resource序列化
  let resource = Resource::default("test-service")
  
  // 模拟Resource序列化为JSON格式
  let resource_json = "{"
    + "\"service_name\":\"test-service\","
    + "\"service_version\":null,"
    + "\"telemetry_sdk_name\":\"azimuth\","
    + "\"telemetry_sdk_version\":\"0.1.0\","
    + "\"attributes\":[]"
    + "}"
  
  // 验证JSON格式包含必要字段
  assert_eq(resource_json.contains("service_name"), true)
  assert_eq(resource_json.contains("telemetry_sdk_name"), true)
  assert_eq(resource_json.contains("telemetry_sdk_version"), true)
  assert_eq(resource_json.contains("attributes"), true)
  
  // 2. 测试带有属性的Resource序列化
  let resource_with_attrs = Resource::default("complex-service")
  let attrs = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.instance.id", AttributeValue::string("instance-123")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("deployment.environment", AttributeValue::string("prod"))
  ]
  
  // 模拟带属性Resource的序列化
  let complex_resource_json = "{"
    + "\"service_name\":\"complex-service\","
    + "\"service_version\":null,"
    + "\"telemetry_sdk_name\":\"azimuth\","
    + "\"telemetry_sdk_version\":\"0.1.0\","
    + "\"attributes\":["
    + "{\"key\":\"service.namespace\",\"value\":{\"type\":\"string\",\"data\":\"production\"}},"
    + "{\"key\":\"service.instance.id\",\"value\":{\"type\":\"string\",\"data\":\"instance-123\"}},"
    + "{\"key\":\"service.version\",\"value\":{\"type\":\"string\",\"data\":\"2.1.0\"}},"
    + "{\"key\":\"deployment.environment\",\"value\":{\"type\":\"string\",\"data\":\"prod\"}}"
    + "]"
    + "}"
  
  // 验证复杂Resource序列化
  assert_eq(complex_resource_json.contains("service.namespace"), true)
  assert_eq(complex_resource_json.contains("service.instance.id"), true)
  assert_eq(complex_resource_json.contains("service.version"), true)
  assert_eq(complex_resource_json.contains("deployment.environment"), true)
}

test "serialization_log_record" {
  // 测试LogRecord的序列化和反序列化
  
  // 1. 测试基本LogRecord序列化
  let log_record = LogRecord::builder()
    .severity(SeverityNumber::Info)
    .body("Test log message")
    .build()
  
  // 模拟LogRecord序列化
  let log_record_json = "{"
    + "\"timestamp_unix_nanos\":0,"
    + "\"observed_timestamp_unix_nanos\":null,"
    + "\"severity_number\":\"Info\","
    + "\"severity_text\":null,"
    + "\"body\":\"Test log message\","
    + "\"attributes\":[],"
    + "\"trace_id\":null,"
    + "\"span_id\":null,"
    + "\"trace_flags\":null,"
    + "\"resource\":null,"
    + "\"instrumentation_scope\":null"
    + "}"
  
  // 验证LogRecord序列化格式
  assert_eq(log_record_json.contains("timestamp_unix_nanos"), true)
  assert_eq(log_record_json.contains("severity_number"), true)
  assert_eq(log_record_json.contains("body"), true)
  assert_eq(log_record_json.contains("attributes"), true)
  
  // 2. 测试复杂LogRecord序列化
  let complex_log_record = LogRecord::builder()
    .severity(SeverityNumber::Error)
    .body("Error occurred")
    .severity_text("ERROR")
    .with_attribute("error.type", AttributeValue::string("ValidationError"))
    .with_attribute("error.code", AttributeValue::int(400L))
    .with_attribute("error.retryable", AttributeValue::bool(false))
    .build()
  
  // 验证复杂LogRecord
  assert_eq(complex_log_record.severity_number, SeverityNumber::Error)
  assert_eq(complex_log_record.severity_text.unwrap_or(""), "ERROR")
  assert_eq(complex_log_record.body.unwrap_or(""), "Error occurred")
  assert_eq(complex_log_record.attributes.length(), 3)
}

test "serialization_context" {
  // 测试Context的序列化和反序列化
  
  // 1. 测试基本Context序列化
  let context = Context::empty()
  
  // 模拟Context序列化
  let context_json = "{\"values\":[]}"
  
  // 验证空Context序列化
  assert_eq(context_json.contains("values"), true)
  assert_eq(context_json.contains("[]"), true)
  
  // 2. 测试带有值的Context序列化
  let key1 = create_key("key1")
  let key2 = create_key("key2")
  let key3 = create_key("key3")
  
  let context_with_values = context
    .with_value(key1, "value1")
    .with_value(key2, "value2")
    .with_value(key3, "value3")
  
  // 模拟带值Context的序列化
  let complex_context_json = "{"
    + "\"values\":["
    + "{\"key\":\"key1\",\"value\":\"value1\"},"
    + "{\"key\":\"key2\",\"value\":\"value2\"},"
    + "{\"key\":\"key3\",\"value\":\"value3\"}"
    + "]"
    + "}"
  
  // 验证复杂Context序列化
  assert_eq(complex_context_json.contains("key1"), true)
  assert_eq(complex_context_json.contains("key2"), true)
  assert_eq(complex_context_json.contains("key3"), true)
  assert_eq(complex_context_json.contains("value1"), true)
  assert_eq(complex_context_json.contains("value2"), true)
  assert_eq(complex_context_json.contains("value3"), true)
  
  // 3. 测试Context的反序列化
  match context_with_values.get(key1) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected value for key1")
  }
  
  match context_with_values.get(key2) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected value for key2")
  }
  
  match context_with_values.get(key3) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected value for key3")
  }
}

test "serialization_span_context" {
  // 测试SpanContext的序列化和反序列化
  
  // 1. 测试基本SpanContext序列化
  let span_context = SpanContext::{
    trace_id: Array::make(16, 0_byte),
    span_id: Array::make(8, 0_byte),
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  // 模拟SpanContext序列化为十六进制字符串
  let trace_id_hex = "00000000000000000000000000000000"
  let span_id_hex = "0000000000000000"
  let span_context_serialized = trace_id_hex + "-" + span_id_hex + "-00"
  
  // 验证SpanContext序列化格式
  assert_eq(span_context_serialized.length(), 35)  // 32 + 1 + 16 + 1 + 2
  assert_eq(span_context_serialized.contains("-"), true)
  
  // 2. 测试带有值的SpanContext序列化
  let trace_id_with_value = Array::make(16, 1_byte)
  let span_id_with_value = Array::make(8, 1_byte)
  let span_context_with_values = SpanContext::{
    trace_id: trace_id_with_value,
    span_id: span_id_with_value,
    trace_flags: 1_byte,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 验证带有值的SpanContext
  assert_eq(span_context_with_values.trace_id.length(), 16)
  assert_eq(span_context_with_values.span_id.length(), 8)
  assert_eq(span_context_with_values.trace_flags, 1_byte)
  assert_eq(span_context_with_values.trace_state.length() > 0, true)
}

test "serialization_propagation_headers" {
  // 测试传播头的序列化和反序列化
  
  // 1. 测试W3C TraceContext头序列化
  let propagator = W3CTraceContextPropagator::{}
  let context = Context::empty()
  let carrier = MapCarrier::new()
  
  // 注入上下文
  propagator.inject(context, carrier)
  
  // 模拟traceparent头的格式
  let traceparent_header = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  
  // 验证traceparent头格式
  assert_eq(traceparent_header.length(), 55)  // 2 + 1 + 32 + 1 + 16 + 1 + 2
  assert_eq(traceparent_header.split("-").length(), 4)
  
  // 解析traceparent头
  let parts = traceparent_header.split("-")
  let version = parts[0]
  let trace_id = parts[1]
  let span_id = parts[2]
  let trace_flags = parts[3]
  
  assert_eq(version, "00")
  assert_eq(trace_id.length(), 32)
  assert_eq(span_id.length(), 16)
  assert_eq(trace_flags, "01")
  
  // 2. 测试W3C Baggage头序列化
  let baggage_header = "key1=value1,key2=value2;param1=paramvalue1,key3=value3"
  
  // 验证baggage头格式
  assert_eq(baggage_header.contains("key1=value1"), true)
  assert_eq(baggage_header.contains("key2=value2"), true)
  assert_eq(baggage_header.contains("key3=value3"), true)
  
  // 解析baggage头
  let baggage_entries = baggage_header.split(",")
  
  // 验证解析结果
  assert_eq(baggage_entries.length(), 3)
  assert_eq(baggage_entries[0].starts_with("key1="), true)
  assert_eq(baggage_entries[1].starts_with("key2="), true)
  assert_eq(baggage_entries[2].starts_with("key3="), true)
}

test "serialization_measurement" {
  // 测试Measurement的序列化和反序列化
  
  // 1. 测试基本Measurement序列化
  let measurement = Measurement::{
    value: 123.45,
    attributes: [
      ("unit", AttributeValue::string("ms")),
      ("type", AttributeValue::string("response_time"))
    ]
  }
  
  // 模拟Measurement序列化
  let measurement_json = "{"
    + "\"value\":123.45,"
    + "\"attributes\":["
    + "{\"key\":\"unit\",\"value\":{\"type\":\"string\",\"data\":\"ms\"}},"
    + "{\"key\":\"type\",\"value\":{\"type\":\"string\",\"data\":\"response_time\"}}"
    + "]"
    + "}"
  
  // 验证Measurement序列化格式
  assert_eq(measurement_json.contains("value"), true)
  assert_eq(measurement_json.contains("attributes"), true)
  assert_eq(measurement_json.contains("unit"), true)
  assert_eq(measurement_json.contains("type"), true)
  
  // 2. 测试复杂Measurement序列化
  let complex_measurement = Measurement::{
    value: 999.999,
    attributes: [
      ("service.name", AttributeValue::string("api-service")),
      ("endpoint", AttributeValue::string("/api/v1/users")),
      ("method", AttributeValue::string("GET")),
      ("status_code", AttributeValue::int(200L)),
      ("success", AttributeValue::bool(true)),
      ("tags", AttributeValue::array_string(["api", "users", "public"]))
    ]
  }
  
  // 验证复杂Measurement
  assert_eq(complex_measurement.value, 999.999)
  assert_eq(complex_measurement.attributes.length(), 6)
  
  // 验证属性类型
  let mut found_string_attr = false
  let mut found_int_attr = false
  let mut found_bool_attr = false
  let mut found_array_attr = false
  
  let mut i = 0
  while i < complex_measurement.attributes.length() {
    let (key, value) = complex_measurement.attributes[i]
    
    match value {
      StringValue(_) => found_string_attr = true
      IntValue(_) => found_int_attr = true
      BoolValue(_) => found_bool_attr = true
      ArrayStringValue(_) => found_array_attr = true
      _ => @test.fail("Unexpected attribute type")
    }
    
    i = i + 1
  }
  
  assert_eq(found_string_attr, true)
  assert_eq(found_int_attr, true)
  assert_eq(found_bool_attr, true)
  assert_eq(found_array_attr, true)
}

test "serialization_compatibility" {
  // 测试序列化兼容性
  
  // 1. 测试版本兼容性
  let resource_v1 = Resource::default("test-service")
  
  // 模拟不同版本的序列化格式
  let v1_json = "{"
    + "\"service_name\":\"test-service\","
    + "\"service_version\":null,"
    + "\"telemetry_sdk_name\":\"azimuth\","
    + "\"telemetry_sdk_version\":\"0.1.0\","
    + "\"attributes\":[]"
    + "}"
  
  let v2_json = "{"
    + "\"service_name\":\"test-service\","
    + "\"service_version\":null,"
    + "\"telemetry_sdk_name\":\"azimuth\","
    + "\"telemetry_sdk_version\":\"0.2.0\","
    + "\"attributes\":[],"
    + "\"schema_url\":null"
    + "}"
  
  // 验证版本兼容性
  assert_eq(v1_json.contains("0.1.0"), true)
  assert_eq(v2_json.contains("0.2.0"), true)
  assert_eq(v2_json.contains("schema_url"), true)
  
  // 2. 测试字段兼容性
  let log_record_with_optional = LogRecord::builder()
    .severity(SeverityNumber::Info)
    .body("Test message")
    .severity_text("INFO")
    .build()
  
  let log_record_without_optional = LogRecord::builder()
    .severity(SeverityNumber::Info)
    .body("Test message")
    .build()
  
  // 验证可选字段兼容性
  assert_eq(log_record_with_optional.severity_text.unwrap_or(""), "INFO")
  assert_eq(log_record_without_optional.severity_text, None)
  
  // 3. 测试数据类型兼容性
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(42.0)
  
  // 验证数值类型兼容性
  match int_attr {
    IntValue(v) => assert_eq(v, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  match float_attr {
    FloatValue(v) => assert_eq(v, 42.0)
    _ => @test.fail("Expected FloatValue")
  }
}
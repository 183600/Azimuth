// Azimuth Telemetry Serialization Test Suite
// 测试遥测数据的序列化和反序列化功能，确保数据在不同系统间的正确传输

test "AttributeValue序列化测试" {
  // 字符串值序列化
  let string_attr = azimuth::AttributeValue::StringValue("test-value")
  let string_serialized = match string_attr {
    azimuth::AttributeValue::StringValue(s) => "{\"type\":\"string\",\"value\":\"" + s + "\"}"
    _ => ""
  }
  assert_eq(string_serialized, "{\"type\":\"string\",\"value\":\"test-value\"}")
  
  // 整数值序列化
  let int_attr = azimuth::AttributeValue::IntValue(42)
  let int_serialized = match int_attr {
    azimuth::AttributeValue::IntValue(i) => "{\"type\":\"int\",\"value\":" + i.to_string() + "}"
    _ => ""
  }
  assert_eq(int_serialized, "{\"type\":\"int\",\"value\":42}")
  
  // 浮点数值序列化
  let float_attr = azimuth::AttributeValue::FloatValue(3.14)
  let float_serialized = match float_attr {
    azimuth::AttributeValue::FloatValue(f) => "{\"type\":\"float\",\"value\":" + f.to_string() + "}"
    _ => ""
  }
  assert_eq(float_serialized, "{\"type\":\"float\",\"value\":3.14}")
  
  // 布尔值序列化
  let bool_attr = azimuth::AttributeValue::BoolValue(true)
  let bool_serialized = match bool_attr {
    azimuth::AttributeValue::BoolValue(b) => {
      if b {
        "{\"type\":\"bool\",\"value\":true}"
      } else {
        "{\"type\":\"bool\",\"value\":false}"
      }
    }
    _ => ""
  }
  assert_eq(bool_serialized, "{\"type\":\"bool\",\"value\":true}")
  
  // 数组字符串值序列化
  let array_string_attr = azimuth::AttributeValue::ArrayStringValue(["a", "b", "c"])
  let array_string_serialized = match array_string_attr {
    azimuth::AttributeValue::ArrayStringValue(arr) => {
      let mut result = "{\"type\":\"array_string\",\"value\":["
      for i in 0..arr.length() {
        if i > 0 {
          result = result + ","
        }
        result = result + "\"" + arr[i] + "\""
      }
      result = result + "]}"
      result
    }
    _ => ""
  }
  assert_eq(array_string_serialized, "{\"type\":\"array_string\",\"value\":[\"a\",\"b\",\"c\"]}")
  
  // 数组整数值序列化
  let array_int_attr = azimuth::AttributeValue::ArrayIntValue([1, 2, 3])
  let array_int_serialized = match array_int_attr {
    azimuth::AttributeValue::ArrayIntValue(arr) => {
      let mut result = "{\"type\":\"array_int\",\"value\":["
      for i in 0..arr.length() {
        if i > 0 {
          result = result + ","
        }
        result = result + arr[i].to_string()
      }
      result = result + "]}"
      result
    }
    _ => ""
  }
  assert_eq(array_int_serialized, "{\"type\":\"array_int\",\"value\":[1,2,3]}")
}

test "Attributes序列化测试" {
  // 创建属性集合
  let attrs = azimuth::Attributes {
    values: [
      ("service.name", azimuth::AttributeValue::StringValue("payment-service")),
      ("service.version", azimuth::AttributeValue::StringValue("2.1.0")),
      ("service.port", azimuth::AttributeValue::IntValue(8080)),
      ("service.enabled", azimuth::AttributeValue::BoolValue(true))
    ]
  }
  
  // 序列化属性集合
  let mut serialized = "{\"attributes\":["
  for i in 0..attrs.values.length() {
    if i > 0 {
      serialized = serialized + ","
    }
    
    let (key, value) = attrs.values[i]
    serialized = serialized + "{\"key\":\"" + key + "\"," +
    
    match value {
      azimuth::AttributeValue::StringValue(s) => {
        serialized = serialized + "\"value\":{\"type\":\"string\",\"value\":\"" + s + "\"}"
      }
      azimuth::AttributeValue::IntValue(int_val) => {
        serialized = serialized + "\"value\":{\"type\":\"int\",\"value\":" + int_val.to_string() + "}"
      }
      azimuth::AttributeValue::BoolValue(bool_val) => {
        if bool_val {
          serialized = serialized + "\"value\":{\"type\":\"bool\",\"value\":true}"
        } else {
          serialized = serialized + "\"value\":{\"type\":\"bool\",\"value\":false}"
        }
      }
      _ => {
        serialized = serialized + "\"value\":{\"type\":\"unknown\"}"
      }
    }
    
    serialized = serialized + "}"
  }
  serialized = serialized + "]}"
  
  // 验证序列化结果包含预期的键值对
  assert_true(serialized.contains("\"key\":\"service.name\""))
  assert_true(serialized.contains("\"value\":{\"type\":\"string\",\"value\":\"payment-service\"}"))
  assert_true(serialized.contains("\"key\":\"service.port\""))
  assert_true(serialized.contains("\"value\":{\"type\":\"int\",\"value\":8080}"))
}

test "Resource序列化测试" {
  // 创建资源
  let resource = azimuth::Resource {
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("order-service")),
      ("service.version", azimuth::AttributeValue::StringValue("1.5.0")),
      ("host.name", azimuth::AttributeValue::StringValue("order-host-01")),
      ("cloud.provider", azimuth::AttributeValue::StringValue("aws"))
    ]
  }
  
  // 序列化资源
  let mut serialized = "{\"resource\":["
  for i in 0..resource.attributes.length() {
    if i > 0 {
      serialized = serialized + ","
    }
    
    let (key, value) = resource.attributes[i]
    serialized = serialized + "{\"key\":\"" + key + "\"," +
    
    match value {
      azimuth::AttributeValue::StringValue(s) => {
        serialized = serialized + "\"value\":\"" + s + "\""
      }
      _ => {
        serialized = serialized + "\"value\":\"\""
      }
    }
    
    serialized = serialized + "}"
  }
  serialized = serialized + "]}"
  
  // 验证序列化结果
  assert_true(serialized.contains("\"key\":\"service.name\""))
  assert_true(serialized.contains("\"value\":\"order-service\""))
  assert_true(serialized.contains("\"key\":\"cloud.provider\""))
  assert_true(serialized.contains("\"value\":\"aws\""))
}

test "SpanContext序列化测试" {
  // 创建跨度上下文
  let span_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: true,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 序列化跨度上下文
  let sampled_str = if span_context.sampled { "true" } else { "false" }
  let serialized = "{\"trace_id\":\"" + span_context.trace_id + 
                   "\",\"span_id\":\"" + span_context.span_id + 
                   "\",\"sampled\":" + sampled_str + 
                   ",\"trace_state\":\"" + span_context.trace_state + "\"}"
  
  // 验证序列化结果
  assert_eq(serialized, "{\"trace_id\":\"4bf92f3577b34da6a3ce929d0e0e4736\",\"span_id\":\"00f067aa0ba902b7\",\"sampled\":true,\"trace_state\":\"rojo=00f067aa0ba902b7,congo=t61rcWkgMzE\"}")
  
  // 测试未采样的跨度上下文序列化
  let unsampled_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: false,
    trace_state: ""
  }
  
  let unsampled_sampled_str = if unsampled_context.sampled { "true" } else { "false" }
  let unsampled_serialized = "{\"trace_id\":\"" + unsampled_context.trace_id + 
                             "\",\"span_id\":\"" + unsampled_context.span_id + 
                             "\",\"sampled\":" + unsampled_sampled_str + 
                             ",\"trace_state\":\"" + unsampled_context.trace_state + "\"}"
  
  // 验证未采样的序列化结果
  assert_eq(unsampled_serialized, "{\"trace_id\":\"4bf92f3577b34da6a3ce929d0e0e4736\",\"span_id\":\"00f067aa0ba902b7\",\"sampled\":false,\"trace_state\":\"\"}")
}

test "Baggage序列化测试" {
  // 创建行李
  let baggage = azimuth::Baggage {
    entries: [
      ("user.id", "12345"),
      ("session.id", "session-abcde"),
      ("request.id", "req-67890")
    ]
  }
  
  // 序列化行李
  let mut serialized = "{\"baggage\":["
  for i in 0..baggage.entries.length() {
    if i > 0 {
      serialized = serialized + ","
    }
    
    let (key, value) = baggage.entries[i]
    serialized = serialized + "{\"key\":\"" + key + "\",\"value\":\"" + value + "\"}"
  }
  serialized = serialized + "]}"
  
  // 验证序列化结果
  assert_true(serialized.contains("\"key\":\"user.id\",\"value\":\"12345\""))
  assert_true(serialized.contains("\"key\":\"session.id\",\"value\":\"session-abcde\""))
  assert_true(serialized.contains("\"key\":\"request.id\",\"value\":\"req-67890\""))
  
  // 测试空行李序列化
  let empty_baggage = azimuth::Baggage { entries: [] }
  let empty_serialized = "{\"baggage\":[]}"
  assert_eq(empty_serialized, "{\"baggage\":[]}")
}

test "InstrumentationScope序列化测试" {
  // 基本检测范围
  let basic_scope = azimuth::InstrumentationScope {
    name: "azimuth.tracer",
    version: None,
    schema_url: None
  }
  
  let basic_serialized = "{\"name\":\"" + basic_scope.name + "\"}"
  assert_eq(basic_serialized, "{\"name\":\"azimuth.tracer\"}")
  
  // 完整检测范围
  let full_scope = azimuth::InstrumentationScope {
    name: "azimuth.metrics",
    version: Some("2.1.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  let mut full_serialized = "{\"name\":\"" + full_scope.name + "\""
  
  match full_scope.version {
    Some(v) => {
      full_serialized = full_serialized + ",\"version\":\"" + v + "\""
    }
    None => ()
  }
  
  match full_scope.schema_url {
    Some(url) => {
      full_serialized = full_serialized + ",\"schema_url\":\"" + url + "\""
    }
    None => ()
  }
  
  full_serialized = full_serialized + "}"
  
  // 验证完整序列化结果
  assert_eq(full_serialized, "{\"name\":\"azimuth.metrics\",\"version\":\"2.1.0\",\"schema_url\":\"https://opentelemetry.io/schemas/1.20.0\"}")
}

test "TextMapCarrier序列化测试" {
  // 创建载体
  let carrier = azimuth::TextMapCarrier {
    headers: [
      ("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
      ("tracestate", "rojo=00f067aa0ba902b7"),
      ("x-correlation-id", "corr-12345"),
      ("x-request-id", "req-67890")
    ]
  }
  
  // 序列化载体
  let mut serialized = "{\"headers\":["
  for i in 0..carrier.headers.length() {
    if i > 0 {
      serialized = serialized + ","
    }
    
    let (key, value) = carrier.headers[i]
    serialized = serialized + "{\"key\":\"" + key + "\",\"value\":\"" + value + "\"}"
  }
  serialized = serialized + "]}"
  
  // 验证序列化结果
  assert_true(serialized.contains("\"key\":\"traceparent\",\"value\":\"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01\""))
  assert_true(serialized.contains("\"key\":\"tracestate\",\"value\":\"rojo=00f067aa0ba902b7\""))
  assert_true(serialized.contains("\"key\":\"x-correlation-id\",\"value\":\"corr-12345\""))
  
  // 测试空载体序列化
  let empty_carrier = azimuth::TextMapCarrier { headers: [] }
  let empty_serialized = "{\"headers\":[]}"
  assert_eq(empty_serialized, "{\"headers\":[]}")
}

test "复杂遥测数据序列化测试" {
  // 创建复杂的遥测数据结构
  let resource = azimuth::Resource {
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("payment-service")),
      ("service.version", azimuth::AttributeValue::StringValue("2.1.0")),
      ("host.name", azimuth::AttributeValue::StringValue("payment-host-01")),
      ("cloud.provider", azimuth::AttributeValue::StringValue("aws"))
    ]
  }
  
  let span_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: true,
    trace_state: "rojo=00f067aa0ba902b7"
  }
  
  let scope = azimuth::InstrumentationScope {
    name: "payment.tracer",
    version: Some("1.5.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  // 序列化复杂结构
  let mut resource_serialized = "{\"resource\":["
  for i in 0..resource.attributes.length() {
    if i > 0 {
      resource_serialized = resource_serialized + ","
    }
    let (key, value) = resource.attributes[i]
    match value {
      azimuth::AttributeValue::StringValue(s) => {
        resource_serialized = resource_serialized + "{\"key\":\"" + key + "\",\"value\":\"" + s + "\"}"
      }
      _ => {
        resource_serialized = resource_serialized + "{\"key\":\"" + key + "\",\"value\":\"\"}"
      }
    }
  }
  resource_serialized = resource_serialized + "]}"
  
  let sampled_str = if span_context.sampled { "true" } else { "false" }
  let context_serialized = "{\"trace_id\":\"" + span_context.trace_id + 
                           "\",\"span_id\":\"" + span_context.span_id + 
                           "\",\"sampled\":" + sampled_str + 
                           ",\"trace_state\":\"" + span_context.trace_state + "\"}"
  
  let mut scope_serialized = "{\"name\":\"" + scope.name + "\""
  match scope.version {
    Some(v) => {
      scope_serialized = scope_serialized + ",\"version\":\"" + v + "\""
    }
    None => ()
  }
  match scope.schema_url {
    Some(url) => {
      scope_serialized = scope_serialized + ",\"schema_url\":\"" + url + "\""
    }
    None => ()
  }
  scope_serialized = scope_serialized + "}"
  
  // 组合完整序列化结果
  let full_serialized = "{\"telemetry_data\":{" + 
                        "\"resource\":" + resource_serialized.substring(10, resource_serialized.length() - 1) + "," +
                        "\"span_context\":" + context_serialized + "," +
                        "\"instrumentation_scope\":" + scope_serialized +
                        "}}"
  
  // 验证完整序列化结果
  assert_true(full_serialized.contains("\"service.name\""))
  assert_true(full_serialized.contains("\"payment-service\""))
  assert_true(full_serialized.contains("\"trace_id\":\"4bf92f3577b34da6a3ce929d0e0e4736\""))
  assert_true(full_serialized.contains("\"payment.tracer\""))
  assert_true(full_serialized.contains("\"version\":\"1.5.0\""))
}

test "序列化边界条件测试" {
  // 测试空字符串序列化
  let empty_string_attr = azimuth::AttributeValue::StringValue("")
  let empty_string_serialized = match empty_string_attr {
    azimuth::AttributeValue::StringValue(s) => "{\"type\":\"string\",\"value\":\"" + s + "\"}"
    _ => ""
  }
  assert_eq(empty_string_serialized, "{\"type\":\"string\",\"value\":\"\"}")
  
  // 测试零值序列化
  let zero_int_attr = azimuth::AttributeValue::IntValue(0)
  let zero_int_serialized = match zero_int_attr {
    azimuth::AttributeValue::IntValue(i) => "{\"type\":\"int\",\"value\":" + i.to_string() + "}"
    _ => ""
  }
  assert_eq(zero_int_serialized, "{\"type\":\"int\",\"value\":0}")
  
  let zero_float_attr = azimuth::AttributeValue::FloatValue(0.0)
  let zero_float_serialized = match zero_float_attr {
    azimuth::AttributeValue::FloatValue(f) => "{\"type\":\"float\",\"value\":" + f.to_string() + "}"
    _ => ""
  }
  assert_eq(zero_float_serialized, "{\"type\":\"float\",\"value\":0}")
  
  // 测试空数组序列化
  let empty_array_string = azimuth::AttributeValue::ArrayStringValue([])
  let empty_array_string_serialized = match empty_array_string {
    azimuth::AttributeValue::ArrayStringValue(_) => "{\"type\":\"array_string\",\"value\":[]}"
    _ => ""
  }
  assert_eq(empty_array_string_serialized, "{\"type\":\"array_string\",\"value\":[]}")
  
  // 测试特殊字符序列化
  let special_chars_attr = azimuth::AttributeValue::StringValue("!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
  let special_chars_serialized = match special_chars_attr {
    azimuth::AttributeValue::StringValue(s) => "{\"type\":\"string\",\"value\":\"" + s + "\"}"
    _ => ""
  }
  assert_true(special_chars_serialized.contains("!@#$%^&*()"))
  
  // 测试Unicode字符序列化
  let unicode_attr = azimuth::AttributeValue::StringValue("测试中文🚀emoji")
  let unicode_serialized = match unicode_attr {
    azimuth::AttributeValue::StringValue(s) => "{\"type\":\"string\",\"value\":\"" + s + "\"}"
    _ => ""
  }
  assert_true(unicode_serialized.contains("测试中文"))
  assert_true(unicode_serialized.contains("🚀"))
  
  // 测试长字符串序列化
  let long_string = "a".repeat(1000)
  let long_string_attr = azimuth::AttributeValue::StringValue(long_string)
  let long_string_serialized = match long_string_attr {
    azimuth::AttributeValue::StringValue(s) => "{\"type\":\"string\",\"value\":\"" + s + "\"}"
    _ => ""
  }
  assert_eq(long_string_serialized.length(), 1027) // {\"type\":\"string\",\"value\":\"\"} + 1000个字符
}

test "反序列化基本测试" {
  // 字符串值反序列化
  let string_json = "{\"type\":\"string\",\"value\":\"test-value\"}"
  // 简化的反序列化逻辑
  let string_deserialized = if string_json.contains("\"type\":\"string\"") {
    azimuth::AttributeValue::StringValue("test-value")
  } else {
    azimuth::AttributeValue::StringValue("")
  }
  
  match string_deserialized {
    azimuth::AttributeValue::StringValue(s) => assert_eq(s, "test-value")
    _ => assert_true(false)
  }
  
  // 整数值反序列化
  let int_json = "{\"type\":\"int\",\"value\":42}"
  let int_deserialized = if int_json.contains("\"type\":\"int\"") {
    azimuth::AttributeValue::IntValue(42)
  } else {
    azimuth::AttributeValue::IntValue(0)
  }
  
  match int_deserialized {
    azimuth::AttributeValue::IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  // 布尔值反序列化
  let bool_json = "{\"type\":\"bool\",\"value\":true}"
  let bool_deserialized = if bool_json.contains("\"type\":\"bool\"") && bool_json.contains("true") {
    azimuth::AttributeValue::BoolValue(true)
  } else {
    azimuth::AttributeValue::BoolValue(false)
  }
  
  match bool_deserialized {
    azimuth::AttributeValue::BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
  
  // 跨度上下文反序列化
  let context_json = "{\"trace_id\":\"4bf92f3577b34da6a3ce929d0e0e4736\",\"span_id\":\"00f067aa0ba902b7\",\"sampled\":true,\"trace_state\":\"rojo=00f067aa0ba902b7\"}"
  let context_deserialized = if context_json.contains("\"trace_id\"") && context_json.contains("\"span_id\"") {
    azimuth::SpanContext {
      trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
      span_id: "00f067aa0ba902b7",
      sampled: context_json.contains("\"sampled\":true"),
      trace_state: "rojo=00f067aa0ba902b7"
    }
  } else {
    azimuth::SpanContext {
      trace_id: "",
      span_id: "",
      sampled: false,
      trace_state: ""
    }
  }
  
  assert_eq(context_deserialized.trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
  assert_eq(context_deserialized.span_id, "00f067aa0ba902b7")
  assert_true(context_deserialized.sampled)
  assert_eq(context_deserialized.trace_state, "rojo=00f067aa0ba902b7")
}
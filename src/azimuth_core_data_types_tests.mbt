// Azimuth Core Data Types Test Suite
// 测试核心数据类型的创建、操作和边界条件

// 导入必要的模块
// 注意：这些测试假设已经定义了基本的Azimuth数据类型

test "AttributeValue枚举类型测试" {
  // 测试字符串值
  let string_attr = azimuth::AttributeValue::StringValue("test_value")
  match string_attr {
    azimuth::AttributeValue::StringValue(s) => assert_eq(s, "test_value")
    _ => assert_true(false)
  }
  
  // 测试整数值
  let int_attr = azimuth::AttributeValue::IntValue(42)
  match int_attr {
    azimuth::AttributeValue::IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  // 测试浮点数值
  let float_attr = azimuth::AttributeValue::FloatValue(3.14)
  match float_attr {
    azimuth::AttributeValue::FloatValue(f) => assert_true(f > 3.13 && f < 3.15)
    _ => assert_true(false)
  }
  
  // 测试布尔值
  let bool_attr = azimuth::AttributeValue::BoolValue(true)
  match bool_attr {
    azimuth::AttributeValue::BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
  
  // 测试数组字符串值
  let array_string_attr = azimuth::AttributeValue::ArrayStringValue(["a", "b", "c"])
  match array_string_attr {
    azimuth::AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_true(false)
  }
  
  // 测试数组整数值
  let array_int_attr = azimuth::AttributeValue::ArrayIntValue([1, 2, 3, 4, 5])
  match array_int_attr {
    azimuth::AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
    }
    _ => assert_true(false)
  }
}

test "Attributes结构体测试" {
  // 创建空的属性集合
  let empty_attrs = azimuth::Attributes { values: [] }
  assert_eq(empty_attrs.values.length(), 0)
  
  // 创建包含多个属性的集合
  let attrs = azimuth::Attributes {
    values: [
      ("key1", azimuth::AttributeValue::StringValue("value1")),
      ("key2", azimuth::AttributeValue::IntValue(100)),
      ("key3", azimuth::AttributeValue::BoolValue(true))
    ]
  }
  
  assert_eq(attrs.values.length(), 3)
  
  // 验证第一个属性
  let (key1, value1) = attrs.values[0]
  assert_eq(key1, "key1")
  match value1 {
    azimuth::AttributeValue::StringValue(s) => assert_eq(s, "value1")
    _ => assert_true(false)
  }
  
  // 验证第二个属性
  let (key2, value2) = attrs.values[1]
  assert_eq(key2, "key2")
  match value2 {
    azimuth::AttributeValue::IntValue(i) => assert_eq(i, 100)
    _ => assert_true(false)
  }
  
  // 验证第三个属性
  let (key3, value3) = attrs.values[2]
  assert_eq(key3, "key3")
  match value3 {
    azimuth::AttributeValue::BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
}

test "Resource结构体测试" {
  // 创建空资源
  let empty_resource = azimuth::Resource { attributes: [] }
  assert_eq(empty_resource.attributes.length(), 0)
  
  // 创建包含服务信息的资源
  let service_resource = azimuth::Resource {
    attributes: [
      ("service.name", azimuth::AttributeValue::StringValue("azimuth-service")),
      ("service.version", azimuth::AttributeValue::StringValue("1.0.0")),
      ("service.instance.id", azimuth::AttributeValue::StringValue("instance-123")),
      ("host.name", azimuth::AttributeValue::StringValue("localhost"))
    ]
  }
  
  assert_eq(service_resource.attributes.length(), 4)
  
  // 验证服务名称
  let (service_name_key, service_name_value) = service_resource.attributes[0]
  assert_eq(service_name_key, "service.name")
  match service_name_value {
    azimuth::AttributeValue::StringValue(name) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
}

test "InstrumentationScope结构体测试" {
  // 创建基本检测范围
  let basic_scope = azimuth::InstrumentationScope {
    name: "azimuth.tracer",
    version: None,
    schema_url: None
  }
  assert_eq(basic_scope.name, "azimuth.tracer")
  assert_eq(basic_scope.version, None)
  assert_eq(basic_scope.schema_url, None)
  
  // 创建完整的检测范围
  let full_scope = azimuth::InstrumentationScope {
    name: "azimuth.metrics",
    version: Some("2.1.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  assert_eq(full_scope.name, "azimuth.metrics")
  
  match full_scope.version {
    Some(v) => assert_eq(v, "2.1.0")
    None => assert_true(false)
  }
  
  match full_scope.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.20.0")
    None => assert_true(false)
  }
}

test "Context和ContextKey测试" {
  // 创建空上下文
  let empty_context = azimuth::Context { data: None }
  assert_eq(empty_context.data, None)
  
  // 创建包含数据的上下文
  let data_context = azimuth::Context { 
    data: Some(("trace-id", "context-value")) 
  }
  
  match data_context.data {
    Some((key, value)) => {
      assert_eq(key, "trace-id")
      assert_eq(value, "context-value")
    }
    None => assert_true(false)
  }
  
  // 创建上下文键
  let trace_key = azimuth::ContextKey[String] { key: "trace.context" }
  assert_eq(trace_key.key, "trace.context")
  
  let user_key = azimuth::ContextKey[String] { key: "user.context" }
  assert_eq(user_key.key, "user.context")
}

test "Baggage结构体测试" {
  // 创建空行李
  let empty_baggage = azimuth::Baggage { entries: [] }
  assert_eq(empty_baggage.entries.length(), 0)
  
  // 创建包含多个条目的行李
  let baggage = azimuth::Baggage {
    entries: [
      ("user.id", "12345"),
      ("request.id", "req-67890"),
      ("session.id", "sess-abcde")
    ]
  }
  
  assert_eq(baggage.entries.length(), 3)
  
  // 验证行李条目
  let (user_key, user_value) = baggage.entries[0]
  assert_eq(user_key, "user.id")
  assert_eq(user_value, "12345")
  
  let (request_key, request_value) = baggage.entries[1]
  assert_eq(request_key, "request.id")
  assert_eq(request_value, "req-67890")
  
  let (session_key, session_value) = baggage.entries[2]
  assert_eq(session_key, "session.id")
  assert_eq(session_value, "sess-abcde")
}

test "SpanContext结构体测试" {
  // 创建基本跨度上下文
  let span_context = azimuth::SpanContext {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    sampled: true,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  assert_eq(span_context.trace_id, "0af7651916cd43dd8448eb211c80319c")
  assert_eq(span_context.span_id, "b7ad6b7169203331")
  assert_true(span_context.sampled)
  assert_eq(span_context.trace_state, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  // 测试未采样的跨度上下文
  let unsampled_span_context = azimuth::SpanContext {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    sampled: false,
    trace_state: ""
  }
  
  assert_eq(unsampled_span_context.trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
  assert_eq(unsampled_span_context.span_id, "00f067aa0ba902b7")
  assert_false(unsampled_span_context.sampled)
  assert_eq(unsampled_span_context.trace_state, "")
}

test "TextMapCarrier结构体测试" {
  // 创建空的载体
  let empty_carrier = azimuth::TextMapCarrier { headers: [] }
  assert_eq(empty_carrier.headers.length(), 0)
  
  // 创建包含追踪头的载体
  let carrier = azimuth::TextMapCarrier {
    headers: [
      ("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
      ("tracestate", "rojo=00f067aa0ba902b7"),
      ("x-correlation-id", "correlation-12345")
    ]
  }
  
  assert_eq(carrier.headers.length(), 3)
  
  // 验证追踪头
  let (traceparent_key, traceparent_value) = carrier.headers[0]
  assert_eq(traceparent_key, "traceparent")
  assert_eq(traceparent_value, "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
  
  let (tracestate_key, tracestate_value) = carrier.headers[1]
  assert_eq(tracestate_key, "tracestate")
  assert_eq(tracestate_value, "rojo=00f067aa0ba902b7")
  
  let (correlation_key, correlation_value) = carrier.headers[2]
  assert_eq(correlation_key, "x-correlation-id")
  assert_eq(correlation_value, "correlation-12345")
}

test "数据类型边界条件测试" {
  // 测试空字符串
  let empty_string_attr = azimuth::AttributeValue::StringValue("")
  match empty_string_attr {
    azimuth::AttributeValue::StringValue(s) => assert_eq(s, "")
    _ => assert_true(false)
  }
  
  // 测试零值
  let zero_int_attr = azimuth::AttributeValue::IntValue(0)
  match zero_int_attr {
    azimuth::AttributeValue::IntValue(i) => assert_eq(i, 0)
    _ => assert_true(false)
  }
  
  let zero_float_attr = azimuth::AttributeValue::FloatValue(0.0)
  match zero_float_attr {
    azimuth::AttributeValue::FloatValue(f) => assert_true(f == 0.0)
    _ => assert_true(false)
  }
  
  // 测试空数组
  let empty_array_string = azimuth::AttributeValue::ArrayStringValue([])
  match empty_array_string {
    azimuth::AttributeValue::ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
  
  let empty_array_int = azimuth::AttributeValue::ArrayIntValue([])
  match empty_array_int {
    azimuth::AttributeValue::ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
  
  // 测试负数
  let negative_int_attr = azimuth::AttributeValue::IntValue(-42)
  match negative_int_attr {
    azimuth::AttributeValue::IntValue(i) => assert_eq(i, -42)
    _ => assert_true(false)
  }
  
  let negative_float_attr = azimuth::AttributeValue::FloatValue(-3.14)
  match negative_float_attr {
    azimuth::AttributeValue::FloatValue(f) => assert_true(f < -3.13 && f > -3.15)
    _ => assert_true(false)
  }
}
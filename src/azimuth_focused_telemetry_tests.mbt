// Azimuth Focused Telemetry Tests
// 专注于遥测系统特定功能的测试用例

// 测试1: 属性值类型验证
test "attribute value type validation" {
  // 测试字符串属性值
  let string_attr = StringValue("test_value")
  match string_attr {
    StringValue(v) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  // 测试整数属性值
  let int_attr = IntValue(42)
  match int_attr {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // 测试浮点数属性值
  let float_attr = FloatValue(3.14)
  match float_attr {
    FloatValue(v) => assert_eq(v, 3.14)
    _ => assert_true(false)
  }
  
  // 测试布尔属性值
  let bool_attr = BoolValue(true)
  match bool_attr {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
}

// 测试2: 属性结构操作
test "attributes structure operations" {
  // 创建属性集合
  let attrs = Attributes {
    values : [
      ("service.name", StringValue("azimuth-service")),
      ("service.version", StringValue("1.0.0")),
      ("port", IntValue(8080)),
      ("enabled", BoolValue(true))
    ]
  }
  
  // 验证属性数量
  assert_eq(attrs.values.length(), 4)
  
  // 验证特定属性值
  let service_name = attrs.values[0]
  assert_eq(service_name.0, "service.name")
  match service_name.1 {
    StringValue(v) => assert_eq(v, "azimuth-service")
    _ => assert_true(false)
  }
}

// 测试3: Span上下文创建和验证
test "span context creation and validation" {
  let span_ctx = SpanContext {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "1234567890abcdef",
    sampled : true,
    trace_state : "key1=value1,key2=value2"
  }
  
  // 验证trace_id格式（应该是32个字符的十六进制字符串）
  assert_eq(span_ctx.trace_id.length(), 32)
  
  // 验证span_id格式（应该是16个字符的十六进制字符串）
  assert_eq(span_ctx.span_id.length(), 16)
  
  // 验证采样标志
  assert_true(span_ctx.sampled)
  
  // 验证trace_state
  assert_eq(span_ctx.trace_state, "key1=value1,key2=value2")
}

// 测试4: Baggage操作
test "baggage operations" {
  let baggage = Baggage {
    entries : [
      ("user.id", "12345"),
      ("request.id", "req-67890"),
      ("region", "us-west-2")
    ]
  }
  
  // 验证条目数量
  assert_eq(baggage.entries.length(), 3)
  
  // 验证特定条目
  let user_entry = baggage.entries[0]
  assert_eq(user_entry.0, "user.id")
  assert_eq(user_entry.1, "12345")
  
  // 测试空baggage
  let empty_baggage = Baggage { entries : [] }
  assert_eq(empty_baggage.entries.length(), 0)
}

// 测试5: 仪器化作用域配置
test "instrumentation scope configuration" {
  let scope = InstrumentationScope {
    name : "azimuth.tracer",
    version : Some("1.0.0"),
    schema_url : Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  // 验证scope名称
  assert_eq(scope.name, "azimuth.tracer")
  
  // 验证版本
  match scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_true(false)
  }
  
  // 验证schema URL
  match scope.schema_url {
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.20.0")
    None => assert_true(false)
  }
}

// 测试6: Text Map Carrier操作
test "text map carrier operations" {
  let carrier = TextMapCarrier {
    headers : [
      ("traceparent", "00-1234567890abcdef1234567890abcdef-1234567890abcdef-01"),
      ("tracestate", "key1=value1,key2=value2"),
      ("baggage", "user.id=12345,region=us-west-2")
    ]
  }
  
  // 验证header数量
  assert_eq(carrier.headers.length(), 3)
  
  // 验证特定header
  let traceparent = carrier.headers[0]
  assert_eq(traceparent.0, "traceparent")
  assert_eq(traceparent.1, "00-1234567890abcdef1234567890abcdef-1234567890abcdef-01")
  
  // 测试空carrier
  let empty_carrier = TextMapCarrier { headers : [] }
  assert_eq(empty_carrier.headers.length(), 0)
}

// 测试7: 上下文键操作
test "context key operations" {
  let string_key = ContextKey[String] { key : "correlation.id" }
  assert_eq(string_key.key, "correlation.id")
  
  let int_key = ContextKey[Int] { key : "retry.count" }
  assert_eq(int_key.key, "retry.count")
  
  let bool_key = ContextKey[Bool] { key : "debug.enabled" }
  assert_eq(bool_key.key, "debug.enabled")
}

// 测试8: 资源属性管理
test "resource attributes management" {
  let resource = Resource {
    attributes : [
      ("service.name", StringValue("payment-service")),
      ("service.instance.id", StringValue("instance-123")),
      ("host.name", StringValue("prod-server-01")),
      ("process.pid", IntValue(1234)),
      ("cloud.provider", StringValue("aws")),
      ("cloud.region", StringValue("us-east-1"))
    ]
  }
  
  // 验证资源属性数量
  assert_eq(resource.attributes.length(), 6)
  
  // 验证服务名称
  let service_name = resource.attributes[0]
  assert_eq(service_name.0, "service.name")
  match service_name.1 {
    StringValue(v) => assert_eq(v, "payment-service")
    _ => assert_true(false)
  }
  
  // 验证进程ID
  let process_pid = resource.attributes[3]
  assert_eq(process_pid.0, "process.pid")
  match process_pid.1 {
    IntValue(v) => assert_eq(v, 1234)
    _ => assert_true(false)
  }
}
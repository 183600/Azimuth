// Azimuth Telemetry Core Functionality Tests
// 测试遥测系统的核心功能

test "attribute value creation and type checking" {
  // 测试字符串属性值
  let string_attr = @azimuth.StringValue("test_value")
  match string_attr {
    @azimuth.StringValue(v) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  // 测试整数属性值
  let int_attr = @azimuth.IntValue(42)
  match int_attr {
    @azimuth.IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // 测试浮点数属性值
  let float_attr = @azimuth.FloatValue(3.14)
  match float_attr {
    @azimuth.FloatValue(v) => assert_eq(v, 3.14)
    _ => assert_true(false)
  }
  
  // 测试布尔属性值
  let bool_attr = @azimuth.BoolValue(true)
  match bool_attr {
    @azimuth.BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
}

test "attributes structure operations" {
  // 创建属性集合
  let attrs = @azimuth.Attributes {
    values : [
      ("service.name", @azimuth.StringValue("azimuth-service")),
      ("service.version", @azimuth.StringValue("1.0.0")),
      ("port", @azimuth.IntValue(8080)),
      ("enabled", @azimuth.BoolValue(true))
    ]
  }
  
  // 验证属性数量
  assert_eq(attrs.values.length(), 4)
  
  // 验证特定属性值
  let service_name = attrs.values[0]
  assert_eq(service_name.0, "service.name")
  match service_name.1 {
    @azimuth.StringValue(v) => assert_eq(v, "azimuth-service")
    _ => assert_true(false)
  }
}

test "span context creation and validation" {
  let span_ctx = @azimuth.SpanContext {
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

test "baggage operations" {
  let baggage = @azimuth.Baggage {
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
  let empty_baggage = @azimuth.Baggage { entries : [] }
  assert_eq(empty_baggage.entries.length(), 0)
}

test "instrumentation scope configuration" {
  let scope = @azimuth.InstrumentationScope {
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

test "text map carrier operations" {
  let carrier = @azimuth.TextMapCarrier {
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
  let empty_carrier = @azimuth.TextMapCarrier { headers : [] }
  assert_eq(empty_carrier.headers.length(), 0)
}

test "context key operations" {
  let string_key = @azimuth.ContextKey[String] { key : "correlation.id" }
  assert_eq(string_key.key, "correlation.id")
  
  let int_key = @azimuth.ContextKey[Int] { key : "retry.count" }
  assert_eq(int_key.key, "retry.count")
  
  let bool_key = @azimuth.ContextKey[Bool] { key : "debug.enabled" }
  assert_eq(bool_key.key, "debug.enabled")
}

test "resource attributes management" {
  let resource = @azimuth.Resource {
    attributes : [
      ("service.name", @azimuth.StringValue("payment-service")),
      ("service.instance.id", @azimuth.StringValue("instance-123")),
      ("host.name", @azimuth.StringValue("prod-server-01")),
      ("process.pid", @azimuth.IntValue(1234)),
      ("cloud.provider", @azimuth.StringValue("aws")),
      ("cloud.region", @azimuth.StringValue("us-east-1"))
    ]
  }
  
  // 验证资源属性数量
  assert_eq(resource.attributes.length(), 6)
  
  // 验证服务名称
  let service_name = resource.attributes[0]
  assert_eq(service_name.0, "service.name")
  match service_name.1 {
    @azimuth.StringValue(v) => assert_eq(v, "payment-service")
    _ => assert_true(false)
  }
  
  // 验证进程ID
  let process_pid = resource.attributes[3]
  assert_eq(process_pid.0, "process.pid")
  match process_pid.1 {
    @azimuth.IntValue(v) => assert_eq(v, 1234)
    _ => assert_true(false)
  }
}
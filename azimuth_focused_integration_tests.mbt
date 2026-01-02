// Azimuth Focused Integration Tests
// 集成测试用例，专注于核心遥测功能的集成

// Test 1: AttributeValue 类型操作
test "attribute value type operations" {
  let string_attr = AttributeValue::StringValue("test.value")
  let int_attr = AttributeValue::IntValue(42)
  let float_attr = AttributeValue::FloatValue(3.14)
  let bool_attr = AttributeValue::BoolValue(true)
  let array_string_attr = AttributeValue::ArrayStringValue(["a", "b", "c"])
  let array_int_attr = AttributeValue::ArrayIntValue([1, 2, 3])
  
  // 验证类型创建
  match string_attr {
    AttributeValue::StringValue(v) => assert_eq(v, "test.value")
    _ => assert_true(false)
  }
  
  match int_attr {
    AttributeValue::IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  match float_attr {
    AttributeValue::FloatValue(v) => assert_eq(v, 3.14)
    _ => assert_true(false)
  }
  
  match bool_attr {
    AttributeValue::BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
  
  match array_string_attr {
    AttributeValue::ArrayStringValue(v) => assert_eq(v.length(), 3)
    _ => assert_true(false)
  }
  
  match array_int_attr {
    AttributeValue::ArrayIntValue(v) => assert_eq(v.length(), 3)
    _ => assert_true(false)
  }
}

// Test 2: Attributes 结构体操作
test "attributes structure operations" {
  let attrs = Attributes { values: [
    ("service.name", AttributeValue::StringValue("azimuth-service")),
    ("service.version", AttributeValue::StringValue("1.0.0")),
    ("service.instance.id", AttributeValue::StringValue("instance-123")),
    ("port", AttributeValue::IntValue(8080)),
    ("enabled", AttributeValue::BoolValue(true))
  ]}
  
  // 验证属性数量
  assert_eq(attrs.values.length(), 5)
  
  // 验证特定属性存在
  let service_name = attrs.values.find(fn(pair) { pair.0 == "service.name" })
  match service_name {
    Some(pair) => {
      match pair.1 {
        AttributeValue::StringValue(v) => assert_eq(v, "azimuth-service")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证整数属性
  let port = attrs.values.find(fn(pair) { pair.0 == "port" })
  match port {
    Some(pair) => {
      match pair.1 {
        AttributeValue::IntValue(v) => assert_eq(v, 8080)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 3: SpanContext 创建和验证
test "span context creation and validation" {
  let span_ctx = SpanContext {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    sampled: true,
    trace_state: "rojo=00f067aa0ba902b7"
  }
  
  // 验证 trace_id 格式 (32个十六进制字符)
  assert_eq(span_ctx.trace_id.length(), 32)
  
  // 验证 span_id 格式 (16个十六进制字符)
  assert_eq(span_ctx.span_id.length(), 16)
  
  // 验证采样状态
  assert_true(span_ctx.sampled)
  
  // 验证 trace_state
  assert_eq(span_ctx.trace_state, "rojo=00f067aa0ba902b7")
}

// Test 4: Baggage 操作
test "baggage operations" {
  let baggage = Baggage { entries: [
    ("user.id", "12345"),
    ("request.id", "req-67890"),
    ("region", "us-west-2")
  ]}
  
  // 验证条目数量
  assert_eq(baggage.entries.length(), 3)
  
  // 验证特定条目存在
  let user_id = baggage.entries.find(fn(pair) { pair.0 == "user.id" })
  match user_id {
    Some(pair) => assert_eq(pair.1, "12345")
    None => assert_true(false)
  }
  
  // 验证区域条目
  let region = baggage.entries.find(fn(pair) { pair.0 == "region" })
  match region {
    Some(pair) => assert_eq(pair.1, "us-west-2")
    None => assert_true(false)
  }
}

// Test 5: TextMapCarrier 操作
test "text map carrier operations" {
  let carrier = TextMapCarrier { headers: [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7"),
    ("baggage", "user.id=12345,request.id=req-67890"),
    ("x-request-id", "req-abc-123")
  ]}
  
  // 验证头部数量
  assert_eq(carrier.headers.length(), 4)
  
  // 验证特定头部存在
  let traceparent = carrier.headers.find(fn(pair) { pair.0 == "traceparent" })
  match traceparent {
    Some(pair) => assert_true(pair.1.contains("0af7651916cd43dd8448eb211c80319c"))
    None => assert_true(false)
  }
  
  // 验证 baggage 头部
  let baggage = carrier.headers.find(fn(pair) { pair.0 == "baggage" })
  match baggage {
    Some(pair) => {
      assert_true(pair.1.contains("user.id=12345"))
      assert_true(pair.1.contains("request.id=req-67890"))
    }
    None => assert_true(false)
  }
}

// Test 6: SpanKind 枚举测试
test "span kind enumeration" {
  let internal_span = SpanKind::Internal
  let server_span = SpanKind::Server
  let client_span = SpanKind::Client
  let producer_span = SpanKind::Producer
  let consumer_span = SpanKind::Consumer
  
  // 验证枚举值创建
  match internal_span {
    SpanKind::Internal => assert_true(true)
    _ => assert_true(false)
  }
  
  match server_span {
    SpanKind::Server => assert_true(true)
    _ => assert_true(false)
  }
  
  match client_span {
    SpanKind::Client => assert_true(true)
    _ => assert_true(false)
  }
  
  match producer_span {
    SpanKind::Producer => assert_true(true)
    _ => assert_true(false)
  }
  
  match consumer_span {
    SpanKind::Consumer => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 7: StatusCode 枚举测试
test "status code enumeration" {
  let unset_status = StatusCode::Unset
  let ok_status = StatusCode::Ok
  let error_status = StatusCode::Error
  
  // 验证枚举值创建
  match unset_status {
    StatusCode::Unset => assert_true(true)
    _ => assert_true(false)
  }
  
  match ok_status {
    StatusCode::Ok => assert_true(true)
    _ => assert_true(false)
  }
  
  match error_status {
    StatusCode::Error => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 8: Instrument 类型测试
test "instrument type operations" {
  let counter = Instrument::Counter(
    "http.requests.total",
    Some("Total HTTP requests"),
    Some("requests")
  )
  
  let histogram = Instrument::Histogram(
    "http.request.duration",
    Some("HTTP request duration"),
    Some("ms")
  )
  
  let updown_counter = Instrument::UpDownCounter(
    "active.connections",
    Some("Active connections"),
    Some("connections")
  )
  
  let gauge = Instrument::Gauge(
    "memory.usage",
    Some("Memory usage"),
    Some("bytes")
  )
  
  // 验证 Counter 创建
  match counter {
    Instrument::Counter(name, desc, unit) => {
      assert_eq(name, "http.requests.total")
      assert_eq(desc, Some("Total HTTP requests"))
      assert_eq(unit, Some("requests"))
    }
    _ => assert_true(false)
  }
  
  // 验证 Histogram 创建
  match histogram {
    Instrument::Histogram(name, desc, unit) => {
      assert_eq(name, "http.request.duration")
      assert_eq(desc, Some("HTTP request duration"))
      assert_eq(unit, Some("ms"))
    }
    _ => assert_true(false)
  }
  
  // 验证 UpDownCounter 创建
  match updown_counter {
    Instrument::UpDownCounter(name, desc, unit) => {
      assert_eq(name, "active.connections")
      assert_eq(desc, Some("Active connections"))
      assert_eq(unit, Some("connections"))
    }
    _ => assert_true(false)
  }
  
  // 验证 Gauge 创建
  match gauge {
    Instrument::Gauge(name, desc, unit) => {
      assert_eq(name, "memory.usage")
      assert_eq(desc, Some("Memory usage"))
      assert_eq(unit, Some("bytes"))
    }
    _ => assert_true(false)
  }
}

// Test 9: Resource 结构体测试
test "resource structure operations" {
  let resource = Resource { attributes: [
    ("service.name", AttributeValue::StringValue("payment-service")),
    ("service.namespace", AttributeValue::StringValue("production")),
    ("service.instance.id", AttributeValue::StringValue("instance-456")),
    ("service.version", AttributeValue::StringValue("2.1.0")),
    ("host.name", AttributeValue::StringValue("prod-server-01")),
    ("host.id", AttributeValue::StringValue("host-789")),
    ("cloud.provider", AttributeValue::StringValue("aws")),
    ("cloud.region", AttributeValue::StringValue("us-east-1")),
    ("deployment.environment", AttributeValue::StringValue("production"))
  ]}
  
  // 验证属性数量
  assert_eq(resource.attributes.length(), 9)
  
  // 验证服务名称
  let service_name = resource.attributes.find(fn(pair) { pair.0 == "service.name" })
  match service_name {
    Some(pair) => {
      match pair.1 {
        AttributeValue::StringValue(v) => assert_eq(v, "payment-service")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证云提供商
  let cloud_provider = resource.attributes.find(fn(pair) { pair.0 == "cloud.provider" })
  match cloud_provider {
    Some(pair) => {
      match pair.1 {
        AttributeValue::StringValue(v) => assert_eq(v, "aws")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证部署环境
  let deployment_env = resource.attributes.find(fn(pair) { pair.0 == "deployment.environment" })
  match deployment_env {
    Some(pair) => {
      match pair.1 {
        AttributeValue::StringValue(v) => assert_eq(v, "production")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 10: InstrumentationScope 结构体测试
test "instrumentation scope operations" {
  let full_scope = InstrumentationScope {
    name: "azimuth.telemetry",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  let minimal_scope = InstrumentationScope {
    name: "minimal.scope",
    version: None,
    schema_url: None
  }
  
  // 验证完整范围
  assert_eq(full_scope.name, "azimuth.telemetry")
  assert_eq(full_scope.version, Some("1.0.0"))
  assert_eq(full_scope.schema_url, Some("https://opentelemetry.io/schemas/1.20.0"))
  
  // 验证最小范围
  assert_eq(minimal_scope.name, "minimal.scope")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
  
  // 验证版本存在性检查
  match full_scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_true(false)
  }
  
  match minimal_scope.version {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 验证 schema URL 存在性检查
  match full_scope.schema_url {
    Some(url) => assert_true(url.contains("opentelemetry.io"))
    None => assert_true(false)
  }
  
  match minimal_scope.schema_url {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}
// Azimuth 类型系统测试用例
// 专注于测试遥测系统的类型定义和基本操作

// 测试1: AttributeValue类型操作
test "AttributeValue类型操作测试" {
  // 测试字符串值
  let string_val = StringValue("test-value")
  match string_val {
    StringValue(s) => assert_eq(s, "test-value")
    _ => assert_true(false)
  }
  
  // 测试整数值
  let int_val = IntValue(42)
  match int_val {
    IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  // 测试浮点数值
  let float_val = FloatValue(3.14)
  match float_val {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => assert_true(false)
  }
  
  // 测试布尔值
  let bool_val = BoolValue(true)
  match bool_val {
    BoolValue(b) => assert_eq(b, true)
    _ => assert_true(false)
  }
  
  // 测试字符串数组值
  let array_val = ArrayStringValue(["a", "b", "c"])
  match array_val {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_true(false)
  }
  
  // 测试整数数组值
  let int_array_val = ArrayIntValue([1, 2, 3, 4, 5])
  match int_array_val {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
    }
    _ => assert_true(false)
  }
}

// 测试2: Attributes结构操作
test "Attributes结构操作测试" {
  // 创建属性集合
  let attrs = Attributes { values = [
    ("service.name", StringValue("azimuth-test")),
    ("service.version", StringValue("1.0.0")),
    ("port", IntValue(8080)),
    ("enabled", BoolValue(true)),
    ("tags", ArrayStringValue(["test", "demo"]))
  ] }
  
  // 验证属性数量
  assert_eq(attrs.values.length(), 5)
  
  // 验证特定属性
  assert_eq(attrs.values[0], ("service.name", StringValue("azimuth-test")))
  assert_eq(attrs.values[1], ("service.version", StringValue("1.0.0")))
  assert_eq(attrs.values[2], ("port", IntValue(8080)))
  assert_eq(attrs.values[3], ("enabled", BoolValue(true)))
  assert_eq(attrs.values[4], ("tags", ArrayStringValue(["test", "demo"])))
}

// 测试3: Resource结构操作
test "Resource结构操作测试" {
  // 创建资源
  let resource = Resource { attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("localhost")),
    ("os.type", StringValue("linux")),
    ("process.pid", IntValue(1234))
  ] }
  
  // 验证资源属性
  assert_eq(resource.attributes.length(), 5)
  assert_eq(resource.attributes[0], ("service.name", StringValue("azimuth-service")))
  assert_eq(resource.attributes[1], ("service.instance.id", StringValue("instance-123")))
  assert_eq(resource.attributes[2], ("host.name", StringValue("localhost")))
  assert_eq(resource.attributes[3], ("os.type", StringValue("linux")))
  assert_eq(resource.attributes[4], ("process.pid", IntValue(1234)))
}

// 测试4: InstrumentationScope结构操作
test "InstrumentationScope结构操作测试" {
  // 创建完整的作用域
  let full_scope = InstrumentationScope {
    name = "azimuth.instrument",
    version = Some("1.0.0"),
    schema_url = Some("https://example.com/schema")
  }
  
  assert_eq(full_scope.name, "azimuth.instrument")
  match full_scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_true(false)
  }
  match full_scope.schema_url {
    Some(url) => assert_eq(url, "https://example.com/schema")
    None => assert_true(false)
  }
  
  // 创建最小作用域
  let minimal_scope = InstrumentationScope {
    name = "minimal.instrument",
    version = None,
    schema_url = None
  }
  
  assert_eq(minimal_scope.name, "minimal.instrument")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
}

// 测试5: Context和ContextKey操作
test "Context和ContextKey操作测试" {
  // 创建上下文键
  let trace_key = ContextKey { key = "trace-id" }
  let user_key = ContextKey { key = "user-id" }
  
  assert_eq(trace_key.key, "trace-id")
  assert_eq(user_key.key, "user-id")
  
  // 创建带数据的上下文
  let context_with_data = Context { 
    data = Some(("trace-123", "user-456")) 
  }
  
  match context_with_data.data {
    Some((trace, user)) => {
      assert_eq(trace, "trace-123")
      assert_eq(user, "user-456")
    }
    None => assert_true(false)
  }
  
  // 创建空上下文
  let empty_context = Context { data = None }
  assert_eq(empty_context.data, None)
}

// 测试6: Baggage结构操作
test "Baggage结构操作测试" {
  // 创建行李项集合
  let baggage = Baggage { 
    entries = [
      ("user-id", "12345"),
      ("request-id", "req-67890"),
      ("session-id", "sess-abc"),
      ("region", "us-west-2")
    ] 
  }
  
  // 验证行李项数量
  assert_eq(baggage.entries.length(), 4)
  
  // 验证特定行李项
  assert_eq(baggage.entries[0], ("user-id", "12345"))
  assert_eq(baggage.entries[1], ("request-id", "req-67890"))
  assert_eq(baggage.entries[2], ("session-id", "sess-abc"))
  assert_eq(baggage.entries[3], ("region", "us-west-2"))
  
  // 创建空行李
  let empty_baggage = Baggage { entries = [] }
  assert_eq(empty_baggage.entries.length(), 0)
}

// 测试7: SpanContext结构操作
test "SpanContext结构操作测试" {
  // 创建Span上下文
  let span_context = SpanContext {
    trace_id = "trace-1234567890abcdef",
    span_id = "span-1234567890abcdef",
    sampled = true,
    trace_state = "rojo=00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
  }
  
  // 验证Span上下文属性
  assert_eq(span_context.trace_id, "trace-1234567890abcdef")
  assert_eq(span_context.span_id, "span-1234567890abcdef")
  assert_eq(span_context.sampled, true)
  assert_eq(span_context.trace_state, "rojo=00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
  
  // 创建未采样的Span上下文
  let unsampled_context = SpanContext {
    trace_id = "trace-fedcba0987654321",
    span_id = "span-fedcba0987654321",
    sampled = false,
    trace_state = ""
  }
  
  assert_eq(unsampled_context.sampled, false)
  assert_eq(unsampled_context.trace_state, "")
}

// 测试8: SpanKind枚举操作
test "SpanKind枚举操作测试" {
  // 测试所有Span种类
  let internal_span = Internal
  let server_span = Server
  let client_span = Client
  let producer_span = Producer
  let consumer_span = Consumer
  
  // 验证Span种类
  match internal_span {
    Internal => assert_true(true)
    _ => assert_true(false)
  }
  
  match server_span {
    Server => assert_true(true)
    _ => assert_true(false)
  }
  
  match client_span {
    Client => assert_true(true)
    _ => assert_true(false)
  }
  
  match producer_span {
    Producer => assert_true(true)
    _ => assert_true(false)
  }
  
  match consumer_span {
    Consumer => assert_true(true)
    _ => assert_true(false)
  }
}

// 测试9: StatusCode枚举操作
test "StatusCode枚举操作测试" {
  // 测试所有状态码
  let unset_status = Unset
  let ok_status = Ok
  let error_status = Error
  
  // 验证状态码
  match unset_status {
    Unset => assert_true(true)
    _ => assert_true(false)
  }
  
  match ok_status {
    Ok => assert_true(true)
    _ => assert_true(false)
  }
  
  match error_status {
    Error => assert_true(true)
    _ => assert_true(false)
  }
}

// 测试10: Span结构操作
test "Span结构操作测试" {
  // 创建Span上下文
  let span_context = SpanContext {
    trace_id = "trace-1234567890abcdef",
    span_id = "span-1234567890abcdef",
    sampled = true,
    trace_state = ""
  }
  
  // 创建作用域
  let scope = InstrumentationScope {
    name = "test.instrument",
    version = Some("1.0.0"),
    schema_url = None
  }
  
  // 创建Span
  let span = Span {
    name = "test-operation",
    kind = Server,
    recording: true,
    span_context: span_context
  }
  
  // 验证Span属性
  assert_eq(span.name, "test-operation")
  
  match span.kind {
    Server => assert_true(true)
    _ => assert_true(false)
  }
  
  assert_eq(span.recording, true)
  assert_eq(span.span_context.trace_id, "trace-1234567890abcdef")
  assert_eq(span.span_context.span_id, "span-1234567890abcdef")
  assert_eq(span.span_context.sampled, true)
}

// 测试11: Instrument枚举操作
test "Instrument枚举操作测试" {
  // 测试所有乐器类型
  let counter_instrument = Counter("request.count", Some("Total number of requests"), Some("requests"))
  let histogram_instrument = Histogram("request.duration", Some("Request duration histogram"), Some("ms"))
  let updown_counter_instrument = UpDownCounter("active.connections", Some("Current active connections"), Some("connections"))
  let gauge_instrument = Gauge("memory.usage", Some("Current memory usage"), Some("bytes"))
  
  // 验证计数器乐器
  match counter_instrument {
    Counter(name, description, unit) => {
      assert_eq(name, "request.count")
      match description {
        Some(desc) => assert_eq(desc, "Total number of requests")
        None => assert_true(false)
      }
      match unit {
        Some(u) => assert_eq(u, "requests")
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 验证直方图乐器
  match histogram_instrument {
    Histogram(name, description, unit) => {
      assert_eq(name, "request.duration")
      match description {
        Some(desc) => assert_eq(desc, "Request duration histogram")
        None => assert_true(false)
      }
      match unit {
        Some(u) => assert_eq(u, "ms")
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 验证上下计数器乐器
  match updown_counter_instrument {
    UpDownCounter(name, description, unit) => {
      assert_eq(name, "active.connections")
      match description {
        Some(desc) => assert_eq(desc, "Current active connections")
        None => assert_true(false)
      }
      match unit {
        Some(u) => assert_eq(u, "connections")
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 验证仪表乐器
  match gauge_instrument {
    Gauge(name, description, unit) => {
      assert_eq(name, "memory.usage")
      match description {
        Some(desc) => assert_eq(desc, "Current memory usage")
        None => assert_true(false)
      }
      match unit {
        Some(u) => assert_eq(u, "bytes")
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// 测试12: TextMapCarrier结构操作
test "TextMapCarrier结构操作测试" {
  // 创建文本映射载体
  let carrier = TextMapCarrier {
    headers = [
      ("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
      ("tracestate", "rojo=00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
      ("baggage", "userId=user123,sessionId=session456"),
      ("x-request-id", "req-789")
    ]
  }
  
  // 验证头部数量
  assert_eq(carrier.headers.length(), 4)
  
  // 验证特定头部
  assert_eq(carrier.headers[0], ("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))
  assert_eq(carrier.headers[1], ("tracestate", "rojo=00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))
  assert_eq(carrier.headers[2], ("baggage", "userId=user123,sessionId=session456"))
  assert_eq(carrier.headers[3], ("x-request-id", "req-789"))
  
  // 创建空载体
  let empty_carrier = TextMapCarrier { headers = [] }
  assert_eq(empty_carrier.headers.length(), 0)
}
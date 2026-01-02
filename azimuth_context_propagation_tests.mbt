// Azimuth Context Propagation Tests
// 测试上下文传播功能

test "context creation and management" {
  // 创建基础上下文
  let base_context = @azimuth.Context { data : Some(("key1", "value1")) }
  
  // 验证上下文数据
  match base_context.data {
    Some((key, value)) => {
      assert_eq(key, "key1")
      assert_eq(value, "value1")
    }
    None => assert_true(false)
  }
  
  // 创建空上下文
  let empty_context = @azimuth.Context { data : None }
  match empty_context.data {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

test "context key type safety" {
  // 创建不同类型的上下文键
  let string_key = @azimuth.ContextKey[String] { key : "user.id" }
  let int_key = @azimuth.ContextKey[Int] { key : "retry.count" }
  let bool_key = @azimuth.ContextKey[Bool] { key : "debug.enabled" }
  
  // 验证键名
  assert_eq(string_key.key, "user.id")
  assert_eq(int_key.key, "retry.count")
  assert_eq(bool_key.key, "debug.enabled")
  
  // 验证类型安全（通过编译时检查）
  // 这些不同的键类型确保了类型安全
  assert_eq(string_key.key.length(), 7)
  assert_eq(int_key.key.length(), 11)
  assert_eq(bool_key.key.length(), 13)
}

test "span context propagation" {
  // 创建父span上下文
  let parent_span_ctx = @azimuth.SpanContext {
    trace_id : "abcdef1234567890abcdef1234567890",
    span_id : "1234567890abcdef",
    sampled : true,
    trace_state : "key1=value1,key2=value2"
  }
  
  // 验证trace_id格式
  assert_eq(parent_span_ctx.trace_id.length(), 32)
  assert_eq(parent_span_ctx.trace_id, "abcdef1234567890abcdef1234567890")
  
  // 验证span_id格式
  assert_eq(parent_span_ctx.span_id.length(), 16)
  assert_eq(parent_span_ctx.span_id, "1234567890abcdef")
  
  // 创建子span上下文（继承trace_id，但有不同的span_id）
  let child_span_ctx = @azimuth.SpanContext {
    trace_id : parent_span_ctx.trace_id,
    span_id : "fedcba0987654321",
    sampled : parent_span_ctx.sampled,
    trace_state : parent_span_ctx.trace_state
  }
  
  // 验证继承关系
  assert_eq(child_span_ctx.trace_id, parent_span_ctx.trace_id)
  assert_not_eq(child_span_ctx.span_id, parent_span_ctx.span_id)
  assert_eq(child_span_ctx.sampled, parent_span_ctx.sampled)
  assert_eq(child_span_ctx.trace_state, parent_span_ctx.trace_state)
}

test "baggage propagation across service boundaries" {
  // 创建初始baggage
  let initial_baggage = @azimuth.Baggage {
    entries : [
      ("user.id", "12345"),
      ("request.id", "req-abc123"),
      ("region", "us-west-2"),
      ("debug.enabled", "true")
    ]
  }
  
  // 验证初始baggage条目
  assert_eq(initial_baggage.entries.length(), 4)
  
  // 模拟服务间传播 - 添加新的baggage条目
  let propagated_baggage = @azimuth.Baggage {
    entries : initial_baggage.entries + [
      ("service.name", "payment-service"),
      ("operation.type", "process-payment"),
      ("latency.threshold", "100")
    ]
  }
  
  // 验证传播后的baggage
  assert_eq(propagated_baggage.entries.length(), 7)
  
  // 验证原有条目保持不变
  assert_eq(propagated_baggage.entries[0], ("user.id", "12345"))
  assert_eq(propagated_baggage.entries[1], ("request.id", "req-abc123"))
  assert_eq(propagated_baggage.entries[2], ("region", "us-west-2"))
  assert_eq(propagated_baggage.entries[3], ("debug.enabled", "true"))
  
  // 验证新增条目
  assert_eq(propagated_baggage.entries[4], ("service.name", "payment-service"))
  assert_eq(propagated_baggage.entries[5], ("operation.type", "process-payment"))
  assert_eq(propagated_baggage.entries[6], ("latency.threshold", "100"))
}

test "text map carrier propagation" {
  // 创建包含追踪信息的carrier
  let carrier = @azimuth.TextMapCarrier {
    headers : [
      ("traceparent", "00-abcdef1234567890abcdef1234567890-1234567890abcdef-01"),
      ("tracestate", "key1=value1,key2=value2"),
      ("baggage", "user.id=12345,region=us-west-2"),
      ("x-request-id", "req-xyz789"),
      ("x-correlation-id", "corr-abc456")
    ]
  }
  
  // 验证carrier中的headers数量
  assert_eq(carrier.headers.length(), 5)
  
  // 验证特定的追踪header
  let traceparent = carrier.headers[0]
  assert_eq(traceparent.0, "traceparent")
  assert_eq(traceparent.1, "00-abcdef1234567890abcdef1234567890-1234567890abcdef-01")
  
  // 验证tracestate
  let tracestate = carrier.headers[1]
  assert_eq(tracestate.0, "tracestate")
  assert_eq(tracestate.1, "key1=value1,key2=value2")
  
  // 验证baggage
  let baggage = carrier.headers[2]
  assert_eq(baggage.0, "baggage")
  assert_eq(baggage.1, "user.id=12345,region=us-west-2")
  
  // 模拟跨服务传播 - 添加新的headers
  let propagated_carrier = @azimuth.TextMapCarrier {
    headers : carrier.headers + [
      ("x-service-name", "order-service"),
      ("x-service-version", "1.2.3"),
      ("x-start-time", "2023-01-01T00:00:00Z")
    ]
  }
  
  // 验证传播后的headers
  assert_eq(propagated_carrier.headers.length(), 8)
  
  // 验证原有headers保持不变
  assert_eq(propagated_carrier.headers[0], carrier.headers[0])
  assert_eq(propagated_carrier.headers[1], carrier.headers[1])
  assert_eq(propagated_carrier.headers[2], carrier.headers[2])
  
  // 验证新增headers
  assert_eq(propagated_carrier.headers[5], ("x-service-name", "order-service"))
  assert_eq(propagated_carrier.headers[6], ("x-service-version", "1.2.3"))
  assert_eq(propagated_carrier.headers[7], ("x-start-time", "2023-01-01T00:00:00Z"))
}

test "context propagation chain" {
  // 创建上下文传播链
  let mut contexts = []
  
  // 初始上下文
  let ctx1 = @azimuth.Context { data : Some(("service.entry", "api-gateway")) }
  contexts = contexts + [ctx1]
  
  // 第二个上下文（添加更多信息）
  let ctx2 = @azimuth.Context { data : Some(("auth.user", "user123")) }
  contexts = contexts + [ctx2]
  
  // 第三个上下文
  let ctx3 = @azimuth.Context { data : Some(("operation.type", "create-order")) }
  contexts = contexts + [ctx3]
  
  // 验证上下文链
  assert_eq(contexts.length(), 3)
  
  // 验证每个上下文的数据
  match contexts[0].data {
    Some((key, value)) => {
      assert_eq(key, "service.entry")
      assert_eq(value, "api-gateway")
    }
    None => assert_true(false)
  }
  
  match contexts[1].data {
    Some((key, value)) => {
      assert_eq(key, "auth.user")
      assert_eq(value, "user123")
    }
    None => assert_true(false)
  }
  
  match contexts[2].data {
    Some((key, value)) => {
      assert_eq(key, "operation.type")
      assert_eq(value, "create-order")
    }
    None => assert_true(false)
  }
}

test "cross-service context consistency" {
  // 创建服务A的上下文
  let service_a_context = @azimuth.SpanContext {
    trace_id : "1234567890abcdef1234567890abcdef",
    span_id : "1111111111111111",
    sampled : true,
    trace_state : "service.a=active,operation=query"
  }
  
  // 服务A创建的baggage
  let service_a_baggage = @azimuth.Baggage {
    entries : [
      ("user.id", "user123"),
      ("session.id", "sess456"),
      ("request.id", "req789")
    ]
  }
  
  // 服务B继承上下文（相同的trace_id，不同的span_id）
  let service_b_context = @azimuth.SpanContext {
    trace_id : service_a_context.trace_id,
    span_id : "2222222222222222",
    sampled : service_a_context.sampled,
    trace_state : service_a_context.trace_state + ",service.b=processing"
  }
  
  // 服务B添加的baggage
  let service_b_baggage = @azimuth.Baggage {
    entries : service_a_baggage.entries + [
      ("service.b.name", "order-service"),
      ("service.b.operation", "validate-order")
    ]
  }
  
  // 验证trace_id一致性
  assert_eq(service_a_context.trace_id, service_b_context.trace_id)
  
  // 验证span_id不同
  assert_not_eq(service_a_context.span_id, service_b_context.span_id)
  
  // 验证采样标志一致
  assert_eq(service_a_context.sampled, service_b_context.sampled)
  
  // 验证trace_state扩展
  assert_eq(service_b_context.trace_state, "service.a=active,operation=query,service.b=processing")
  
  // 验证baggage传播和扩展
  assert_eq(service_b_baggage.entries.length(), 5)
  assert_eq(service_b_baggage.entries[0], ("user.id", "user123"))
  assert_eq(service_b_baggage.entries[1], ("session.id", "sess456"))
  assert_eq(service_b_baggage.entries[2], ("request.id", "req789"))
  assert_eq(service_b_baggage.entries[3], ("service.b.name", "order-service"))
  assert_eq(service_b_baggage.entries[4], ("service.b.operation", "validate-order"))
}

test "context propagation edge cases" {
  // 测试空上下文传播
  let empty_context = @azimuth.Context { data : None }
  match empty_context.data {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试空baggage传播
  let empty_baggage = @azimuth.Baggage { entries : [] }
  assert_eq(empty_baggage.entries.length(), 0)
  
  // 测试空carrier传播
  let empty_carrier = @azimuth.TextMapCarrier { headers : [] }
  assert_eq(empty_carrier.headers.length(), 0)
  
  // 测试包含特殊字符的上下文值
  let special_context = @azimuth.Context { 
    data : Some(("special.key", "value with spaces & symbols!@#$%")) 
  }
  match special_context.data {
    Some((key, value)) => {
      assert_eq(key, "special.key")
      assert_eq(value, "value with spaces & symbols!@#$%")
    }
    None => assert_true(false)
  }
  
  // 测试包含Unicode的baggage
  let unicode_baggage = @azimuth.Baggage {
    entries : [
      ("用户名", "张三"),
      ("地区", "北京"),
      ("操作", "创建订单")
    ]
  }
  assert_eq(unicode_baggage.entries.length(), 3)
  assert_eq(unicode_baggage.entries[0], ("用户名", "张三"))
  assert_eq(unicode_baggage.entries[1], ("地区", "北京"))
  assert_eq(unicode_baggage.entries[2], ("操作", "创建订单"))
}
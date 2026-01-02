// Azimuth New Tests - 新增测试用例
// 包含属性操作、上下文传播、Span追踪、Baggage传播和序列化的测试用例

// 测试1: 属性操作测试
test "属性基本操作测试" {
  let attrs = Attributes::new()
  
  // 测试设置和获取字符串属性
  Attributes::set(attrs, "service.name", StringValue("azimuth-test"))
  match Attributes::get(attrs, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "azimuth-test")
    _ => assert_true(false)
  }
  
  // 测试设置和获取数值属性
  Attributes::set(attrs, "service.instance.id", IntValue(12345))
  match Attributes::get(attrs, "service.instance.id") {
    Some(IntValue(id)) => assert_eq(id, 12345)
    _ => assert_true(false)
  }
  
  // 测试设置和获取布尔属性
  Attributes::set(attrs, "service.enabled", BoolValue(true))
  match Attributes::get(attrs, "service.enabled") {
    Some(BoolValue(enabled)) => assert_eq(enabled, true)
    _ => assert_true(false)
  }
  
  // 测试设置和获取浮点数属性
  Attributes::set(attrs, "service.version", FloatValue(1.0))
  match Attributes::get(attrs, "service.version") {
    Some(FloatValue(version)) => assert_eq(version, 1.0)
    _ => assert_true(false)
  }
}

// 测试2: 属性数组操作测试
test "属性数组操作测试" {
  let attrs = Attributes::new()
  
  // 测试字符串数组
  let string_array = Array[String]::["tag1", "tag2", "tag3"]
  Attributes::set(attrs, "service.tags", ArrayStringValue(string_array))
  match Attributes::get(attrs, "service.tags") {
    Some(ArrayStringValue(tags)) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags[0], "tag1")
      assert_eq(tags[1], "tag2")
      assert_eq(tags[2], "tag3")
    }
    _ => assert_true(false)
  }
  
  // 测试整数数组
  let int_array = Array[Int]::[100, 200, 300]
  Attributes::set(attrs, "service.ports", ArrayIntValue(int_array))
  match Attributes::get(attrs, "service.ports") {
    Some(ArrayIntValue(ports)) => {
      assert_eq(ports.length(), 3)
      assert_eq(ports[0], 100)
      assert_eq(ports[1], 200)
      assert_eq(ports[2], 300)
    }
    _ => assert_true(false)
  }
}

// 测试3: 上下文操作测试
test "上下文操作测试" {
  let root_ctx = Context::root()
  
  // 创建上下文键
  let user_id_key = ContextKey::new("user.id")
  let request_id_key = ContextKey::new("request.id")
  
  // 设置上下文值
  let ctx_with_user = Context::with_value(root_ctx, user_id_key, "user123")
  let ctx_with_request = Context::with_value(ctx_with_user, request_id_key, "req456")
  
  // 验证上下文值
  assert_eq(Context::get(ctx_with_user, user_id_key), Some("user123"))
  assert_eq(Context::get(ctx_with_request, user_id_key), Some("user123"))
  assert_eq(Context::get(ctx_with_request, request_id_key), Some("req456"))
  assert_eq(Context::get(root_ctx, user_id_key), None)
}

// 测试4: Span上下文测试
test "Span上下文测试" {
  // 创建有效的Span上下文
  let valid_ctx = SpanContext::new("trace123456789", "span987654321", true, "state=test")
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_true(SpanContext::is_sampled(valid_ctx))
  
  // 创建无效的Span上下文
  let invalid_ctx_trace = SpanContext::new("", "span987654321", true, "state=test")
  assert_false(SpanContext::is_valid(invalid_ctx_trace))
  
  let invalid_ctx_span = SpanContext::new("trace123456789", "", true, "state=test")
  assert_false(SpanContext::is_valid(invalid_ctx_span))
  
  // 测试未采样的Span上下文
  let unsampled_ctx = SpanContext::new("trace123456789", "span987654321", false, "state=test")
  assert_true(SpanContext::is_valid(unsampled_ctx))
  assert_false(SpanContext::is_sampled(unsampled_ctx))
}

// 测试5: Baggage操作测试
test "Baggage操作测试" {
  let baggage = Baggage::new()
  
  // 添加baggage项
  Baggage::set(baggage, "user.id", "user123")
  Baggage::set(baggage, "request.id", "req456")
  Baggage::set(baggage, "session.id", "sess789")
  
  // 验证baggage项
  assert_eq(Baggage::get(baggage, "user.id"), Some("user123"))
  assert_eq(Baggage::get(baggage, "request.id"), Some("req456"))
  assert_eq(Baggage::get(baggage, "session.id"), Some("sess789"))
  
  // 验证不存在的项
  assert_eq(Baggage::get(baggage, "nonexistent"), None)
  
  // 删除baggage项
  Baggage::remove(baggage, "user.id")
  assert_eq(Baggage::get(baggage, "user.id"), None)
  assert_eq(Baggage::get(baggage, "request.id"), Some("req456"))
}

// 测试6: 资源操作测试
test "资源操作测试" {
  let resource = Resource::new()
  
  // 设置资源属性
  Resource::set_attribute(resource, "service.name", StringValue("azimuth-service"))
  Resource::set_attribute(resource, "service.version", StringValue("1.0.0"))
  Resource::set_attribute(resource, "service.namespace", StringValue("production"))
  Resource::set_attribute(resource, "host.name", StringValue("server-01"))
  
  // 验证资源属性
  match Resource::get_attribute(resource, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource, "service.version") {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource, "service.namespace") {
    Some(StringValue(namespace)) => assert_eq(namespace, "production")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource, "host.name") {
    Some(StringValue(hostname)) => assert_eq(hostname, "server-01")
    _ => assert_true(false)
  }
}

// 测试7: TextMapCarrier操作测试
test "TextMapCarrier操作测试" {
  let carrier = TextMapCarrier::new()
  
  // 设置头部
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  TextMapCarrier::set(carrier, "baggage", "userId=user123,sessionId=sess456")
  
  // 验证头部
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(TextMapCarrier::get(carrier, "tracestate"), Some("rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"))
  assert_eq(TextMapCarrier::get(carrier, "baggage"), Some("userId=user123,sessionId=sess456"))
  
  // 验证不存在的头部
  assert_eq(TextMapCarrier::get(carrier, "nonexistent"), None)
  
  // 删除头部
  TextMapCarrier::remove(carrier, "tracestate")
  assert_eq(TextMapCarrier::get(carrier, "tracestate"), None)
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
}

// 测试8: 仪器化范围测试
test "仪器化范围测试" {
  // 创建仪器化范围
  let scope = InstrumentationScope::new("azimuth.instrument", Some("1.0.0"), Some("https://example.com/schema"))
  
  // 验证仪器化范围属性
  assert_eq(InstrumentationScope::get_name(scope), "azimuth.instrument")
  assert_eq(InstrumentationScope::get_version(scope), Some("1.0.0"))
  assert_eq(InstrumentationScope::get_schema_url(scope), Some("https://example.com/schema"))
  
  // 创建最小仪器化范围
  let minimal_scope = InstrumentationScope::new("minimal.instrument", None, None)
  assert_eq(InstrumentationScope::get_name(minimal_scope), "minimal.instrument")
  assert_eq(InstrumentationScope::get_version(minimal_scope), None)
  assert_eq(InstrumentationScope::get_schema_url(minimal_scope), None)
}

// 测试9: 属性值类型转换测试
test "属性值类型转换测试" {
  let attrs = Attributes::new()
  
  // 测试字符串到其他类型的转换
  Attributes::set(attrs, "string.to.int", StringValue("123"))
  Attributes::set(attrs, "string.to.float", StringValue("123.45"))
  Attributes::set(attrs, "string.to.bool", StringValue("true"))
  
  // 测试数值类型的转换
  Attributes::set(attrs, "int.to.string", IntValue(456))
  Attributes::set(attrs, "float.to.string", FloatValue(789.01))
  Attributes::set(attrs, "bool.to.string", BoolValue(false))
  
  // 验证原始值
  match Attributes::get(attrs, "string.to.int") {
    Some(StringValue(value)) => assert_eq(value, "123")
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "string.to.float") {
    Some(StringValue(value)) => assert_eq(value, "123.45")
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "string.to.bool") {
    Some(StringValue(value)) => assert_eq(value, "true")
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "int.to.string") {
    Some(IntValue(value)) => assert_eq(value, 456)
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "float.to.string") {
    Some(FloatValue(value)) => assert_eq(value, 789.01)
    _ => assert_true(false)
  }
  
  match Attributes::get(attrs, "bool.to.string") {
    Some(BoolValue(value)) => assert_eq(value, false)
    _ => assert_true(false)
  }
}

// 测试10: 复合场景测试
test "复合场景测试" {
  // 创建资源
  let resource = Resource::new()
  Resource::set_attribute(resource, "service.name", StringValue("azimuth-composite"))
  Resource::set_attribute(resource, "service.version", StringValue("2.0.0"))
  
  // 创建Span上下文
  let span_ctx = SpanContext::new("trace123", "span456", true, "state=production")
  
  // 创建上下文
  let root_ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  let ctx_with_user = Context::with_value(root_ctx, user_key, "user789")
  
  // 创建Baggage
  let baggage = Baggage::new()
  Baggage::set(baggage, "request.id", "req123")
  Baggage::set(baggage, "session.id", "sess456")
  
  // 创建TextMapCarrier
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-trace123-span456-01")
  TextMapCarrier::set(carrier, "baggage", "request.id=req123,session.id=sess456")
  
  // 验证所有组件
  assert_true(SpanContext::is_valid(span_ctx))
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_eq(Context::get(ctx_with_user, user_key), Some("user789"))
  assert_eq(Baggage::get(baggage, "request.id"), Some("req123"))
  assert_eq(Baggage::get(baggage, "session.id"), Some("sess456"))
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-trace123-span456-01"))
  
  match Resource::get_attribute(resource, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "azimuth-composite")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource, "service.version") {
    Some(StringValue(version)) => assert_eq(version, "2.0.0")
    _ => assert_true(false)
  }
}
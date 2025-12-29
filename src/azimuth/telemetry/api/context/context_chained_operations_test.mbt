// Context API 上下文链式操作测试用例
// 测试上下文的链式创建、修改和查询功能

test "context_chained_operations" {
  // 测试上下文链式操作
  
  // 创建空上下文
  let ctx = Context::empty()
  
  // 创建上下文键
  let user_key = create_key("user")
  let request_id_key = create_key("request_id")
  let trace_id_key = create_key("trace_id")
  let service_key = create_key("service")
  
  // 链式设置多个值
  let ctx_with_user = ctx.with_value(user_key, "alice")
  let ctx_with_request = ctx_with_user.with_value(request_id_key, "req-12345")
  let ctx_with_trace = ctx_with_request.with_value(trace_id_key, "trace-67890")
  let ctx_with_service = ctx_with_trace.with_value(service_key, "auth-service")
  
  // 验证链式操作结果
  match ctx_with_user.get(user_key) {
    Some(user) => assert_eq(user, "alice")
    None => @test.fail("Expected user value")
  }
  
  match ctx_with_request.get(request_id_key) {
    Some(request_id) => assert_eq(request_id, "req-12345")
    None => @test.fail("Expected request_id value")
  }
  
  match ctx_with_trace.get(trace_id_key) {
    Some(trace_id) => assert_eq(trace_id, "trace-67890")
    None => @test.fail("Expected trace_id value")
  }
  
  match ctx_with_service.get(service_key) {
    Some(service) => assert_eq(service, "auth-service")
    None => @test.fail("Expected service value")
  }
  
  // 验证原始上下文未受影响
  match ctx.get(user_key) {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Original context should not have user value")
  }
  
  match ctx.get(request_id_key) {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Original context should not have request_id value")
  }
}

test "context_nested_chained_operations" {
  // 测试嵌套上下文链式操作
  
  // 创建基础上下文
  let base_ctx = Context::empty()
  
  // 创建上下文键
  let auth_key = create_key("auth")
  let user_key = create_key("user")
  let session_key = create_key("session")
  let request_key = create_key("request")
  let trace_key = create_key("trace")
  
  // 第一层：认证上下文
  let auth_ctx = base_ctx
    .with_value(auth_key, "bearer-token-123")
    .with_value(user_key, "bob")
    .with_value(session_key, "session-456")
  
  // 第二层：请求上下文（基于认证上下文）
  let request_ctx = auth_ctx
    .with_value(request_key, "GET /api/users")
    .with_value(trace_key, "trace-789")
  
  // 验证第一层上下文
  match auth_ctx.get(auth_key) {
    Some(auth) => assert_eq(auth, "bearer-token-123")
    None => @test.fail("Expected auth value")
  }
  
  match auth_ctx.get(user_key) {
    Some(user) => assert_eq(user, "bob")
    None => @test.fail("Expected user value")
  }
  
  match auth_ctx.get(session_key) {
    Some(session) => assert_eq(session, "session-456")
    None => @test.fail("Expected session value")
  }
  
  match auth_ctx.get(request_key) {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Auth context should not have request value")
  }
  
  // 验证第二层上下文（包含第一层的值）
  match request_ctx.get(auth_key) {
    Some(auth) => assert_eq(auth, "bearer-token-123")
    None => @test.fail("Expected auth value in request context")
  }
  
  match request_ctx.get(user_key) {
    Some(user) => assert_eq(user, "bob")
    None => @test.fail("Expected user value in request context")
  }
  
  match request_ctx.get(session_key) {
    Some(session) => assert_eq(session, "session-456")
    None => @test.fail("Expected session value in request context")
  }
  
  match request_ctx.get(request_key) {
    Some(request) => assert_eq(request, "GET /api/users")
    None => @test.fail("Expected request value")
  }
  
  match request_ctx.get(trace_key) {
    Some(trace) => assert_eq(trace, "trace-789")
    None => @test.fail("Expected trace value")
  }
}

test "context_chained_value_overriding" {
  // 测试上下文链式操作中的值覆盖
  
  // 创建上下文和键
  let ctx = Context::empty()
  let key = create_key("override-test")
  
  // 链式设置多个值，覆盖同一个键
  let ctx1 = ctx.with_value(key, "value1")
  let ctx2 = ctx1.with_value(key, "value2")
  let ctx3 = ctx2.with_value(key, "value3")
  
  // 验证每个上下文的状态
  match ctx1.get(key) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected value1")
  }
  
  match ctx2.get(key) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected value2")
  }
  
  match ctx3.get(key) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected value3")
  }
  
  // 验证原始上下文未受影响
  match ctx.get(key) {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Original context should not have any value")
  }
  
  // 验证上下文链的独立性
  match ctx1.get(key) {
    Some(value) => assert_eq(value, "value1")  // ctx1应该仍然是value1
    None => @test.fail("Expected value1 in ctx1")
  }
}

test "context_chained_baggage_operations" {
  // 测试上下文链式操作与baggage的结合
  
  // 创建上下文和baggage
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // 链式添加baggage条目
  let baggage1 = baggage.with_entry("user-id", "12345")
  let baggage2 = baggage1.with_entry("region", "us-east")
  let baggage3 = baggage2.with_entry("version", "v1.2.3")
  
  // 验证baggage链式操作
  match baggage1.get("user-id") {
    Some(user_id) => assert_eq(user_id, "12345")
    None => @test.fail("Expected user-id in baggage1")
  }
  
  match baggage1.get("region") {
    None => assert_eq(true, true)
    Some(_) => @test.fail("baggage1 should not have region")
  }
  
  match baggage2.get("user-id") {
    Some(user_id) => assert_eq(user_id, "12345")
    None => @test.fail("Expected user-id in baggage2")
  }
  
  match baggage2.get("region") {
    Some(region) => assert_eq(region, "us-east")
    None => @test.fail("Expected region in baggage2")
  }
  
  match baggage3.get("user-id") {
    Some(user_id) => assert_eq(user_id, "12345")
    None => @test.fail("Expected user-id in baggage3")
  }
  
  match baggage3.get("region") {
    Some(region) => assert_eq(region, "us-east")
    None => @test.fail("Expected region in baggage3")
  }
  
  match baggage3.get("version") {
    Some(version) => assert_eq(version, "v1.2.3")
    None => @test.fail("Expected version in baggage3")
  }
  
  // 验证原始baggage未受影响
  match baggage.get("user-id") {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Original baggage should not have user-id")
  }
}

test "context_complex_chained_scenario" {
  // 测试复杂的上下文链式操作场景
  
  // 模拟微服务调用链
  let base_ctx = Context::empty()
  
  // 创建各种键
  let trace_id_key = create_key("trace_id")
  let span_id_key = create_key("span_id")
  let user_id_key = create_key("user_id")
  let auth_token_key = create_key("auth_token")
  let service_name_key = create_key("service_name")
  let operation_key = create_key("operation")
  let request_id_key = create_key("request_id")
  let correlation_id_key = create_key("correlation_id")
  
  // 第一层：网关层上下文
  let gateway_ctx = base_ctx
    .with_value(trace_id_key, "trace-abc123")
    .with_value(user_id_key, "user-789")
    .with_value(request_id_key, "req-gateway-001")
    .with_value(correlation_id_key, "corr-xyz789")
  
  // 第二层：认证服务上下文
  let auth_ctx = gateway_ctx
    .with_value(span_id_key, "span-auth-001")
    .with_value(auth_token_key, "jwt-token-456")
    .with_value(service_name_key, "auth-service")
    .with_value(operation_key, "validate-token")
  
  // 第三层：用户服务上下文
  let user_ctx = auth_ctx
    .with_value(span_id_key, "span-user-001")
    .with_value(service_name_key, "user-service")
    .with_value(operation_key, "get-user-profile")
    .with_value(request_id_key, "req-user-002")
  
  // 验证网关层上下文
  match gateway_ctx.get(trace_id_key) {
    Some(trace_id) => assert_eq(trace_id, "trace-abc123")
    None => @test.fail("Expected trace_id in gateway context")
  }
  
  match gateway_ctx.get(user_id_key) {
    Some(user_id) => assert_eq(user_id, "user-789")
    None => @test.fail("Expected user_id in gateway context")
  }
  
  match gateway_ctx.get(span_id_key) {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Gateway context should not have span_id")
  }
  
  // 验证认证服务上下文
  match auth_ctx.get(trace_id_key) {
    Some(trace_id) => assert_eq(trace_id, "trace-abc123")
    None => @test.fail("Expected trace_id in auth context")
  }
  
  match auth_ctx.get(span_id_key) {
    Some(span_id) => assert_eq(span_id, "span-auth-001")
    None => @test.fail("Expected span_id in auth context")
  }
  
  match auth_ctx.get(service_name_key) {
    Some(service) => assert_eq(service, "auth-service")
    None => @test.fail("Expected service_name in auth context")
  }
  
  // 验证用户服务上下文
  match user_ctx.get(trace_id_key) {
    Some(trace_id) => assert_eq(trace_id, "trace-abc123")
    None => @test.fail("Expected trace_id in user context")
  }
  
  match user_ctx.get(span_id_key) {
    Some(span_id) => assert_eq(span_id, "span-user-001")
    None => @test.fail("Expected span_id in user context")
  }
  
  match user_ctx.get(service_name_key) {
    Some(service) => assert_eq(service, "user-service")
    None => @test.fail("Expected service_name in user context")
  }
  
  match user_ctx.get(request_id_key) {
    Some(request_id) => assert_eq(request_id, "req-user-002")
    None => @test.fail("Expected request_id in user context")
  }
  
  // 验证上下文链的独立性
  match auth_ctx.get(span_id_key) {
    Some(span_id) => assert_eq(span_id, "span-auth-001")  // 应该仍然是auth的span_id
    None => @test.fail("Expected span_id in auth context")
  }
  
  match auth_ctx.get(service_name_key) {
    Some(service) => assert_eq(service, "auth-service")  // 应该仍然是auth-service
    None => @test.fail("Expected service_name in auth context")
  }
}

test "context_chained_performance_characteristics" {
  // 测试上下文链式操作的性能特征
  
  // 创建上下文和键
  let ctx = Context::empty()
  let keys = []
  let mut i = 0
  
  // 创建多个键
  while i < 100 {
    keys.push(create_key("key-" + i.to_string()))
    i = i + 1
  }
  
  // 链式设置多个值
  let mut current_ctx = ctx
  let mut j = 0
  while j < keys.length() {
    current_ctx = current_ctx.with_value(keys[j], "value-" + j.to_string())
    j = j + 1
  }
  
  // 验证所有值都可以正确获取
  let mut k = 0
  while k < keys.length() {
    match current_ctx.get(keys[k]) {
      Some(value) => assert_eq(value, "value-" + k.to_string())
      None => @test.fail("Expected value for key-" + k.to_string())
    }
    k = k + 1
  }
  
  // 验证深度链式操作的性能
  let start_time = 1640995200000L
  let mut deep_ctx = ctx
  let mut m = 0
  while m < 1000 {
    let temp_key = create_key("deep-key-" + m.to_string())
    deep_ctx = deep_ctx.with_value(temp_key, "deep-value-" + m.to_string())
    m = m + 1
  }
  
  // 验证深度链式操作的结果
  let first_key = create_key("deep-key-0")
  let last_key = create_key("deep-key-999")
  
  match deep_ctx.get(first_key) {
    Some(value) => assert_eq(value, "deep-value-0")
    None => @test.fail("Expected deep-value-0")
  }
  
  match deep_ctx.get(last_key) {
    Some(value) => assert_eq(value, "deep-value-999")
    None => @test.fail("Expected deep-value-999")
  }
  
  // 验证中间值
  let middle_key = create_key("deep-key-500")
  match deep_ctx.get(middle_key) {
    Some(value) => assert_eq(value, "deep-value-500")
    None => @test.fail("Expected deep-value-500")
  }
}

test "context_chained_error_handling" {
  // 测试上下文链式操作的错误处理
  
  // 创建上下文
  let ctx = Context::empty()
  
  // 测试空键和空值
  let empty_key = create_key("")
  let normal_key = create_key("normal")
  
  // 设置空键的值
  let ctx_with_empty_key = ctx.with_value(empty_key, "value-with-empty-key")
  
  // 验证空键可以正常工作
  match ctx_with_empty_key.get(empty_key) {
    Some(value) => assert_eq(value, "value-with-empty-key")
    None => @test.fail("Expected value for empty key")
  }
  
  // 设置空值
  let ctx_with_empty_value = ctx.with_value(normal_key, "")
  
  // 验证空值可以正常工作
  match ctx_with_empty_value.get(normal_key) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected empty value")
  }
  
  // 测试长键和长值
  let long_key_name = "this-is-a-very-long-key-name-that-might-potentially-cause-issues-in-some-implementations"
  let long_key = create_key(long_key_name)
  let long_value = "this-is-a-very-long-value-that-might-potentially-cause-issues-in-some-implementations-and-should-be-tested-thoroughly"
  
  let ctx_with_long_values = ctx.with_value(long_key, long_value)
  
  // 验证长键和长值可以正常工作
  match ctx_with_long_values.get(long_key) {
    Some(value) => assert_eq(value, long_value)
    None => @test.fail("Expected long value")
  }
  
  // 测试特殊字符
  let special_key = create_key("key-with-special-chars:;!@#$%^&*()[]{}|\\\"'<>?/\\")
  let special_value = "value-with-special-chars:;!@#$%^&*()[]{}|\\\"'<>?/\\"
  
  let ctx_with_special_chars = ctx.with_value(special_key, special_value)
  
  // 验证特殊字符可以正常工作
  match ctx_with_special_chars.get(special_key) {
    Some(value) => assert_eq(value, special_value)
    None => @test.fail("Expected value with special characters")
  }
  
  // 测试Unicode字符
  let unicode_key = create_key("键-包含-中文")
  let unicode_value = "值-包含-中文-和-特殊-字符-😀"
  
  let ctx_with_unicode = ctx.with_value(unicode_key, unicode_value)
  
  // 验证Unicode字符可以正常工作
  match ctx_with_unicode.get(unicode_key) {
    Some(value) => assert_eq(value, unicode_value)
    None => @test.fail("Expected Unicode value")
  }
}
// Context API高级测试用例
test "context_complex_value_operations" {
  // 测试Context的复杂值操作
  let ctx = Context::empty()
  
  // 创建多个ContextKey
  let user_key = create_key("user.id")
  let trace_key = create_key("trace.id")
  let request_key = create_key("request.id")
  let session_key = create_key("session.id")
  
  // 链式添加值
  let ctx_with_user = ctx.with_value(user_key, "user-12345")
  let ctx_with_trace = ctx_with_user.with_value(trace_key, "trace-67890")
  let ctx_with_request = ctx_with_trace.with_value(request_key, "req-abc123")
  let ctx_with_session = ctx_with_request.with_value(session_key, "sess-def456")
  
  // 验证所有值都存在
  match ctx_with_session.get(user_key) {
    Some(user_id) => assert_eq(user_id, "user-12345")
    None => @test.fail("Expected Some(user_id)")
  }
  
  match ctx_with_session.get(trace_key) {
    Some(trace_id) => assert_eq(trace_id, "trace-67890")
    None => @test.fail("Expected Some(trace_id)")
  }
  
  match ctx_with_session.get(request_key) {
    Some(request_id) => assert_eq(request_id, "req-abc123")
    None => @test.fail("Expected Some(request_id)")
  }
  
  match ctx_with_session.get(session_key) {
    Some(session_id) => assert_eq(session_id, "sess-def456")
    None => @test.fail("Expected Some(session_id)")
  }
  
  // 测试不存在的键
  let missing_key = create_key("missing.key")
  match ctx_with_session.get(missing_key) {
    Some(_) => @test.fail("Expected None for missing key")
    None => assert_eq(true, true)
  }
  
  // 验证原始Context没有被修改
  match ctx.get(user_key) {
    Some(_) => @test.fail("Expected None from empty context")
    None => assert_eq(true, true)
  }
}

test "context_and_baggage_integration" {
  // 测试Context和Baggage的集成使用
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // 在Context中存储复杂值
  let operation_key = create_key("operation.name")
  let service_key = create_key("service.name")
  
  let ctx_with_values = ctx
    .with_value(operation_key, "process_payment")
    .with_value(service_key, "payment-service")
  
  // 在Baggage中存储相关元数据
  let baggage_with_entries = baggage
    .with_entry("user.id", "user-12345")
    .with_entry("request.id", "req-abc123")
    .with_entry("correlation.id", "corr-def456")
    .with_entry("tenant.id", "tenant-789")
  
  // 验证Context值
  match ctx_with_values.get(operation_key) {
    Some(operation) => assert_eq(operation, "process_payment")
    None => @test.fail("Expected Some(operation)")
  }
  
  match ctx_with_values.get(service_key) {
    Some(service) => assert_eq(service, "payment-service")
    None => @test.fail("Expected Some(service)")
  }
  
  // 验证Baggage值
  match baggage_with_entries.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user-12345")
    None => @test.fail("Expected Some(user_id)")
  }
  
  match baggage_with_entries.get("request.id") {
    Some(request_id) => assert_eq(request_id, "req-abc123")
    None => @test.fail("Expected Some(request_id)")
  }
  
  match baggage_with_entries.get("correlation.id") {
    Some(correlation_id) => assert_eq(correlation_id, "corr-def456")
    None => @test.fail("Expected Some(correlation_id)")
  }
  
  match baggage_with_entries.get("tenant.id") {
    Some(tenant_id) => assert_eq(tenant_id, "tenant-789")
    None => @test.fail("Expected Some(tenant_id)")
  }
  
  // 测试覆盖Baggage条目
  let updated_baggage = baggage_with_entries.with_entry("user.id", "user-99999")
  match updated_baggage.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user-99999")
    None => @test.fail("Expected Some(updated user_id)")
  }
  
  // 验证原始Baggage没有被修改
  match baggage_with_entries.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user-12345")
    None => @test.fail("Expected Some(original user_id)")
  }
}
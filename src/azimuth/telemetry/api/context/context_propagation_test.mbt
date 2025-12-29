// 上下文传播测试用例
// 测试Context和Baggage的传播和管理功能

test "context_basic_operations" {
  // 测试Context的基本操作
  
  let empty_ctx = Context::empty()
  let trace_key = create_key("trace_id")
  let span_key = create_key("span_id")
  
  // 验证空上下文
  assert_eq(empty_ctx.get(trace_key), None)
  assert_eq(empty_ctx.get(span_key), None)
  
  // 添加值到上下文
  let ctx_with_trace = empty_ctx.with_value(trace_key, "0af7651916cd43dd8448eb211c80319c")
  let ctx_with_span = ctx_with_trace.with_value(span_key, "b7ad6b7169203331")
  
  // 验证上下文中的值
  match ctx_with_trace.get(trace_key) {
    Some(value) => {
      assert_eq(value, "0af7651916cd43dd8448eb211c80319c")
      assert_eq(value.length(), 32)
    }
    None => assert_eq(false, true)
  }
  
  match ctx_with_span.get(trace_key) {
    Some(value) => assert_eq(value, "0af7651916cd43dd8448eb211c80319c")
    None => assert_eq(false, true)
  }
  
  match ctx_with_span.get(span_key) {
    Some(value) => {
      assert_eq(value, "b7ad6b7169203331")
      assert_eq(value.length(), 16)
    }
    None => assert_eq(false, true)
  }
}

test "context_value_overwrite" {
  // 测试Context值的覆盖
  
  let empty_ctx = Context::empty()
  let user_key = create_key("user_id")
  
  // 添加初始值
  let ctx_with_user = empty_ctx.with_value(user_key, "user123")
  
  // 验证初始值
  match ctx_with_user.get(user_key) {
    Some(value) => assert_eq(value, "user123")
    None => assert_eq(false, true)
  }
  
  // 覆盖值
  let ctx_with_new_user = ctx_with_user.with_value(user_key, "user456")
  
  // 验证新值
  match ctx_with_new_user.get(user_key) {
    Some(value) => assert_eq(value, "user456")
    None => assert_eq(false, true)
  }
  
  // 验证旧上下文不受影响
  match ctx_with_user.get(user_key) {
    Some(value) => assert_eq(value, "user123")
    None => assert_eq(false, true)
  }
}

test "context_multiple_keys" {
  // 测试Context多个键的管理
  
  let empty_ctx = Context::empty()
  let keys = [
    create_key("trace_id"),
    create_key("span_id"),
    create_key("user_id"),
    create_key("request_id"),
    create_key("session_id")
  ]
  
  let values = [
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    "user123",
    "req456",
    "sess789"
  ]
  
  // 逐步添加键值对
  let mut ctx = empty_ctx
  let mut i = 0
  while i < keys.length() {
    ctx = ctx.with_value(keys[i], values[i])
    i = i + 1
  }
  
  // 验证所有键值对
  i = 0
  while i < keys.length() {
    match ctx.get(keys[i]) {
      Some(value) => assert_eq(value, values[i])
      None => assert_eq(false, true)
    }
    i = i + 1
  }
  
  // 验证不存在的键
  let nonexistent_key = create_key("nonexistent")
  match ctx.get(nonexistent_key) {
    Some(_) => assert_eq(false, true)
    None => assert_eq(true, true)  // 预期为None
  }
}

test "baggage_basic_operations" {
  // 测试Baggage的基本操作
  
  let empty_baggage = Baggage::empty()
  
  // 验证空baggage
  match empty_baggage.get("user_id") {
    Some(_) => assert_eq(false, true)
    None => assert_eq(true, true)  // 预期为None
  }
  
  // 添加条目
  let baggage_with_user = empty_baggage.with_entry("user_id", "user123")
  let baggage_with_session = baggage_with_user.with_entry("session_id", "sess456")
  
  // 验证条目
  match baggage_with_user.get("user_id") {
    Some(value) => {
      assert_eq(value, "user123")
      assert_eq(value.has_prefix("user"), true)
    }
    None => assert_eq(false, true)
  }
  
  match baggage_with_session.get("user_id") {
    Some(value) => assert_eq(value, "user123")
    None => assert_eq(false, true)
  }
  
  match baggage_with_session.get("session_id") {
    Some(value) => {
      assert_eq(value, "sess456")
      assert_eq(value.has_prefix("sess"), true)
    }
    None => assert_eq(false, true)
  }
}

test "baggage_duplicate_keys" {
  // 测试Baggage重复键的处理
  
  let empty_baggage = Baggage::empty()
  
  // 添加初始条目
  let baggage_with_user = empty_baggage.with_entry("user_id", "user123")
  
  // 添加相同键的不同值
  let baggage_with_new_user = baggage_with_user.with_entry("user_id", "user456")
  
  // 验证新值
  match baggage_with_new_user.get("user_id") {
    Some(value) => assert_eq(value, "user456")
    None => assert_eq(false, true)
  }
  
  // 验证旧baggage不受影响
  match baggage_with_user.get("user_id") {
    Some(value) => assert_eq(value, "user123")
    None => assert_eq(false, true)
  }
}

test "context_propagation_chain" {
  // 测试上下文传播链
  
  let empty_ctx = Context::empty()
  let trace_key = create_key("trace_id")
  let span_key = create_key("span_id")
  let user_key = create_key("user_id")
  
  // 创建上下文链
  let ctx1 = empty_ctx.with_value(trace_key, "trace123")
  let ctx2 = ctx1.with_value(span_key, "span456")
  let ctx3 = ctx2.with_value(user_key, "user789")
  
  // 验证传播链中的所有值
  match ctx3.get(trace_key) {
    Some(value) => assert_eq(value, "trace123")
    None => assert_eq(false, true)
  }
  
  match ctx3.get(span_key) {
    Some(value) => assert_eq(value, "span456")
    None => assert_eq(false, true)
  }
  
  match ctx3.get(user_key) {
    Some(value) => assert_eq(value, "user789")
    None => assert_eq(false, true)
  }
  
  // 验证早期上下文不受影响
  match ctx1.get(trace_key) {
    Some(value) => assert_eq(value, "trace123")
    None => assert_eq(false, true)
  }
  
  match ctx1.get(span_key) {
    Some(_) => assert_eq(false, true)
    None => assert_eq(true, true)  // 预期为None
  }
}

test "baggage_complex_entries" {
  // 测试Baggage复杂条目
  
  let empty_baggage = Baggage::empty()
  
  // 添加复杂键值对
  let complex_entries = [
    ("http.method", "GET"),
    ("http.status_code", "200"),
    ("user.agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
    ("service.name", "payment-service"),
    ("deployment.environment", "production"),
    ("custom.trace.id", "0af7651916cd43dd8448eb211c80319c"),
    ("request.timeout", "30s"),
    ("retry.count", "3")
  ]
  
  // 逐步添加复杂条目
  let mut baggage = empty_baggage
  let mut i = 0
  while i < complex_entries.length() {
    let (key, value) = complex_entries[i]
    baggage = baggage.with_entry(key, value)
    i = i + 1
  }
  
  // 验证所有复杂条目
  i = 0
  while i < complex_entries.length() {
    let (key, expected_value) = complex_entries[i]
    match baggage.get(key) {
      Some(actual_value) => {
        assert_eq(actual_value, expected_value)
        assert_eq(actual_value.length() > 0, true)
      }
      None => assert_eq(false, true)
    }
    i = i + 1
  }
  
  // 测试特殊字符键值对
  let special_baggage = baggage.with_entry("special.key!@#$%", "special!@#$%^&*()value")
  match special_baggage.get("special.key!@#$%") {
    Some(value) => {
      assert_eq(value, "special!@#$%^&*()value")
      assert_eq(value.contains("!@#$%"), true)
    }
    None => assert_eq(false, true)
  }
}

test "context_baggage_integration" {
  // 测试Context和Baggage的集成
  
  let empty_ctx = Context::empty()
  let empty_baggage = Baggage::empty()
  
  let trace_key = create_key("trace_id")
  let ctx_with_trace = empty_ctx.with_value(trace_key, "trace123")
  
  let baggage_with_user = empty_baggage.with_entry("user_id", "user456")
  
  // 验证Context和Baggage独立工作
  match ctx_with_trace.get(trace_key) {
    Some(value) => assert_eq(value, "trace123")
    None => assert_eq(false, true)
  }
  
  match baggage_with_user.get("user_id") {
    Some(value) => assert_eq(value, "user456")
    None => assert_eq(false, true)
  }
  
  // 验证Context中无法获取Baggage的值
  match ctx_with_trace.get(create_key("user_id")) {
    Some(_) => assert_eq(false, true)
    None => assert_eq(true, true)  // 预期为None
  }
  
  // 验证Baggage中无法获取Context的值
  match baggage_with_user.get("trace_id") {
    Some(_) => assert_eq(false, true)
    None => assert_eq(true, true)  // 预期为None
  }
}
// Context API基础功能测试
test "ContextKey creation" {
  let key = create_key("test-key")
  assert key.name == "test-key"
}

test "Context empty creation" {
  let ctx = Context::empty()
  assert ctx.values.length == 0
}

test "Context with_value and get operations" {
  let ctx = Context::empty()
  let key = create_key("user-id")
  let updated_ctx = ctx.with_value(key, "12345")
  
  // 验证值已添加
  assert updated_ctx.values.length == 1
  assert match (updated_ctx.get(key)) {
    Some(value) => value == "12345"
    None => false
  }
  
  // 验证原始context未修改
  assert ctx.values.length == 0
}

test "Context multiple values" {
  let ctx = Context::empty()
  let user_key = create_key("user-id")
  let session_key = create_key("session-id")
  let trace_key = create_key("trace-id")
  
  let ctx1 = ctx.with_value(user_key, "user123")
  let ctx2 = ctx1.with_value(session_key, "session456")
  let ctx3 = ctx2.with_value(trace_key, "trace789")
  
  // 验证所有值都存在
  assert ctx3.values.length == 3
  assert match (ctx3.get(user_key)) {
    Some(value) => value == "user123"
    None => false
  }
  assert match (ctx3.get(session_key)) {
    Some(value) => value == "session456"
    None => false
  }
  assert match (ctx3.get(trace_key)) {
    Some(value) => value == "trace789"
    None => false
  }
}

test "Context get non-existent key" {
  let ctx = Context::empty()
  let key = create_key("non-existent")
  
  assert match (ctx.get(key)) {
    None => true
    Some(_) => false
  }
}

test "Context value overwrite" {
  let ctx = Context::empty()
  let key = create_key("mutable-key")
  
  let ctx1 = ctx.with_value(key, "initial-value")
  let ctx2 = ctx1.with_value(key, "updated-value")
  
  // 验证值被覆盖
  assert ctx2.values.length == 1
  assert match (ctx2.get(key)) {
    Some(value) => value == "updated-value"
    None => false
  }
  
  // 验证前一个context的值未受影响
  assert match (ctx1.get(key)) {
    Some(value) => value == "initial-value"
    None => false
  }
}

test "Baggage empty creation" {
  let baggage = Baggage::empty()
  assert baggage.entries.length == 0
}

test "Baggage with_entry and get operations" {
  let baggage = Baggage::empty()
  let updated_baggage = baggage.with_entry("correlation-id", "corr-123")
  
  // 验证条目已添加
  assert updated_baggage.entries.length == 1
  assert match (updated_baggage.get("correlation-id")) {
    Some(value) => value == "corr-123"
    None => false
  }
  
  // 验证原始baggage未修改
  assert baggage.entries.length == 0
}

test "Baggage multiple entries" {
  let baggage = Baggage::empty()
  
  let baggage1 = baggage.with_entry("user-id", "user123")
  let baggage2 = baggage1.with_entry("request-id", "req456")
  let baggage3 = baggage2.with_entry("tenant-id", "tenant789")
  
  // 验证所有条目都存在
  assert baggage3.entries.length == 3
  assert match (baggage3.get("user-id")) {
    Some(value) => value == "user123"
    None => false
  }
  assert match (baggage3.get("request-id")) {
    Some(value) => value == "req456"
    None => false
  }
  assert match (baggage3.get("tenant-id")) {
    Some(value) => value == "tenant789"
    None => false
  }
}

test "Baggage get non-existent entry" {
  let baggage = Baggage::empty()
  
  assert match (baggage.get("non-existent")) {
    None => true
    Some(_) => false
  }
}

test "Baggage entry overwrite" {
  let baggage = Baggage::empty()
  
  let baggage1 = baggage.with_entry("mutable-entry", "initial")
  let baggage2 = baggage1.with_entry("mutable-entry", "updated")
  
  // 验证条目被覆盖
  assert baggage2.entries.length == 1
  assert match (baggage2.get("mutable-entry")) {
    Some(value) => value == "updated"
    None => false
  }
  
  // 验证前一个baggage的条目未受影响
  assert match (baggage1.get("mutable-entry")) {
    Some(value) => value == "initial"
    None => false
  }
}

test "Context and Baggage integration" {
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  let ctx_key = create_key("context-data")
  let updated_ctx = ctx.with_value(ctx_key, "context-value")
  
  let updated_baggage = baggage.with_entry("baggage-data", "baggage-value")
  
  // 验证Context和Baggage独立工作
  assert match (updated_ctx.get(ctx_key)) {
    Some(value) => value == "context-value"
    None => false
  }
  
  assert match (updated_baggage.get("baggage-data")) {
    Some(value) => value == "baggage-value"
    None => false
  }
  
  assert updated_ctx.values.length == 1
  assert updated_baggage.entries.length == 1
}
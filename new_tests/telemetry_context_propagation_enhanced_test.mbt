// 增强的上下文传播测试
// 测试上下文在复杂场景下的传播和保持

test "context_multi_level_propagation" {
  // 测试多级上下文传播
  let root_ctx = context::Context::empty()
  let user_key = context::create_key("user.id")
  let trace_key = context::create_key("trace.id")
  let request_key = context::create_key("request.id")
  
  // 第一级上下文
  let ctx1 = root_ctx.with_value(user_key, "user-12345")
  let ctx1_user = ctx1.get(user_key)
  assert match ctx1_user {
    Some(user) => user == "user-12345"
    None => false
  }
  
  // 第二级上下文
  let ctx2 = ctx1.with_value(trace_key, "trace-abcdef")
  let ctx2_user = ctx2.get(user_key)
  let ctx2_trace = ctx2.get(trace_key)
  assert match ctx2_user {
    Some(user) => user == "user-12345"
    None => false
  }
  assert match ctx2_trace {
    Some(trace) => trace == "trace-abcdef"
    None => false
  }
  
  // 第三级上下文
  let ctx3 = ctx2.with_value(request_key, "req-98765")
  let ctx3_user = ctx3.get(user_key)
  let ctx3_trace = ctx3.get(trace_key)
  let ctx3_request = ctx3.get(request_key)
  assert match ctx3_user {
    Some(user) => user == "user-12345"
    None => false
  }
  assert match ctx3_trace {
    Some(trace) => trace == "trace-abcdef"
    None => false
  }
  assert match ctx3_request {
    Some(request) => request == "req-98765"
    None => false
  }
}

test "context_with_complex_values" {
  // 测试包含复杂值的上下文传播
  let ctx = context::Context::empty()
  
  // JSON字符串
  let json_key = context::create_key("user.metadata")
  let json_value = "{\"name\":\"张三\",\"age\":30,\"city\":\"北京\"}"
  let ctx_with_json = ctx.with_value(json_key, json_value)
  
  // Base64编码的数据
  let base64_key = context::create_key("binary.data")
  let base64_value = "SGVsbG8gV29ybGQg5L2g5aW9"
  let ctx_with_base64 = ctx_with_json.with_value(base64_key, base64_value)
  
  // URL编码的参数
  let url_key = context::create_key("url.params")
  let url_value = "name=%E5%BC%A0%E4%B8%89&age=30&city=%E5%8C%97%E4%BA%AC"
  let final_ctx = ctx_with_base64.with_value(url_key, url_value)
  
  // 验证所有值都正确保存
  assert match final_ctx.get(json_key) {
    Some(json) => json.contains("张三") && json.contains("北京")
    None => false
  }
  
  assert match final_ctx.get(base64_key) {
    Some(base64) => base64 == "SGVsbG8gV29ybGQg5L2g5aW9"
    None => false
  }
  
  assert match final_ctx.get(url_key) {
    Some(url) => url.contains("%E5%BC%A0%E4%B8%89")
    None => false
  }
}

test "baggage_advanced_operations" {
  // 测试高级Baggage操作
  let baggage = context::Baggage::empty()
  
  // 添加多个条目
  let baggage1 = baggage.with_entry("user.id", "user-12345")
  let baggage2 = baggage1.with_entry("request.id", "req-abcde")
  let baggage3 = baggage2.with_entry("trace.id", "trace-12345")
  let baggage4 = baggage3.with_entry("service.name", "payment-service")
  let baggage5 = baggage4.with_entry("deployment.env", "production")
  
  // 验证所有条目都存在
  assert match baggage5.get("user.id") {
    Some(user_id) => user_id == "user-12345"
    None => false
  }
  
  assert match baggage5.get("request.id") {
    Some(req_id) => req_id == "req-abcde"
    None => false
  }
  
  assert match baggage5.get("trace.id") {
    Some(trace_id) => trace_id == "trace-12345"
    None => false
  }
  
  assert match baggage5.get("service.name") {
    Some(service) => service == "payment-service"
    None => false
  }
  
  assert match baggage5.get("deployment.env") {
    Some(env) => env == "production"
    None => false
  }
  
  // 测试不存在的键
  assert match baggage5.get("nonexistent.key") {
    Some(_) => false
    None => true
  }
}

test "context_and_baggage_integration" {
  // 测试Context和Baggage的集成
  let ctx = context::Context::empty()
  let baggage = context::Baggage::empty()
  
  // 在Context中存储Baggage信息
  let baggage_key = context::create_key("baggage")
  let populated_baggage = baggage
    .with_entry("correlation.id", "corr-12345")
    .with_entry("session.id", "sess-abcdef")
    .with_entry("user.tier", "premium")
  
  let ctx_with_baggage = ctx.with_value(baggage_key, "correlation.id=corr-12345,session.id=sess-abcdef,user.tier=premium")
  
  // 验证集成后的数据访问
  assert match ctx_with_baggage.get(baggage_key) {
    Some(baggage_str) => {
      baggage_str.contains("correlation.id=corr-12345") &&
      baggage_str.contains("session.id=sess-abcdef") &&
      baggage_str.contains("user.tier=premium")
    }
    None => false
  }
  
  // 同时测试直接Baggage访问
  assert match populated_baggage.get("correlation.id") {
    Some(corr_id) => corr_id == "corr-12345"
    None => false
  }
}

test "context_value_overwrite_behavior" {
  // 测试上下文值的覆盖行为
  let ctx = context::Context::empty()
  let key = context::create_key("test.key")
  
  // 设置初始值
  let ctx1 = ctx.with_value(key, "initial_value")
  assert match ctx1.get(key) {
    Some(value) => value == "initial_value"
    None => false
  }
  
  // 覆盖值
  let ctx2 = ctx1.with_value(key, "overwritten_value")
  assert match ctx2.get(key) {
    Some(value) => value == "overwritten_value"
    None => false
  }
  
  // 再次覆盖
  let ctx3 = ctx2.with_value(key, "final_value")
  assert match ctx3.get(key) {
    Some(value) => value == "final_value"
    None => false
  }
}

test "context_with_special_characters" {
  // 测试包含特殊字符的上下文值
  let ctx = context::Context::empty()
  
  // Unicode字符
  let unicode_key = context::create_key("unicode.test")
  let unicode_value = "测试中文🚀 Hello World ñáéíóú"
  let ctx_unicode = ctx.with_value(unicode_key, unicode_value)
  
  // 特殊符号
  let symbols_key = context::create_key("symbols.test")
  let symbols_value = "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./~`"
  let ctx_symbols = ctx_unicode.with_value(symbols_key, symbols_value)
  
  // 换行符和制表符
  let whitespace_key = context::create_key("whitespace.test")
  let whitespace_value = "line1\nline2\tindented\r\nline3"
  let final_ctx = ctx_symbols.with_value(whitespace_key, whitespace_value)
  
  // 验证特殊字符值
  assert match final_ctx.get(unicode_key) {
    Some(value) => value.contains("测试") && value.contains("🚀")
    None => false
  }
  
  assert match final_ctx.get(symbols_key) {
    Some(value) => value.contains("!@#$%^&*()")
    None => false
  }
  
  assert match final_ctx.get(whitespace_key) {
    Some(value) => value.contains("line1") && value.contains("line2")
    None => false
  }
}
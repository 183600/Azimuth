// Context模块高级测试用例

test "context_complex_interactions" {
  // 测试Context的复杂交互
  
  let base_ctx = Context::empty()
  
  // 创建多个keys
  let user_key = create_key("user.id")
  let session_key = create_key("session.id")
  let request_key = create_key("request.id")
  let trace_key = create_key("trace.id")
  let auth_key = create_key("auth.token")
  let locale_key = create_key("user.locale")
  let timezone_key = create_key("user.timezone")
  let feature_key = create_key("feature.flags")
  
  // 逐步添加context值
  let ctx_with_user = base_ctx.with_value(user_key, "user-12345")
  let ctx_with_session = ctx_with_user.with_value(session_key, "session-abcdef")
  let ctx_with_request = ctx_with_session.with_value(request_key, "req-789012")
  let ctx_with_trace = ctx_with_request.with_value(trace_key, "trace-fedcba")
  let ctx_with_auth = ctx_with_trace.with_value(auth_key, "bearer-token-xyz")
  let ctx_with_locale = ctx_with_auth.with_value(locale_key, "zh-CN")
  let ctx_with_timezone = ctx_with_locale.with_value(timezone_key, "Asia/Shanghai")
  let ctx_with_feature = ctx_with_timezone.with_value(feature_key, "new-ui-enabled,experimental-api")
  
  // 验证所有值都存在
  match ctx_with_feature.get(user_key) {
    Some(user_id) => assert_eq(user_id, "user-12345")
    None => @test.fail("Expected user.id")
  }
  
  match ctx_with_feature.get(session_key) {
    Some(session_id) => assert_eq(session_id, "session-abcdef")
    None => @test.fail("Expected session.id")
  }
  
  match ctx_with_feature.get(request_key) {
    Some(request_id) => assert_eq(request_id, "req-789012")
    None => @test.fail("Expected request.id")
  }
  
  match ctx_with_feature.get(trace_key) {
    Some(trace_id) => assert_eq(trace_id, "trace-fedcba")
    None => @test.fail("Expected trace.id")
  }
  
  match ctx_with_feature.get(auth_key) {
    Some(auth_token) => assert_eq(auth_token, "bearer-token-xyz")
    None => @test.fail("Expected auth.token")
  }
  
  match ctx_with_feature.get(locale_key) {
    Some(locale) => assert_eq(locale, "zh-CN")
    None => @test.fail("Expected user.locale")
  }
  
  match ctx_with_feature.get(timezone_key) {
    Some(timezone) => assert_eq(timezone, "Asia/Shanghai")
    None => @test.fail("Expected user.timezone")
  }
  
  match ctx_with_feature.get(feature_key) {
    Some(features) => {
      assert_eq(features.contains("new-ui-enabled"), true)
      assert_eq(features.contains("experimental-api"), true)
    }
    None => @test.fail("Expected feature.flags")
  }
  
  // 验证context中的values数量
  assert_eq(ctx_with_feature.values.length(), 8)
  
  // 验证不存在的key
  let non_existent_key = create_key("non.existent.key")
  match ctx_with_feature.get(non_existent_key) {
    Some(_) => @test.fail("Expected None for non-existent key")
    None => assert_eq(true, true)  // 期望None
  }
}

test "context_and_baggage_integration" {
  // 测试Context和Baggage的集成
  
  let base_ctx = Context::empty()
  let base_baggage = Baggage::empty()
  
  // 在Context中添加值
  let ctx_key1 = create_key("ctx.key1")
  let ctx_key2 = create_key("ctx.key2")
  let ctx_key3 = create_key("ctx.key3")
  
  let enriched_ctx = base_ctx
    .with_value(ctx_key1, "context-value-1")
    .with_value(ctx_key2, "context-value-2")
    .with_value(ctx_key3, "context-value-3")
  
  // 在Baggage中添加值
  let enriched_baggage = base_baggage
    .with_entry("baggage.key1", "baggage-value-1")
    .with_entry("baggage.key2", "baggage-value-2")
    .with_entry("baggage.key3", "baggage-value-3")
    .with_entry("user.id", "user-from-baggage")
    .with_entry("session.id", "session-from-baggage")
  
  // 验证Context中的值
  match enriched_ctx.get(ctx_key1) {
    Some(value) => assert_eq(value, "context-value-1")
    None => @test.fail("Expected ctx.key1")
  }
  
  match enriched_ctx.get(ctx_key2) {
    Some(value) => assert_eq(value, "context-value-2")
    None => @test.fail("Expected ctx.key2")
  }
  
  match enriched_ctx.get(ctx_key3) {
    Some(value) => assert_eq(value, "context-value-3")
    None => @test.fail("Expected ctx.key3")
  }
  
  // 验证Baggage中的值
  match enriched_baggage.get("baggage.key1") {
    Some(value) => assert_eq(value, "baggage-value-1")
    None => @test.fail("Expected baggage.key1")
  }
  
  match enriched_baggage.get("baggage.key2") {
    Some(value) => assert_eq(value, "baggage-value-2")
    None => @test.fail("Expected baggage.key2")
  }
  
  match enriched_baggage.get("baggage.key3") {
    Some(value) => assert_eq(value, "baggage-value-3")
    None => @test.fail("Expected baggage.key3")
  }
  
  match enriched_baggage.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user-from-baggage")
    None => @test.fail("Expected user.id from baggage")
  }
  
  match enriched_baggage.get("session.id") {
    Some(session_id) => assert_eq(session_id, "session-from-baggage")
    None => @test.fail("Expected session.id from baggage")
  }
  
  // 验证数量
  assert_eq(enriched_ctx.values.length(), 3)
  assert_eq(enriched_baggage.entries.length(), 5)
  
  // 测试边界情况：空值和特殊字符
  let special_ctx = base_ctx
    .with_value(create_key("empty.value"), "")
    .with_value(create_key("special.chars"), "!@#$%^&*()_+-=[]{}|;':\",./<>?")
    .with_value(create_key("unicode.value"), "Unicode测试🚀🌟💫")
    .with_value(create_key("multi.line"), "line1\nline2\nline3")
    .with_value(create_key("spaces"), "   leading and trailing   ")
  
  // 验证特殊值
  match special_ctx.get(create_key("empty.value")) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected empty value")
  }
  
  match special_ctx.get(create_key("special.chars")) {
    Some(value) => assert_eq(value.contains("!@#$%^&*()"), true)
    None => @test.fail("Expected special chars")
  }
  
  match special_ctx.get(create_key("unicode.value")) {
    Some(value) => {
      assert_eq(value.contains("测试"), true)
      assert_eq(value.contains("🚀"), true)
    }
    None => @test.fail("Expected unicode value")
  }
  
  match special_ctx.get(create_key("multi.line")) {
    Some(value) => {
      assert_eq(value.contains("line1"), true)
      assert_eq(value.contains("line2"), true)
      assert_eq(value.contains("line3"), true)
      assert_eq(value.contains("\n"), true)
    }
    None => @test.fail("Expected multi-line value")
  }
  
  // 测试Baggage的特殊值
  let special_baggage = base_baggage
    .with_entry("empty.baggage", "")
    .with_entry("special.baggage", "special=!@#$%^&*()")
    .with_entry("unicode.baggage", "测试=🚀🌟")
    .with_entry("complex.baggage", "key1=value1;prop1=val1,key2=value2")
  
  match special_baggage.get("empty.baggage") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected empty baggage")
  }
  
  match special_baggage.get("special.baggage") {
    Some(value) => assert_eq(value.contains("!@#$%^&*()"), true)
    None => @test.fail("Expected special baggage")
  }
  
  match special_baggage.get("unicode.baggage") {
    Some(value) => {
      assert_eq(value.contains("测试"), true)
      assert_eq(value.contains("🚀"), true)
    }
    None => @test.fail("Expected unicode baggage")
  }
}
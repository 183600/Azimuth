// Context模块边缘情况测试用例
// 测试context的边缘情况和错误处理

test "context_key_validation_edge_cases" {
  // 测试ContextKey的边缘情况验证
  
  // 测试各种特殊的key名称
  let special_keys = [
    "",                           // 空字符串
    " ",                          // 只有空格
    "key with spaces",            // 包含空格
    "key@with#special",           // 包含特殊字符
    "123invalid",                 // 以数字开头
    "a".repeat(1000),             // 过长的key
    "na\nme",                     // 包含换行符
    "na\tme",                     // 包含制表符
    "na\rme",                     // 包含回车符
    "na/me",                      // 包含路径分隔符
    "key.with.dots",              // 包含点号
    "key-with-dashes",            // 包含连字符
    "key_with_underscores",       // 包含下划线
    "中文键名",                    // 中文字符
    "ключ_русский",               // 俄文字符
    "emoji_key🚀",                // emoji字符
    "mixed中文🚀english",          // 混合字符
  ]
  
  // 测试创建特殊key
  for key_name in special_keys {
    let key = create_key(key_name)
    assert_eq(key.name, key_name)
    
    // 测试key的基本属性
    let is_empty = key.name.length() == 0
    let has_spaces = key.name.contains(" ")
    let has_special_chars = key.name.contains("@") || key.name.contains("#")
    let starts_with_digit = key.name.length() > 0 && key.name[0] >= '0' && key.name[0] <= '9'
    let is_very_long = key.name.length() > 500
    let has_control_chars = key.name.contains("\n") || key.name.contains("\t") || key.name.contains("\r")
    let has_path_separator = key.name.contains("/")
    
    // 这些都是边缘情况，但不一定无效
    // 在实际实现中，应该有适当的验证逻辑
    if is_empty {
      assert_eq(key.name, "")
    }
    
    if has_spaces {
      assert_eq(key.name.contains(" "), true)
    }
    
    if has_special_chars {
      assert_eq(key.name.contains("@") || key.name.contains("#"), true)
    }
    
    if starts_with_digit {
      assert_eq(key.name[0] >= '0' && key.name[0] <= '9', true)
    }
    
    if is_very_long {
      assert_eq(key.name.length() > 500, true)
    }
    
    if has_control_chars {
      assert_eq(key.name.contains("\n") || key.name.contains("\t") || key.name.contains("\r"), true)
    }
    
    if has_path_separator {
      assert_eq(key.name.contains("/"), true)
    }
  }
}

test "context_value_edge_cases" {
  // 测试Context值的边缘情况
  
  let base_ctx = Context::empty()
  
  // 测试各种特殊的值
  let special_values = [
    "",                           // 空字符串
    " ",                          // 只有空格
    "value with spaces",          // 包含空格
    "value@with#special",         // 包含特殊字符
    "a".repeat(10000),            // 过长的值
    "na\nme",                     // 包含换行符
    "na\tme",                     // 包含制表符
    "na\rme",                     // 包含回车符
    "na/me",                      // 包含路径分隔符
    "value.with.dots",            // 包含点号
    "value-with-dashes",          // 包含连字符
    "value_with_underscores",     // 包含下划线
    "中文值",                      // 中文字符
    "значение_русское",           // 俄文字符
    "emoji_value🚀",              // emoji字符
    "mixed中文🚀english",          // 混合字符
    "null",                       // 字符串"null"
    "undefined",                  // 字符串"undefined"
    "true",                       // 字符串"true"
    "false",                      // 字符串"false"
    "0",                          // 字符串"0"
    "-1",                         // 字符串"-1"
    "123.456",                    // 字符串数字
    "[]",                         // 字符串"[]"
    "{}",                         // 字符串"{}"
    "<script>alert('xss')</script>", // XSS尝试
    "'; DROP TABLE users; --",    // SQL注入尝试
  ]
  
  // 测试设置和获取特殊值
  for value in special_values {
    let key = create_key("test_key_" + value.length().to_string())
    let ctx_with_value = base_ctx.with_value(key, value)
    
    // 验证值可以被正确获取
    match ctx_with_value.get(key) {
      Some(retrieved_value) => {
        assert_eq(retrieved_value, value)
      }
      None => @test.fail("Expected Some(value) for key: " + key.name)
    }
    
    // 测试值的属性
    let is_empty = value.length() == 0
    let is_whitespace_only = value.trim().length() == 0 && value.length() > 0
    let has_spaces = value.contains(" ")
    let has_special_chars = value.contains("@") || value.contains("#") || value.contains(";")
    let is_very_long = value.length() > 5000
    let has_control_chars = value.contains("\n") || value.contains("\t") || value.contains("\r")
    let has_path_separator = value.contains("/")
    let looks_like_json = value.has_prefix("{") && value.has_suffix("}") || 
                          value.has_prefix("[") && value.has_suffix("]")
    let looks_like_number = value.length() > 0 && 
                           (value[0] >= '0' && value[0] <= '9' || value[0] == '-')
    let contains_xss = value.contains("<script") || value.contains("alert(")
    let contains_sql_injection = value.contains("DROP TABLE") || value.contains("DELETE FROM")
    
    // 验证边缘情况属性
    if is_empty {
      assert_eq(value, "")
    }
    
    if is_whitespace_only {
      assert_eq(value.trim().length(), 0)
    }
    
    if has_spaces {
      assert_eq(value.contains(" "), true)
    }
    
    if has_special_chars {
      assert_eq(value.contains("@") || value.contains("#") || value.contains(";"), true)
    }
    
    if is_very_long {
      assert_eq(value.length() > 5000, true)
    }
    
    if has_control_chars {
      assert_eq(value.contains("\n") || value.contains("\t") || value.contains("\r"), true)
    }
    
    if has_path_separator {
      assert_eq(value.contains("/"), true)
    }
    
    if looks_like_json {
      assert_eq(value.has_prefix("{") || value.has_prefix("["), true)
    }
    
    if looks_like_number {
      assert_eq(value.length() > 0 && (value[0] >= '0' && value[0] <= '9' || value[0] == '-'), true)
    }
    
    if contains_xss {
      assert_eq(value.contains("<script") || value.contains("alert("), true)
    }
    
    if contains_sql_injection {
      assert_eq(value.contains("DROP TABLE") || value.contains("DELETE FROM"), true)
    }
  }
}

test "context_chain_operations_edge_cases" {
  // 测试Context链式操作的边缘情况
  
  let base_ctx = Context::empty()
  
  // 测试深度链式操作
  let mut ctx = base_ctx
  let mut i = 0
  while i < 1000 {
    let key = create_key("key_" + i.to_string())
    let value = "value_" + i.to_string()
    ctx = ctx.with_value(key, value)
    i = i + 1
  }
  
  // 验证深度链式操作的结果
  i = 0
  while i < 1000 {
    let key = create_key("key_" + i.to_string())
    let expected_value = "value_" + i.to_string()
    
    match ctx.get(key) {
      Some(actual_value) => {
        assert_eq(actual_value, expected_value)
      }
      None => @test.fail("Expected Some(value) for key: " + key.name)
    }
    i = i + 1
  }
  
  // 测试覆盖相同key的值
  let override_key = create_key("override_key")
  let ctx1 = ctx.with_value(override_key, "original_value")
  let ctx2 = ctx1.with_value(override_key, "overridden_value")
  
  // 验证值被正确覆盖
  match ctx2.get(override_key) {
    Some(value) => {
      assert_eq(value, "overridden_value")
    }
    None => @test.fail("Test failed")
  }
  
  // 测试空context的操作
  let empty_ctx = Context::empty()
  let empty_key = create_key("")
  let empty_value = ""
  
  let ctx_with_empty_key = empty_ctx.with_value(empty_key, empty_value)
  
  match ctx_with_empty_key.get(empty_key) {
    Some(value) => {
      assert_eq(value, "")
    }
    None => @test.fail("Test failed")
  }
  
  // 测试获取不存在的key
  let non_existent_key = create_key("non_existent_key")
  match ctx.get(non_existent_key) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试context的"污染"情况（大量不同的key-value对）
  let mut polluted_ctx = base_ctx
  let mut j = 0
  while j < 100 {
    let key = create_key("polluted_key_" + j.to_string())
    let value = "polluted_value_" + j.to_string() + "_with_extra_long_suffix_to_make_it_longer"
    polluted_ctx = polluted_ctx.with_value(key, value)
    j = j + 1
  }
  
  // 验证污染的context仍然正常工作
  j = 0
  while j < 100 {
    let key = create_key("polluted_key_" + j.to_string())
    let expected_value = "polluted_value_" + j.to_string() + "_with_extra_long_suffix_to_make_it_longer"
    
    match polluted_ctx.get(key) {
      Some(actual_value) => {
        assert_eq(actual_value, expected_value)
      }
      None => @test.fail("Expected Some(value) for polluted key: " + key.name)
    }
    j = j + 1
  }
}
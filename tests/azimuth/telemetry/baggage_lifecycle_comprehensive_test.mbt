// Baggage Lifecycle Comprehensive Test
// 测试Baggage的完整生命周期，包括创建、设置、获取、删除等操作

test "baggage creation and initial state" {
  // 测试Baggage的创建和初始状态
  let baggage = @azimuth.Baggage::new()
  
  // 初始状态应该是空的
  let non_existent_value = @azimuth.Baggage::get_entry(baggage, "non.existent.key")
  assert_eq(non_existent_value, None)
  
  // 测试在空baggage上设置值
  let updated_baggage = @azimuth.Baggage::set_entry(baggage, "test.key", "test.value")
  
  // 验证值是否被设置
  let retrieved_value = @azimuth.Baggage::get_entry(updated_baggage, "test.key")
  assert_eq(retrieved_value, Some("test.value"))
}

test "baggage multiple entries management" {
  // 测试Baggage中多个条目的管理
  let baggage = @azimuth.Baggage::new()
  
  // 设置多个条目
  let baggage1 = @azimuth.Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage2 = @azimuth.Baggage::set_entry(baggage1, "request.id", "req-67890")
  let baggage3 = @azimuth.Baggage::set_entry(baggage2, "session.id", "session-abcde")
  let baggage4 = @azimuth.Baggage::set_entry(baggage3, "trace.flag", "enabled")
  
  // 验证所有条目都能正确获取
  assert_eq(@azimuth.Baggage::get_entry(baggage4, "user.id"), Some("user-12345"))
  assert_eq(@azimuth.Baggage::get_entry(baggage4, "request.id"), Some("req-67890"))
  assert_eq(@azimuth.Baggage::get_entry(baggage4, "session.id"), Some("session-abcde"))
  assert_eq(@azimuth.Baggage::get_entry(baggage4, "trace.flag"), Some("enabled"))
  
  // 验证不存在的条目返回None
  assert_eq(@azimuth.Baggage::get_entry(baggage4, "non.existent"), None)
}

test "baggage entry update and override" {
  // 测试Baggage条目的更新和覆盖
  let baggage = @azimuth.Baggage::new()
  
  // 设置初始值
  let baggage1 = @azimuth.Baggage::set_entry(baggage, "key1", "initial.value")
  assert_eq(@azimuth.Baggage::get_entry(baggage1, "key1"), Some("initial.value"))
  
  // 更新值
  let baggage2 = @azimuth.Baggage::set_entry(baggage1, "key1", "updated.value")
  assert_eq(@azimuth.Baggage::get_entry(baggage2, "key1"), Some("updated.value"))
  
  // 设置多个键值对，包括重复的键
  let baggage3 = @azimuth.Baggage::set_entry(baggage2, "key2", "value2")
  let baggage4 = @azimuth.Baggage::set_entry(baggage3, "key1", "final.value")
  let baggage5 = @azimuth.Baggage::set_entry(baggage4, "key3", "value3")
  
  // 验证最终的值
  assert_eq(@azimuth.Baggage::get_entry(baggage5, "key1"), Some("final.value"))
  assert_eq(@azimuth.Baggage::get_entry(baggage5, "key2"), Some("value2"))
  assert_eq(@azimuth.Baggage::get_entry(baggage5, "key3"), Some("value3"))
}

test "baggage entry removal operations" {
  // 测试Baggage条目的删除操作
  let baggage = @azimuth.Baggage::new()
  
  // 添加多个条目
  let baggage1 = @azimuth.Baggage::set_entry(baggage, "keep.me", "value1")
  let baggage2 = @azimuth.Baggage::set_entry(baggage1, "delete.me", "value2")
  let baggage3 = @azimuth.Baggage::set_entry(baggage2, "also.keep", "value3")
  
  // 验证所有条目都存在
  assert_eq(@azimuth.Baggage::get_entry(baggage3, "keep.me"), Some("value1"))
  assert_eq(@azimuth.Baggage::get_entry(baggage3, "delete.me"), Some("value2"))
  assert_eq(@azimuth.Baggage::get_entry(baggage3, "also.keep"), Some("value3"))
  
  // 删除条目
  let baggage4 = @azimuth.Baggage::remove_entry(baggage3, "delete.me")
  
  // 验证删除后的状态
  assert_eq(@azimuth.Baggage::get_entry(baggage4, "keep.me"), Some("value1"))
  assert_eq(@azimuth.Baggage::get_entry(baggage4, "delete.me"), None)
  assert_eq(@azimuth.Baggage::get_entry(baggage4, "also.keep"), Some("value3"))
  
  // 尝试删除不存在的条目
  let baggage5 = @azimuth.Baggage::remove_entry(baggage4, "non.existent")
  
  // 验证现有条目仍然存在
  assert_eq(@azimuth.Baggage::get_entry(baggage5, "keep.me"), Some("value1"))
  assert_eq(@azimuth.Baggage::get_entry(baggage5, "also.keep"), Some("value3"))
}

test "baggage special character handling" {
  // 测试Baggage中特殊字符的处理
  let baggage = @azimuth.Baggage::new()
  
  // 测试包含特殊字符的键和值
  let special_cases = [
    ("key.with.dots", "value.with.dots"),
    ("key-with-dashes", "value-with-dashes"),
    ("key_with_underscores", "value_with_underscores"),
    ("key with spaces", "value with spaces"),
    ("key@with@symbols", "value@with@symbols"),
    ("key-with-unicode-测试", "value-with-unicode-测试"),
    ("empty.value", ""),
    ("key.with.numbers123", "value.with.numbers456")
  ]
  
  let mut current_baggage = baggage
  for (key, value) in special_cases {
    current_baggage = @azimuth.Baggage::set_entry(current_baggage, key, value)
  }
  
  // 验证所有特殊字符的键值对都能正确存储和检索
  for (key, expected_value) in special_cases {
    let actual_value = @azimuth.Baggage::get_entry(current_baggage, key)
    assert_eq(actual_value, Some(expected_value))
  }
}

test "baggage case sensitivity" {
  // 测试Baggage的键大小写敏感性
  let baggage = @azimuth.Baggage::new()
  
  // 设置不同大小写的键
  let baggage1 = @azimuth.Baggage::set_entry(baggage, "CaseSensitive", "value1")
  let baggage2 = @azimuth.Baggage::set_entry(baggage1, "casesensitive", "value2")
  let baggage3 = @azimuth.Baggage::set_entry(baggage2, "CASESENSITIVE", "value3")
  
  // 验证大小写不同的键被视为不同的键
  assert_eq(@azimuth.Baggage::get_entry(baggage3, "CaseSensitive"), Some("value1"))
  assert_eq(@azimuth.Baggage::get_entry(baggage3, "casesensitive"), Some("value2"))
  assert_eq(@azimuth.Baggage::get_entry(baggage3, "CASESENSITIVE"), Some("value3"))
  
  // 验证大小写不匹配的查询返回None
  assert_eq(@azimuth.Baggage::get_entry(baggage3, "casesensitivE"), None)
}

test "baggage large values handling" {
  // 测试Baggage处理大值的能力
  let baggage = @azimuth.Baggage::new()
  
  // 创建大字符串值
  let large_value = "x" * 1000  // 1000个字符的字符串
  let very_large_value = "y" * 10000  // 10000个字符的字符串
  
  // 设置大值
  let baggage1 = @azimuth.Baggage::set_entry(baggage, "large.value", large_value)
  let baggage2 = @azimuth.Baggage::set_entry(baggage1, "very.large.value", very_large_value)
  
  // 验证大值能够正确存储和检索
  let retrieved_large = @azimuth.Baggage::get_entry(baggage2, "large.value")
  let retrieved_very_large = @azimuth.Baggage::get_entry(baggage2, "very.large.value")
  
  assert_eq(retrieved_large, Some(large_value))
  assert_eq(retrieved_very_large, Some(very_large_value))
  
  // 验证值的长度
  match retrieved_large {
    Some(value) => assert_eq(value.length, 1000)?
    None => @test.fail("Expected large value to be retrieved")?
  }
  
  match retrieved_very_large {
    Some(value) => assert_eq(value.length, 10000)?
    None => @test.fail("Expected very large value to be retrieved")?
  }
}
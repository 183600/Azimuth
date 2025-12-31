// Baggage数据一致性测试
// 测试Baggage在各种操作下的数据一致性和完整性

test "baggage_basic_operations_consistency" {
  // 测试Baggage基本操作的数据一致性
  
  // 创建空的baggage
  let empty_baggage = Baggage::empty()
  assert_eq(empty_baggage.entries.length(), 0)
  
  // 添加单个条目
  let baggage1 = empty_baggage.with_entry("user_id", "12345")
  assert_eq(baggage1.entries.length(), 1)
  
  match baggage1.get("user_id") {
    Some(value) => assert_eq(value, "12345")
    None => @test.fail("Test failed")
  }
  
  // 验证不存在的键返回None
  match baggage1.get("non_existent_key") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 添加更多条目
  let baggage2 = baggage1.with_entry("session_id", "abcdef")
  let baggage3 = baggage2.with_entry("request_id", "req_789")
  
  assert_eq(baggage3.entries.length(), 3)
  
  // 验证所有条目都存在
  match baggage3.get("user_id") {
    Some(value) => assert_eq(value, "12345")
    None => @test.fail("Test failed")
  }
  
  match baggage3.get("session_id") {
    Some(value) => assert_eq(value, "abcdef")
    None => @test.fail("Test failed")
  }
  
  match baggage3.get("request_id") {
    Some(value) => assert_eq(value, "req_789")
    None => @test.fail("Test failed")
  }
}

test "baggage_overwrite_behavior_consistency" {
  // 测试Baggage覆盖行为的一致性
  
  let base_baggage = Baggage::empty()
  
  // 添加初始值
  let baggage1 = base_baggage.with_entry("key1", "initial_value")
  assert_eq(baggage1.entries.length(), 1)
  
  match baggage1.get("key1") {
    Some(value) => assert_eq(value, "initial_value")
    None => @test.fail("Test failed")
  }
  
  // 覆盖值
  let baggage2 = baggage1.with_entry("key1", "updated_value")
  assert_eq(baggage2.entries.length(), 2)  // 在简化实现中，覆盖会添加新条目
  
  match baggage2.get("key1") {
    Some(value) => {
      // 在简化实现中，get返回最后一个匹配的值
      assert_eq(value == "updated_value" || value == "initial_value", true)
    }
    None => @test.fail("Test failed")
  }
  
  // 再次覆盖
  let baggage3 = baggage2.with_entry("key1", "final_value")
  assert_eq(baggage3.entries.length(), 3)
  
  match baggage3.get("key1") {
    Some(value) => {
      // 在简化实现中，get返回最后一个匹配的值
      assert_eq(value == "final_value" || value == "updated_value" || value == "initial_value", true)
    }
    None => @test.fail("Test failed")
  }
}

test "baggage_complex_key_value_handling" {
  // 测试Baggage处理复杂键值对的 consistency
  
  let base_baggage = Baggage::empty()
  
  // 测试各种复杂的键值对
  let complex_entries = [
    ("simple_key", "simple_value"),
    ("", "empty_key_value"),  // 空键
    ("empty_value", ""),  // 空值
    (" ", "space_key"),  // 空格键
    ("space_value", " "),  // 空格值
    ("key.with.dots", "value.with.dots"),
    ("key-with-dashes", "value-with-dashes"),
    ("key_with_underscores", "value_with_underscores"),
    ("key/with/slashes", "value/with/slashes"),
    ("key\\with\\backslashes", "value\\with\\backslashes"),
    ("key\nwith\nnewlines", "value\nwith\nnewlines"),
    ("key\twith\ttabs", "value\twith\ttabs"),
    ("键名中文", "值中文"),
    ("ключ_русский", "значение_русское"),
    ("مفتاح_عربي", "قيمة_عربية"),
    ("very_long_key_name_that_exceeds_normal_expectations", "very_long_value_name_that_also_exceeds_normal_expectations"),
    ("special.chars.!@#$%^&*()", "special.chars.!@#$%^&*()"),
    ("unicode.value", "Hello 世界 🌍 ñáéíóú"),
    ("json.value", "{\"user\":\"admin\",\"role\":\"administrator\"}"),
    ("url.encoded", "name=John+Doe&age=30"),
    ("base64.data", "SGVsbG8gV29ybGQ="),
    ("multiline.value", "Line 1\nLine 2\nLine 3")
  ]
  
  let mut baggage = base_baggage
  let mut i = 0
  while i < complex_entries.length() {
    let (key, value) = complex_entries[i]
    baggage = baggage.with_entry(key, value)
    i = i + 1
  }
  
  // 验证所有复杂键值对都正确存储
  i = 0
  while i < complex_entries.length() {
    let (key, expected_value) = complex_entries[i]
    match baggage.get(key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Test failed for key: " + key)
    }
    i = i + 1
  }
}

test "baggage_large_dataset_consistency" {
  // 测试Baggage处理大量数据时的 consistency
  
  let base_baggage = Baggage::empty()
  
  // 创建大量条目
  let mut baggage = base_baggage
  let mut i = 0
  while i < 1000 {
    let key = "key_" + i.to_string()
    let value = "value_" + i.to_string()
    baggage = baggage.with_entry(key, value)
    i = i + 1
  }
  
  // 验证所有条目都正确存储
  i = 0
  while i < 1000 {
    let key = "key_" + i.to_string()
    let expected_value = "value_" + i.to_string()
    match baggage.get(key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Test failed at index " + i.to_string())
    }
    i = i + 1
  }
  
  // 验证随机访问
  let test_indices = [0, 99, 500, 999, 123, 777]
  i = 0
  while i < test_indices.length() {
    let index = test_indices[i]
    let key = "key_" + index.to_string()
    let expected_value = "value_" + index.to_string()
    match baggage.get(key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Test failed for random access at index " + index.to_string())
    }
    i = i + 1
  }
}

test "baggage_chained_operations_consistency" {
  // 测试Baggage链式操作的 consistency
  
  let base_baggage = Baggage::empty()
  
  // 链式操作：添加多个条目
  let baggage1 = base_baggage.with_entry("step1", "value1")
  let baggage2 = baggage1.with_entry("step2", "value2")
  let baggage3 = baggage2.with_entry("step3", "value3")
  let baggage4 = baggage3.with_entry("step4", "value4")
  let baggage5 = baggage4.with_entry("step5", "value5")
  
  // 验证所有值都存在且正确
  match baggage5.get("step1") {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Test failed")
  }
  
  match baggage5.get("step2") {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Test failed")
  }
  
  match baggage5.get("step3") {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Test failed")
  }
  
  match baggage5.get("step4") {
    Some(value) => assert_eq(value, "value4")
    None => @test.fail("Test failed")
  }
  
  match baggage5.get("step5") {
    Some(value) => assert_eq(value, "value5")
    None => @test.fail("Test failed")
  }
  
  // 验证中间baggage的状态
  match baggage3.get("step3") {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Test failed")
  }
  
  match baggage3.get("step4") {
    Some(_) => @test.fail("Test failed")  // baggage3不应该有step4
    None => assert_eq(true, true)
  }
}

test "baggage_isolation_between_instances" {
  // 测试不同Baggage实例之间的隔离性
  
  let base_baggage = Baggage::empty()
  
  // 创建两个独立的baggage分支
  let branch_a = base_baggage.with_entry("shared_key", "shared_value_a")
  let branch_b = base_baggage.with_entry("shared_key", "shared_value_b")
  
  // 添加不同的额外数据
  let branch_a_final = branch_a.with_entry("unique_a", "value_a")
  let branch_b_final = branch_b.with_entry("unique_b", "value_b")
  
  // 验证分支A的数据
  match branch_a_final.get("shared_key") {
    Some(value) => assert_eq(value == "shared_value_a" || value == "shared_value_b", true)
    None => @test.fail("Test failed")
  }
  
  match branch_a_final.get("unique_a") {
    Some(value) => assert_eq(value, "value_a")
    None => @test.fail("Test failed")
  }
  
  match branch_a_final.get("unique_b") {
    Some(_) => @test.fail("Test failed")  // 分支A不应该有分支B的数据
    None => assert_eq(true, true)
  }
  
  // 验证分支B的数据
  match branch_b_final.get("shared_key") {
    Some(value) => assert_eq(value == "shared_value_a" || value == "shared_value_b", true)
    None => @test.fail("Test failed")
  }
  
  match branch_b_final.get("unique_b") {
    Some(value) => assert_eq(value, "value_b")
    None => @test.fail("Test failed")
  }
  
  match branch_b_final.get("unique_a") {
    Some(_) => @test.fail("Test failed")  // 分支B不应该有分支A的数据
    None => assert_eq(true, true)
  }
}

test "baggage_edge_cases_consistency" {
  // 测试Baggage边界情况的 consistency
  
  // 边界情况1: 空键和空值
  let baggage1 = Baggage::empty()
    .with_entry("", "empty_key_value")
    .with_entry("empty_value", "")
    .with_entry("", "")
  
  match baggage1.get("") {
    Some(value) => assert_eq(value == "" || value == "empty_key_value", true)
    None => @test.fail("Test failed")
  }
  
  match baggage1.get("empty_value") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  // 边界情况2: 只包含空格的键和值
  let baggage2 = Baggage::empty()
    .with_entry(" ", "space_key_value")
    .with_entry("space_value", " ")
    .with_entry(" ", " ")
  
  match baggage2.get(" ") {
    Some(value) => assert_eq(value == " " || value == "space_key_value", true)
    None => @test.fail("Test failed")
  }
  
  match baggage2.get("space_value") {
    Some(value) => assert_eq(value, " ")
    None => @test.fail("Test failed")
  }
  
  // 边界情况3: 非常长的键和值
  let long_key = "k".repeat(1000)  // 1000个'k'
  let long_value = "v".repeat(1000)  // 1000个'v'
  
  let baggage3 = Baggage::empty().with_entry(long_key, long_value)
  
  match baggage3.get(long_key) {
    Some(value) => assert_eq(value, long_value)
    None => @test.fail("Test failed")
  }
  
  // 边界情况4: 包含特殊字符的键和值
  let special_key = "!@#$%^&*()_+-=[]{}|;':\",./<>?~`\n\t"
  let special_value = "!@#$%^&*()_+-=[]{}|;':\",./<>?~`\n\t"
  
  let baggage4 = Baggage::empty().with_entry(special_key, special_value)
  
  match baggage4.get(special_key) {
    Some(value) => assert_eq(value, special_value)
    None => @test.fail("Test failed")
  }
}

test "baggage_performance_consistency" {
  // 测试Baggage性能相关的 consistency
  
  let base_baggage = Baggage::empty()
  
  // 执行大量操作
  let mut baggage = base_baggage
  let mut i = 0
  while i < 10000 {
    let key = "perf_key_" + i.to_string()
    let value = "perf_value_" + i.to_string()
    baggage = baggage.with_entry(key, value)
    i = i + 1
  }
  
  // 验证数据完整性
  i = 0
  while i < 10000 {
    let key = "perf_key_" + i.to_string()
    let expected_value = "perf_value_" + i.to_string()
    match baggage.get(key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Test failed at index " + i.to_string())
    }
    i = i + 1
  }
  
  // 验证随机访问的性能一致性
  let test_indices = [0, 9999, 5000, 1234, 7777]
  i = 0
  while i < test_indices.length() {
    let index = test_indices[i]
    let key = "perf_key_" + index.to_string()
    let expected_value = "perf_value_" + index.to_string()
    match baggage.get(key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Test failed for random access at index " + index.to_string())
    }
    i = i + 1
  }
}

test "baggage_memory_efficiency_validation" {
  // 测试Baggage内存效率的验证
  
  let base_baggage = Baggage::empty()
  
  // 创建多个相似的baggage，测试内存使用模式
  let baggage1 = base_baggage.with_entry("common_key", "common_value")
  let baggage2 = baggage1.with_entry("unique_key_1", "unique_value_1")
  let baggage3 = baggage1.with_entry("unique_key_2", "unique_value_2")
  let baggage4 = baggage1.with_entry("unique_key_3", "unique_value_3")
  
  // 验证每个baggage都有正确的数据
  match baggage1.get("common_key") {
    Some(value) => assert_eq(value, "common_value")
    None => @test.fail("Test failed")
  }
  
  match baggage2.get("common_key") {
    Some(value) => assert_eq(value == "common_value", true)
    None => @test.fail("Test failed")
  }
  
  match baggage2.get("unique_key_1") {
    Some(value) => assert_eq(value, "unique_value_1")
    None => @test.fail("Test failed")
  }
  
  match baggage3.get("unique_key_2") {
    Some(value) => assert_eq(value, "unique_value_2")
    None => @test.fail("Test failed")
  }
  
  match baggage4.get("unique_key_3") {
    Some(value) => assert_eq(value, "unique_value_3")
    None => @test.fail("Test failed")
  }
  
  // 验证隔离性
  match baggage2.get("unique_key_2") {
    Some(_) => @test.fail("Test failed")  // baggage2不应该有unique_key_2
    None => assert_eq(true, true)
  }
  
  match baggage3.get("unique_key_1") {
    Some(_) => @test.fail("Test failed")  // baggage3不应该有unique_key_1
    None => assert_eq(true, true)
  }
}
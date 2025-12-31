// 属性验证测试
// 测试各种属性类型的验证规则和边界条件

test "attribute_value_string_validation" {
  // 测试字符串属性值的验证
  let valid_strings = [
    "simple_string",
    "string-with-dashes",
    "string_with_underscores",
    "string.with.dots",
    "string/with/slashes",
    "string\\with\\backslashes",
    "string with spaces",
    "string@with#special$characters%",
    "中文测试字符串",
    "Test with emoji 🚀",
    "",
    " ",  // 空格
    "a",  // 单字符
    "very_long_string_that_tests_length_validation_and_contains_many_characters_to_exceed_normal_limits"
  ]
  
  let mut i = 0
  while i < valid_strings.length() {
    let attr_value = common::AttributeValue::string(valid_strings[i])
    
    // 验证字符串属性创建成功
    match attr_value {
      common::StringValue(s) => assert_eq(s, valid_strings[i])
      _ => @test.fail("String attribute creation failed")
    }
    
    i = i + 1
  }
  
  // 测试字符串长度边界
  let max_length_string = "a".repeat(1000)  // 假设最大长度为1000
  let max_attr = common::AttributeValue::string(max_length_string)
  match max_attr {
    common::StringValue(s) => assert_eq(s.length(), 1000)
    _ => @test.fail("Max length string creation failed")
  }
}

test "attribute_value_numeric_validation" {
  // 测试数值属性值的验证
  let valid_ints = [
    0L,
    1L,
    -1L,
    9223372036854775807L,  // Max Int64
    -9223372036854775808L,  // Min Int64
    1234567890L,
    -1234567890L
  ]
  
  let valid_floats = [
    0.0,
    1.0,
    -1.0,
    3.14159265359,
    1.7976931348623157e308,  // Max Double
    -1.7976931348623157e308,  // Min Double
    1.0/0.0,  // Infinity
    -1.0/0.0,  // -Infinity
    0.0/0.0   // NaN
  ]
  
  // 验证整数属性
  let mut i = 0
  while i < valid_ints.length() {
    let attr_value = common::AttributeValue::int(valid_ints[i])
    match attr_value {
      common::IntValue(v) => assert_eq(v, valid_ints[i])
      _ => @test.fail("Int attribute creation failed")
    }
    i = i + 1
  }
  
  // 验证浮点数属性
  i = 0
  while i < valid_floats.length() {
    let attr_value = common::AttributeValue::float(valid_floats[i])
    match attr_value {
      common::FloatValue(v) => {
        if valid_floats[i].is_nan() {
          assert(v.is_nan())
        } else {
          assert_eq(v, valid_floats[i])
        }
      }
      _ => @test.fail("Float attribute creation failed")
    }
    i = i + 1
  }
}

test "attribute_value_boolean_validation" {
  // 测试布尔属性值的验证
  let true_attr = common::AttributeValue::bool(true)
  let false_attr = common::AttributeValue::bool(false)
  
  match true_attr {
    common::BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("True boolean attribute creation failed")
  }
  
  match false_attr {
    common::BoolValue(b) => assert_eq(b, false)
    _ => @test.fail("False boolean attribute creation failed")
  }
}

test "attribute_value_array_validation" {
  // 测试数组属性值的验证
  
  // 字符串数组
  let string_arrays = [
    [],  // 空数组
    ["single_item"],
    ["item1", "item2", "item3"],
    ["中文", "测试", "数组"],
    ["emoji", "🚀", "test"],
    ["", " ", "   "],  // 包含空字符串
    ["very_long_item_that_tests_array_item_length_validation"]
  ]
  
  let mut i = 0
  while i < string_arrays.length() {
    let attr_value = common::AttributeValue::array_string(string_arrays[i])
    match attr_value {
      common::ArrayStringValue(arr) => {
        assert_eq(arr.length(), string_arrays[i].length())
        let mut j = 0
        while j < arr.length() {
          assert_eq(arr[j], string_arrays[i][j])
          j = j + 1
        }
      }
      _ => @test.fail("String array attribute creation failed")
    }
    i = i + 1
  }
  
  // 整数数组
  let int_arrays = [
    [],
    [1L],
    [1L, 2L, 3L, 4L, 5L],
    [0L, -1L, 9223372036854775807L, -9223372036854775808L]
  ]
  
  i = 0
  while i < int_arrays.length() {
    let attr_value = common::AttributeValue::array_int(int_arrays[i])
    match attr_value {
      common::ArrayIntValue(arr) => {
        assert_eq(arr.length(), int_arrays[i].length())
        let mut j = 0
        while j < arr.length() {
          assert_eq(arr[j], int_arrays[i][j])
          j = j + 1
        }
      }
      _ => @test.fail("Int array attribute creation failed")
    }
    i = i + 1
  }
  
  // 浮点数数组
  let float_arrays = [
    [],
    [3.14],
    [0.0, 1.0, -1.0, 1.7976931348623157e308, -1.7976931348623157e308],
    [1.0/0.0, -1.0/0.0, 0.0/0.0]  // Infinity, -Infinity, NaN
  ]
  
  i = 0
  while i < float_arrays.length() {
    let attr_value = common::AttributeValue::array_float(float_arrays[i])
    match attr_value {
      common::ArrayFloatValue(arr) => {
        assert_eq(arr.length(), float_arrays[i].length())
        let mut j = 0
        while j < arr.length() {
          if float_arrays[i][j].is_nan() {
            assert(arr[j].is_nan())
          } else {
            assert_eq(arr[j], float_arrays[i][j])
          }
          j = j + 1
        }
      }
      _ => @test.fail("Float array attribute creation failed")
    }
    i = i + 1
  }
  
  // 布尔数组
  let bool_arrays = [
    [],
    [true],
    [false],
    [true, false, true, false],
    [true, true, true],
    [false, false, false]
  ]
  
  i = 0
  while i < bool_arrays.length() {
    let attr_value = common::AttributeValue::array_bool(bool_arrays[i])
    match attr_value {
      common::ArrayBoolValue(arr) => {
        assert_eq(arr.length(), bool_arrays[i].length())
        let mut j = 0
        while j < arr.length() {
          assert_eq(arr[j], bool_arrays[i][j])
          j = j + 1
        }
      }
      _ => @test.fail("Bool array attribute creation failed")
    }
    i = i + 1
  }
}

test "attribute_key_validation" {
  // 测试属性键的验证
  let valid_keys = [
    "simple_key",
    "key.with.dots",
    "key-with-dashes",
    "key_with_underscores",
    "key/with/slashes",
    "key123",
    "key_123",
    "a",  // 单字符键
    "very.very.very.very.very.very.very.very.very.very.very.very.long.key.name.that.tests.validation.rules",
    "key.with.numbers.123",
    "key.with.special.chars!@#$%^&*()",
    "中文键名",
    "key.🚀.emoji"
  ]
  
  // 测试无效键（在实际实现中可能会被拒绝）
  let potentially_invalid_keys = [
    "",  // 空键
    " ",  // 空格键
    ".starts.with.dot",
    "ends.with.dot.",
    "has..double.dots",
    "has/invalid/special\\chars",
    "has\nnewline",
    "has\ttab",
    "has\rcarriage_return"
  ]
  
  // 创建属性对来验证键
  let mut i = 0
  while i < valid_keys.length() {
    let attributes : common::Attributes = [
      (valid_keys[i], common::AttributeValue::string("test_value"))
    ]
    
    // 验证属性创建成功
    assert_eq(attributes.length(), 1)
    assert_eq(attributes[0].0, valid_keys[i])
    
    i = i + 1
  }
  
  // 测试可能无效的键（在当前实现中可能仍然被接受）
  i = 0
  while i < potentially_invalid_keys.length() {
    let attributes : common::Attributes = [
      (potentially_invalid_keys[i], common::AttributeValue::string("test_value"))
    ]
    
    // 在实际实现中，这些键可能会被验证和拒绝
    // 但在当前简化实现中，我们只验证它们可以被存储
    assert_eq(attributes.length(), 1)
    assert_eq(attributes[0].0, potentially_invalid_keys[i])
    
    i = i + 1
  }
}

test "attribute_set_validation" {
  // 测试属性集的验证
  let small_attribute_set = [
    ("key1", common::AttributeValue::string("value1")),
    ("key2", common::AttributeValue::int(123L)),
    ("key3", common::AttributeValue::float(3.14)),
    ("key4", common::AttributeValue::bool(true))
  ]
  
  let medium_attribute_set = [
    ("service.name", common::AttributeValue::string("payment-service")),
    ("service.version", common::AttributeValue::string("2.1.0")),
    ("service.namespace", common::AttributeValue::string("ecommerce")),
    ("deployment.environment", common::AttributeValue::string("production")),
    ("host.name", common::AttributeValue::string("payment-server-01")),
    ("host.ip", common::AttributeValue::string("10.0.1.100")),
    ("process.pid", common::AttributeValue::int(12345L)),
    ("process.executable.name", common::AttributeValue::string("payment-service")),
    ("process.cpu.percent", common::AttributeValue::float(75.5)),
    ("process.memory.rss", common::AttributeValue::float(512.0))
  ]
  
  // 验证小属性集
  assert_eq(small_attribute_set.length(), 4)
  let mut i = 0
  while i < small_attribute_set.length() {
    let (key, value) = small_attribute_set[i]
    assert(key.has_prefix("key"))
    match value {
      common::StringValue(_) => assert(key == "key1")
      common::IntValue(_) => assert(key == "key2")
      common::FloatValue(_) => assert(key == "key3")
      common::BoolValue(_) => assert(key == "key4")
      _ => @test.fail("Unexpected attribute value type")
    }
    i = i + 1
  }
  
  // 验证中等属性集
  assert_eq(medium_attribute_set.length(), 10)
  i = 0
  while i < medium_attribute_set.length() {
    let (key, value) = medium_attribute_set[i]
    assert(key.contains("."))
    
    // 验证特定键值对
    match key {
      "service.name" => {
        match value {
          common::StringValue(name) => assert_eq(name, "payment-service")
          _ => @test.fail("service.name should be string")
        }
      }
      "process.pid" => {
        match value {
          common::IntValue(pid) => assert_eq(pid, 12345L)
          _ => @test.fail("process.pid should be int")
        }
      }
      "process.cpu.percent" => {
        match value {
          common::FloatValue(cpu) => assert_eq(cpu, 75.5)
          _ => @test.fail("process.cpu.percent should be float")
        }
      }
      _ => ()  // 其他键不需要特殊验证
    }
    
    i = i + 1
  }
}

test "attribute_edge_cases_and_boundaries" {
  // 测试属性的边界情况和边缘条件
  
  // 空属性集
  let empty_attributes : common::Attributes = []
  assert_eq(empty_attributes.length(), 0)
  
  // 单个属性
  let single_attribute = [
    ("single.key", common::AttributeValue::string("single_value"))
  ]
  assert_eq(single_attribute.length(), 1)
  
  // 重复键（在实际实现中可能会被覆盖或拒绝）
  let duplicate_keys = [
    ("duplicate.key", common::AttributeValue::string("value1")),
    ("duplicate.key", common::AttributeValue::string("value2")),
    ("duplicate.key", common::AttributeValue::string("value3"))
  ]
  assert_eq(duplicate_keys.length(), 3)
  
  // 验证重复键的值
  assert_eq(duplicate_keys[0].1, common::StringValue("value1"))
  assert_eq(duplicate_keys[1].1, common::StringValue("value2"))
  assert_eq(duplicate_keys[2].1, common::StringValue("value3"))
  
  // 极大的属性集
  let mut large_attributes = []
  let mut i = 0
  while i < 1000 {
    large_attributes.push(("key." + i.to_string(), common::AttributeValue::string("value." + i.to_string())))
    i = i + 1
  }
  assert_eq(large_attributes.length(), 1000)
  
  // 验证大属性集中的特定元素
  assert_eq(large_attributes[0].0, "key.0")
  assert_eq(large_attributes[999].0, "key.999")
  assert_eq(large_attributes[500].0, "key.500")
  
  match large_attributes[0].1 {
    common::StringValue(value) => assert_eq(value, "value.0")
    _ => @test.fail("Test failed")
  }
  
  match large_attributes[999].1 {
    common::StringValue(value) => assert_eq(value, "value.999")
    _ => @test.fail("Test failed")
  }
}

test "attribute_type_conversion_validation" {
  // 测试属性类型转换的验证
  
  // 字符串到其他类型的转换验证
  let string_value = common::AttributeValue::string("123")
  let int_value = common::AttributeValue::int(123L)
  let float_value = common::AttributeValue::float(123.0)
  let bool_value = common::AttributeValue::bool(true)
  
  // 验证类型不匹配
  match string_value {
    common::StringValue(s) => assert_eq(s, "123")
    _ => @test.fail("Type mismatch for string value")
  }
  
  match int_value {
    common::IntValue(i) => assert_eq(i, 123L)
    _ => @test.fail("Type mismatch for int value")
  }
  
  match float_value {
    common::FloatValue(f) => assert_eq(f, 123.0)
    _ => @test.fail("Type mismatch for float value")
  }
  
  match bool_value {
    common::BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Type mismatch for bool value")
  }
  
  // 数组类型验证
  let string_array = common::AttributeValue::array_string(["1", "2", "3"])
  let int_array = common::AttributeValue::array_int([1L, 2L, 3L])
  let float_array = common::AttributeValue::array_float([1.0, 2.0, 3.0])
  let bool_array = common::AttributeValue::array_bool([true, false, true])
  
  match string_array {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "1")
      assert_eq(arr[1], "2")
      assert_eq(arr[2], "3")
    }
    _ => @test.fail("Type mismatch for string array")
  }
  
  match int_array {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1L)
      assert_eq(arr[1], 2L)
      assert_eq(arr[2], 3L)
    }
    _ => @test.fail("Type mismatch for int array")
  }
  
  match float_array {
    common::ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1.0)
      assert_eq(arr[1], 2.0)
      assert_eq(arr[2], 3.0)
    }
    _ => @test.fail("Type mismatch for float array")
  }
  
  match bool_array {
    common::ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => @test.fail("Type mismatch for bool array")
  }
}
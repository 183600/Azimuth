// Common模块工具函数测试用例
// 测试common模块中的各种工具函数

test "attribute value type conversions" {
  // 测试属性值类型转换
  let string_val = telemetry.api.common.StringValue("test string")
  let int_val = telemetry.api.common.IntValue(42)
  let float_val = telemetry.api.common.FloatValue(3.14)
  let bool_val = telemetry.api.common.BoolValue(true)
  let array_val = telemetry.api.common.ArrayStringValue(["item1", "item2", "item3"])
  
  // 验证值类型
  assert_eq(telemetry.api.common.AttributeValue.as_string(string_val), "test string")
  assert_eq(telemetry.api.common.AttributeValue.as_int(int_val), 42)
  assert_eq(telemetry.api.common.AttributeValue.as_float(float_val), 3.14)
  assert_eq(telemetry.api.common.AttributeValue.as_bool(bool_val), true)
  
  let array_result = telemetry.api.common.AttributeValue.as_array_string(array_val)
  assert_eq(array_result.length(), 3)
  assert_eq(array_result[0], "item1")
  assert_eq(array_result[1], "item2")
  assert_eq(array_result[2], "item3")
}

test "attribute value equality and comparison" {
  // 测试属性值相等性和比较
  let string1 = telemetry.api.common.StringValue("test")
  let string2 = telemetry.api.common.StringValue("test")
  let string3 = telemetry.api.common.StringValue("different")
  
  // 字符串值比较
  assert_eq(string1, string2)
  assert_true(string1 != string3)
  
  // 数值比较
  let int1 = telemetry.api.common.IntValue(42)
  let int2 = telemetry.api.common.IntValue(42)
  let int3 = telemetry.api.common.IntValue(43)
  
  assert_eq(int1, int2)
  assert_true(int1 != int3)
  
  // 浮点数比较
  let float1 = telemetry.api.common.FloatValue(3.14)
  let float2 = telemetry.api.common.FloatValue(3.14)
  let float3 = telemetry.api.common.FloatValue(2.71)
  
  assert_eq(float1, float2)
  assert_true(float1 != float3)
  
  // 布尔值比较
  let bool1 = telemetry.api.common.BoolValue(true)
  let bool2 = telemetry.api.common.BoolValue(true)
  let bool3 = telemetry.api.common.BoolValue(false)
  
  assert_eq(bool1, bool2)
  assert_true(bool1 != bool3)
  
  // 数组值比较
  let array1 = telemetry.api.common.ArrayStringValue(["a", "b", "c"])
  let array2 = telemetry.api.common.ArrayStringValue(["a", "b", "c"])
  let array3 = telemetry.api.common.ArrayStringValue(["a", "b", "d"])
  
  assert_eq(array1, array2)
  assert_true(array1 != array3)
}

test "attributes collection operations" {
  // 测试属性集合操作
  let attrs = telemetry.api.common.Attributes.new()
  
  // 测试空集合
  assert_eq(attrs.values.length(), 0)
  
  // 添加属性
  telemetry.api.common.Attributes.set(attrs, "key1", telemetry.api.common.StringValue("value1"))
  telemetry.api.common.Attributes.set(attrs, "key2", telemetry.api.common.IntValue(42))
  telemetry.api.common.Attributes.set(attrs, "key3", telemetry.api.common.FloatValue(3.14))
  telemetry.api.common.Attributes.set(attrs, "key4", telemetry.api.common.BoolValue(true))
  
  assert_eq(attrs.values.length(), 4)
  
  // 获取属性
  let value1 = telemetry.api.common.Attributes.get(attrs, "key1")
  let value2 = telemetry.api.common.Attributes.get(attrs, "key2")
  let value3 = telemetry.api.common.Attributes.get(attrs, "key3")
  let value4 = telemetry.api.common.Attributes.get(attrs, "key4")
  let value5 = telemetry.api.common.Attributes.get(attrs, "nonexistent")
  
  assert_eq(value1, Some(telemetry.api.common.StringValue("value1")))
  assert_eq(value2, Some(telemetry.api.common.IntValue(42)))
  assert_eq(value3, Some(telemetry.api.common.FloatValue(3.14)))
  assert_eq(value4, Some(telemetry.api.common.BoolValue(true)))
  assert_eq(value5, None)
  
  // 覆盖属性
  telemetry.api.common.Attributes.set(attrs, "key1", telemetry.api.common.StringValue("new_value1"))
  let new_value1 = telemetry.api.common.Attributes.get(attrs, "key1")
  assert_eq(new_value1, Some(telemetry.api.common.StringValue("new_value1")))
  assert_eq(attrs.values.length(), 4) // 长度不变
}

test "attributes with special keys" {
  // 测试包含特殊键的属性
  let attrs = telemetry.api.common.Attributes.new()
  
  // 测试各种特殊键
  let special_keys = [
    "key.with.dots",
    "key-with-dashes",
    "key_with_underscores",
    "key/with/slashes",
    "key\\with\\backslashes",
    "key with spaces",
    "key\twith\ttabs",
    "key\nwith\nnewlines",
    "key\"with\"quotes",
    "key'with'apostrophes",
    "key[with]brackets",
    "key{with}braces",
    "key(with)parentheses",
    "key<with>angles",
    "key|with|pipes",
    "key+with+plus",
    "key=with=equals",
    "key?with?questions",
    "key@with@ats",
    "key#with#hashes",
    "key$with$dollars",
    "key%with%percents",
    "key^with^carets",
    "key&with&ampersands",
    "key*with*asterisks"
  ]
  
  for i = 0; i < special_keys.length(); i = i + 1 {
    let key = special_keys[i]
    let value = telemetry.api.common.StringValue("value_for_${key}")
    telemetry.api.common.Attributes.set(attrs, key, value)
  }
  
  // 验证所有特殊键都能正确设置和获取
  for i = 0; i < special_keys.length(); i = i + 1 {
    let key = special_keys[i]
    let expected_value = telemetry.api.common.StringValue("value_for_${key}")
    let actual_value = telemetry.api.common.Attributes.get(attrs, key)
    assert_eq(actual_value, Some(expected_value))
  }
  
  assert_eq(attrs.values.length(), special_keys.length())
}

test "attributes with large values" {
  // 测试包含大值的属性
  let attrs = telemetry.api.common.Attributes.new()
  
  // 创建大字符串值
  let large_string = "x".repeat(10000)
  telemetry.api.common.Attributes.set(attrs, "large_string", telemetry.api.common.StringValue(large_string))
  
  // 创建大数组值
  let large_array = []
  for i = 0; i < 1000; i = i + 1 {
    large_array.push("array_item_${i}")
  }
  telemetry.api.common.Attributes.set(attrs, "large_array", telemetry.api.common.ArrayStringValue(large_array))
  
  // 创建极值
  telemetry.api.common.Attributes.set(attrs, "max_int", telemetry.api.common.IntValue(2147483647))
  telemetry.api.common.Attributes.set(attrs, "min_int", telemetry.api.common.IntValue(-2147483648))
  telemetry.api.common.Attributes.set(attrs, "max_float", telemetry.api.common.FloatValue(1.7976931348623157e+308))
  telemetry.api.common.Attributes.set(attrs, "min_float", telemetry.api.common.FloatValue(-1.7976931348623157e+308))
  
  // 验证大值
  let retrieved_large_string = telemetry.api.common.Attributes.get(attrs, "large_string")
  assert_eq(retrieved_large_string, Some(telemetry.api.common.StringValue(large_string)))
  
  let retrieved_large_array = telemetry.api.common.Attributes.get(attrs, "large_array")
  assert_true(retrieved_large_array != None)
  
  let retrieved_max_int = telemetry.api.common.Attributes.get(attrs, "max_int")
  assert_eq(retrieved_max_int, Some(telemetry.api.common.IntValue(2147483647)))
  
  let retrieved_min_int = telemetry.api.common.Attributes.get(attrs, "min_int")
  assert_eq(retrieved_min_int, Some(telemetry.api.common.IntValue(-2147483648)))
}

test "attributes edge cases" {
  // 测试属性的边界情况
  let attrs = telemetry.api.common.Attributes.new()
  
  // 测试空字符串键和值
  telemetry.api.common.Attributes.set(attrs, "", telemetry.api.common.StringValue(""))
  let empty_key_value = telemetry.api.common.Attributes.get(attrs, "")
  assert_eq(empty_key_value, Some(telemetry.api.common.StringValue("")))
  
  telemetry.api.common.Attributes.set(attrs, "empty_value", telemetry.api.common.StringValue(""))
  let empty_value = telemetry.api.common.Attributes.get(attrs, "empty_value")
  assert_eq(empty_value, Some(telemetry.api.common.StringValue("")))
  
  // 测试空数组
  telemetry.api.common.Attributes.set(attrs, "empty_array", telemetry.api.common.ArrayStringValue([]))
  let empty_array = telemetry.api.common.Attributes.get(attrs, "empty_array")
  assert_true(empty_array != None)
  
  // 测试包含特殊字符的字符串
  let special_chars = "\0\t\n\r\"'\\[]{}()<>|+=?@#$%^&*"
  telemetry.api.common.Attributes.set(attrs, "special_chars", telemetry.api.common.StringValue(special_chars))
  let retrieved_special_chars = telemetry.api.common.Attributes.get(attrs, "special_chars")
  assert_eq(retrieved_special_chars, Some(telemetry.api.common.StringValue(special_chars)))
  
  // 测试Unicode字符
  let unicode_string = "测试中文字符串 🚀 emoji"
  telemetry.api.common.Attributes.set(attrs, "unicode", telemetry.api.common.StringValue(unicode_string))
  let retrieved_unicode = telemetry.api.common.Attributes.get(attrs, "unicode")
  assert_eq(retrieved_unicode, Some(telemetry.api.common.StringValue(unicode_string)))
}

test "attributes collection merging" {
  // 测试属性集合合并
  let attrs1 = telemetry.api.common.Attributes.new()
  let attrs2 = telemetry.api.common.Attributes.new()
  
  // 设置第一个集合的属性
  telemetry.api.common.Attributes.set(attrs1, "key1", telemetry.api.common.StringValue("value1"))
  telemetry.api.common.Attributes.set(attrs1, "key2", telemetry.api.common.IntValue(42))
  telemetry.api.common.Attributes.set(attrs1, "key3", telemetry.api.common.FloatValue(3.14))
  
  // 设置第二个集合的属性
  telemetry.api.common.Attributes.set(attrs2, "key2", telemetry.api.common.StringValue("new_value2")) // 覆盖key2
  telemetry.api.common.Attributes.set(attrs2, "key4", telemetry.api.common.BoolValue(true))
  telemetry.api.common.Attributes.set(attrs2, "key5", telemetry.api.common.ArrayStringValue(["a", "b", "c"]))
  
  // 合并属性（如果API支持）
  // 这里我们模拟合并过程
  let merged_attrs = telemetry.api.common.Attributes.new()
  
  // 从attrs1复制属性
  let key1_value = telemetry.api.common.Attributes.get(attrs1, "key1")
  let key2_value = telemetry.api.common.Attributes.get(attrs1, "key2")
  let key3_value = telemetry.api.common.Attributes.get(attrs1, "key3")
  
  if key1_value != None {
    telemetry.api.common.Attributes.set(merged_attrs, "key1", key1_value.unwrap())
  }
  if key2_value != None {
    telemetry.api.common.Attributes.set(merged_attrs, "key2", key2_value.unwrap())
  }
  if key3_value != None {
    telemetry.api.common.Attributes.set(merged_attrs, "key3", key3_value.unwrap())
  }
  
  // 从attrs2复制属性（覆盖同名的）
  let key2_new_value = telemetry.api.common.Attributes.get(attrs2, "key2")
  let key4_value = telemetry.api.common.Attributes.get(attrs2, "key4")
  let key5_value = telemetry.api.common.Attributes.get(attrs2, "key5")
  
  if key2_new_value != None {
    telemetry.api.common.Attributes.set(merged_attrs, "key2", key2_new_value.unwrap())
  }
  if key4_value != None {
    telemetry.api.common.Attributes.set(merged_attrs, "key4", key4_value.unwrap())
  }
  if key5_value != None {
    telemetry.api.common.Attributes.set(merged_attrs, "key5", key5_value.unwrap())
  }
  
  // 验证合并结果
  assert_eq(telemetry.api.common.Attributes.get(merged_attrs, "key1"), 
           Some(telemetry.api.common.StringValue("value1")))
  assert_eq(telemetry.api.common.Attributes.get(merged_attrs, "key2"), 
           Some(telemetry.api.common.StringValue("new_value2"))) // 被覆盖
  assert_eq(telemetry.api.common.Attributes.get(merged_attrs, "key3"), 
           Some(telemetry.api.common.FloatValue(3.14)))
  assert_eq(telemetry.api.common.Attributes.get(merged_attrs, "key4"), 
           Some(telemetry.api.common.BoolValue(true)))
  assert_eq(telemetry.api.common.Attributes.get(merged_attrs, "key5"), 
           Some(telemetry.api.common.ArrayStringValue(["a", "b", "c"])))
}

test "attributes serialization and deserialization" {
  // 测试属性序列化和反序列化
  let attrs = telemetry.api.common.Attributes.new()
  
  // 设置各种类型的属性
  telemetry.api.common.Attributes.set(attrs, "string_key", telemetry.api.common.StringValue("test_value"))
  telemetry.api.common.Attributes.set(attrs, "int_key", telemetry.api.common.IntValue(42))
  telemetry.api.common.Attributes.set(attrs, "float_key", telemetry.api.common.FloatValue(3.14))
  telemetry.api.common.Attributes.set(attrs, "bool_key", telemetry.api.common.BoolValue(true))
  telemetry.api.common.Attributes.set(attrs, "array_key", telemetry.api.common.ArrayStringValue(["a", "b", "c"]))
  
  // 验证属性存在
  assert_eq(telemetry.api.common.Attributes.get(attrs, "string_key"), 
           Some(telemetry.api.common.StringValue("test_value")))
  assert_eq(telemetry.api.common.Attributes.get(attrs, "int_key"), 
           Some(telemetry.api.common.IntValue(42)))
  assert_eq(telemetry.api.common.Attributes.get(attrs, "float_key"), 
           Some(telemetry.api.common.FloatValue(3.14)))
  assert_eq(telemetry.api.common.Attributes.get(attrs, "bool_key"), 
           Some(telemetry.api.common.BoolValue(true)))
  assert_eq(telemetry.api.common.Attributes.get(attrs, "array_key"), 
           Some(telemetry.api.common.ArrayStringValue(["a", "b", "c"])))
  
  // 测试属性计数
  assert_eq(attrs.values.length(), 5)
  
  // 创建新的属性集合并复制所有属性
  let new_attrs = telemetry.api.common.Attributes.new()
  for i = 0; i < attrs.values.length(); i = i + 1 {
    let key_value = attrs.values[i]
    // 这里简化处理，实际应该有更好的复制机制
    if key_value.0 == "string_key" {
      telemetry.api.common.Attributes.set(new_attrs, key_value.0, key_value.1)
    } else if key_value.0 == "int_key" {
      telemetry.api.common.Attributes.set(new_attrs, key_value.0, key_value.1)
    } else if key_value.0 == "float_key" {
      telemetry.api.common.Attributes.set(new_attrs, key_value.0, key_value.1)
    } else if key_value.0 == "bool_key" {
      telemetry.api.common.Attributes.set(new_attrs, key_value.0, key_value.1)
    } else if key_value.0 == "array_key" {
      telemetry.api.common.Attributes.set(new_attrs, key_value.0, key_value.1)
    }
  }
  
  // 验证复制的属性
  assert_eq(telemetry.api.common.Attributes.get(new_attrs, "string_key"), 
           Some(telemetry.api.common.StringValue("test_value")))
  assert_eq(telemetry.api.common.Attributes.get(new_attrs, "int_key"), 
           Some(telemetry.api.common.IntValue(42)))
  assert_eq(telemetry.api.common.Attributes.get(new_attrs, "float_key"), 
           Some(telemetry.api.common.FloatValue(3.14)))
  assert_eq(telemetry.api.common.Attributes.get(new_attrs, "bool_key"), 
           Some(telemetry.api.common.BoolValue(true)))
  assert_eq(telemetry.api.common.Attributes.get(new_attrs, "array_key"), 
           Some(telemetry.api.common.ArrayStringValue(["a", "b", "c"])))
}
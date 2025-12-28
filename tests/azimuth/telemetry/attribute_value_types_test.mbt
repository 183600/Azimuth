// Azimuth Telemetry System - Attribute Value Type Conversion Tests
// 测试属性值类型转换功能

test "属性值类型转换和验证测试" {
  // 测试字符串属性值
  let string_attr = StringValue("test string")
  assert_eq(string_attr, StringValue("test string"))
  
  // 测试整数属性值
  let int_attr = IntValue(42)
  assert_eq(int_attr, IntValue(42))
  
  // 测试浮点数属性值
  let float_attr = FloatValue(3.14159)
  assert_eq(float_attr, FloatValue(3.14159))
  
  // 测试布尔属性值
  let bool_attr = BoolValue(true)
  assert_eq(bool_attr, BoolValue(true))
  
  let bool_attr_false = BoolValue(false)
  assert_eq(bool_attr_false, BoolValue(false))
  
  // 测试字符串数组属性值
  let string_array_attr = ArrayStringValue(["value1", "value2", "value3"])
  assert_eq(string_array_attr, ArrayStringValue(["value1", "value2", "value3"]))
  
  // 测试整数数组属性值
  let int_array_attr = ArrayIntValue([1, 2, 3, 4, 5])
  assert_eq(int_array_attr, ArrayIntValue([1, 2, 3, 4, 5]))
  
  // 测试空数组
  let empty_string_array = ArrayStringValue([])
  let empty_int_array = ArrayIntValue([])
  assert_eq(empty_string_array, ArrayStringValue([]))
  assert_eq(empty_int_array, ArrayIntValue([]))
  
  // 测试单元素数组
  let single_string_array = ArrayStringValue(["single"])
  let single_int_array = ArrayIntValue([99])
  assert_eq(single_string_array, ArrayStringValue(["single"]))
  assert_eq(single_int_array, ArrayIntValue([99]))
}

test "属性边界值和特殊值测试" {
  // 测试边界整数值
  let zero_int = IntValue(0)
  let negative_int = IntValue(-1)
  let max_small_int = IntValue(2147483647) // 2^31 - 1
  let min_small_int = IntValue(-2147483648) // -2^31
  
  assert_eq(zero_int, IntValue(0))
  assert_eq(negative_int, IntValue(-1))
  assert_eq(max_small_int, IntValue(2147483647))
  assert_eq(min_small_int, IntValue(-2147483648))
  
  // 测试特殊浮点数值
  let zero_float = FloatValue(0.0)
  let negative_float = FloatValue(-1.5)
  let small_float = FloatValue(0.000001)
  let large_float = FloatValue(999999.999999)
  
  assert_eq(zero_float, FloatValue(0.0))
  assert_eq(negative_float, FloatValue(-1.5))
  assert_eq(small_float, FloatValue(0.000001))
  assert_eq(large_float, FloatValue(999999.999999))
  
  // 测试特殊字符串值
  let empty_string = StringValue("")
  let whitespace_string = StringValue("   ")
  let unicode_string = StringValue("测试中文")
  let special_chars_string = StringValue("!@#$%^&*()")
  
  assert_eq(empty_string, StringValue(""))
  assert_eq(whitespace_string, StringValue("   "))
  assert_eq(unicode_string, StringValue("测试中文"))
  assert_eq(special_chars_string, StringValue("!@#$%^&*()"))
}

test "属性数组操作和验证测试" {
  // 测试不同长度的字符串数组
  let short_array = ArrayStringValue(["a", "b"])
  let medium_array = ArrayStringValue(["one", "two", "three", "four", "five"])
  let long_array = ArrayStringValue(["item1", "item2", "item3", "item4", "item5", "item6", "item7", "item8", "item9", "item10"])
  
  assert_eq(short_array, ArrayStringValue(["a", "b"]))
  assert_eq(medium_array, ArrayStringValue(["one", "two", "three", "four", "five"]))
  assert_eq(long_array, ArrayStringValue(["item1", "item2", "item3", "item4", "item5", "item6", "item7", "item8", "item9", "item10"]))
  
  // 测试不同类型的整数数组
  let sequential_ints = ArrayIntValue([1, 2, 3, 4, 5])
  let random_ints = ArrayIntValue([42, 17, 99, 8, 23])
  let negative_ints = ArrayIntValue([-1, -2, -3, -4, -5])
  let mixed_ints = ArrayIntValue([0, -1, 2, -3, 4])
  
  assert_eq(sequential_ints, ArrayIntValue([1, 2, 3, 4, 5]))
  assert_eq(random_ints, ArrayIntValue([42, 17, 99, 8, 23]))
  assert_eq(negative_ints, ArrayIntValue([-1, -2, -3, -4, -5]))
  assert_eq(mixed_ints, ArrayIntValue([0, -1, 2, -3, 4]))
  
  // 测试包含特殊值的数组
  let string_with_spaces = ArrayStringValue(["hello world", "  spaced  ", ""])
  let int_with_zero = ArrayIntValue([0, 1, 2, 0, 3])
  
  assert_eq(string_with_spaces, ArrayStringValue(["hello world", "  spaced  ", ""]))
  assert_eq(int_with_zero, ArrayIntValue([0, 1, 2, 0, 3]))
}

test "属性值一致性验证测试" {
  // 测试相同值的比较
  let string1 = StringValue("same")
  let string2 = StringValue("same")
  let string3 = StringValue("different")
  
  assert_eq(string1, string2)
  assert_true(string1 != string3)
  
  // 测试数值比较
  let int1 = IntValue(123)
  let int2 = IntValue(123)
  let int3 = IntValue(456)
  
  assert_eq(int1, int2)
  assert_true(int1 != int3)
  
  // 测试浮点数比较
  let float1 = FloatValue(1.23)
  let float2 = FloatValue(1.23)
  let float3 = FloatValue(4.56)
  
  assert_eq(float1, float2)
  assert_true(float1 != float3)
  
  // 测试布尔值比较
  let bool1 = BoolValue(true)
  let bool2 = BoolValue(true)
  let bool3 = BoolValue(false)
  
  assert_eq(bool1, bool2)
  assert_true(bool1 != bool3)
  
  // 测试数组比较
  let array1 = ArrayStringValue(["a", "b", "c"])
  let array2 = ArrayStringValue(["a", "b", "c"])
  let array3 = ArrayStringValue(["x", "y", "z"])
  
  assert_eq(array1, array2)
  assert_true(array1 != array3)
  
  let int_array1 = ArrayIntValue([1, 2, 3])
  let int_array2 = ArrayIntValue([1, 2, 3])
  let int_array3 = ArrayIntValue([4, 5, 6])
  
  assert_eq(int_array1, int_array2)
  assert_true(int_array1 != int_array3)
}
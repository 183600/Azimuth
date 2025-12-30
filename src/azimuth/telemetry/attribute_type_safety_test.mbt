// 属性值转换的类型安全测试
// 测试各种属性值类型的转换和类型安全性

test "attribute_string_value_type_safety" {
  // 测试字符串属性值的类型安全
  let normal_string = AttributeValue::string("normal_string")
  let empty_string = AttributeValue::string("")
  let unicode_string = AttributeValue::string("测试字符串")
  let special_chars_string = AttributeValue::string("special!@#$%^&*()")
  let very_long_string = AttributeValue::string("a" * 10000)
  let numeric_string = AttributeValue::string("12345")
  let boolean_string = AttributeValue::string("true")
  
  // 验证所有字符串值都是StringValue类型
  match normal_string {
    StringValue(_) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match empty_string {
    StringValue("") => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match unicode_string {
    StringValue("测试字符串") => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match special_chars_string {
    StringValue("special!@#$%^&*()") => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match very_long_string {
    StringValue(s) => @assertion.assert_eq(s.length(), 10000)
    _ => @assertion.assert_true(false)
  }
}

test "attribute_int_value_type_safety" {
  // 测试整数属性值的类型安全
  let zero_int = AttributeValue::int(0L)
  let positive_int = AttributeValue::int(42L)
  let negative_int = AttributeValue::int(-42L)
  let max_int = AttributeValue::int(Int64::max_value())
  let min_int = AttributeValue::int(Int64::min_value())
  
  // 验证所有整数值都是IntValue类型
  match zero_int {
    IntValue(0L) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match positive_int {
    IntValue(42L) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match negative_int {
    IntValue(-42L) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match max_int {
    IntValue(value) => @assertion.assert_eq(value, Int64::max_value())
    _ => @assertion.assert_true(false)
  }
  
  match min_int {
    IntValue(value) => @assertion.assert_eq(value, Int64::min_value())
    _ => @assertion.assert_true(false)
  }
}

test "attribute_float_value_type_safety" {
  // 测试浮点数属性值的类型安全
  let zero_float = AttributeValue::float(0.0)
  let positive_float = AttributeValue::float(3.14159)
  let negative_float = AttributeValue::float(-3.14159)
  let very_small_float = AttributeValue::float(Double::min_positive)
  let very_large_float = AttributeValue::float(Double::max_value())
  let infinity_float = AttributeValue::float(Double::infinity)
  let neg_infinity_float = AttributeValue::float(Double::neg_infinity)
  let nan_float = AttributeValue::float(Double::nan)
  
  // 验证所有浮点数值都是FloatValue类型
  match zero_float {
    FloatValue(0.0) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match positive_float {
    FloatValue(3.14159) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match negative_float {
    FloatValue(-3.14159) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match infinity_float {
    FloatValue(value) => @assertion.assert_true(value.is_infinite() && value > 0.0)
    _ => @assertion.assert_true(false)
  }
  
  match nan_float {
    FloatValue(value) => @assertion.assert_true(value.is_nan())
    _ => @assertion.assert_true(false)
  }
}

test "attribute_bool_value_type_safety" {
  // 测试布尔属性值的类型安全
  let true_value = AttributeValue::bool(true)
  let false_value = AttributeValue::bool(false)
  
  // 验证所有布尔值都是BoolValue类型
  match true_value {
    BoolValue(true) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match false_value {
    BoolValue(false) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
}

test "attribute_array_string_type_safety" {
  // 测试字符串数组属性值的类型安全
  let empty_array = AttributeValue::array_string([])
  let single_element_array = AttributeValue::array_string(["single"])
  let multi_element_array = AttributeValue::array_string(["a", "b", "c"])
  let unicode_array = AttributeValue::array_string(["测试", "字符串", "数组"])
  let special_chars_array = AttributeValue::array_string(["!@#", "$%^", "&*()"])
  let large_array = AttributeValue::array_string(Array::range_with(0, 1000, 1).map(fn(i) { "item_" + i.to_string() }))
  
  // 验证所有字符串数组都是ArrayStringValue类型
  match empty_array {
    ArrayStringValue(arr) => @assertion.assert_eq(arr.length(), 0)
    _ => @assertion.assert_true(false)
  }
  
  match single_element_array {
    ArrayStringValue(arr) => @assertion.assert_eq(arr.length(), 1)
    _ => @assertion.assert_true(false)
  }
  
  match multi_element_array {
    ArrayStringValue(["a", "b", "c"]) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match large_array {
    ArrayStringValue(arr) => @assertion.assert_eq(arr.length(), 1000)
    _ => @assertion.assert_true(false)
  }
}

test "attribute_array_int_type_safety" {
  // 测试整数数组属性值的类型安全
  let empty_int_array = AttributeValue::array_int([])
  let single_int_array = AttributeValue::array_int([42L])
  let multi_int_array = AttributeValue::array_int([1L, 2L, 3L, 4L, 5L])
  let negative_int_array = AttributeValue::array_int([-1L, -2L, -3L])
  let mixed_int_array = AttributeValue::array_int([Int64::min_value(), 0L, Int64::max_value()])
  let large_int_array = AttributeValue::array_int(Array::range_with(0, 1000, 1).map(fn(i) { i.to_int64() }))
  
  // 验证所有整数数组都是ArrayIntValue类型
  match empty_int_array {
    ArrayIntValue(arr) => @assertion.assert_eq(arr.length(), 0)
    _ => @assertion.assert_true(false)
  }
  
  match single_int_array {
    ArrayIntValue(arr) => @assertion.assert_eq(arr.length(), 1)
    _ => @assertion.assert_true(false)
  }
  
  match multi_int_array {
    ArrayIntValue([1L, 2L, 3L, 4L, 5L]) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match large_int_array {
    ArrayIntValue(arr) => @assertion.assert_eq(arr.length(), 1000)
    _ => @assertion.assert_true(false)
  }
}

test "attribute_array_float_type_safety" {
  // 测试浮点数数组属性值的类型安全
  let empty_float_array = AttributeValue::array_float([])
  let single_float_array = AttributeValue::array_float([3.14])
  let multi_float_array = AttributeValue::array_float([1.1, 2.2, 3.3])
  let special_float_array = AttributeValue::array_float([Double::infinity, Double::neg_infinity, Double::nan])
  let scientific_float_array = AttributeValue::array_float([1.23e-10, 1.23e10, 0.0])
  let large_float_array = AttributeValue::array_float(Array::range_with(0, 1000, 1).map(fn(i) { (i + 1).to_double() * 1.1 }))
  
  // 验证所有浮点数数组都是ArrayFloatValue类型
  match empty_float_array {
    ArrayFloatValue(arr) => @assertion.assert_eq(arr.length(), 0)
    _ => @assertion.assert_true(false)
  }
  
  match single_float_array {
    ArrayFloatValue(arr) => @assertion.assert_eq(arr.length(), 1)
    _ => @assertion.assert_true(false)
  }
  
  match multi_float_array {
    ArrayFloatValue([1.1, 2.2, 3.3]) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match large_float_array {
    ArrayFloatValue(arr) => @assertion.assert_eq(arr.length(), 1000)
    _ => @assertion.assert_true(false)
  }
}

test "attribute_array_bool_type_safety" {
  // 测试布尔数组属性值的类型安全
  let empty_bool_array = AttributeValue::array_bool([])
  let single_bool_array = AttributeValue::array_bool([true])
  let multi_bool_array = AttributeValue::array_bool([true, false, true, false])
  let all_true_array = AttributeValue::array_bool([true, true, true, true, true])
  let all_false_array = AttributeValue::array_bool([false, false, false, false, false])
  let large_bool_array = AttributeValue::array_bool(Array::range_with(0, 1000, 1).map(fn(i) { i % 2 == 0 }))
  
  // 验证所有布尔数组都是ArrayBoolValue类型
  match empty_bool_array {
    ArrayBoolValue(arr) => @assertion.assert_eq(arr.length(), 0)
    _ => @assertion.assert_true(false)
  }
  
  match single_bool_array {
    ArrayBoolValue(arr) => @assertion.assert_eq(arr.length(), 1)
    _ => @assertion.assert_true(false)
  }
  
  match multi_bool_array {
    ArrayBoolValue([true, false, true, false]) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match large_bool_array {
    ArrayBoolValue(arr) => @assertion.assert_eq(arr.length(), 1000)
    _ => @assertion.assert_true(false)
  }
}

test "attribute_mixed_type_collection_safety" {
  // 测试混合类型属性集合的类型安全
  let mixed_attributes = [
    ("string_attr", AttributeValue::string("string_value")),
    ("int_attr", AttributeValue::int(42L)),
    ("float_attr", AttributeValue::float(3.14)),
    ("bool_attr", AttributeValue::bool(true)),
    ("array_string_attr", AttributeValue::array_string(["a", "b", "c"])),
    ("array_int_attr", AttributeValue::array_int([1L, 2L, 3L])),
    ("array_float_attr", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("array_bool_attr", AttributeValue::array_bool([true, false, true]))
  ]
  
  // 验证混合类型集合的完整性
  @assertion.assert_eq(mixed_attributes.length(), 8)
  
  // 验证每种类型的属性都存在
  let type_counts = mixed_attributes.map(fn(attr) {
    match attr[1] {
      StringValue(_) => "string"
      IntValue(_) => "int"
      FloatValue(_) => "float"
      BoolValue(_) => "bool"
      ArrayStringValue(_) => "array_string"
      ArrayIntValue(_) => "array_int"
      ArrayFloatValue(_) => "array_float"
      ArrayBoolValue(_) => "array_bool"
    }
  }).to_set()
  
  @assertion.assert_eq(type_counts.size(), 8)
}

test "attribute_type_conversion_edge_cases" {
  // 测试属性值类型转换的边界情况
  let numeric_string = AttributeValue::string("123")
  let boolean_string = AttributeValue::string("true")
  let float_string = AttributeValue::string("3.14")
  let empty_string_value = AttributeValue::string("")
  let whitespace_string = AttributeValue::string("   ")
  
  // 验证这些值保持为字符串类型，不自动转换
  match numeric_string {
    StringValue("123") => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match boolean_string {
    StringValue("true") => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match float_string {
    StringValue("3.14") => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match empty_string_value {
    StringValue("") => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match whitespace_string {
    StringValue("   ") => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
}

test "attribute_large_payload_type_safety" {
  // 测试大负载属性值的类型安全性
  let very_long_string = "a" * 100000
  let very_large_string_array = Array::range_with(0, 10000, 1).map(fn(i) { "item_" + i.to_string() })
  let very_large_int_array = Array::range_with(0, 10000, 1).map(fn(i) { i.to_int64() })
  let very_large_float_array = Array::range_with(0, 10000, 1).map(fn(i) { i.to_double() })
  let very_large_bool_array = Array::range_with(0, 10000, 1).map(fn(i) { i % 2 == 0 })
  
  let large_string_attr = AttributeValue::string(very_long_string)
  let large_string_array_attr = AttributeValue::array_string(very_large_string_array)
  let large_int_array_attr = AttributeValue::array_int(very_large_int_array)
  let large_float_array_attr = AttributeValue::array_float(very_large_float_array)
  let large_bool_array_attr = AttributeValue::array_bool(very_large_bool_array)
  
  // 验证大负载属性保持正确的类型
  match large_string_attr {
    StringValue(s) => @assertion.assert_eq(s.length(), 100000)
    _ => @assertion.assert_true(false)
  }
  
  match large_string_array_attr {
    ArrayStringValue(arr) => @assertion.assert_eq(arr.length(), 10000)
    _ => @assertion.assert_true(false)
  }
  
  match large_int_array_attr {
    ArrayIntValue(arr) => @assertion.assert_eq(arr.length(), 10000)
    _ => @assertion.assert_true(false)
  }
  
  match large_float_array_attr {
    ArrayFloatValue(arr) => @assertion.assert_eq(arr.length(), 10000)
    _ => @assertion.assert_true(false)
  }
  
  match large_bool_array_attr {
    ArrayBoolValue(arr) => @assertion.assert_eq(arr.length(), 10000)
    _ => @assertion.assert_true(false)
  }
}
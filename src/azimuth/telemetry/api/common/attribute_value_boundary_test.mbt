// AttributeValue类型转换边界测试用例
// 测试各种边界情况、特殊值和类型转换

test "attribute_value_string_boundary_cases" {
  // 测试字符串类型的边界情况
  
  // 空字符串
  let empty_string = AttributeValue::string("")
  match empty_string {
    StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Expected StringValue")
  }
  
  // 单字符字符串
  let single_char = AttributeValue::string("a")
  match single_char {
    StringValue(s) => assert_eq(s, "a")
    _ => @test.fail("Expected StringValue")
  }
  
  // 包含特殊字符的字符串
  let special_chars = AttributeValue::string("!@#$%^&*()_+-=[]{}|;':\",./<>?")
  match special_chars {
    StringValue(s) => assert_eq(s, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
    _ => @test.fail("Expected StringValue")
  }
  
  // Unicode字符串
  let unicode_string = AttributeValue::string("测试中文字符串🚀🌟💫")
  match unicode_string {
    StringValue(s) => assert_eq(s, "测试中文字符串🚀🌟💫")
    _ => @test.fail("Expected StringValue")
  }
  
  // 仅包含空格的字符串
  let spaces_only = AttributeValue::string("   ")
  match spaces_only {
    StringValue(s) => assert_eq(s, "   ")
    _ => @test.fail("Expected StringValue")
  }
  
  // 包含换行符的字符串
  let with_newlines = AttributeValue::string("line1\nline2\r\nline3")
  match with_newlines {
    StringValue(s) => assert_eq(s, "line1\nline2\r\nline3")
    _ => @test.fail("Expected StringValue")
  }
  
  // 包含制表符的字符串
  let with_tabs = AttributeValue::string("col1\tcol2\tcol3")
  match with_tabs {
    StringValue(s) => assert_eq(s, "col1\tcol2\tcol3")
    _ => @test.fail("Expected StringValue")
  }
  
  // 非常长的字符串
  let long_string = "a".repeat(10000)
  let very_long = AttributeValue::string(long_string)
  match very_long {
    StringValue(s) => assert_eq(s.length(), 10000)
    _ => @test.fail("Expected StringValue")
  }
}

test "attribute_value_numeric_boundary_cases" {
  // 测试数值类型的边界情况
  
  // 整数边界值
  let max_int = AttributeValue::int(9223372036854775807L)  // Int64最大值
  match max_int {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Expected IntValue")
  }
  
  let min_int = AttributeValue::int(-9223372036854775808L)  // Int64最小值
  match min_int {
    IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Expected IntValue")
  }
  
  let zero_int = AttributeValue::int(0L)
  match zero_int {
    IntValue(i) => assert_eq(i, 0L)
    _ => @test.fail("Expected IntValue")
  }
  
  // 特殊整数值
  let one_int = AttributeValue::int(1L)
  let neg_one_int = AttributeValue::int(-1L)
  let large_positive = AttributeValue::int(1000000000000L)
  let large_negative = AttributeValue::int(-1000000000000L)
  
  match one_int {
    IntValue(i) => assert_eq(i, 1L)
    _ => @test.fail("Expected IntValue")
  }
  
  match neg_one_int {
    IntValue(i) => assert_eq(i, -1L)
    _ => @test.fail("Expected IntValue")
  }
  
  // 浮点数边界值
  let max_float = AttributeValue::float(1.7976931348623157e+308)  // Double最大值
  match max_float {
    FloatValue(f) => assert_eq(f, 1.7976931348623157e+308)
    _ => @test.fail("Expected FloatValue")
  }
  
  let min_float = AttributeValue::float(-1.7976931348623157e+308)  // Double最小值
  match min_float {
    FloatValue(f) => assert_eq(f, -1.7976931348623157e+308)
    _ => @test.fail("Expected FloatValue")
  }
  
  let zero_float = AttributeValue::float(0.0)
  match zero_float {
    FloatValue(f) => assert_eq(f, 0.0)
    _ => @test.fail("Expected FloatValue")
  }
  
  // 特殊浮点值
  let positive_infinity = AttributeValue::float(1.0/0.0)
  let negative_infinity = AttributeValue::float(-1.0/0.0)
  let nan_value = AttributeValue::float(0.0/0.0)
  
  match positive_infinity {
    FloatValue(f) => assert_eq(f, 1.0/0.0)  // 正无穷
    _ => @test.fail("Expected FloatValue")
  }
  
  match negative_infinity {
    FloatValue(f) => assert_eq(f, -1.0/0.0)  // 负无穷
    _ => @test.fail("Expected FloatValue")
  }
  
  match nan_value {
    FloatValue(f) => assert_eq(f, 0.0/0.0)  // NaN
    _ => @test.fail("Expected FloatValue")
  }
  
  // 极小浮点值
  let very_small_positive = AttributeValue::float(1.0e-308)
  let very_small_negative = AttributeValue::float(-1.0e-308)
  
  match very_small_positive {
    FloatValue(f) => assert_eq(f, 1.0e-308)
    _ => @test.fail("Expected FloatValue")
  }
  
  match very_small_negative {
    FloatValue(f) => assert_eq(f, -1.0e-308)
    _ => @test.fail("Expected FloatValue")
  }
}

test "attribute_value_boolean_boundary_cases" {
  // 测试布尔类型的边界情况
  
  let true_value = AttributeValue::bool(true)
  match true_value {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  let false_value = AttributeValue::bool(false)
  match false_value {
    BoolValue(b) => assert_eq(b, false)
    _ => @test.fail("Expected BoolValue")
  }
}

test "attribute_value_array_boundary_cases" {
  // 测试数组类型的边界情况
  
  // 空数组
  let empty_string_array = AttributeValue::array_string([])
  let empty_int_array = AttributeValue::array_int([])
  let empty_float_array = AttributeValue::array_float([])
  let empty_bool_array = AttributeValue::array_bool([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match empty_float_array {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match empty_bool_array {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayBoolValue")
  }
  
  // 单元素数组
  let single_string_array = AttributeValue::array_string(["single"])
  let single_int_array = AttributeValue::array_int([42L])
  let single_float_array = AttributeValue::array_float([3.14])
  let single_bool_array = AttributeValue::array_bool([true])
  
  match single_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], "single")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match single_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 42L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match single_float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 3.14)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match single_bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], true)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
  
  // 包含特殊值的数组
  let special_string_array = AttributeValue::array_string(["", " ", "\n\t", "测试🚀", "!@#$%"])
  let special_int_array = AttributeValue::array_int([0L, -1L, 9223372036854775807L, -9223372036854775808L])
  let special_float_array = AttributeValue::array_float([0.0, -0.0, 1.0/0.0, -1.0/0.0, 0.0/0.0])
  let special_bool_array = AttributeValue::array_bool([true, false, true, false])
  
  match special_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], "")
      assert_eq(arr[1], " ")
      assert_eq(arr[2], "\n\t")
      assert_eq(arr[3], "测试🚀")
      assert_eq(arr[4], "!@#$%")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match special_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], 0L)
      assert_eq(arr[1], -1L)
      assert_eq(arr[2], 9223372036854775807L)
      assert_eq(arr[3], -9223372036854775808L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match special_float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 0.0)
      assert_eq(arr[1], -0.0)
      assert_eq(arr[2], 1.0/0.0)
      assert_eq(arr[3], -1.0/0.0)
      assert_eq(arr[4], 0.0/0.0)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match special_bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 4)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
      assert_eq(arr[3], false)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
  
  // 大型数组
  let mut large_string_array : Array[String] = []
  let mut large_int_array : Array[Int64] = []
  let mut large_float_array : Array[Double] = []
  let mut large_bool_array : Array[Bool] = []
  
  let mut i = 0
  while i < 1000 {
    large_string_array.push("item." + i.to_string())
    large_int_array.push(i.to_int64())
    large_float_array.push(i.to_double() * 1.5)
    large_bool_array.push(i % 2 == 0)
    i = i + 1
  }
  
  let large_string_attr = AttributeValue::array_string(large_string_array)
  let large_int_attr = AttributeValue::array_int(large_int_array)
  let large_float_attr = AttributeValue::array_float(large_float_array)
  let large_bool_attr = AttributeValue::array_bool(large_bool_array)
  
  match large_string_attr {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 1000)
      assert_eq(arr[0], "item.0")
      assert_eq(arr[999], "item.999")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match large_int_attr {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 1000)
      assert_eq(arr[0], 0L)
      assert_eq(arr[999], 999L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match large_float_attr {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 1000)
      assert_eq(arr[0], 0.0)
      assert_eq(arr[999], 1498.5)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match large_bool_attr {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 1000)
      assert_eq(arr[0], true)
      assert_eq(arr[999], false)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
}

test "attribute_value_type_consistency" {
  // 测试AttributeValue类型一致性
  
  // 创建所有类型的AttributeValue
  let string_val = AttributeValue::string("test")
  let int_val = AttributeValue::int(42L)
  let float_val = AttributeValue::float(3.14)
  let bool_val = AttributeValue::bool(true)
  let string_array_val = AttributeValue::array_string(["a", "b"])
  let int_array_val = AttributeValue::array_int([1L, 2L])
  let float_array_val = AttributeValue::array_float([1.1, 2.2])
  let bool_array_val = AttributeValue::array_bool([true, false])
  
  // 验证每个值的类型
  match string_val {
    StringValue(_) => assert_eq(true, true)
    _ => @test.fail("Expected StringValue")
  }
  
  match int_val {
    IntValue(_) => assert_eq(true, true)
    _ => @test.fail("Expected IntValue")
  }
  
  match float_val {
    FloatValue(_) => assert_eq(true, true)
    _ => @test.fail("Expected FloatValue")
  }
  
  match bool_val {
    BoolValue(_) => assert_eq(true, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  match string_array_val {
    ArrayStringValue(_) => assert_eq(true, true)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match int_array_val {
    ArrayIntValue(_) => assert_eq(true, true)
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match float_array_val {
    ArrayFloatValue(_) => assert_eq(true, true)
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match bool_array_val {
    ArrayBoolValue(_) => assert_eq(true, true)
    _ => @test.fail("Expected ArrayBoolValue")
  }
}

test "attribute_value_conversion_edge_cases" {
  // 测试AttributeValue转换的边界情况
  
  // 数值精度测试
  let precise_float = AttributeValue::float(123456789.987654321)
  match precise_float {
    FloatValue(f) => {
      // 测试浮点数精度保持
      assert_eq(f > 123456789.9876, true)
      assert_eq(f < 123456790.0, true)
    }
    _ => @test.fail("Expected FloatValue")
  }
  
  // 科学计数法
  let scientific_float = AttributeValue::float(1.23e-10)
  match scientific_float {
    FloatValue(f) => assert_eq(f, 1.23e-10)
    _ => @test.fail("Expected FloatValue")
  }
  
  let large_scientific_float = AttributeValue::float(1.23e+20)
  match large_scientific_float {
    FloatValue(f) => assert_eq(f, 1.23e+20)
    _ => @test.fail("Expected FloatValue")
  }
  
  // 负零测试
  let negative_zero = AttributeValue::float(-0.0)
  match negative_zero {
    FloatValue(f) => assert_eq(f, -0.0)
    _ => @test.fail("Expected FloatValue")
  }
  
  // 混合类型数组测试
  let mixed_string_array = AttributeValue::array_string(["", "normal", "123", "3.14", "true", "测试", "🚀", "!@#$"])
  match mixed_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 8)
      assert_eq(arr[0], "")
      assert_eq(arr[1], "normal")
      assert_eq(arr[2], "123")
      assert_eq(arr[3], "3.14")
      assert_eq(arr[4], "true")
      assert_eq(arr[5], "测试")
      assert_eq(arr[6], "🚀")
      assert_eq(arr[7], "!@#$")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 混合数值数组
  let mixed_int_array = AttributeValue::array_int([0L, -1L, 1L, 123456789L, -987654321L])
  match mixed_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 0L)
      assert_eq(arr[1], -1L)
      assert_eq(arr[2], 1L)
      assert_eq(arr[3], 123456789L)
      assert_eq(arr[4], -987654321L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  // 混合浮点数组
  let mixed_float_array = AttributeValue::array_float([0.0, -0.0, 1.0, -1.0, 3.14159, -2.71828, 1.0/0.0, -1.0/0.0, 0.0/0.0])
  match mixed_float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 9)
      assert_eq(arr[0], 0.0)
      assert_eq(arr[1], -0.0)
      assert_eq(arr[2], 1.0)
      assert_eq(arr[3], -1.0)
      assert_eq(arr[4], 3.14159)
      assert_eq(arr[5], -2.71828)
      assert_eq(arr[6], 1.0/0.0)
      assert_eq(arr[7], -1.0/0.0)
      assert_eq(arr[8], 0.0/0.0)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
}
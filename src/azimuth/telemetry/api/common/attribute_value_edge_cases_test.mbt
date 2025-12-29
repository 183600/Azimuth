// AttributeValue边界情况的测试用例
// 测试AttributeValue在边界条件下的行为

test "attribute_value_extreme_string_values" {
  // 测试极端字符串值
  
  // 空字符串
  let empty_string = AttributeValue::string("")
  match empty_string {
    StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Test failed")
  }
  
  // 单字符字符串
  let single_char = AttributeValue::string("a")
  match single_char {
    StringValue(s) => assert_eq(s, "a")
    _ => @test.fail("Test failed")
  }
  
  // 包含特殊字符的字符串
  let special_chars = AttributeValue::string("!@#$%^&*()_+-=[]{}|;':\",./<>?")
  match special_chars {
    StringValue(s) => assert_eq(s, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
    _ => @test.fail("Test failed")
  }
  
  // Unicode字符串
  let unicode = AttributeValue::string("测试中文 🚀 ñáéíóú")
  match unicode {
    StringValue(s) => assert_eq(s, "测试中文 🚀 ñáéíóú")
    _ => @test.fail("Test failed")
  }
  
  // 包含空格的字符串
  let spaces = AttributeValue::string("   leading and trailing spaces   ")
  match spaces {
    StringValue(s) => assert_eq(s, "   leading and trailing spaces   ")
    _ => @test.fail("Test failed")
  }
  
  // 包含换行符的字符串
  let newlines = AttributeValue::string("line1\nline2\r\nline3")
  match newlines {
    StringValue(s) => assert_eq(s, "line1\nline2\r\nline3")
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_extreme_numeric_values" {
  // 测试极端数值
  
  // 零值
  let zero_int = AttributeValue::int(0L)
  let zero_float = AttributeValue::float(0.0)
  match zero_int {
    IntValue(i) => assert_eq(i, 0L)
    _ => @test.fail("Test failed")
  }
  match zero_float {
    FloatValue(f) => assert_eq(f, 0.0)
    _ => @test.fail("Test failed")
  }
  
  // 极大值
  let max_int = AttributeValue::int(9223372036854775807L) // Int64最大值
  let max_float = AttributeValue::float(1.7976931348623157e+308) // Double最大值
  match max_int {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Test failed")
  }
  match max_float {
    FloatValue(f) => assert_eq(f, 1.7976931348623157e+308)
    _ => @test.fail("Test failed")
  }
  
  // 极小值
  let min_int = AttributeValue::int(-9223372036854775808L) // Int64最小值
  let min_float = AttributeValue::float(-1.7976931348623157e+308) // Double最小值
  match min_int {
    IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Test failed")
  }
  match min_float {
    FloatValue(f) => assert_eq(f, -1.7976931348623157e+308)
    _ => @test.fail("Test failed")
  }
  
  // 特殊浮点值
  let infinity = AttributeValue::float(1.0/0.0) // 正无穷
  let neg_infinity = AttributeValue::float(-1.0/0.0) // 负无穷
  let nan = AttributeValue::float(0.0/0.0) // NaN
  
  match infinity {
    FloatValue(f) => assert_eq(f, 1.0/0.0)
    _ => @test.fail("Test failed")
  }
  match neg_infinity {
    FloatValue(f) => assert_eq(f, -1.0/0.0)
    _ => @test.fail("Test failed")
  }
  match nan {
    FloatValue(f) => assert_eq(f, 0.0/0.0)
    _ => @test.fail("Test failed")
  }
  
  // 极小浮点数
  let tiny_float = AttributeValue::float(1.0e-308)
  match tiny_float {
    FloatValue(f) => assert_eq(f, 1.0e-308)
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_array_edge_cases" {
  // 测试数组边界情况
  
  // 空数组
  let empty_string_array = AttributeValue::array_string([])
  let empty_int_array = AttributeValue::array_int([])
  let empty_float_array = AttributeValue::array_float([])
  let empty_bool_array = AttributeValue::array_bool([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  match empty_float_array {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
  }
  match empty_bool_array {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Test failed")
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
    _ => @test.fail("Test failed")
  }
  match single_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 42L)
    }
    _ => @test.fail("Test failed")
  }
  
  // 包含特殊值的数组
  let special_string_array = AttributeValue::array_string(["", " ", "\n\t", "null", "undefined"])
  let special_int_array = AttributeValue::array_int([0L, -1L, 9223372036854775807L, -9223372036854775808L])
  let special_float_array = AttributeValue::array_float([0.0, -0.0, 1.0/0.0, -1.0/0.0, 0.0/0.0])
  let special_bool_array = AttributeValue::array_bool([true, false])
  
  match special_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], "")
      assert_eq(arr[1], " ")
      assert_eq(arr[2], "\n\t")
    }
    _ => @test.fail("Test failed")
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
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_consistency_operations" {
  // 测试AttributeValue的一致性操作
  
  // 创建所有类型的AttributeValue
  let string_attr = AttributeValue::string("test")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14)
  let bool_attr = AttributeValue::bool(true)
  let string_array_attr = AttributeValue::array_string(["a", "b", "c"])
  let int_array_attr = AttributeValue::array_int([1L, 2L, 3L])
  let float_array_attr = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_attr = AttributeValue::array_bool([true, false, true])
  
  // 验证类型一致性
  let is_string = match string_attr {
    StringValue(_) => true
    _ => false
  }
  let is_int = match int_attr {
    IntValue(_) => true
    _ => false
  }
  let is_float = match float_attr {
    FloatValue(_) => true
    _ => false
  }
  let is_bool = match bool_attr {
    BoolValue(_) => true
    _ => false
  }
  let is_string_array = match string_array_attr {
    ArrayStringValue(_) => true
    _ => false
  }
  let is_int_array = match int_array_attr {
    ArrayIntValue(_) => true
    _ => false
  }
  let is_float_array = match float_array_attr {
    ArrayFloatValue(_) => true
    _ => false
  }
  let is_bool_array = match bool_array_attr {
    ArrayBoolValue(_) => true
    _ => false
  }
  
  assert_eq(is_string, true)
  assert_eq(is_int, true)
  assert_eq(is_float, true)
  assert_eq(is_bool, true)
  assert_eq(is_string_array, true)
  assert_eq(is_int_array, true)
  assert_eq(is_float_array, true)
  assert_eq(is_bool_array, true)
}

test "attribute_value_pattern_matching" {
  // 测试AttributeValue的模式匹配
  
  let attributes = [
    ("string.key", AttributeValue::string("string.value")),
    ("int.key", AttributeValue::int(123L)),
    ("float.key", AttributeValue::float(456.789)),
    ("bool.key", AttributeValue::bool(true)),
    ("string.array.key", AttributeValue::array_string(["x", "y", "z"])),
    ("int.array.key", AttributeValue::array_int([10L, 20L, 30L]))
  ]
  
  let mut string_count = 0
  let mut int_count = 0
  let mut float_count = 0
  let mut bool_count = 0
  let mut array_count = 0
  
  let mut i = 0
  while i < attributes.length() {
    let (_, attr_value) = attributes[i]
    
    match attr_value {
      StringValue(_) => string_count = string_count + 1,
      IntValue(_) => int_count = int_count + 1,
      FloatValue(_) => float_count = float_count + 1,
      BoolValue(_) => bool_count = bool_count + 1,
      ArrayStringValue(_) | ArrayIntValue(_) | ArrayFloatValue(_) | ArrayBoolValue(_) => array_count = array_count + 1
    }
    
    i = i + 1
  }
  
  assert_eq(string_count, 1)
  assert_eq(int_count, 1)
  assert_eq(float_count, 1)
  assert_eq(bool_count, 1)
  assert_eq(array_count, 2)
}

test "attribute_value_nested_structures" {
  // 测试嵌套结构（通过字符串表示）
  
  // JSON字符串
  let json_string = AttributeValue::string("{\"key\": \"value\", \"number\": 42, \"array\": [1, 2, 3]}")
  match json_string {
    StringValue(s) => {
      assert_eq(s.contains("\"key\""), true)
      assert_eq(s.contains("42"), true)
      assert_eq(s.contains("[1, 2, 3]"), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // XML字符串
  let xml_string = AttributeValue::string("<root><child attribute=\"value\">text</child></root>")
  match xml_string {
    StringValue(s) => {
      assert_eq(s.contains("<root>"), true)
      assert_eq(s.contains("<child"), true)
      assert_eq(s.contains("attribute=\"value\""), true)
    }
    _ => @test.fail("Test failed")
  }
  
  // Base64编码的数据
  let base64_string = AttributeValue::string("SGVsbG8gV29ybGQ=") // "Hello World"的Base64编码
  match base64_string {
    StringValue(s) => assert_eq(s, "SGVsbG8gV29ybGQ=")
    _ => @test.fail("Test failed")
  }
  
  // CSV字符串
  let csv_string = AttributeValue::string("name,age,city\nJohn,30,New York\nJane,25,London")
  match csv_string {
    StringValue(s) => {
      assert_eq(s.contains("name,age,city"), true)
      assert_eq(s.contains("John,30"), true)
    }
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_large_data_handling" {
  // 测试大数据处理
  
  // 大字符串
  let large_string_content = "x".repeat(1000)
  let large_string = AttributeValue::string(large_string_content)
  match large_string {
    StringValue(s) => assert_eq(s.length(), 1000)
    _ => @test.fail("Test failed")
  }
  
  // 大数组
  let mut large_string_array : Array[String] = []
  let mut i = 0
  while i < 100 {
    large_string_array.push("item." + i.to_string())
    i = i + 1
  }
  let large_array = AttributeValue::array_string(large_string_array)
  match large_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 100)
    _ => @test.fail("Test failed")
  }
  
  // 数值精度测试
  let precise_float = AttributeValue::float(3.14159265358979323846264338327950288419716939937510)
  match precise_float {
    FloatValue(f) => {
      // 验证精度（注意：浮点数精度有限）
      assert_eq(f > 3.14, true)
      assert_eq(f < 3.15, true)
    }
    _ => @test.fail("Test failed")
  }
}
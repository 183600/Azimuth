test "AttributeValue type conversion and boundary test" {
  // 测试字符串值
  let string_val = AttributeValue::string("test")
  match string_val {
    StringValue(s) => assert_eq(s, "test")
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试整数值
  let int_val = AttributeValue::int(42L)
  match int_val {
    IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  // 测试浮点数值
  let float_val = AttributeValue::float(3.14)
  match float_val {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Expected FloatValue")
  }
  
  // 测试布尔值
  let bool_val = AttributeValue::bool(true)
  match bool_val {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  // 测试字符串数组
  let string_array = ["a", "b", "c"]
  let string_array_val = AttributeValue::array_string(string_array)
  match string_array_val {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 测试整数数组
  let int_array = [1L, 2L, 3L]
  let int_array_val = AttributeValue::array_int(int_array)
  match int_array_val {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1L)
      assert_eq(arr[1], 2L)
      assert_eq(arr[2], 3L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  // 测试浮点数数组
  let float_array = [1.1, 2.2, 3.3]
  let float_array_val = AttributeValue::array_float(float_array)
  match float_array_val {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[1], 2.2)
      assert_eq(arr[2], 3.3)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  // 测试布尔数组
  let bool_array = [true, false, true]
  let bool_array_val = AttributeValue::array_bool(bool_array)
  match bool_array_val {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[2], true)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
  
  // 测试边界值
  let max_int = AttributeValue::int(9223372036854775807L)  // Int64最大值
  let min_int = AttributeValue::int(-9223372036854775808L) // Int64最小值
  let max_float = AttributeValue::float(1.7976931348623157e+308) // Double最大值
  let min_float = AttributeValue::float(-1.7976931348623157e+308) // Double最小值
  
  match max_int {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Expected IntValue for max int")
  }
  
  match min_int {
    IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Expected IntValue for min int")
  }
  
  match max_float {
    FloatValue(f) => assert_eq(f, 1.7976931348623157e+308)
    _ => @test.fail("Expected FloatValue for max float")
  }
  
  match min_float {
    FloatValue(f) => assert_eq(f, -1.7976931348623157e+308)
    _ => @test.fail("Expected FloatValue for min float")
  }
  
  // 测试空数组
  let empty_string_array = AttributeValue::array_string([])
  let empty_int_array = AttributeValue::array_int([])
  let empty_float_array = AttributeValue::array_float([])
  let empty_bool_array = AttributeValue::array_bool([])
  
  match empty_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty ArrayStringValue")
  }
  
  match empty_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty ArrayIntValue")
  }
  
  match empty_float_array {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty ArrayFloatValue")
  }
  
  match empty_bool_array {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected empty ArrayBoolValue")
  }
}
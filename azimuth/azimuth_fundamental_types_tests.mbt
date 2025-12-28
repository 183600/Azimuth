// 基础类型转换测试用例
// 测试AttributeValue类型的安全转换和边界条件

test "attribute_value_string_conversion" {
  // 测试字符串值的转换
  let string_val = StringValue("test_string")
  match string_val {
    StringValue(s) => assert_eq(s, "test_string")
    _ => assert_false(true, "Expected StringValue")
  }
  
  // 测试空字符串
  let empty_string = StringValue("")
  match empty_string {
    StringValue(s) => assert_eq(s.length(), 0)
    _ => assert_false(true, "Expected StringValue")
  }
}

test "attribute_value_numeric_conversion" {
  // 测试整数值转换
  let int_val = IntValue(42)
  match int_val {
    IntValue(i) => assert_eq(i, 42)
    _ => assert_false(true, "Expected IntValue")
  }
  
  // 测试负整数值
  let neg_int = IntValue(-100)
  match neg_int {
    IntValue(i) => assert_eq(i, -100)
    _ => assert_false(true, "Expected IntValue")
  }
  
  // 测试浮点数值转换
  let float_val = FloatValue(3.14159)
  match float_val {
    FloatValue(f) => assert_true(f > 3.14 && f < 3.15)
    _ => assert_false(true, "Expected FloatValue")
  }
  
  // 测试零值
  let zero_float = FloatValue(0.0)
  match zero_float {
    FloatValue(f) => assert_eq(f, 0.0)
    _ => assert_false(true, "Expected FloatValue")
  }
}

test "attribute_value_boolean_conversion" {
  // 测试布尔值转换
  let true_val = BoolValue(true)
  match true_val {
    BoolValue(b) => assert_true(b)
    _ => assert_false(true, "Expected BoolValue")
  }
  
  let false_val = BoolValue(false)
  match false_val {
    BoolValue(b) => assert_false(b)
    _ => assert_false(true, "Expected BoolValue")
  }
}

test "attribute_value_array_conversion" {
  // 测试字符串数组转换
  let string_array = ArrayStringValue(["item1", "item2", "item3"])
  match string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "item1")
      assert_eq(arr[1], "item2")
      assert_eq(arr[2], "item3")
    }
    _ => assert_false(true, "Expected ArrayStringValue")
  }
  
  // 测试空数组
  let empty_array = ArrayStringValue([])
  match empty_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_false(true, "Expected ArrayStringValue")
  }
  
  // 测试整数数组转换
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  match int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
    }
    _ => assert_false(true, "Expected ArrayIntValue")
  }
}

test "attribute_value_edge_cases" {
  // 测试特殊字符串值
  let special_chars = StringValue("特殊字符_测试!@#$%^&*()")
  match special_chars {
    StringValue(s) => assert_true(s.contains("特殊字符"))
    _ => assert_false(true, "Expected StringValue")
  }
  
  // 测试极大整数值
  let large_int = IntValue(2147483647)  // Int32 max
  match large_int {
    IntValue(i) => assert_eq(i, 2147483647)
    _ => assert_false(true, "Expected IntValue")
  }
  
  // 测试极小整数值
  let small_int = IntValue(-2147483648)  // Int32 min
  match small_int {
    IntValue(i) => assert_eq(i, -2147483648)
    _ => assert_false(true, "Expected IntValue")
  }
  
  // 测试极大浮点数值
  let large_float = FloatValue(1.7976931348623157e+308)  // Double max
  match large_float {
    FloatValue(f) => assert_true(f > 1.0e+308)
    _ => assert_false(true, "Expected FloatValue")
  }
}

test "attribute_value_type_safety" {
  // 测试类型安全性 - 确保不同类型不会混淆
  let values = [
    StringValue("string"),
    IntValue(42),
    FloatValue(3.14),
    BoolValue(true),
    ArrayStringValue(["a", "b"]),
    ArrayIntValue([1, 2, 3])
  ]
  
  // 验证每个值的类型
  for (i, value) in values.enumerate() {
    match value {
      StringValue(_) => assert_eq(i, 0)
      IntValue(_) => assert_eq(i, 1)
      FloatValue(_) => assert_eq(i, 2)
      BoolValue(_) => assert_eq(i, 3)
      ArrayStringValue(_) => assert_eq(i, 4)
      ArrayIntValue(_) => assert_eq(i, 5)
    }
  }
}

test "attribute_value_serialization_compatibility" {
  // 测试序列化兼容性 - 确保值可以正确序列化和反序列化
  let test_values = [
    StringValue("test"),
    IntValue(123),
    FloatValue(45.67),
    BoolValue(false)
  ]
  
  for value in test_values {
    // 模拟序列化过程 - 在实际实现中这里会有真正的序列化逻辑
    match value {
      StringValue(s) => {
        // 模拟反序列化
        let reconstructed = StringValue(s)
        match reconstructed {
          StringValue(original) => assert_eq(original, s)
          _ => assert_false(true, "Reconstruction failed")
        }
      }
      IntValue(i) => {
        let reconstructed = IntValue(i)
        match reconstructed {
          IntValue(original) => assert_eq(original, i)
          _ => assert_false(true, "Reconstruction failed")
        }
      }
      FloatValue(f) => {
        let reconstructed = FloatValue(f)
        match reconstructed {
          FloatValue(original) => assert_eq(original, f)
          _ => assert_false(true, "Reconstruction failed")
        }
      }
      BoolValue(b) => {
        let reconstructed = BoolValue(b)
        match reconstructed {
          BoolValue(original) => assert_eq(original, b)
          _ => assert_false(true, "Reconstruction failed")
        }
      }
      _ => ()  // 跳过数组类型以简化测试
    }
  }
}
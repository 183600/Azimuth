// AttributeValue类型安全和转换测试
test "attribute_value_type_safety_and_conversion" {
  // 测试基本类型的创建和匹配
  let string_val = AttributeValue::string("test_string")
  let int_val = AttributeValue::int(123L)
  let float_val = AttributeValue::float(45.67)
  let bool_val = AttributeValue::bool(true)
  
  // 测试类型匹配的安全性
  match string_val {
    StringValue(s) => {
      assert_eq(s, "test_string")
      // 测试字符串长度边界
      assert_eq(s.length(), 11)
    }
    _ => @test.fail("Test failed")
  }
  
  match int_val {
    IntValue(i) => {
      assert_eq(i, 123L)
      // 测试整数边界值
      assert_eq(i > 0L, true)
      assert_eq(i >= 123L, true)
      assert_eq(i <= 123L, true)
    }
    _ => @test.fail("Test failed")
  }
  
  match float_val {
    FloatValue(f) => {
      assert_eq(f, 45.67)
      // 测试浮点数精度
      assert_eq(f > 45.0, true)
      assert_eq(f < 46.0, true)
    }
    _ => @test.fail("Test failed")
  }
  
  match bool_val {
    BoolValue(b) => {
      assert_eq(b, true)
      // 测试布尔逻辑
      assert_eq(b && true, true)
      assert_eq(b || false, true)
    }
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_array_type_safety" {
  // 测试数组类型的类型安全
  let string_array = AttributeValue::array_string(["a", "b", "c"])
  let int_array = AttributeValue::array_int([1L, 2L, 3L, 4L, 5L])
  let float_array = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array = AttributeValue::array_bool([true, false, true, false])
  
  // 测试字符串数组
  match string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
      
      // 测试数组边界访问
      let mut i = 0
      while i < arr.length() {
        match arr[i] {
          s => assert_eq(s.length() > 0, true)
        }
        i = i + 1
      }
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试整数数组
  match int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      // 测试数组求和
      let mut sum = 0L
      let mut i = 0
      while i < arr.length() {
        sum = sum + arr[i]
        i = i + 1
      }
      assert_eq(sum, 15L)
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试浮点数数组
  match float_array {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      // 测试数组平均值
      let mut sum = 0.0
      let mut i = 0
      while i < arr.length() {
        sum = sum + arr[i]
        i = i + 1
      }
      let avg = sum / arr.length().to_double()
      assert_eq(avg > 2.0, true)
      assert_eq(avg < 2.3, true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试布尔数组
  match bool_array {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 4)
      // 测试布尔数组逻辑操作
      let mut true_count = 0
      let mut false_count = 0
      let mut i = 0
      while i < arr.length() {
        if arr[i] {
          true_count = true_count + 1
        } else {
          false_count = false_count + 1
        }
        i = i + 1
      }
      assert_eq(true_count, 2)
      assert_eq(false_count, 2)
    }
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_edge_cases_and_limits" {
  // 测试边界情况和限制
  
  // 测试极值
  let max_int = AttributeValue::int(9223372036854775807L)
  let min_int = AttributeValue::int(-9223372036854775808L)
  let zero_int = AttributeValue::int(0L)
  
  match max_int {
    IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Test failed")
  }
  
  match min_int {
    IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Test failed")
  }
  
  match zero_int {
    IntValue(i) => assert_eq(i, 0L)
    _ => @test.fail("Test failed")
  }
  
  // 测试特殊浮点值
  let inf_float = AttributeValue::float(1.0/0.0)
  let neg_inf_float = AttributeValue::float(-1.0/0.0)
  let nan_float = AttributeValue::float(0.0/0.0)
  let very_small_float = AttributeValue::float(1.0e-308)
  let very_large_float = AttributeValue::float(1.0e308)
  
  match inf_float {
    FloatValue(f) => {
      // 检查无穷大
      assert_eq(f > 0.0, true)
      assert_eq(f > 1.0e308, true)
    }
    _ => @test.fail("Test failed")
  }
  
  match neg_inf_float {
    FloatValue(f) => {
      // 检查负无穷大
      assert_eq(f < 0.0, true)
      assert_eq(f < -1.0e308, true)
    }
    _ => @test.fail("Test failed")
  }
  
  match nan_float {
    FloatValue(f) => {
      // NaN不等于任何值，包括自身
      assert_eq(f != f, true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试空数组
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
}

test "attribute_value_complex_scenarios" {
  // 测试复杂场景
  
  // 测试包含特殊字符的字符串
  let special_chars = AttributeValue::string("!@#$%^&*()_+-=[]{}|;':\",./<>?")
  let unicode_chars = AttributeValue::string("测试中文🚀emoji")
  let whitespace = AttributeValue::string("   \t\n\r   ")
  let very_long_string = AttributeValue::string("a".repeat(10000))
  
  match special_chars {
    StringValue(s) => {
      assert_eq(s.length(), 29)
      assert_eq(s[0], "!")
      assert_eq(s[28], "?")
    }
    _ => @test.fail("Test failed")
  }
  
  match unicode_chars {
    StringValue(s) => {
      assert_eq(s.length(), 9)
      assert_eq(s[0:6], "测试中文")
      assert_eq(s[6], "🚀")
    }
    _ => @test.fail("Test failed")
  }
  
  match whitespace {
    StringValue(s) => {
      assert_eq(s.length(), 9)
      assert_eq(s[0], " ")
      assert_eq(s[3], "\t")
      assert_eq(s[4], "\n")
      assert_eq(s[5], "\r")
    }
    _ => @test.fail("Test failed")
  }
  
  match very_long_string {
    StringValue(s) => {
      assert_eq(s.length(), 10000)
      assert_eq(s[0], "a")
      assert_eq(s[9999], "a")
    }
    _ => @test.fail("Test failed")
  }
  
  // 测试大型数组
  let large_int_array = AttributeValue::array_int([for i = 0; i < 1000; i = i + 1].map(fn(_) { 42L }))
  let large_string_array = AttributeValue::array_string([for i = 0; i < 100; i = i + 1].map(fn(_) { "test" }))
  
  match large_int_array {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 1000)
      assert_eq(arr[0], 42L)
      assert_eq(arr[999], 42L)
    }
    _ => @test.fail("Test failed")
  }
  
  match large_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 100)
      assert_eq(arr[0], "test")
      assert_eq(arr[99], "test")
    }
    _ => @test.fail("Test failed")
  }
}

test "attribute_value_type_consistency" {
  // 测试类型一致性
  
  // 创建属性集合
  let attributes : Attributes = [
    ("string.key", AttributeValue::string("string_value")),
    ("int.key", AttributeValue::int(42L)),
    ("float.key", AttributeValue::float(3.14)),
    ("bool.key", AttributeValue::bool(true)),
    ("string_array.key", AttributeValue::array_string(["a", "b", "c"])),
    ("int_array.key", AttributeValue::array_int([1L, 2L, 3L])),
    ("float_array.key", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool_array.key", AttributeValue::array_bool([true, false, true]))
  ]
  
  // 验证每个属性的类型和值
  let mut i = 0
  while i < attributes.length() {
    let (key, value) = attributes[i]
    
    match key {
      "string.key" => {
        match value {
          StringValue(s) => assert_eq(s, "string_value")
          _ => @test.fail("Test failed")
        }
      }
      "int.key" => {
        match value {
          IntValue(i) => assert_eq(i, 42L)
          _ => @test.fail("Test failed")
        }
      }
      "float.key" => {
        match value {
          FloatValue(f) => assert_eq(f, 3.14)
          _ => @test.fail("Test failed")
        }
      }
      "bool.key" => {
        match value {
          BoolValue(b) => assert_eq(b, true)
          _ => @test.fail("Test failed")
        }
      }
      "string_array.key" => {
        match value {
          ArrayStringValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], "a")
            assert_eq(arr[1], "b")
            assert_eq(arr[2], "c")
          }
          _ => @test.fail("Test failed")
        }
      }
      "int_array.key" => {
        match value {
          ArrayIntValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], 1L)
            assert_eq(arr[1], 2L)
            assert_eq(arr[2], 3L)
          }
          _ => @test.fail("Test failed")
        }
      }
      "float_array.key" => {
        match value {
          ArrayFloatValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], 1.1)
            assert_eq(arr[1], 2.2)
            assert_eq(arr[2], 3.3)
          }
          _ => @test.fail("Test failed")
        }
      }
      "bool_array.key" => {
        match value {
          ArrayBoolValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], true)
            assert_eq(arr[1], false)
            assert_eq(arr[2], true)
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => @test.fail("Unexpected key: " + key)
    }
    
    i = i + 1
  }
}

test "attribute_value_performance_considerations" {
  // 测试性能相关的边界情况
  
  // 测试大量属性的处理
  let mut large_attributes : Attributes = []
  let mut i = 0
  while i < 1000 {
    large_attributes.push(("attr." + i.to_string(), AttributeValue::int(i.to_int64())))
    i = i + 1
  }
  
  assert_eq(large_attributes.length(), 1000)
  
  // 验证第一个和最后一个属性
  match large_attributes[0].1 {
    IntValue(val) => assert_eq(val, 0L)
    _ => @test.fail("Test failed")
  }
  
  match large_attributes[999].1 {
    IntValue(val) => assert_eq(val, 999L)
    _ => @test.fail("Test failed")
  }
  
  // 测试大型字符串数组的处理
  let mut large_string_elements = []
  i = 0
  while i < 100 {
    large_string_elements.push("string_" + i.to_string())
    i = i + 1
  }
  
  let large_string_array = AttributeValue::array_string(large_string_elements)
  
  match large_string_array {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 100)
      assert_eq(arr[0], "string_0")
      assert_eq(arr[99], "string_99")
    }
    _ => @test.fail("Test failed")
  }
}
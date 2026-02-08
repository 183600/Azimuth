// 标准 MoonBit 测试用例集合
// 使用标准的 MoonBit 测试语法，包含 8 个核心测试用例

test "string_length_calculation" {
  // 字符串长度计算测试
  assert_eq(5, "Hello".length())
  assert_eq(0, "".length())
  assert_eq(6, "World!".length())
  assert_eq(3, "123".length())
}

test "array_basic_operations" {
  // 数组基本操作测试
  let arr = [1, 2, 3, 4, 5]
  assert_eq(5, arr.length())
  assert_eq(1, arr[0])
  assert_eq(5, arr[4])
  
  let empty_arr : Array[Int] = []
  assert_eq(0, empty_arr.length())
}

test "option_type_handling" {
  // Option 类型处理测试
  let some_value = Some(42)
  let none_value : Option[Int] = None
  
  match some_value {
    Some(x) => assert_eq(42, x)
    None => assert_eq(0, 1) // 不应该到达这里
  }
  
  match none_value {
    Some(x) => assert_eq(0, 1) // 不应该到达这里
    None => assert_eq(0, 0) // 预期结果
  }
}

test "result_type_operations" {
  // Result 类型操作测试
  let success_result = Ok(100)
  let error_result = Err("Error message")
  
  match success_result {
    Ok(value) => assert_eq(100, value)
    Err(msg) => assert_eq(0, 1) // 不应该到达这里
  }
  
  match error_result {
    Ok(value) => assert_eq(0, 1) // 不应该到达这里
    Err(msg) => assert_eq_string("Error message", msg)
  }
}

test "character_operations" {
  // 字符操作测试
  let char_a = 'a'
  let char_Z = 'Z'
  let char_1 = '1'
  
  assert_eq(true, char_a <= 'z')
  assert_eq(true, char_Z >= 'A')
  assert_eq(true, char_1 >= '0' && char_1 <= '9')
}

test "boolean_logic_operations" {
  // 布尔逻辑操作测试
  assert_eq(true, true && true)
  assert_eq(false, true && false)
  assert_eq(false, false && true)
  assert_eq(false, false && false)
  
  assert_eq(true, true || true)
  assert_eq(true, true || false)
  assert_eq(true, false || true)
  assert_eq(false, false || false)
  
  assert_eq(false, !true)
  assert_eq(true, !false)
}

test "numeric_comparisons" {
  // 数值比较测试
  assert_eq(true, 5 > 3)
  assert_eq(true, 3 < 5)
  assert_eq(true, 5 >= 5)
  assert_eq(true, 5 <= 5)
  assert_eq(true, 5 == 5)
  assert_eq(false, 5 == 3)
  assert_eq(true, 5 != 3)
  
  // 负数比较
  assert_eq(true, -1 > -5)
  assert_eq(true, -10 < -2)
}

test "string_concatenation" {
  // 字符串连接测试
  let str1 = "Hello"
  let str2 = " "
  let str3 = "MoonBit"
  
  assert_eq_string("Hello MoonBit", str1 + str2 + str3)
  assert_eq_string("Hello", str1)
  assert_eq_string("", "")
  assert_eq_string("123", "1" + "2" + "3")
}
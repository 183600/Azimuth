// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例
// 测试覆盖核心功能：基本运算、字符串处理、数组操作、边界条件

test "integer_addition_basic" {
  // 测试整数加法基本功能
  assert_eq(0, 0 + 0)
  assert_eq(5, 2 + 3)
  assert_eq(100, 50 + 50)
  assert_eq(-10, -5 + -5)
  assert_eq(0, 10 + -10)
}

test "integer_multiplication_properties" {
  // 测试整数乘法的数学性质
  assert_eq(0, 0 * 42)
  assert_eq(42, 1 * 42)
  assert_eq(42, 42 * 1)
  assert_eq(-42, -1 * 42)
  assert_eq(42, -1 * -42)
  assert_eq(100, 10 * 10)
}

test "string_concatenation" {
  // 测试字符串连接功能
  assert_eq("", "" ++ "")
  assert_eq("hello", "hello" ++ "")
  assert_eq("world", "" ++ "world")
  assert_eq("helloworld", "hello" ++ "world")
  assert_eq("hello world", "hello" ++ " " ++ "world")
}

test "array_basic_operations" {
  // 测试数组基本操作
  let empty_array : Array[Int] = []
  assert_eq(0, empty_array.length())
  
  let single_array = [1]
  assert_eq(1, single_array.length())
  assert_eq(1, single_array[0])
  
  let multi_array = [1, 2, 3, 4, 5]
  assert_eq(5, multi_array.length())
  assert_eq(1, multi_array[0])
  assert_eq(5, multi_array[4])
}

test "boolean_logic_operations" {
  // 测试布尔逻辑运算
  assert_eq(true, true && true)
  assert_eq(false, true && false)
  assert_eq(false, false && true)
  assert_eq(false, false && false)
  
  assert_eq(true, true || true)
  assert_eq(true, true || false)
  assert_eq(true, false || true)
  assert_eq(false, false || false)
  
  assert_eq(false, not(true))
  assert_eq(true, not(false))
}

test "comparison_operations" {
  // 测试比较操作
  assert_eq(true, 5 == 5)
  assert_eq(false, 5 == 3)
  assert_eq(true, 5 != 3)
  assert_eq(false, 5 != 5)
  
  assert_eq(true, 5 > 3)
  assert_eq(false, 3 > 5)
  assert_eq(true, 3 < 5)
  assert_eq(false, 5 < 3)
  
  assert_eq(true, 5 >= 5)
  assert_eq(true, 5 >= 3)
  assert_eq(false, 3 >= 5)
  assert_eq(true, 3 <= 3)
  assert_eq(true, 3 <= 5)
  assert_eq(false, 5 <= 3)
}

test "edge_case_zero_operations" {
  // 测试零的边界情况
  assert_eq(0, 0 + 0)
  assert_eq(0, 0 * 42)
  assert_eq(0, 42 * 0)
  assert_eq(0, 0 - 0)
  
  assert_eq(true, 0 == 0)
  assert_eq(false, 0 != 0)
  assert_eq(false, 0 > 0)
  assert_eq(false, 0 < 0)
  assert_eq(true, 0 >= 0)
  assert_eq(true, 0 <= 0)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, -5 + -5)
  assert_eq(0, -5 + 5)
  assert_eq(10, 5 + 5)
  assert_eq(25, -5 * -5)
  assert_eq(-25, -5 * 5)
  assert_eq(25, 5 * -5)
  
  assert_eq(true, -5 < 0)
  assert_eq(true, -5 < 5)
  assert_eq(false, -5 > 5)
  assert_eq(true, -5 <= -5)
  assert_eq(true, -5 >= -5)
}

test "string_length_operations" {
  // 测试字符串长度操作
  assert_eq(0, "".length())
  assert_eq(5, "hello".length())
  assert_eq(11, "hello world".length())
  
  let single_char = "a"
  assert_eq(1, single_char.length())
  
  let space = " "
  assert_eq(1, space.length())
  
  let mixed = "a1!@#"
  assert_eq(5, mixed.length())
}

test "type_coercion_and_casting" {
  // 测试类型转换和强制转换
  let int_val : Int = 42
  let bool_true : Bool = true
  let bool_false : Bool = false
  
  // 布尔到整数的隐式转换测试
  assert_eq(1, int_of_bool(true))
  assert_eq(0, int_of_bool(false))
  
  // 整数到布尔的转换测试
  assert_eq(true, bool_of_int(1))
  assert_eq(false, bool_of_int(0))
  
  // 保持原有值
  assert_eq(42, int_val)
  assert_eq(true, bool_true)
  assert_eq(false, bool_false)
}
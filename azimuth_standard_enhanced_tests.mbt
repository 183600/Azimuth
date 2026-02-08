// Azimuth 项目标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法
// 测试核心功能和边界情况

///|
test "int_basic_arithmetic" {
  // 基本整数算术运算测试
  assert_eq(10 + 5, 15)
  assert_eq(10 - 5, 5)
  assert_eq(10 * 5, 50)
  assert_eq(10 / 3, 3)
  assert_eq(10 % 3, 1)
  
  // 测试负数运算
  assert_eq(-10 + 5, -5)
  assert_eq(-10 - 5, -15)
  assert_eq(-10 * 5, -50)
  assert_eq(-10 / 3, -3)
  assert_eq(-10 % 3, -1)
}

///|
test "int_bitwise_operations" {
  // 位运算测试
  assert_eq(10 & 12, 8)        // 1010 & 1100 = 1000
  assert_eq(10 | 12, 14)       // 1010 | 1100 = 1110
  assert_eq(10 ^ 12, 6)        // 1010 ^ 1100 = 0110
  assert_eq(~10, -11)          // 按位取反
  assert_eq(10 << 2, 40)       // 左移
  assert_eq(10 >> 2, 2)        // 右移
  
  // 测试边界情况
  assert_eq(0 & 255, 0)
  assert_eq(255 | 0, 255)
  assert_eq(0 ^ 255, 255)
}

///|
test "string_basic_operations" {
  // 基本字符串操作测试
  let s1 = "Hello"
  let s2 = "World"
  let s3 = s1 + " " + s2
  assert_eq(s3, "Hello World")
  
  // 测试字符串长度
  assert_eq(s1.length(), 5)
  assert_eq(s2.length(), 5)
  assert_eq(s3.length(), 11)
  
  // 测试空字符串
  let empty = ""
  assert_eq(empty.length(), 0)
  assert_eq(empty + "test", "test")
}

///|
test "bool_logical_operations" {
  // 布尔逻辑运算测试
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && true)
  assert_false(false && false)
  
  assert_true(true || true)
  assert_true(true || false)
  assert_true(false || true)
  assert_false(false || false)
  
  assert_false(!true)
  assert_true(!false)
  
  // 测试布尔值比较
  assert_eq(true, true)
  assert_ne(true, false)
  assert_eq(false, false)
}

///|
test "comparison_operations" {
  // 比较运算测试
  assert_true(10 > 5)
  assert_false(5 > 10)
  assert_false(10 > 10)
  
  assert_true(5 < 10)
  assert_false(10 < 5)
  assert_false(10 < 10)
  
  assert_true(10 >= 10)
  assert_true(10 >= 5)
  assert_false(5 >= 10)
  
  assert_true(10 <= 10)
  assert_true(5 <= 10)
  assert_false(10 <= 5)
  
  assert_true(10 == 10)
  assert_false(10 == 5)
  
  assert_true(10 != 5)
  assert_false(10 != 10)
}

///|
test "option_basic_operations" {
  // Option 类型基本操作测试
  let some_int : Int? = Some(42)
  let none_int : Int? = None
  
  // 测试 is_some 和 is_none
  assert_true(some_int.is_some())
  assert_false(some_int.is_none())
  assert_false(none_int.is_some())
  assert_true(none_int.is_none())
  
  // 测试 unwrap_or
  assert_eq(some_int.unwrap_or(0), 42)
  assert_eq(none_int.unwrap_or(0), 0)
  
  // 测试 map
  assert_eq(some_int.map(x => x * 2), Some(84))
  assert_eq(none_int.map(x => x * 2), None)
}

///|
test "array_basic_operations" {
  // 数组基本操作测试
  let arr = [1, 2, 3, 4, 5]
  
  // 测试数组长度
  assert_eq(arr.length(), 5)
  
  // 测试数组访问
  assert_eq(arr[0], 1)
  assert_eq(arr[2], 3)
  assert_eq(arr[4], 5)
  
  // 测试数组边界
  assert_eq(Array::get(arr, 0), Some(1))
  assert_eq(Array::get(arr, 4), Some(5))
  assert_eq(Array::get(arr, 5), None)
  assert_eq(Array::get(arr, -1), None)
}

///|
test "type_conversion_operations" {
  // 类型转换操作测试
  let int_val = 42
  let uint_val = 42U
  let int64_val = 42L
  
  // 测试整数到字符串转换
  assert_eq(int_val.to_string(), "42")
  assert_eq(uint_val.to_string(), "42")
  assert_eq(int64_val.to_string(), "42")
  
  // 测试字符串到整数转换
  assert_eq("42".to_int(), Some(42))
  assert_eq("-42".to_int(), Some(-42))
  assert_eq("abc".to_int(), None)
  
  // 测试不同整数类型之间的转换
  assert_eq(int_val.to_uint(), 42U)
  assert_eq(int_val.to_int64(), 42L)
  assert_eq(uint_val.to_int(), 42)
  assert_eq(int64_val.to_int(), 42)
}
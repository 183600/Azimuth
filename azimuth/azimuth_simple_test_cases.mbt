// Azimuth 简单测试用例
// 包含5个基本测试用例，使用标准 MoonBit 测试语法

test "add_function_simple" {
  // 测试加法函数
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
}

test "multiply_function_simple" {
  // 测试乘法函数
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
}

test "greet_function_simple" {
  // 测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "divide_with_ceil_simple" {
  // 测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "subtract_function_simple" {
  // 测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
}
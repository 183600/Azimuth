// Azimuth 项目标准测试套件
// 使用标准 MoonBit 测试语法，测试核心功能

test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-5, -5))
}

test "add_function_mixed_signs" {
  // 测试混合符号加法
  assert_eq(0, add(5, -5))
  assert_eq(-2, add(3, -5))
  assert_eq(2, add(-3, 5))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "multiply_function_negative" {
  // 测试负数乘法
  assert_eq(6, multiply(-2, -3))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(-7, 0))
}

test "greet_function_basic" {
  // 测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(3, divide_with_ceil(6, 2))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative" {
  // 测试负数向上取整除法
  assert_eq(-2, divide_with_ceil(-5, 2))
  assert_eq(-3, divide_with_ceil(-6, 2))
  assert_eq(-2, divide_with_ceil(5, -2))
}

test "divide_with_ceil_zero_division" {
  // 测试零除处理
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "assert_eq_function" {
  // 测试自定义断言函数
  assert_eq(5, 5)
  assert_eq(0, 0)
  assert_eq(-10, -10)
}

test "assert_eq_string_function" {
  // 测试字符串断言函数
  assert_eq_string("hello", "hello")
  assert_eq_string("", "")
  assert_eq_string("MoonBit", "MoonBit")
}
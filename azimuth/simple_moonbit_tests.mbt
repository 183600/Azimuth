// 简单的 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_test" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "add_overflow_test" {
  // 测试溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
}

test "multiply_basic_test" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(2, -3))
}

test "multiply_overflow_test" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_test" {
  // 测试字符串拼接功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_test" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(5, -3))
}

test "assert_functions_test" {
  // 测试断言函数功能
  assert_true(true)
  assert_true(1 == 1)
  assert_false(false)
  assert_false(1 == 2)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "complex_expressions_test" {
  // 测试复杂数学表达式
  let a = 10
  let b = 3
  
  assert_eq(39, multiply(add(a, b), 3))
  assert_eq(4, divide_with_ceil(multiply(a, b), 8))
}

test "boundary_conditions_test" {
  // 测试边界条件和特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(0, min_val))
  assert_eq(1, divide_with_ceil(1, 1))
}
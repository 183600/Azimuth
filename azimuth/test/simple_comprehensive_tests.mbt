test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
}

test "add_edge_cases" {
  // 测试加法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, add(max_val - 1, 2))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(min_val, multiply(-1, min_val))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "divide_with_ceil_negative" {
  // 测试向上取整除法负数情况
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(-2, divide_with_ceil(-4, 2))
}

test "greet_function_basic" {
  // 测试问候函数基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "assert_functions_basic" {
  // 测试断言函数基本功能
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "complex_arithmetic" {
  // 测试复杂算术运算
  let a = 10
  let b = 20
  
  assert_eq(30, add(a, b))
  assert_eq(200, multiply(a, b))
  assert_eq(7, divide_with_ceil(20, 3))
}
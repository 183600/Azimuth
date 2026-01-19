test "add_function_basic_operations" {
  // 测试基本加法运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "add_function_zero_handling" {
  // 测试零值处理
  assert_eq(10, add(0, 10))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, add(0, 0))
}

test "add_function_overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, add(max_val - 1, 2))
  assert_eq(max_val, add(max_val, 1))
  
  // 负数溢出测试
  assert_eq(min_val, add(min_val + 1, -2))
  assert_eq(min_val, add(min_val, -1))
}

test "multiply_function_basic_operations" {
  // 测试基本乘法运算
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_function_identity_elements" {
  // 测试单位元
  assert_eq(5, multiply(1, 5))
  assert_eq(7, multiply(7, 1))
  assert_eq(0, multiply(0, 5))
  assert_eq(-5, multiply(-1, 5))
}

test "multiply_function_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, multiply(max_val, 2))
  
  // 负数溢出测试
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(-1, min_val))
}

test "greet_function" {
  // 测试字符串拼接
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
}

test "assert_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}
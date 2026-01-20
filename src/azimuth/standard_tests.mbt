// MoonBit 标准测试用例
// 覆盖 azimuth 包的核心功能

test "safe_addition_basic" {
  // 基础安全加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "safe_addition_mixed_signs" {
  // 混合符号加法测试
  assert_eq(0, add(5, -5))
  assert_eq(100, add(150, -50))
  assert_eq(-100, add(-200, 100))
}

test "safe_addition_overflow_protection" {
  // 溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, add(max_val, max_val))
}

test "safe_multiplication_basic" {
  // 基础安全乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 123))
  assert_eq(-12, multiply(-3, 4))
}

test "safe_multiplication_boundary" {
  // 边界值乘法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "safe_multiplication_overflow_protection" {
  // 乘法溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_function_various_inputs" {
  // greet 函数多种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试：计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}
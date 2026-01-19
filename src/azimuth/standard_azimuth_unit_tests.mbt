// 标准 Azimuth 单元测试
// 测试核心功能：add, multiply, greet 函数

test "add_positive_numbers" {
  // 测试正数相加
  assert_eq(10, add(3, 7))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(-5, 5))
}

test "add_mixed_numbers" {
  // 测试正负数混合相加
  assert_eq(5, add(10, -5))
  assert_eq(-3, add(2, -5))
  assert_eq(0, add(-10, 10))
}

test "multiply_basic" {
  // 测试基本乘法
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(15, multiply(3, 5))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(0, multiply(-5, 0))
}

test "greet_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "greet_edge_cases" {
  // 测试问候功能的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  
  // 测试乘法溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "complex_calculations" {
  // 测试复合计算
  // (2 + 3) * 4 = 20
  let sum = add(2, 3)
  let result = multiply(sum, 4)
  assert_eq(20, result)
  
  // 10 * (5 + 3) = 80
  let sum2 = add(5, 3)
  let result2 = multiply(10, sum2)
  assert_eq(80, result2)
}
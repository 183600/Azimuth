// 专注的单元测试用例
// 测试 azimuth 包的核心功能

test "add_positive_numbers" {
  // 测试正数相加
  assert_eq(10, add(3, 7))
  assert_eq(100, add(50, 50))
  assert_eq(1000, add(600, 400))
}

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(-100, add(-50, -50))
  assert_eq(-1000, add(-600, -400))
}

test "add_mixed_numbers" {
  // 测试正负数混合相加
  assert_eq(0, add(10, -10))
  assert_eq(5, add(10, -5))
  assert_eq(-5, add(-10, 5))
}

test "multiply_by_zero" {
  // 测试乘以零的情况
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 0))
}

test "multiply_by_one" {
  // 测试乘以一的情况
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(-5, multiply(-5, 1))
  assert_eq(-5, multiply(1, -5))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(-25, multiply(5, -5))
}

test "greet_simple_names" {
  // 测试简单名称的问候
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, Eve!", greet("Eve"))
}

test "greet_special_characters" {
  // 测试包含特殊字符的名称
  assert_eq_string("Hello, John_Doe!", greet("John_Doe"))
  assert_eq_string("Hello, user@123!", greet("user@123"))
  assert_eq_string("Hello, test-case!", greet("test-case"))
}

test "boundary_value_addition" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "boundary_value_multiplication" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}
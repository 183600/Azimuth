// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，测试核心功能

test "add_basic_positive_numbers" {
  assert_eq(5, add(2, 3))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "add_basic_negative_numbers" {
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(-5, 5))
  assert_eq(-100, add(-50, -50))
}

test "add_overflow_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出保护
  assert_eq(max_val, add(max_val - 1, 2))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 测试负数溢出保护
  assert_eq(min_val, add(min_val + 1, -2))
  assert_eq(min_val, add(-1000000000, -1500000000))
}

test "add_edge_cases" {
  let min_val = -2147483648
  
  // 测试最小值情况
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(0, min_val))
  assert_eq(min_val, add(min_val, -10))
  assert_eq(-10, add(min_val, 2147483638))
}

test "multiply_basic_cases" {
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
}

test "multiply_edge_cases" {
  let min_val = -2147483648
  let max_val = 2147483647
  
  // 测试 -1 的特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(5, multiply(-5, -1))
  
  // 测试最小值情况
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(2, min_val))
  assert_eq(0, multiply(min_val, 0))
}

test "multiply_overflow_protection" {
  let max_val = 2147483647
  
  // 测试正数溢出保护
  assert_eq(max_val, multiply(50000, 50000))
  assert_eq(max_val, multiply(100000, 30000))
  
  // 测试负数溢出保护
  assert_eq(-max_val, multiply(-50000, 50000))
  assert_eq(-max_val, multiply(50000, -50000))
}

test "greet_functionality" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "assert_true_functionality" {
  assert_true(true)
  assert_true(1 == 1)
  assert_true(0 < 1)
  assert_true(-5 < 0)
}

test "assert_false_functionality" {
  assert_false(false)
  assert_false(1 == 0)
  assert_false(1 < 0)
  assert_false(0 < -5)
}
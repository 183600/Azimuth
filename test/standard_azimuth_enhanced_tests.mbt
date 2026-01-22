// 标准 Azimuth 增强测试用例
// 覆盖 azimuth 包的核心功能和边界情况

test "add_positive_numbers" {
  assert_eq(7, add(3, 4))
  assert_eq(100, add(50, 50))
  assert_eq(1, add(0, 1))
}

test "add_negative_numbers" {
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-15, 5))
}

test "multiply_various_cases" {
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(16, multiply(-4, -4))
  assert_eq(0, multiply(0, 100))
}

test "greet_different_names" {
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_exact_division" {
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "divide_with_ceil_positive_ceil" {
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "divide_with_ceil_negative_cases" {
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(5, -3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "divide_with_ceil_edge_cases" {
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}
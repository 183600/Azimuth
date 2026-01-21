// 简洁的 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_positive_numbers" {
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
}

test "add_negative_numbers" {
  assert_eq(-10, add(-7, -3))
  assert_eq(0, add(-5, 5))
}

test "multiply_basic" {
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
}

test "multiply_with_negative" {
  assert_eq(-15, multiply(3, -5))
  assert_eq(15, multiply(-3, -5))
}

test "greet_simple" {
  assert_eq_string("Hello, Alice!", greet("Alice"))
}

test "greet_empty_string" {
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "divide_with_ceil_zero_divisor" {
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative" {
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}
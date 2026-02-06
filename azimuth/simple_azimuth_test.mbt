// 简单的 Azimuth 测试 - 5个基本测试
// 测试 azimuth 库的核心功能

test "add_simple" {
  assert_eq(5, add(2, 3))
}

test "multiply_simple" {
  assert_eq(6, multiply(2, 3))
}

test "greet_simple" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "divide_simple" {
  assert_eq(4, divide_with_ceil(10, 3))
}

test "subtract_simple" {
  assert_eq(2, subtract(5, 3))
}
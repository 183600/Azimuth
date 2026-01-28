// Azimuth 高级测试用例
// 包含复杂场景和边界条件的全面测试

test "simple_addition_test" {
  assert_eq(5, add(2, 3))
}

test "simple_multiplication_test" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "simple_divide_with_ceil_test" {
  assert_eq(4, divide_with_ceil(10, 3))
}
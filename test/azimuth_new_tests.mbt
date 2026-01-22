// 标准 Azimuth 增强测试用例

test "add_simple_test" {
  assert_eq(7, add(3, 4))
}

test "multiply_simple_test" {
  assert_eq(6, multiply(2, 3))
}

test "greet_simple_test" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "divide_with_ceil_simple_test" {
  assert_eq(3, divide_with_ceil(10, 4))
}

test "add_negative_test" {
  assert_eq(-5, add(-2, -3))
}

test "multiply_negative_test" {
  assert_eq(-12, multiply(-3, 4))
}

test "divide_with_ceil_zero_test" {
  assert_eq(0, divide_with_ceil(5, 0))
}
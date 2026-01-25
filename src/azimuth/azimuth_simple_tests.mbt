// Azimuth 简单测试用例

test "add_test" {
  assert_eq(5, add(2, 3))
}

test "multiply_test" {
  assert_eq(6, multiply(2, 3))
}

test "greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "divide_test" {
  assert_eq(4, divide_with_ceil(10, 3))
}
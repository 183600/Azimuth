// 简单的 Azimuth 测试用例
test "simple_addition" {
  assert_eq(5, add(2, 3))
}

test "simple_multiplication" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "simple_divide_ceil" {
  assert_eq(4, divide_with_ceil(10, 3))
}

test "simple_subtract" {
  assert_eq(2, subtract(5, 3))
}
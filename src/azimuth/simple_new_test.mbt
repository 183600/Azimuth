// 简单的新测试用例
test "simple_subtract_test" {
  assert_eq(5, subtract(8, 3))
}

test "simple_divide_test" {
  assert_eq(3, divide_with_ceil(10, 3))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}
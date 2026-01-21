// 简单的增强测试用例

test "simple_add_test" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "simple_multiply_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "simple_divide_test" {
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(0, divide_with_ceil(0, 5))
}
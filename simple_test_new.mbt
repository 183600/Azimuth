// 简单的新测试用例

test "simple_add_test" {
  assert_eq(5, add(2, 3))
}

test "simple_multiply_test" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "simple_assert_test" {
  assert_true(true)
  assert_false(false)
}

test "simple_divide_ceil_test" {
  assert_eq(3, divide_with_ceil(10, 4))
}
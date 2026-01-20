// 简单的新测试用例

test "simple_add_test" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
}

test "simple_multiply_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}
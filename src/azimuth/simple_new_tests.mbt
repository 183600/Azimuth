// 简单的新测试用例

test "simple_addition" {
  assert_eq(5, add(2, 3))
}

test "simple_multiplication" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet" {
  assert_eq_string("Hello, Test!", greet("Test"))
}
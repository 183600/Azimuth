// 简单的测试文件，用于验证测试系统
test "simple_test_addition" {
  assert_eq(5, add(2, 3))
}

test "simple_test_multiplication" {
  assert_eq(6, multiply(2, 3))
}

test "simple_test_greet" {
  assert_eq_string("Hello, Test!", greet("Test"))
}
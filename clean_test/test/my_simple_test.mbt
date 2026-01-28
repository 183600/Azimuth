// 简单的测试用例
test "simple_add_test" {
  @clean_test.assert_eq(5, @clean_test.add(2, 3))
}

test "simple_multiply_test" {
  @clean_test.assert_eq(6, @clean_test.multiply(2, 3))
}

test "simple_greet_test" {
  @clean_test.assert_eq_string("Hello, World!", @clean_test.greet("World"))
}
// 简单的单元测试
test "basic_addition" {
  assert_eq(5, 2 + 3)
}

test "string_test" {
  assert_eq_string("hello", "hello")
}

test "boolean_test" {
  assert_eq(true, true && true)
}
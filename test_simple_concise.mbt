// 简单测试验证
test "verify_simple_addition" {
  assert_eq(add(2, 3), 5)
}

test "verify_simple_greeting" {
  assert_eq_string(greet("Test"), "Hello, Test!")
}

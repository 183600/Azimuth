// 测试辅助函数定义
test "basic_add_comprehensive" {
  if 5 != @clean_test.add(2, 3) { @builtin.panic() }
}
test "basic_multiply_comprehensive" {
  if 6 != @clean_test.multiply(2, 3) { @builtin.panic() }
}
test "basic_greet_comprehensive" {
  if "Hello, World!" != @clean_test.greet("World") { @builtin.panic() }
}

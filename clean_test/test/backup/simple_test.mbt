// 测试辅助函数定义
// 直接在测试文件中定义所需的函数，避免包导入问题
// 直接在测试文件中定义所需的函数，避免包导入问题
// 使用 clean_test 包中的函数
test "add_basic" {
  if 5 != @clean_test.add(2, 3) { @builtin.panic() }
}
test "multiply_basic" {
  if 6 != @clean_test.multiply(2, 3) { @builtin.panic() }
}
test "greet_basic" {
  if "Hello, World!" != @clean_test.greet("World") { @builtin.panic() }
}

// 测试辅助函数定义
test "simple_math_test" {
  // Test basic addition
  let result = @clean_test.add(2, 3)
  if result != 5 {
    @builtin.panic()
  }
  // Test basic multiplication
  let result2 = @clean_test.multiply(2, 3)
  if result2 != 6 {
    @builtin.panic()
  }
  // Test string concatenation
  let result3 = "Hello, " + "World!"
  if result3 != "Hello, World!" {
    @builtin.panic()
  }
}

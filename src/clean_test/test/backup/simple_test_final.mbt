// 测试文件 - 使用clean_test包中的函数
test "simple_math_test" {
  // Test basic addition
  let result = 2 + 3
  if result != 5 {
    @builtin.panic()
  }
  
  // Test basic multiplication
  let result2 = 2 * 3
  if result2 != 6 {
    @builtin.panic()
  }
  
  // Test string concatenation
  let result3 = "Hello, " + "World!"
  if result3 != "Hello, World!" {
    @builtin.panic()
  }
}

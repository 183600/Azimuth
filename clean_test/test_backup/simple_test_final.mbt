// 测试文件 - 使用clean_test包中的函数
test "simple_math_test" {
  // Test basic addition
  let result = @clean_test.add(2, 3)
  @clean_test.assert_eq(5, result)
  
  // Test basic multiplication
  let result2 = @clean_test.multiply(2, 3)
  @clean_test.assert_eq(6, result2)
  
  // Test string concatenation
  let result3 = @clean_test.greet("World")
  @clean_test.assert_eq_string("Hello, World!", result3)
}
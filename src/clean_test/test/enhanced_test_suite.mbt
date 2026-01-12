// 测试文件 - 使用内置操作符测试基本功能
test "basic_add_enhanced_suite" {
  let result = @clean_test.add(2, 3)
  if result != 5 {
    // println("Test failed")
  }
}
test "basic_multiply_enhanced_suite" {
  let result = @clean_test.multiply(2, 3)
  if result != 6 {
    // println("Test failed")
  }
}
test "basic_greet_enhanced_suite" {
  let result = @clean_test.greet("World")
  if result != "Hello, World!" {
    // println("Test failed")
  }
}

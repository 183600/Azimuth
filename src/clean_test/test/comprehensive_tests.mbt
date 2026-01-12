// 测试文件 - 使用clean_test包中的函数
test "basic_add_comprehensive" {
  let result = @clean_test.add(2, 3)
  if result != 5 {
    // println("Test failed")
  }
}
test "basic_multiply_comprehensive" {
  let result  = @clean_test.multiply(2, 3)
  if result != 6 {
    // println("Test failed")
  }
}
test "basic_greet_comprehensive" {
  let result  = @clean_test.greet("World")
  if result != "Hello, World!" {
    // println("Test failed")
  }
}

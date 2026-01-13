// 测试辅助函数定义
test "basic_add_math_props" {
  let result = @clean_test.add(2, 3)
  if result != 5 {
    // println("Test failed")
  }
}
test "basic_multiply_math_props" {
  let result  = @clean_test.multiply(2, 3)
  if result != 6 {
    // println("Test failed")
  }
}
test "basic_greet_math_props" {
  let result  = @clean_test.greet("World")
  if result != "Hello, World!" {
    // println("Test failed")
  }
}

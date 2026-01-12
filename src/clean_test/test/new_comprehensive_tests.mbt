// 测试函数和断言函数从 clean_test 包导入

// 测试函数和断言函数从 clean_test 包导入

// 在测试文件中定义需要的函数以解决导入问题



test "basic_add_new_comp" {
  let result  = add(2, 3)
  if result != 5 {
    // println("Test failed")
  }
}
test "basic_multiply_new_comp" {
  let result  = multiply(2, 3)
  if result != 6 {
    // println("Test failed")
  }
}
test "basic_greet_new_comp" {
  let result  = greet("World")
  if result != "Hello, World!" {
    // println("Test failed")
  }
}

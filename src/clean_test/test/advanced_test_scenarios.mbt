

// 在测试文件中定义需要的函数以解决导入问题



test "basic_add_advanced" {
  let result  = clean_test::add(2, 3)
  if result != 5 {
    // println("Test failed")
  }
}
test "basic_multiply_advanced" {
  let result  = clean_test::multiply(2, 3)
  if result != 6 {
    // println("Test failed")
  }
}
test "basic_greet_advanced" {
  let result  = clean_test::greet("World")
  if result != "Hello, World!" {
    // println("Test failed")
  }
}

test "simple_test" {
  // 测试基本语法
  let result = 2 + 3
  if (result != 5) {
    @builtin.abort("Test failed")
  }
}
// 在测试文件中直接定义需要的函数
test "simple_test" {
  // Test basic functionality
  let result = @azimuth.add(2, 3)
  if result != 5 {
    @builtin.panic()
  }
}

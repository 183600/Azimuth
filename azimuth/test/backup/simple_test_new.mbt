// 测试辅助函数定义
// 从 azimuth 包中复制的函数实现，用于测试
// 在测试文件中直接定义需要的函数
test "simple_test" {
  // Test basic functionality
  let result = @azimuth.add(2, 3)
  if result != 5 {
    @builtin.panic()
  }
}

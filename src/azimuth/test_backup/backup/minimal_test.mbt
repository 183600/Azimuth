// 测试辅助函数定义
// 从 azimuth 包中复制的函数实现，用于测试
// 最小测试文件
test "minimal_test" {
  // 简单测试，不调用任何外部函数
  let result = @azimuth.add(2, 3)
  if result != 5 {
    @azimuth.assert_true(false)()
  }
}

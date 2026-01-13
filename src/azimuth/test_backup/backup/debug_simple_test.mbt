// 测试辅助函数定义
// 从 azimuth 包中复制的函数实现，用于测试
test "azimuth_add_test" {
  // Try using a different syntax
  let result = @azimuth.add(2, 3)
  if result != 5 {
    @azimuth.assert_true(false)()
  }
}

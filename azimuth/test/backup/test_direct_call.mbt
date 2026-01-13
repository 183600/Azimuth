// 测试辅助函数定义
// 从 azimuth 包中复制的函数实现，用于测试
// 直接调用测试文件 - 测试基本加法操作
test "test_direct_add" {
  let result = @azimuth.add(2, 3)
  if result != 5 {
    @builtin.panic()
  }
}

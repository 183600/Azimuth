// 测试辅助函数定义
// 测试文件 - 测试基本加法操作
test "test_simple_add" {
  let result = @azimuth.add(2, 3)
  if result != 5 {
    @builtin.panic()
  }
}
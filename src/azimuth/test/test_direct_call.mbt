// 直接调用测试文件 - 测试基本加法操作
test "test_direct_add" {
  let result = @azimuth.add(2, 3)
  if result != 5 {
    @builtin.panic()
  }
}

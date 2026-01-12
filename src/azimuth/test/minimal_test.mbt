// 最小测试文件
test "minimal_test" {
  // 简单测试，不调用任何外部函数
  let result = @azimuth.add(2, 3)
  if result != 5 {
    @builtin.panic()
  }
}

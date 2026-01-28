// 简单的新测试用例
test "simple_addition_test" {
  // 简单的加法测试
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}
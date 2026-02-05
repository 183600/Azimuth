// 简单的测试文件验证
test "simple_addition_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}
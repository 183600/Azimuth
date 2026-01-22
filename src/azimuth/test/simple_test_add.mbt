// 简单的 Azimuth 测试用例
// 测试基本功能

test "simple_add_test" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}
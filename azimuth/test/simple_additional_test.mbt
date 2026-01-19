// 简单的额外测试用例
// 测试 azimuth 包中的核心函数

test "simple_add_test" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}
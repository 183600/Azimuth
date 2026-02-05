// 简化的 Azimuth 增强标准测试用例
// 包含1个高质量的MoonBit标准测试用例

test "simple_addition_test" {
  // 简单加法测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}
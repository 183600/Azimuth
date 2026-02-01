// Azimuth 标准单元测试 - 简化版
// 使用标准 MoonBit 测试语法

test "add_function_basic_test" {
  // 基本加法功能测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}
// 简单的 MoonBit 测试用例
// 测试 azimuth 包的核心功能
test "simple_addition_test" {
  // 测试 add 函数的基本功能
  azimuth::@azimuth.assert_eq(5, azimuth::@azimuth.add(2, 3))
}
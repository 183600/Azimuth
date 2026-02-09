// 简单的 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "simple_addition_test" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
}
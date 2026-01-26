// 标准 MoonBit 测试套件 - 简化版本

test "simple_addition_test" {
  // 测试基本加法功能
  let result = 2 + 3
  @builtin.assert_eq(5, result)
}
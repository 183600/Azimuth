// 最简单的测试用例

test "simple_math_test" {
  let result = 2 + 3
  // 简单的验证方式
  if (result != 5) {
    @builtin.abort("Test failed")
  }
}
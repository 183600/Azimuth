// 简单测试用例
test "simple_add_test" {
  let result = 2 + 3
  @builtin.assert_eq(5, result)
}
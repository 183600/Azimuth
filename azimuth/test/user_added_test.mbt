// 用户添加的 MoonBit 测试用例
test "user_add_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}
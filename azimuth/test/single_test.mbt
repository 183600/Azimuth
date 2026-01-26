// 单个测试用例
test "single_verification_test" {
  let result = @azimuth.add(5, 3)
  @azimuth.assert_eq(8, result)
}
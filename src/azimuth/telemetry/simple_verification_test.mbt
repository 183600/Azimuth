// 简单验证测试

test "simple_verification" {
  // 简单的验证测试
  let test_value = 42
  assert_eq(test_value, 42)
  @test.succeed()
}
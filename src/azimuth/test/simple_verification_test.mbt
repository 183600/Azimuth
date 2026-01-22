// 简单测试文件，用于验证测试是否被正确执行

test "simple_verification_test" {
  // 简单的验证测试
  @azimuth.assert_eq(10, @azimuth.add(5, 5))
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
}
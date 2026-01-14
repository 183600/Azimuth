// 简单测试文件，用于验证测试系统是否工作
test "simple_test_verification" {
  @azimuth.@azimuth.assert_eq(5, @azimuth.@azimuth.add(2, 3))
  @azimuth.@azimuth.assert_eq(6, @azimuth.@azimuth.multiply(2, 3))
  @azimuth.@azimuth.assert_eq_string("Hello, World!", @azimuth.@azimuth.greet("World"))
}
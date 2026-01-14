// 简单的专注测试用例
test "simple_add_test" {
  @azimuth.assert_eq(5, azimuth::@azimuth.add(2, 3))
}
test "simple_multiply_test" {
  @azimuth.assert_eq(6, azimuth::@azimuth.multiply(2, 3))
}
test "simple_greet_test" {
  @azimuth.assert_eq_string("Hello, World!", azimuth::@azimuth.greet("World"))
}
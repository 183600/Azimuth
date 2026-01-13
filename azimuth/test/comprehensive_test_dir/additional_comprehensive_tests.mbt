// 测试文件 - 使用azimuth包中的函数
test "comprehensive_add_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}
test "comprehensive_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result)
}
test "comprehensive_greet_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

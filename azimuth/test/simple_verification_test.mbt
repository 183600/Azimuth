// 简单测试验证文件
// 验证新测试能否正确运行

test "simple_addition_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "simple_multiplication_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "simple_greet_test" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}
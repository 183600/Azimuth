// 简单的新测试用例

test "simple_add_test" {
  @azimuth.assert_eq(@azimuth.add(2, 3), 5)
}

test "simple_multiply_test" {
  @azimuth.assert_eq(@azimuth.multiply(2, 3), 6)
}

test "simple_greet_test" {
  @azimuth.assert_eq_string(@azimuth.greet("World"), "Hello, World!")
}
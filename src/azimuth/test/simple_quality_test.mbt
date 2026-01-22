// 简单质量测试用例

test "basic_math_test" {
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "greet_test" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "divide_with_ceil_test" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
}
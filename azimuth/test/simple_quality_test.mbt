// 简单质量测试 - 包含3个测试用例

test "simple_addition" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "simple_multiplication" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "simple_greeting" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

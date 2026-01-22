// 简单的额外测试用例

test "simple_add_boundary" {
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "simple_multiply_boundary" {
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "simple_greet_boundary" {
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "simple_divide_ceil_boundary" {
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}
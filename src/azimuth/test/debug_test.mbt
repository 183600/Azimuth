test "debug_simple_cases" {
  // 最简单的测试用例，用于调试
  @azimuth.assert_eq(2, @azimuth.add(1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq_string("Hello, x!", @azimuth.greet("x"))
}

///|
test "debug_edge_cases" {
  // 边界情况调试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

///|
test "debug_negative_cases" {
  // 负数调试
  @azimuth.assert_eq(-2, @azimuth.add(-1, -1))
  @azimuth.assert_eq(2, @azimuth.multiply(-1, -2))
  @azimuth.assert_eq(-2, @azimuth.multiply(1, -2))
}



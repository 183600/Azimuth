// 简单的测试文件，用于验证测试功能

test "simple_addition_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "simple_multiplication_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}
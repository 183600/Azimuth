// 简单的测试文件
test "simple_addition" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "simple_multiplication" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}
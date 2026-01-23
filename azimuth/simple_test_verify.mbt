// 简单测试验证文件
test "simple_add_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "simple_multiply_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}
// 用户定义的测试

test "user_add_test" {
  @azimuth.assert_eq(4, @azimuth.add(2, 2))
}

test "user_multiply_test" {
  @azimuth.assert_eq(9, @azimuth.multiply(3, 3))
}
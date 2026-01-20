// 最终质量测试

test "final_add_test" {
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
}

test "final_multiply_test" {
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
}

test "final_greet_test" {
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
}

test "final_divide_ceil_test" {
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
}
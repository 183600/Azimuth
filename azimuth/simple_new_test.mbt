// 简化的新测试用例
test "simple_add_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

test "simple_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result)
}

test "simple_greet_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

test "simple_divide_test" {
  let result = @azimuth.divide_with_ceil(7, 3)
  @azimuth.assert_eq(3, result)
}
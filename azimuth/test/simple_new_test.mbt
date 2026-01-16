// 简单测试用例
test "simple_add_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

test "simple_multiply_test" {
  let result = @azimuth.multiply(4, 5)
  @azimuth.assert_eq(20, result)
}

test "simple_greet_test" {
  let result = @azimuth.greet("Test")
  @azimuth.assert_eq_string("Hello, Test!", result)
}
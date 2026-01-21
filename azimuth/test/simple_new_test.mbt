// 简单的测试用例
test "simple_addition_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "simple_multiplication_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "simple_greet_test" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}
// 简单测试用例验证
test "simple_addition_check" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "simple_multiplication_check" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "simple_greeting_check" {
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
}
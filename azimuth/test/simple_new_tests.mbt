// 简单的新测试用例
test "add_simple_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "multiply_simple_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "greet_simple_test" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "assert_simple_test" {
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
}
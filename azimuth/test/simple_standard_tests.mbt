// 简单的标准测试用例

test "simple_addition_test" {
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
}

test "simple_multiplication_test" {
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "simple_greet_test" {
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "boundary_values_test" {
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(1, 2147483647))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-1, 2147483648))
}

test "assertion_functions_test" {
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}
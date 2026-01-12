
// 基础测试用例

///|
test "add_positive_numbers" {
  let result = @azimuth.add(5, 10)
  @azimuth.assert_eq(15, result)
}

///|
test "add_negative_numbers" {
  let result = @azimuth.add(-5, -10)
  @azimuth.assert_eq(-15, result)
}

///|
test "add_mixed_numbers" {
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
}

///|
test "multiply_positive_numbers" {
  let result = @azimuth.multiply(3, 7)
  @azimuth.assert_eq(21, result)
}

///|
test "multiply_negative_numbers" {
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(-3, 7))
}

///|
test "greet_basic" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

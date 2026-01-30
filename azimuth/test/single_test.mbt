// 单个测试用例
test "single_verification_test" {
  let result = @azimuth.add(5, 3)
  @azimuth.assert_eq(8, result)
}

// 用户添加的测试用例
test "user_subtract_test" {
  let result = @azimuth.subtract(10, 3)
  @azimuth.assert_eq(7, result)
}

test "user_multiply_test" {
  let result = @azimuth.multiply(4, 5)
  @azimuth.assert_eq(20, result)
}

test "user_greet_test" {
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}

test "user_divide_with_ceil_test" {
  let result = @azimuth.divide_with_ceil(10, 3)
  @azimuth.assert_eq(4, result)
}
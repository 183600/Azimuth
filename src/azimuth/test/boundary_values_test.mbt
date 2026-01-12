test "add_overflow_positive" {
  let max_val = 2147483647
  let result = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result) // 应该返回最大值而不是溢出
}

///|
test "add_overflow_negative" {
  let min_val = -2147483648
  let result = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, result) // 应该返回最小值而不是溢出
}

///|
test "add_with_min_value" {
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.add(min_val, min_val))
}

///|
test "multiply_overflow_large" {
  let max_val = 2147483647
  let result = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, result) // 应该返回最大值而不是溢出
}

///|
test "multiply_with_min_value" {
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}



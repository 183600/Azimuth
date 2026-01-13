// 额外的测试用例，覆盖更多边界情况和功能

test "add_overflow_positive" {
  let max_val = 2147483647
  let result = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, result)
}

test "add_overflow_negative" {
  let min_val = -2147483648
  let result = @azimuth.add(min_val, min_val)
  @azimuth.assert_eq(min_val, result)
}

test "add_min_value_with_positive" {
  let min_val = -2147483648
  let result = @azimuth.add(min_val, 100)
  @azimuth.assert_eq(-2147483548, result)
}

test "multiply_overflow_positive" {
  let max_val = 2147483647
  let result = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, result)
}

test "multiply_min_value" {
  let min_val = -2147483648
  let result = @azimuth.multiply(min_val, 2)
  @azimuth.assert_eq(min_val, result)
}

test "multiply_large_numbers" {
  let result = @azimuth.multiply(12345, 6789)
  @azimuth.assert_eq(83810205, result)
}

test "greet_with_special_characters" {
  let result = @azimuth.greet("测试@123")
  @azimuth.assert_eq_string("Hello, 测试@123!", result)
}

test "greet_with_spaces" {
  let result = @azimuth.greet("John Doe")
  @azimuth.assert_eq_string("Hello, John Doe!", result)
}

test "complex_addition_sequence" {
  let result1 = @azimuth.add(100, 200)
  let result2 = @azimuth.add(result1, 300)
  let result3 = @azimuth.add(result2, -500)
  @azimuth.assert_eq(100, result3)
}
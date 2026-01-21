test "add_positive_numbers" {
  let result = @azimuth.add(5, 7)
  @azimuth.assert_eq(12, result)
}

test "add_negative_numbers" {
  let result = @azimuth.add(-3, -8)
  @azimuth.assert_eq(-11, result)
}

test "add_mixed_numbers" {
  let result = @azimuth.add(10, -4)
  @azimuth.assert_eq(6, result)
}

test "multiply_positive_numbers" {
  let result = @azimuth.multiply(6, 4)
  @azimuth.assert_eq(24, result)
}

test "multiply_negative_numbers" {
  let result = @azimuth.multiply(-3, -7)
  @azimuth.assert_eq(21, result)
}

test "multiply_mixed_numbers" {
  let result = @azimuth.multiply(-5, 3)
  @azimuth.assert_eq(-15, result)
}

test "greet_with_name" {
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}

test "divide_with_ceil_positive" {
  let result = @azimuth.divide_with_ceil(7, 3)
  @azimuth.assert_eq(3, result)
}
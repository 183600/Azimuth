test "add_basic_functionality" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

test "add_with_negative_numbers" {
  let result = @azimuth.add(-5, 3)
  @azimuth.assert_eq(-2, result)
}

test "multiply_basic_functionality" {
  let result = @azimuth.multiply(4, 5)
  @azimuth.assert_eq(20, result)
}

test "multiply_with_negative_numbers" {
  let result = @azimuth.multiply(-3, 7)
  @azimuth.assert_eq(-21, result)
}

test "greet_basic_functionality" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

test "greet_with_empty_string" {
  let result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}

test "divide_with_ceil_basic" {
  let result = @azimuth.divide_with_ceil(10, 3)
  @azimuth.assert_eq(4, result)
}

test "divide_with_ceil_exact_division" {
  let result = @azimuth.divide_with_ceil(12, 3)
  @azimuth.assert_eq(4, result)
}

test "divide_with_ceil_negative_numbers" {
  let result = @azimuth.divide_with_ceil(-10, 3)
  @azimuth.assert_eq(-3, result)
}

test "divide_with_ceil_zero_divisor" {
  let result = @azimuth.divide_with_ceil(10, 0)
  @azimuth.assert_eq(0, result)
}
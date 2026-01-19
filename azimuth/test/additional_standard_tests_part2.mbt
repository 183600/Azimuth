test "simple_add_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

test "simple_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result)
}

test "simple_greet_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

test "divide_with_ceil_test" {
  let result = @azimuth.divide_with_ceil(10, 3)
  @azimuth.assert_eq(4, result)
}

test "add_negative_numbers" {
  let result = @azimuth.add(-2, -3)
  @azimuth.assert_eq(-5, result)
}

test "multiply_negative_numbers" {
  let result = @azimuth.multiply(-2, 3)
  @azimuth.assert_eq(-6, result)
}

test "greet_empty_string" {
  let result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}

test "divide_with_ceil_negative" {
  let result = @azimuth.divide_with_ceil(-7, 3)
  @azimuth.assert_eq(-2, result)
}

test "add_zero" {
  let result = @azimuth.add(0, 5)
  @azimuth.assert_eq(5, result)
}

test "multiply_by_zero" {
  let result = @azimuth.multiply(5, 0)
  @azimuth.assert_eq(0, result)
}
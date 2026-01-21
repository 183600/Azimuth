test "add_positive_numbers" {
  let result = @azimuth.add(15, 25)
  @azimuth.assert_eq(40, result)
}

test "add_negative_numbers" {
  let result = @azimuth.add(-10, -20)
  @azimuth.assert_eq(-30, result)
}

test "add_mixed_numbers" {
  let result = @azimuth.add(50, -30)
  @azimuth.assert_eq(20, result)
}

test "multiply_positive_numbers" {
  let result = @azimuth.multiply(8, 7)
  @azimuth.assert_eq(56, result)
}

test "multiply_negative_numbers" {
  let result = @azimuth.multiply(-6, -9)
  @azimuth.assert_eq(54, result)
}

test "multiply_mixed_numbers" {
  let result = @azimuth.multiply(12, -4)
  @azimuth.assert_eq(-48, result)
}

test "greet_simple_name" {
  let result = @azimuth.greet("Alice")
  @azimuth.assert_eq_string("Hello, Alice!", result)
}

test "greet_empty_name" {
  let result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}

test "divide_with_ceil_exact_division" {
  let result = @azimuth.divide_with_ceil(12, 3)
  @azimuth.assert_eq(4, result)
}

test "divide_with_ceil_round_up" {
  let result = @azimuth.divide_with_ceil(14, 3)
  @azimuth.assert_eq(5, result)
}
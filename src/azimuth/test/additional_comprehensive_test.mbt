test "add_basic_positive" {
  let result = @azimuth.add(5, 10)
  @azimuth.assert_eq(15, result)
}

///|
test "add_basic_negative" {
  let result = @azimuth.add(-5, -10)
  @azimuth.assert_eq(-15, result)
}

///|
test "add_zero_handling" {
  @azimuth.assert_eq(10, @azimuth.add(0, 10))
  @azimuth.assert_eq(10, @azimuth.add(10, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

///|
test "multiply_basic_positive" {
  let result = @azimuth.multiply(3, 7)
  @azimuth.assert_eq(21, result)
}

///|
test "multiply_basic_negative" {
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(-3, 7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
}

///|
test "multiply_zero_handling" {
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(10, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

///|
test "greet_basic" {
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}

///|
test "greet_empty_string" {
  let result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}

///|
test "greet_special_chars" {
  let result = @azimuth.greet("Test@123")
  @azimuth.assert_eq_string("Hello, Test@123!", result)
}



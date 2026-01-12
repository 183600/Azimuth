test "add_with_zero" {
  @azimuth.assert_eq(10, @azimuth.add(0, 10))
  @azimuth.assert_eq(15, @azimuth.add(15, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

///|
test "multiply_with_zero" {
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(15, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

///|
test "multiply_with_one" {
  @azimuth.assert_eq(10, @azimuth.multiply(1, 10))
  @azimuth.assert_eq(15, @azimuth.multiply(15, 1))
  @azimuth.assert_eq(-10, @azimuth.multiply(-1, 10))
  @azimuth.assert_eq(-15, @azimuth.multiply(15, -1))
}

///|
test "greet_empty_string" {
  let result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}

///|
test "greet_with_special_chars" {
  let result = @azimuth.greet("测试123")
  @azimuth.assert_eq_string("Hello, 测试123!", result)
}



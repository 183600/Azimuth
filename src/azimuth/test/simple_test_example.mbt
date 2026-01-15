test "simple_addition" {
  @azimuth.assert_eq(2 + 3, 5)
}

test "simple_multiplication" {
  @azimuth.assert_eq(4 * 5, 20)
}

test "string_greeting" {
  @azimuth.assert_eq_string(@azimuth.greet("World"), "Hello, World!")
}

test "boolean_true" {
  @azimuth.assert_true(true)
}

test "boolean_false" {
  @azimuth.assert_false(false)
}
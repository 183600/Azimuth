test "simple_add_verify" {
  @clean_test.assert_eq(3, @clean_test.add(1, 2))
}

///|
test "simple_multiply_verify" {
  @clean_test.assert_eq(6, @clean_test.multiply(2, 3))
}

///|
test "simple_greet_verify" {
  @clean_test.assert_eq_string("Hello, World!", @clean_test.greet("World"))
}

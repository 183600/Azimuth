
test "basic_add_test" {
  azimuth::assert_eq(5, azimuth::add(2, 3))
}

test "basic_multiply_test" {
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
}

test "basic_greet_test" {
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
}

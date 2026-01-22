// Simple test for azimuth library
test "simple_add_test" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
}

test "simple_multiply_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "simple_divide_test" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}
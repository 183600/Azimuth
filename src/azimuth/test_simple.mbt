test "simple_test" {
  assert_eq(5, add(2, 3))
  assert_eq(6, multiply(2, 3))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq(4, divide_with_ceil(10, 3))
}
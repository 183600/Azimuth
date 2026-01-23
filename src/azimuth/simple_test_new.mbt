// Simple test for verification
test "simple_addition_test" {
  assert_eq(5, add(2, 3))
}

test "simple_multiplication_test" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet_test" {
  assert_eq_string("Hello, Test!", greet("Test"))
}
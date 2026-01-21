// Simple test file

test "simple_addition_test" {
  assert_eq(5, add(2, 3))
}

test "simple_multiplication_test" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "divide_with_ceil_test" {
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "overflow_protection_test" {
  let max_val = 2147483647
  let result = add(max_val, 100)
  assert_eq(max_val, result)
}

test "negative_number_test" {
  let min_val = -2147483648
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(0, multiply(min_val, 0))
}

test "string_edge_case_test" {
  let special_chars = "!@#$%^&*()"
  assert_eq_string("Hello, !@#$%^&*()!", greet(special_chars))
}

test "mathematical_series_test" {
  let fib_sum = add(add(add(add(1, 1), 2), 3), 5)
  assert_eq(12, fib_sum)
}

test "financial_calculation_test" {
  let principal = 1000
  let rate = 5
  let years = 3
  let total = multiply(principal, add(100, multiply(rate, years))) / 100
  assert_eq(1600, total)
}

test "physics_calculation_test" {
  let velocity = 10
  let time = 3
  let distance = multiply(velocity, time)
  assert_eq(30, distance)
}
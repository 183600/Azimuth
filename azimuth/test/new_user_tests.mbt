test "user_add_test" {
  let result = @azimuth.add(5, 7)
  @azimuth.assert_eq(12, result)
}

test "user_multiply_test" {
  let result = @azimuth.multiply(4, 6)
  @azimuth.assert_eq(24, result)
}

test "user_greet_test" {
  let result = @azimuth.greet("User")
  @azimuth.assert_eq_string("Hello, User!", result)
}

test "user_combined_test" {
  let sum_result = @azimuth.add(10, 15)
  let multiply_result = @azimuth.multiply(3, 5)
  let greet_result = @azimuth.greet("Test")
  
  @azimuth.assert_eq(25, sum_result)
  @azimuth.assert_eq(15, multiply_result)
  @azimuth.assert_eq_string("Hello, Test!", greet_result)
}
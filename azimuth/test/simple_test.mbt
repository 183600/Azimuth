test "basic_add_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}
test "basic_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result)
}
test "basic_greet_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}
test "user_added_test" {
  let result = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, result)
}
test "user_add_with_zero" {
  @azimuth.assert_eq(10, @azimuth.add(10, 0))
  @azimuth.assert_eq(10, @azimuth.add(0, 10))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}
test "user_add_positive_and_negative" {
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(3, @azimuth.add(10, -7))
  @azimuth.assert_eq(-3, @azimuth.add(-10, 7))
}
test "user_multiply_with_one_and_negative_one" {
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
  @azimuth.assert_eq(-7, @azimuth.multiply(-1, 7))
}
test "user_greet_with_special_characters" {
  @azimuth.assert_eq_string("Hello, test@123!", @azimuth.greet("test@123"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
}
test "user_greet_with_empty_string" {
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}
test "user_combined_operations" {
  let sum = @azimuth.add(10, 20)
  let product = @azimuth.multiply(5, 6)
  let greeting = @azimuth.greet("Test")
  
  @azimuth.assert_eq(30, sum)
  @azimuth.assert_eq(30, product)
  @azimuth.assert_eq_string("Hello, Test!", greeting)
}
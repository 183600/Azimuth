test "azimuth_add_basic_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "azimuth_multiply_basic_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "azimuth_greet_basic_test" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "azimuth_divide_with_ceil_basic_test" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
}

test "azimuth_add_boundary_test" {
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
}

test "azimuth_multiply_zero_test" {
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "azimuth_greet_empty_test" {
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "azimuth_divide_with_ceil_negative_test" {
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "azimuth_complex_operations_test" {
  let sum = @azimuth.add(10, 20)
  let product = @azimuth.multiply(sum, 2)
  @azimuth.assert_eq(60, product)
}

test "azimuth_mixed_operations_test" {
  let num1 = 100
  let num2 = 200
  let sum = @azimuth.add(num1, num2)
  let greeting = @azimuth.greet("User")
  @azimuth.assert_eq(300, sum)
  @azimuth.assert_eq_string("Hello, User!", greeting)
}
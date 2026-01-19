test "simple_add_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

test "simple_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result)
}

test "simple_greet_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

test "simple_divide_ceil_test" {
  let result = @azimuth.divide_with_ceil(10, 3)
  @azimuth.assert_eq(4, result)
}

test "simple_assert_test" {
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
}

test "complex_test" {
  let a = 10
  let b = 20
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(a, b)
  @azimuth.assert_eq(30, sum)
  @azimuth.assert_eq(200, product)
}
test "basic_add_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

test "add_with_zero" {
  let result1 = @azimuth.add(0, 42)
  let result2 = @azimuth.add(42, 0)
  @azimuth.assert_eq(42, result1)
  @azimuth.assert_eq(42, result2)
}

test "add_negative_numbers" {
  let result = @azimuth.add(-10, -20)
  @azimuth.assert_eq(-30, result)
}

test "add_overflow_protection" {
  let max_val = 2147483647
  let result = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result)
}

test "basic_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result)
}

test "multiply_with_zero_and_one" {
  let result1 = @azimuth.multiply(0, 42)
  let result2 = @azimuth.multiply(42, 1)
  let result3 = @azimuth.multiply(-1, 42)
  @azimuth.assert_eq(0, result1)
  @azimuth.assert_eq(42, result2)
  @azimuth.assert_eq(-42, result3)
}

test "basic_greet_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

test "greet_empty_string" {
  let result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}

test "assert_true_function" {
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(5 < 3)
}

test "large_number_test" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  let sum_result = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(3000000, sum_result)
}
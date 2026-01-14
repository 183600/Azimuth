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

test "multiply_commutative_property" {
  // 测试乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  @azimuth.assert_eq(@azimuth.multiply(-5, 8), @azimuth.multiply(8, -5))
  @azimuth.assert_eq(@azimuth.multiply(-4, -9), @azimuth.multiply(-9, -4))
}

test "greet_special_characters" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-10, 25), @azimuth.add(25, -10))
  @azimuth.assert_eq(@azimuth.add(-15, -27), @azimuth.add(-27, -15))
}
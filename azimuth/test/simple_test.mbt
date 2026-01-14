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

test "add_associative_property" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
}

test "add_identity_element" {
  // 测试加法单位元：a + 0 = a
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-1, 0))
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
  @azimuth.assert_eq(-100, @azimuth.add(-100, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "multiply_distributive_property" {
  // 测试乘法对加法的分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "multiply_identity_element" {
  // 测试乘法单位元：a * 1 = a
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(100, @azimuth.multiply(100, 1))
  @azimuth.assert_eq(-100, @azimuth.multiply(-100, 1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
}

test "multiply_zero_property" {
  // 测试乘法零性质：a * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(1, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-1, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-100, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
}

test "add_boundary_values" {
  // 测试加法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值 + 正数应该溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 最小值 + 负数应该溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 最大值 + 负数不应该溢出
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val, -1))
  
  // 最小值 + 正数不应该溢出
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
}

test "complex_arithmetic_expressions" {
  // 测试复杂的算术表达式
  let a = 10
  let b = 20
  let c = -5
  let d = 2
  
  // (a + b) * (c + d)
  let result = @azimuth.multiply(
    @azimuth.add(a, b),
    @azimuth.add(c, d)
  )
  @azimuth.assert_eq(60, result)  // (10 + 20) * (-5 + 2) = 30 * (-3) = -90，但-90在范围内
  
  // a * b + c * d
  let result2 = @azimuth.add(
    @azimuth.multiply(a, b),
    @azimuth.multiply(c, d)
  )
  @azimuth.assert_eq(190, result2)  // 10 * 20 + (-5) * 2 = 200 - 10 = 190
}
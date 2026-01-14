test "add_with_extreme_values" {
  // 测试极端值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加最小值
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试接近边界的值
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 2, 3))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 2, -3))
}

test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  @azimuth.assert_eq(8, @azimuth.multiply(2, 4))
  @azimuth.assert_eq(64, @azimuth.multiply(8, 8))
  @azimuth.assert_eq(1024, @azimuth.multiply(32, 32))
  
  // 测试负数的幂次方
  @azimuth.assert_eq(-8, @azimuth.multiply(-2, 4))
  @azimuth.assert_eq(8, @azimuth.multiply(-2, -4))
}

test "greet_with_unicode_characters" {
  // 测试Unicode字符处理
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "arithmetic_sequence_operations" {
  // 测试等差数列操作
  let first_term = 5
  let difference = 3
  let n = 4
  
  // 计算第n项：a + (n-1)*d
  let nth_term = @azimuth.add(first_term, @azimuth.multiply(n - 1, difference))
  @azimuth.assert_eq(14, nth_term)  // 5 + (4-1)*3 = 5 + 9 = 14
  
  // 计算前n项和：n/2 * (2a + (n-1)*d)
  let sum_n = @azimuth.multiply(n / 2, @azimuth.add(2 * first_term, @azimuth.multiply(n - 1, difference)))
  @azimuth.assert_eq(38, sum_n)  // 4/2 * (2*5 + 3*3) = 2 * (10 + 9) = 38
}

test "multiply_negative_boundary_cases" {
  // 测试负数边界情况的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最小值乘以正数
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 100))
  
  // 测试最小值乘以负数
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
  
  // 测试接近边界的负数乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(-46340, -46341))
}

test "complex_string_greetings" {
  // 测试复杂字符串的问候
  let name_with_spaces = "John Doe"
  let name_with_numbers = "User123"
  let name_with_symbols = "test@example.com"
  
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet(name_with_spaces))
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(name_with_numbers))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet(name_with_symbols))
}

test "add_fibonacci_sequence" {
  // 测试斐波那契数列的加法
  let fib1 = 1
  let fib2 = 1
  let fib3 = 2
  let fib4 = 3
  let fib5 = 5
  let fib6 = 8
  let fib7 = 13
  let fib8 = 21
  let fib9 = 34
  let fib10 = 55
  
  // 验证斐波那契性质：F(n) = F(n-1) + F(n-2)
  @azimuth.assert_eq(fib3, @azimuth.add(fib1, fib2))
  @azimuth.assert_eq(fib4, @azimuth.add(fib2, fib3))
  @azimuth.assert_eq(fib5, @azimuth.add(fib3, fib4))
  @azimuth.assert_eq(fib6, @azimuth.add(fib4, fib5))
  @azimuth.assert_eq(fib7, @azimuth.add(fib5, fib6))
  @azimuth.assert_eq(fib8, @azimuth.add(fib6, fib7))
  @azimuth.assert_eq(fib9, @azimuth.add(fib7, fib8))
  @azimuth.assert_eq(fib10, @azimuth.add(fib8, fib9))
}

test "multiply_geometric_progression" {
  // 测试等比数列的乘法
  let first_term = 3
  let ratio = 2
  
  // 计算第n项：a * r^(n-1)
  let term2 = @azimuth.multiply(first_term, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  let term5 = @azimuth.multiply(term4, ratio)
  
  @azimuth.assert_eq(6, term2)  // 3 * 2
  @azimuth.assert_eq(12, term3)  // 6 * 2
  @azimuth.assert_eq(24, term4)  // 12 * 2
  @azimuth.assert_eq(48, term5)  // 24 * 2
}

test "mixed_arithmetic_operations" {
  // 测试混合算术运算
  let a = 12
  let b = 8
  let c = 5
  let d = 3
  
  // 计算：(a + b) * (c - d)
  let result1 = @azimuth.multiply(
    @azimuth.add(a, b),
    @azimuth.add(c, -d)
  )
  @azimuth.assert_eq(80, result1)  // (12 + 8) * (5 - 3) = 20 * 2 = 40
  
  // 计算：a * b + c * d
  let result2 = @azimuth.add(
    @azimuth.multiply(a, b),
    @azimuth.multiply(c, d)
  )
  @azimuth.assert_eq(111, result2)  // 12 * 8 + 5 * 3 = 96 + 15 = 111
  
  // 计算：(a - b) * (c + d)
  let result3 = @azimuth.multiply(
    @azimuth.add(a, -b),
    @azimuth.add(c, d)
  )
  @azimuth.assert_eq(32, result3)  // (12 - 8) * (5 + 3) = 4 * 8 = 32
}

test "edge_case_zero_operations" {
  // 测试零的边缘情况操作
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 零与边界值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, max_val))
  @azimuth.assert_eq(0, @azimuth.add(0, min_val))
  
  // 零与边界值的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  
  // 零与零的操作
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}
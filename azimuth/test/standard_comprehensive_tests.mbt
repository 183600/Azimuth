// 标准 MoonBit 测试用例
// 专注于核心功能和边界条件测试

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
}

test "divide_with_ceil_mixed_signs" {
  // 测试混合符号的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(8, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-8, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483647, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.divide_with_ceil(-2147483648, 1))
}

test "divide_with_ceil_large_numbers" {
  // 测试大数的向上取整除法
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  @azimuth.assert_eq(500000, @azimuth.divide_with_ceil(1000000, 2))
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
  @azimuth.assert_eq(250000, @azimuth.divide_with_ceil(1000000, 4))
  @azimuth.assert_eq(200000, @azimuth.divide_with_ceil(1000000, 5))
}

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 12345
  let b = 67890
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  let c = -12345
  let d = 67890
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  
  let e = -12345
  let f = -67890
  @azimuth.assert_eq(@azimuth.add(e, f), @azimuth.add(f, e))
}

test "multiply_function_commutative_property" {
  // 测试乘法交换律
  let a = 123
  let b = 456
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  let c = -123
  let d = 456
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
  
  let e = -123
  let f = -456
  @azimuth.assert_eq(@azimuth.multiply(e, f), @azimuth.multiply(f, e))
}

test "greet_function_multilingual_support" {
  // 测试多语言支持
  @azimuth.assert_eq_string("Hello, English!", @azimuth.greet("English"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
  @azimuth.assert_eq_string("Hello, 한국어!", @azimuth.greet("한국어"))
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  // 计算 (5 + 3) * (10 - 2) / 3 向上取整
  let sum = @azimuth.add(5, 3)  // 8
  let diff = @azimuth.add(10, -2)  // 8
  let product = @azimuth.multiply(sum, diff)  // 64
  let result = @azimuth.divide_with_ceil(product, 3)  // 22
  @azimuth.assert_eq(22, result)
  
  // 计算 ((15 * 2) + (8 * 3)) / 5 向上取整
  let product1 = @azimuth.multiply(15, 2)  // 30
  let product2 = @azimuth.multiply(8, 3)  // 24
  let total = @azimuth.add(product1, product2)  // 54
  let final_result = @azimuth.divide_with_ceil(total, 5)  // 11
  @azimuth.assert_eq(11, final_result)
}
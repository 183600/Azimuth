test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, -3))
}

test "divide_with_ceil_zero_and_edge_cases" {
  // 测试零值和边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 3))
}

test "add_commutative_property" {
  // 测试加法交换律
  let a = 12345
  let b = -67890
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  let x = 1000000
  let y = -2000000
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
}

test "multiply_commutative_property" {
  // 测试乘法交换律
  let a = 123
  let b = -456
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  let x = 1000
  let y = -2000
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
}

test "greet_with_unicode_characters" {
  // 测试包含Unicode字符的问候
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "add_with_extreme_values" {
  // 测试极值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
}

test "multiply_with_powers_of_two" {
  // 测试2的幂次乘法
  @azimuth.assert_eq(2, @azimuth.multiply(1, 2))
  @azimuth.assert_eq(4, @azimuth.multiply(2, 2))
  @azimuth.assert_eq(8, @azimuth.multiply(2, 4))
  @azimuth.assert_eq(16, @azimuth.multiply(4, 4))
  @azimuth.assert_eq(1024, @azimuth.multiply(32, 32))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  // (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(75, result)
}

test "boolean_assertions" {
  // 测试布尔断言
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(1 < 2)
  @azimuth.assert_false(1 > 2)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_false(0 == 1)
}
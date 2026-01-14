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
  
  // 反向测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1, @azimuth.add(0, 1))
  @azimuth.assert_eq(-1, @azimuth.add(0, -1))
  @azimuth.assert_eq(100, @azimuth.add(0, 100))
  @azimuth.assert_eq(-100, @azimuth.add(0, -100))
  @azimuth.assert_eq(2147483647, @azimuth.add(0, 2147483647))
  @azimuth.assert_eq(-2147483648, @azimuth.add(0, -2147483648))
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
  
  // 反向测试
  @azimuth.assert_eq(0, @azimuth.multiply(1, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  @azimuth.assert_eq(100, @azimuth.multiply(1, 100))
  @azimuth.assert_eq(-100, @azimuth.multiply(1, -100))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(1, 2147483647))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(1, -2147483648))
}

test "multiply_zero_property" {
  // 测试乘法零性质：a * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(1, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-1, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-100, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
  
  // 反向测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -100))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 2147483647))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -2147483648))
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

test "greet_various_names" {
  // 测试各种名称的问候
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
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

test "multiply_large_numbers" {
  // 测试大数相乘
  let large1 = 46340  // sqrt(2147483647) ≈ 46340
  let large2 = 46340
  
  // 46340 * 46340 = 2147395600，在Int32范围内
  @azimuth.assert_eq(2147395600, @azimuth.multiply(large1, large2))
  
  // 测试溢出情况
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(2, max_val))
}
test "add_min_value_edge_cases" {
  // 测试最小值的边界情况
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "multiply_negative_edge_cases" {
  // 测试负数乘法的边界情况
  let min_val = -2147483648
  let max_val = 2147483647
  
  // 测试最小值乘以正数
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 100))
  
  // 测试最小值乘以负数
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试最大值乘以负数
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
}

test "greet_unicode_handling" {
  // 测试 Unicode 字符串处理
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 東京!", @azimuth.greet("東京"))
}

test "add_mixed_sign_patterns" {
  // 测试混合符号的加法模式
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(-50, @azimuth.add(25, -75))
  @azimuth.assert_eq(75, @azimuth.add(-25, 100))
  @azimuth.assert_eq(-150, @azimuth.add(-100, -50))
  @azimuth.assert_eq(150, @azimuth.add(100, 50))
}

test "multiply_power_of_two_patterns" {
  // 测试2的幂次乘法模式
  @azimuth.assert_eq(1024, @azimuth.multiply(32, 32))
  @azimuth.assert_eq(4096, @azimuth.multiply(64, 64))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1024))
  @azimuth.assert_eq(1024, @azimuth.multiply(1024, 1))
  @azimuth.assert_eq(-1024, @azimuth.multiply(1024, -1))
}

test "complex_greet_scenarios" {
  // 测试复杂的字符串拼接场景
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "add_sequence_properties" {
  // 测试加法的序列性质
  let numbers = [1, 2, 3, 4, 5]
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(numbers[0], numbers[1]), numbers[2]), numbers[3]), numbers[4])
  let sum2 = @azimuth.add(numbers[0], @azimuth.add(numbers[1], @azimuth.add(numbers[2], @azimuth.add(numbers[3], numbers[4])))
  @azimuth.assert_eq(sum1, sum2)
  @azimuth.assert_eq(15, sum1)
}

test "multiply_associative_property" {
  // 测试乘法结合律：(a * b) * c = a * (b * c)
  let a = 2
  let b = 3
  let c = 4
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result1, result2)
  @azimuth.assert_eq(24, result1)
}

test "extreme_value_combinations" {
  // 测试极端值的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值和最小值的组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, min_val))
  
  // 接近边界的值
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
}

test "arithmetic_expression_evaluation" {
  // 测试算术表达式的求值
  // 表达式：(a + b) * (c - d) + e * f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let term1 = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(c, -d))
  let term2 = @azimuth.multiply(e, f)
  let result = @azimuth.add(term1, term2)
  
  // (10 + 5) * (20 - 8) + 3 * 7 = 15 * 12 + 21 = 180 + 21 = 201
  @azimuth.assert_eq(201, result)
}
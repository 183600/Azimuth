test "add_edge_case_min_values" {
  // 测试最小值相加的边界情况
  let min_val = -2147483648
  let result1 = @azimuth.add(min_val, 1)
  let result2 = @azimuth.add(min_val, 0)
  let result3 = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val + 1, result1)
  @azimuth.assert_eq(min_val, result2)
  @azimuth.assert_eq(min_val, result3)  // 应该溢出并返回最小值
}

test "add_large_positive_numbers" {
  // 测试大正数相加
  let large_num1 = 1000000000
  let large_num2 = 1000000000
  let result = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(2000000000, result)
}

test "add_mixed_sign_numbers" {
  // 测试正负数混合相加
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(-50, @azimuth.add(100, -150))
  @azimuth.assert_eq(50, @azimuth.add(-100, 150))
}

test "multiply_edge_case_min_values" {
  // 测试最小值相乘的边界情况
  let min_val = -2147483648
  let result1 = @azimuth.multiply(min_val, 1)
  let result2 = @azimuth.multiply(min_val, 0)
  let result3 = @azimuth.multiply(min_val, -1)
  let result4 = @azimuth.multiply(min_val, 2)
  @azimuth.assert_eq(min_val, result1)
  @azimuth.assert_eq(0, result2)
  @azimuth.assert_eq(min_val, result3)  // 应该溢出并返回最小值
  @azimuth.assert_eq(min_val, result4)  // 应该溢出并返回最小值
}

test "multiply_large_numbers" {
  // 测试大数相乘
  let result = @azimuth.multiply(46340, 46340)  // 接近sqrt(max_val)
  @azimuth.assert_eq(2147395600, result)
}

test "multiply_negative_by_negative" {
  // 测试负数相乘得正数
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "greet_with_whitespace" {
  // 测试包含空白字符的问候
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, \\t\\n!", @azimuth.greet("\\t\\n"))
  @azimuth.assert_eq_string("Hello,  Hello !", @azimuth.greet(" Hello "))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let a = 100
  let b = -50
  let c = 2
  let d = 25
  
  // ((a + b) * c) - d
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.multiply(result1, c)
  let result3 = @azimuth.add(result2, -d)  // 使用加法实现减法
  @azimuth.assert_eq(75, result3)  // ((100 - 50) * 2) - 25 = 50 * 2 - 25 = 100 - 25 = 75
}
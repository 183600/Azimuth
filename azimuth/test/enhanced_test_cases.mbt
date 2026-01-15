test "greet_with_whitespace" {
  // 测试包含各种空白字符的字符串
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, 	\n!", @azimuth.greet("\t\n"))
  @azimuth.assert_eq_string("Hello,  World !", @azimuth.greet("  World "))
}

test "add_extreme_boundary_values" {
  // 测试更极端的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与自身相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试最小值与自身相加
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_extreme_cases" {
  // 测试更极端的乘法情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341))
  
  // 测试负数相乘的正结果
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  
  // 测试混合符号的乘法
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  @azimuth.assert_eq(-100, @azimuth.multiply(-10, 10))
}

test "mathematical_identities" {
  // 测试数学恒等式
  
  // 乘法幂等性：1 * 1 = 1
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  
  // 加法幂等性：0 + 0 = 0
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 负数的性质：-(-a) = a（通过乘法实现）
  @azimuth.assert_eq(10, @azimuth.multiply(-10, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1))
}

test "sequential_operations" {
  // 测试连续运算的结果
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  let result2 = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4)
  @azimuth.assert_eq(result1, result2)
  
  let mul_result1 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  let mul_result2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 3), 4), 5)
  @azimuth.assert_eq(mul_result1, mul_result2)
}

test "greet_unicode_characters" {
  // 测试各种Unicode字符
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🚀🌙!", @azimuth.greet("🚀🌙"))
  @azimuth.assert_eq_string("Hello, Ωβα!", @azimuth.greet("Ωβα"))
}

test "mixed_arithmetic_operations" {
  // 测试混合算术运算
  let a = 15
  let b = -8
  let c = 3
  let d = -2
  
  // 复杂表达式：(a + b) * (c - d)
  let expr1 = @azimuth.multiply(
    @azimuth.add(a, b),
    @azimuth.add(c, @azimuth.multiply(d, -1))
  )
  
  // 手动计算：(15 + (-8)) * (3 - (-2)) = 7 * 5 = 35
  @azimuth.assert_eq(35, expr1)
  
  // 另一个复杂表达式：a * b + c * d
  let expr2 = @azimuth.add(
    @azimuth.multiply(a, b),
    @azimuth.multiply(c, d)
  )
  
  // 手动计算：15 * (-8) + 3 * (-2) = -120 + (-6) = -126
  @azimuth.assert_eq(-126, expr2)
}

test "boundary_multiply_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试2的幂次方乘法
  @azimuth.assert_eq(1073741824, @azimuth.multiply(32768, 32768))
  
  // 测试接近边界的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试特殊值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "string_edge_cases" {
  // 测试字符串的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  
  // 测试包含转义字符的字符串
  @azimuth.assert_eq_string("Hello, \\n!", @azimuth.greet("\\n"))
  
  // 测试非常长的字符串
  let long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let expected = "Hello, " + long_name + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_name))
}
// Azimuth 额外测试用例
// 覆盖更多边界情况和特殊场景

test "divide_with_ceil_negative_divisor" {
  // 测试负数除数的情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(5, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(2, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
}

test "divide_with_ceil_both_negative" {
  // 测试两个负数的情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, -3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-2, -3))
}

test "divide_with_ceil_exact_division" {
  // 测试能够整除的情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-15, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, -3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-15, -3))
}

test "greet_with_emoji_and_special_chars" {
  // 测试包含表情符号和特殊字符的问候
  @azimuth.assert_eq_string("Hello, 👋!", @azimuth.greet("👋"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, 💻!", @azimuth.greet("💻"))
  @azimuth.assert_eq_string("Hello, 🎉!", @azimuth.greet("🎉"))
}

test "greet_with_whitespace_variations" {
  // 测试不同空白字符的问候
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, 	!", @azimuth.greet("\t"))
  @azimuth.assert_eq_string("Hello, 
!", @azimuth.greet("\n"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet("\r"))
}

test "add_with_large_boundary_values" {
  // 测试大数边界的加法
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
  @azimuth.assert_eq(-1, @azimuth.add(2147483647, -2147483648))
}

test "multiply_with_power_of_two" {
  // 测试2的幂次方乘法
  @azimuth.assert_eq(1024, @azimuth.multiply(32, 32))
  @azimuth.assert_eq(2048, @azimuth.multiply(1024, 2))
  @azimuth.assert_eq(4096, @azimuth.multiply(512, 8))
  @azimuth.assert_eq(8192, @azimuth.multiply(256, 32))
}

test "complex_calculation_with_functions" {
  // 测试复杂的多函数计算
  let a = @azimuth.add(10, 20)
  let b = @azimuth.multiply(a, 3)
  let c = @azimuth.divide_with_ceil(b, 7)
  @azimuth.assert_eq(13, c)  // ((10+20)*3)/7 = 90/7 = 12.85 -> ceil(12.85) = 13
  
  let d = @azimuth.add(@azimuth.multiply(5, 5), @azimuth.multiply(3, 3))
  let e = @azimuth.divide_with_ceil(d, 4)
  @azimuth.assert_eq(9, e)  // (5*5 + 3*3)/4 = 34/4 = 8.5 -> ceil(8.5) = 9
}

test "divide_with_ceil_zero_dividend" {
  // 测试被除数为0的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
}

test "multiply_with_zero_and_one" {
  // 测试乘以0和1的情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(-5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(1, -5))
}
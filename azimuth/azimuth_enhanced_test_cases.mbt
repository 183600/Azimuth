// Azimuth 增强测试用例
// 使用标准的 MoonBit 测试语法，覆盖更多边界情况和复杂场景

test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(2147483647, @azimuth.add(1073741823, 1073741824))
}

test "add_mixed_signs" {
  // 测试混合符号加法
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(-50, @azimuth.add(100, -150))
  @azimuth.assert_eq(50, @azimuth.add(-100, 150))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46341, 46341))
}

test "multiply_fractional_results" {
  // 测试乘法的整数除法特性
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123456))
  @azimuth.assert_eq(123456, @azimuth.multiply(123456, 1))
}

test "divide_with_ceil_negative_dividend" {
  // 测试负数被除数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 5))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负数除数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(11, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(9, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, -5))
}

test "greet_with_unicode" {
  // 测试包含Unicode字符的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, naïve!", @azimuth.greet("naïve"))
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let x = 10
  let y = 20
  let z = -5
  
  // 计算 (x * y) + (x * z) + y
  let xy = @azimuth.multiply(x, y)
  let xz = @azimuth.multiply(x, z)
  let sum1 = @azimuth.add(xy, xz)
  let result = @azimuth.add(sum1, y)
  @azimuth.assert_eq(165, result)
  
  // 计算 (x + y) * (z + 10)
  let sum2 = @azimuth.add(x, y)
  let sum3 = @azimuth.add(z, 10)
  let result2 = @azimuth.multiply(sum2, sum3)
  @azimuth.assert_eq(150, result2)
}

test "edge_case_zero_operations" {
  // 测试零操作的各种边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(12345, @azimuth.add(0, 12345))
  @azimuth.assert_eq(-12345, @azimuth.add(0, -12345))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 12345))
}
// Azimuth 综合测试用例
// 使用标准的 MoonBit 测试语法
// 测试覆盖边界情况、特殊场景和组合操作

test "add_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(2147483647, @azimuth.add(1073741823, 1073741824))
  @azimuth.assert_eq(2147483647, @azimuth.add(1000000000, 1147483647))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-1073741824, -1073741824))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-1500000000, -647483648))
}

test "add_mixed_signs" {
  // 测试混合符号的加法
  @azimuth.assert_eq(1000000, @azimuth.add(2000000, -1000000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-2000000, 1000000))
  @azimuth.assert_eq(1, @azimuth.add(2147483647, -2147483646))
  @azimuth.assert_eq(-1, @azimuth.add(-2147483648, 2147483647))
}

test "multiply_large_numbers" {
  // 测试大数相乘
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46340, 46341))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(32768, 65535))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(46341, -46341))
}

test "multiply_fractional_equivalent" {
  // 测试等价于分数的乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  @azimuth.assert_eq(123456789, @azimuth.multiply(11111, 11111))
  @azimuth.assert_eq(-123456789, @azimuth.multiply(-11111, 11111))
}

test "greet_unicode_characters" {
  // 测试包含Unicode字符的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, naïve!", @azimuth.greet("naïve"))
}

test "divide_with_ceil_negative_cases" {
  // 测试向上取整除法的负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列
  let a = 100
  let b = 200
  let c = -50
  let d = 25
  
  // 计算 ((a + b) * c) + d
  let sum1 = @azimuth.add(a, b)
  let product1 = @azimuth.multiply(sum1, c)
  let result1 = @azimuth.add(product1, d)
  @azimuth.assert_eq(-14975, result1)
  
  // 计算 (a * b) + (c * d)
  let product2 = @azimuth.multiply(a, b)
  let product3 = @azimuth.multiply(c, d)
  let result2 = @azimuth.add(product2, product3)
  @azimuth.assert_eq(18750, result2)
}

test "edge_case_zero_operations" {
  // 测试零操作的各种边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(12345, @azimuth.add(0, 12345))
  @azimuth.assert_eq(-12345, @azimuth.add(0, -12345))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-12345, 0))
}

test "string_operations_edge_cases" {
  // 测试字符串操作的边界情况
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, Hello, World!!", @azimuth.greet("Hello, World!"))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12345
  let y = -6789
  
  // 测试加法交换律
  let sum1 = @azimuth.add(x, y)
  let sum2 = @azimuth.add(y, x)
  @azimuth.assert_eq(sum1, sum2)
  
  // 测试加法结合律（近似，由于溢出处理）
  let z = 1000
  let sum3 = @azimuth.add(@azimuth.add(x, y), z)
  let sum4 = @azimuth.add(x, @azimuth.add(y, z))
  @azimuth.assert_eq(sum3, sum4)
  
  // 测试乘法分配律（近似，由于溢出处理）
  let prod1 = @azimuth.multiply(x, @azimuth.add(y, z))
  let prod2 = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(prod1, prod2)
}
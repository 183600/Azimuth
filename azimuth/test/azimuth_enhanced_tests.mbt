// Azimuth 增强测试用例
// 使用标准的 MoonBit 测试语法

test "add_zero_cases" {
  // 测试加法中的零值情况
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-3, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(100, @azimuth.add(0, 100))
}

test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
  @azimuth.assert_eq(0, @azimuth.add(500000, -500000))
}

test "multiply_zero_cases" {
  // 测试乘法中的零值情况
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-3, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "multiply_by_one" {
  // 测试乘以1的情况
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-3, @azimuth.multiply(-3, 1))
  @azimuth.assert_eq(100, @azimuth.multiply(1, 100))
  @azimuth.assert_eq(-100, @azimuth.multiply(-100, 1))
}

test "multiply_by_negative_one" {
  // 测试乘以-1的情况
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(3, @azimuth.multiply(-3, -1))
  @azimuth.assert_eq(-100, @azimuth.multiply(100, -1))
  @azimuth.assert_eq(100, @azimuth.multiply(-100, -1))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
}

test "greet_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, single!", @azimuth.greet("single"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
  @azimuth.assert_eq_string("Hello, 中文字符!", @azimuth.greet("中文字符"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂算术序列
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c
  let sum_ab = @azimuth.add(a, b)
  let result1 = @azimuth.multiply(sum_ab, c)
  @azimuth.assert_eq(900, result1)
  
  // 计算 a * (b + c)
  let sum_bc = @azimuth.add(b, c)
  let result2 = @azimuth.multiply(a, sum_bc)
  @azimuth.assert_eq(500, result2)
}

test "add_overflow_edge_cases" {
  // 测试加法溢出的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 10, 10))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 10, -10))
}

test "multiply_overflow_edge_cases" {
  // 测试乘法溢出的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46340))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46340, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
}
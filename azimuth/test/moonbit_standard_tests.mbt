// MoonBit 标准测试用例
// 测试 azimuth 包的核心功能

test "add_positive_numbers" {
  // 测试正数相加
  let result = @azimuth.add(15, 25)
  @azimuth.assert_eq(40, result)
}

test "add_mixed_signs" {
  // 测试正负数相加
  let result1 = @azimuth.add(10, -5)
  let result2 = @azimuth.add(-10, 5)
  @azimuth.assert_eq(5, result1)
  @azimuth.assert_eq(-5, result2)
}

test "add_boundary_values" {
  // 测试边界值相加
  let max_val = 2147483647
  let min_val = -2147483648
  let result1 = @azimuth.add(max_val, 0)
  let result2 = @azimuth.add(min_val, 0)
  @azimuth.assert_eq(max_val, result1)
  @azimuth.assert_eq(min_val, result2)
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  let result1 = @azimuth.multiply(-4, 5)
  let result2 = @azimuth.multiply(-4, -5)
  @azimuth.assert_eq(-20, result1)
  @azimuth.assert_eq(20, result2)
}

test "multiply_large_numbers" {
  // 测试大数相乘
  let result = @azimuth.multiply(10000, 1000)
  @azimuth.assert_eq(10000000, result)
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let result = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, result)
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  let result1 = @azimuth.greet("MoonBit")
  let result2 = @azimuth.greet("123")
  let result3 = @azimuth.greet("!@#$")
  @azimuth.assert_eq_string("Hello, MoonBit!", result1)
  @azimuth.assert_eq_string("Hello, 123!", result2)
  @azimuth.assert_eq_string("Hello, !@#$!", result3)
}

test "complex_math_operations" {
  // 测试复合数学运算
  let a = 10
  let b = 20
  let c = 30
  let sum_ab = @azimuth.add(a, b)
  let sum_abc = @azimuth.add(sum_ab, c)
  let product = @azimuth.multiply(sum_ab, 2)
  @azimuth.assert_eq(60, sum_abc)
  @azimuth.assert_eq(60, product)
}
// 额外的核心 MoonBit 测试用例
// 测试 azimuth 库的核心功能和边界情况

test "arithmatic_sequence_sum" {
  // 测试等差数列求和：1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2  // n × (n + 1) / 2
  @azimuth.assert_eq(55, sum)
}

test "geometric_sequence_sum" {
  // 测试等比数列求和：2 + 4 + 8 + 16 = 30
  // 通过连续相加实现
  let term1 = 2
  let term2 = @azimuth.multiply(term1, 2)
  let term3 = @azimuth.multiply(term2, 2)
  let term4 = @azimuth.multiply(term3, 2)
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(30, sum)
}

test "factorial_calculation" {
  // 测试阶乘计算：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let result = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, result)
}

test "power_calculation" {
  // 测试幂运算：3^4 = 3 × 3 × 3 × 3 = 81
  let base = 3
  let exponent = 4
  let result = @azimuth.multiply(@azimuth.multiply(base, base), @azimuth.multiply(base, base))
  @azimuth.assert_eq(81, result)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版 F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
}

test "simple_interest_calculation" {
  // 测试简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 1000
  let rate = 5  // 5%
  let time = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(16000, total)  // 1000 + 1000×5×3 = 16000
}

test "area_perimeter_calculation" {
  // 测试面积和周长计算
  let length = 10
  let width = 5
  
  // 矩形面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
  
  // 矩形周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(30, perimeter)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含数字和字母的字符串
  @azimuth.assert_eq_string("Hello, Test123!", @azimuth.greet("Test123"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与1的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值与1的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 最大值与最小值的和
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "complex_nested_calculations" {
  // 测试复杂嵌套计算：(a + b) × (c - d) + e × f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let diff_cd = @azimuth.add(c, -d)  // 12
  let product_ef = @azimuth.multiply(e, f)  // 21
  
  let result = @azimuth.add(@azimuth.multiply(sum_ab, diff_cd), product_ef)
  @azimuth.assert_eq(201, result)  // 15 × 12 + 21 = 180 + 21 = 201
}
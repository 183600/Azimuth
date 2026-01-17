// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-10, 25), @azimuth.add(25, -10))
  @azimuth.assert_eq(@azimuth.add(0, 100), @azimuth.add(100, 0))
}

test "add_associative_property" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 8
  let c = 12
  
  let result1 = @azimuth.multiply(a, @azimuth.add(b, c))
  let result2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(result1, result2)
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(5, -3))
  @azimuth.assert_eq(15, @azimuth.multiply(-5, -3))
  @azimuth.assert_eq(-100, @azimuth.multiply(-1, 100))
}

test "greet_function_with_special_strings" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, Hello\nWorld!", @azimuth.greet("Hello\nWorld"))
  @azimuth.assert_eq_string("Hello, Tab\tSeparated!", @azimuth.greet("Tab\tSeparated"))
  @azimuth.assert_eq_string("Hello, Quote: \"Test\"!", @azimuth.greet("Quote: \"Test\""))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算复合利率：本金 × (1 + 利率)^年数（简化版本）
  let principal = 1000
  let rate = 5  // 5%
  
  // 简化计算：每年增加 principal × rate
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate), 100))
  
  @azimuth.assert_eq(1157, year3)
}

test "geometric_progression" {
  // 测试等比数列计算
  // 计算第n项：a × r^(n-1)
  let first_term = 2
  let ratio = 3
  
  // 计算前n项和：a × (r^n - 1) / (r - 1)
  let term1 = first_term
  let term2 = @azimuth.multiply(term1, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  
  // 计算前n项和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  
  @azimuth.assert_eq(54, term4)  // 第4项
  @azimuth.assert_eq(80, sum)    // 前4项和
}

test "time_calculations" {
  // 测试时间计算
  // 计算总分钟数：小时 × 60 + 分钟
  let hours = 3
  let minutes = 45
  let total_minutes = @azimuth.add(@azimuth.multiply(hours, 60), minutes)
  @azimuth.assert_eq(225, total_minutes)
  
  // 计算总秒数：总分钟数 × 60
  let total_seconds = @azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(13500, total_seconds)
}

test "statistical_analysis" {
  // 测试统计分析
  // 计算方差：平方的平均值减去平均值的平方
  let value1 = 10
  let value2 = 15
  let value3 = 20
  let value4 = 25
  let value5 = 30
  
  // 计算平均值
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(value1, value2), value3), value4), value5)
  let mean = sum / 5
  
  // 计算平方和
  let squared_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(value1, value1),
    @azimuth.multiply(value2, value2)),
    @azimuth.multiply(value3, value3)),
    @azimuth.multiply(value4, value4)),
    @azimuth.multiply(value5, value5))
  
  let mean_of_squares = squared_sum / 5
  let variance = @azimuth.add(mean_of_squares, @azimuth.multiply(mean, -mean))
  
  @azimuth.assert_eq(20, mean)
  @azimuth.assert_eq(50, variance)
}

test "boundary_testing" {
  // 测试边界条件
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近边界的加法
  @azimuth.assert_eq(2147483646, @azimuth.add(max_int, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_int, 1))
  
  // 测试零值运算
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_int))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
}
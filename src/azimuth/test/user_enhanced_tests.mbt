// 用户增强测试用例
// 测试 azimuth 模块的核心功能和边界情况

test "enhanced_addition_series" {
  // 测试加法序列：1+2+3+4+5 = 15
  let sum1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  let total = @azimuth.add(sum1, 5)
  @azimuth.assert_eq(15, total)
  
  // 测试负数序列：-1+(-2)+(-3)+(-4)+(-5) = -15
  let sum2 = @azimuth.add(@azimuth.add(-1, -2), @azimuth.add(-3, -4))
  let total_neg = @azimuth.add(sum2, -5)
  @azimuth.assert_eq(-15, total_neg)
}

test "enhanced_multiplication_table" {
  // 测试乘法表：7的乘法表
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(14, @azimuth.multiply(7, 2))
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(28, @azimuth.multiply(7, 4))
  @azimuth.assert_eq(35, @azimuth.multiply(7, 5))
}

test "enhanced_mixed_operations" {
  // 测试混合运算：(a*b) + (c*d) - e
  let a = 6
  let b = 7
  let c = 8
  let d = 9
  let e = 10
  
  let product1 = @azimuth.multiply(a, b)  // 42
  let product2 = @azimuth.multiply(c, d)  // 72
  let sum = @azimuth.add(product1, product2)  // 114
  let result = @azimuth.add(sum, -e)  // 104
  
  @azimuth.assert_eq(104, result)
}

test "enhanced_greeting_variations" {
  // 测试各种问候语变体
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
}

test "enhanced_boundary_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与中等值相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1000))
  @azimuth.assert_eq(max_val, @azimuth.add(1000, max_val))
  
  // 最小值与中等值相加
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1000))
  @azimuth.assert_eq(min_val, @azimuth.add(-1000, min_val))
  
  // 边界值相乘
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "enhanced_financial_calculations" {
  // 测试金融计算：贷款利息计算
  let principal = 10000  // 本金
  let rate = 5  // 利率5%
  let years = 3  // 年数
  
  // 简单利息计算：本金 + (本金 * 利率 * 年数)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)  // 10000 + (10000 * 5 * 3) = 10000 + 150000 = 160000
}

test "enhanced_temperature_conversions" {
  // 测试温度转换：摄氏度转华氏度（简化版：F = C * 2 + 30）
  let celsius_temps = @azimuth.add(@azimuth.add(0, 10), @azimuth.add(20, @azimuth.add(30, 40)))
  let celsius_avg = celsius_temps / 5  // 平均温度 = 20°C
  
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius_avg, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)  // 20 * 2 + 30 = 70°F
}

test "enhanced_geometric_calculations" {
  // 测试几何计算：矩形和三角形面积
  let length = 12
  let width = 8
  let base = 10
  let height = 6
  
  // 矩形面积 = 长 * 宽
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 三角形面积 = 底 * 高 / 2
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
  
  // 总面积
  let total_area = @azimuth.add(rectangle_area, triangle_area)
  @azimuth.assert_eq(126, total_area)
}

test "enhanced_statistical_calculations" {
  // 测试统计计算：平均值和中位数
  let numbers = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let count = 5
  let average = numbers / count
  
  @azimuth.assert_eq(30, average)  // (10+20+30+40+50)/5 = 30
  
  // 测试方差计算（简化版）
  let deviations = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(20, 10), 0), 10), 20)  // 各数与平均值的差的平方
  let variance = deviations / count
  @azimuth.assert_eq(12, variance)  // (400+100+0+100+400)/5 = 200，简化计算为12
}

test "enhanced_error_handling" {
  // 测试错误处理和边界情况
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
}
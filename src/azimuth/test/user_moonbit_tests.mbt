// 用户自定义的 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  
  // 测试混合符号相加
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(-50, @azimuth.add(100, -150))
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
  
  // 测试与-1相乘的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_unicode_handling" {
  // 测试问候函数的Unicode处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, Ñoño!", @azimuth.greet("Ñoño"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算多项式值：3x² + 2x + 1，当x=5时
  let x = 5
  let x_squared = @azimuth.multiply(x, x)
  let term1 = @azimuth.multiply(3, x_squared)
  let term2 = @azimuth.multiply(2, x)
  let polynomial_value = @azimuth.add(@azimuth.add(term1, term2), 1)
  @azimuth.assert_eq(86, polynomial_value)  // 3*25 + 2*5 + 1 = 75 + 10 + 1 = 86
  
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1
  let factorial = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial)
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简单利息计算：本金 + (本金 × 利率 × 年数)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
  
  // 复利计算（简化）：本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let compound_total = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(160000, compound_total)
}

test "geometry_calculations" {
  // 测试几何计算
  let radius = 7
  
  // 圆周长（简化）：2 × π × r ≈ 2 × 3 × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(42, circumference)
  
  // 圆面积（简化）：π × r² ≈ 3 × r²
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(147, area)
  
  // 球体体积（简化）：4/3 × π × r³ ≈ 4 × r³
  let volume = @azimuth.multiply(4, @azimuth.multiply(radius, @azimuth.multiply(radius, radius)))
  @azimuth.assert_eq(1372, volume)
}

test "time_calculations" {
  // 测试时间计算
  let hours = 2
  let minutes = 45
  
  // 转换为总分钟数
  let total_minutes = @azimuth.add(@azimuth.multiply(hours, 60), minutes)
  @azimuth.assert_eq(165, total_minutes)
  
  // 转换为总秒数
  let total_seconds = @azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(9900, total_seconds)
  
  // 计算时间差（分钟）
  let start_time = 480  // 8:00 AM
  let end_time = 1020    // 5:00 PM
  let duration = @azimuth.add(end_time, -start_time)
  @azimuth.assert_eq(540, duration)
}

test "shopping_cart_calculations" {
  // 测试购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  let discount = 50
  let shipping = 15
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let total3 = @azimuth.multiply(item3_price, item3_quantity)
  
  // 计算购物车小计
  let subtotal = @azimuth.add(@azimuth.add(total1, total2), total3)
  
  // 应用折扣和运费
  let final_total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  
  // 验证结果：299*2 + 199*3 + 99*1 - 50 + 15 = 598 + 597 + 99 - 50 + 15 = 1259
  @azimuth.assert_eq(1259, final_total)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数运算：F = C × 2 + 30
  
  let celsius_temp = 25
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temp, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_temp)  // 25 * 2 + 30 = 80
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算一组数的总和、平均值、中位数
  let numbers = [@azimuth.add(10, 20), @azimuth.add(30, 40), @azimuth.add(50, 60)]
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(numbers[0], numbers[1]), numbers[2])
  @azimuth.assert_eq(210, sum)  // 30 + 70 + 110 = 210
  
  // 计算平均值
  let average = sum / 3
  @azimuth.assert_eq(70, average)  // 210 / 3 = 70
  
  // 计算方差（简化）
  let diff1 = @azimuth.add(numbers[0], -70)
  let diff2 = @azimuth.add(numbers[1], -70)
  let diff3 = @azimuth.add(numbers[2], -70)
  let variance = @azimuth.add(@azimuth.add(@azimuth.multiply(diff1, diff1), @azimuth.multiply(diff2, diff2)), @azimuth.multiply(diff3, diff3)) / 3
  @azimuth.assert_eq(1333, variance)  // (1600 + 0 + 1600) / 3 = 3200 / 3 = 1066（整数除法）
}
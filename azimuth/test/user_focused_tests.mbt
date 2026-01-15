test "fibonacci_sequence" {
  // 测试斐波那契数列计算（前几项）
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
}

test "mathematical_series" {
  // 测试数学级数计算
  // 等差数列：1 + 3 + 5 + ... + 19（前10个奇数）
  let n = 10
  let first = 1
  let last = 19
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(100, sum)
  
  // 等比数列：2 + 4 + 8 + 16 + 32（前5项）
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16), 32)
  @azimuth.assert_eq(62, geo_sum)
}

test "geometry_calculations" {
  // 测试几何计算
  let radius = 5
  
  // 圆面积：π × r² ≈ 3 × 25 = 75
  let circle_area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, circle_area)
  
  // 矩形面积：长 × 宽
  let length = 8
  let width = 6
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "compound_interest" {
  // 测试复利计算（简化版）
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算：本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let total = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(16000, total)  // 1000 × (1 + 5×3) = 1000 × 16 = 16000
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 9/5 + 32 ≈ C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}

test "string_processing" {
  // 测试字符串处理
  let short = "Hi"
  let medium = "MoonBit"
  let long = "This is a longer string"
  
  // 通过greet函数测试不同长度字符串的处理
  let short_greeting = @azimuth.greet(short)
  let medium_greeting = @azimuth.greet(medium)
  let long_greeting = @azimuth.greet(long)
  
  @azimuth.assert_eq_string("Hello, Hi!", short_greeting)
  @azimuth.assert_eq_string("Hello, MoonBit!", medium_greeting)
  @azimuth.assert_eq_string("Hello, This is a longer string!", long_greeting)
  
  // 测试包含特殊字符的字符串
  let special = "Test@123"
  let special_greeting = @azimuth.greet(special)
  @azimuth.assert_eq_string("Hello, Test@123!", special_greeting)
}

test "power_calculation" {
  // 测试幂运算（通过连续相乘实现）
  // 计算2的幂：2^5 = 32
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2)
  @azimuth.assert_eq(16, power_of_2)
  
  // 计算3的幂：3^4 = 81
  let power_of_3 = @azimuth.multiply(@azimuth.multiply(3, 3), @azimuth.multiply(3, 3))
  @azimuth.assert_eq(81, power_of_3)
  
  // 计算5的平方：5^2 = 25
  let square_of_5 = @azimuth.multiply(5, 5)
  @azimuth.assert_eq(25, square_of_5)
}

test "factorial_calculation" {
  // 测试阶乘计算（通过连续相乘实现）
  // 计算5的阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let factorial = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, factorial)
  
  // 计算4的阶乘：4! = 4 × 3 × 2 × 1 = 24
  let factorial_4 = @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1)))
  @azimuth.assert_eq(24, factorial_4)
}

test "business_calculations" {
  // 测试商业计算场景
  // 计算折扣价格
  let original_price = 100
  let discount_percent = 20  // 20%
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(80, discounted_price)
  
  // 计算税费
  let price = 100
  let tax_rate = 8  // 8%
  let tax_amount = @azimuth.multiply(price, tax_rate) / 100
  let total_with_tax = @azimuth.add(price, tax_amount)
  @azimuth.assert_eq(108, total_with_tax)
  
  // 计算利润率
  let cost = 60
  let selling_price = 100
  let profit = @azimuth.add(selling_price, -cost)
  let profit_margin = @azimuth.multiply(profit, 100) / selling_price
  @azimuth.assert_eq(40, profit_margin)  // 40%
}

test "statistical_analysis" {
  // 测试统计分析
  // 数据集：[10, 20, 30, 40, 50]
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  @azimuth.assert_eq(150, sum)
  
  // 平均值
  let average = sum / 5
  @azimuth.assert_eq(30, average)
  
  // 范围（最大值 - 最小值）
  let range = @azimuth.add(50, -10)
  @azimuth.assert_eq(40, range)
  
  // 加权平均值：(10×1 + 20×2 + 30×3 + 40×4 + 50×5) ÷ (1+2+3+4+5)
  let weighted_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(10, 1), 
    @azimuth.multiply(20, 2)
  ), @azimuth.multiply(30, 3)), @azimuth.multiply(40, 4)), @azimuth.multiply(50, 5))
  let weight_total = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  let weighted_avg = weighted_sum / weight_total
  @azimuth.assert_eq(36, weighted_avg)
}
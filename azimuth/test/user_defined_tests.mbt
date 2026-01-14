// 用户自定义的 MoonBit 测试用例
// 测试 azimuth 库的各种功能

test "factorial_calculation" {
  // 计算5的阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let result = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, result)
}

test "fibonacci_sequence" {
  // 计算斐波那契数列的第10项
  // F(1) = 1, F(2) = 1, F(n) = F(n-1) + F(n-2)
  let f1 = 1
  let f2 = 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  let f9 = @azimuth.add(f7, f8)  // 34
  let f10 = @azimuth.add(f8, f9) // 55
  
  @azimuth.assert_eq(55, f10)
}

test "area_calculations" {
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "temperature_conversion" {
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数计算：F = C × 2 + 30
  let celsius_temp = 25
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temp, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_temp)  // 25 × 2 + 30 = 80
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
}

test "shopping_cart_total" {
  // 购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount = 50  // 折扣
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(total1, total2)
  
  // 应用折扣
  let final_total = @azimuth.add(cart_total, -discount)
  
  // 验证总价：299*2 + 199*3 - 50 = 598 + 597 - 50 = 1145
  @azimuth.assert_eq(1145, final_total)
}

test "compound_interest" {
  // 简化的复利计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let total = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(160000, total)  // 10000 × (1 + 5×3) = 10000 × 16 = 160000
}

test "string_processing_variations" {
  // 测试各种字符串处理情况
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 特殊字符@#$!", @azimuth.greet("特殊字符@#$"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))
}

test "power_of_two_calculation" {
  // 计算2的幂：2^8 = 256
  let result = @azimuth.multiply(
    @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)),
    @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2))
  )
  @azimuth.assert_eq(256, result)
}

test "statistical_calculations" {
  // 基础统计计算
  let numbers = [@azimuth.add(10, 20), @azimuth.add(30, 40), @azimuth.add(50, 60)]
  let sum = @azimuth.add(@azimuth.add(numbers[0], numbers[1]), numbers[2])
  let average = sum / 3
  @azimuth.assert_eq(210, sum)
  @azimuth.assert_eq(70, average)
}

test "game_score_system" {
  // 游戏得分系统
  let base_score = 1000
  let level_multiplier = 3
  let bonus_points = 500
  let penalty = 200
  
  // 计算最终得分
  let level_score = @azimuth.multiply(base_score, level_multiplier)
  let total_with_bonus = @azimuth.add(level_score, bonus_points)
  let final_score = @azimuth.add(total_with_bonus, -penalty)
  
  @azimuth.assert_eq(3300, final_score)
  
  // 计算排名得分（前10%额外加分）
  let top_percentage = 10
  let rank_bonus = @azimuth.multiply(final_score, top_percentage) / 100
  let rank_total = @azimuth.add(final_score, rank_bonus)
  
  @azimuth.assert_eq(3630, rank_total)
}
// 新增 MoonBit 测试用例
// 测试 azimuth 包的额外功能和场景

test "temperature_conversion_calculations" {
  // 测试温度转换计算
  // 摄氏度转华氏度（简化公式：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度（简化公式：C = (F - 30) ÷ 2）
  let fahrenheit_back = 80
  let celsius_back = @azimuth.add(fahrenheit_back, -30) / 2
  @azimuth.assert_eq(25, celsius_back)
  
  // 测试冰点和沸点
  @azimuth.assert_eq(30, @azimuth.add(@azimuth.multiply(0, 2), 30))  // 0°C → 30°F
  @azimuth.assert_eq(230, @azimuth.add(@azimuth.multiply(100, 2), 30))  // 100°C → 230°F
}

test "shopping_cart_advanced_calculations" {
  // 测试高级购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount_threshold = 800
  let discount_rate = 10
  let shipping = 25
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(subtotal1, subtotal2)
  
  // 计算折扣
  let discount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算最终价格
  let discounted_total = @azimuth.add(subtotal, -discount)
  let tax = @azimuth.multiply(discounted_total, tax_rate) / 100
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax), shipping)
  
  @azimuth.assert_eq(1294, final_total)
}

test "geometry_area_calculations" {
  // 测试几何面积计算
  // 矩形面积
  let length = 12
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 三角形面积
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
  
  // 梯形面积
  let top_base = 5
  let bottom_base = 9
  let trapezoid_height = 4
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), trapezoid_height) / 2
  @azimuth.assert_eq(28, trapezoid_area)
}

test "financial_compound_interest" {
  // 测试复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算：本息和 = 本金 × (1 + 利率)^年数
  // 近似为：本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(100, @azimuth.multiply(rate_percent, years))
  let total = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(115000, total)  // 1000 × 115 = 115000
  
  // 计算利息
  let interest = @azimuth.add(total, -principal)
  @azimuth.assert_eq(114000, interest)
}

test "statistics_calculations" {
  // 测试统计计算
  // 计算一组数的总和、平均值、中位数
  let values_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(85, 90), 78), 92), 95)
  @azimuth.assert_eq(440, values_sum)
  
  // 计算平均值
  let count = 5
  let average = values_sum / count
  @azimuth.assert_eq(88, average)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(85, -88)
  let diff2 = @azimuth.add(90, -88)
  let diff3 = @azimuth.add(78, -88)
  let diff4 = @azimuth.add(92, -88)
  let diff5 = @azimuth.add(95, -88)
  
  let variance_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(diff1, diff1), 
    @azimuth.multiply(diff2, diff2)), 
    @azimuth.multiply(diff3, diff3)), 
    @azimuth.multiply(diff4, diff4)), 
    @azimuth.multiply(diff5, diff5))
  
  let variance = variance_sum / count
  @azimuth.assert_eq(34, variance)
}

test "time_calculations" {
  // 测试时间计算
  // 时间转换：小时、分钟、秒
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = @azimuth.add(@azimuth.add(
    @azimuth.multiply(hours, 3600), 
    @azimuth.multiply(minutes, 60)), 
    seconds)
  @azimuth.assert_eq(9930, total_seconds)
  
  // 计算时间差
  let start_hour = 9
  let start_minute = 30
  let end_hour = 14
  let end_minute = 45
  
  let start_total = @azimuth.add(@azimuth.multiply(start_hour, 60), start_minute)
  let end_total = @azimuth.add(@azimuth.multiply(end_hour, 60), end_minute)
  let duration = @azimuth.add(end_total, -start_total)
  @azimuth.assert_eq(315, duration)  // 5小时15分钟 = 315分钟
}

test "game_score_calculations" {
  // 测试游戏得分计算
  let base_score = 1000
  let level_multiplier = 3
  let combo_bonus = 500
  let time_bonus = 200
  let penalty = 150
  
  // 计算基础得分
  let level_score = @azimuth.multiply(base_score, level_multiplier)
  
  // 计算总得分
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(level_score, combo_bonus), time_bonus), -penalty)
  @azimuth.assert_eq(3550, total_score)
  
  // 计算排名加分（前10%额外加20%）
  let rank_bonus = @azimuth.multiply(total_score, 20) / 100
  let final_score = @azimuth.add(total_score, rank_bonus)
  @azimuth.assert_eq(4260, final_score)
}

test "unit_conversion_calculations" {
  // 测试单位转换计算
  // 长度转换：厘米转英寸（1英寸 ≈ 2.54厘米，简化为5/2）
  let centimeters = 100
  let inches = centimeters * 2 / 5  // 100 * 2 / 5 = 40
  @azimuth.assert_eq(40, inches)
  
  // 重量转换：千克转磅（1千克 ≈ 2.2磅，简化为22/10）
  let kilograms = 10
  let pounds = @azimuth.multiply(kilograms, 22) / 10  // 10 * 22 / 10 = 22
  @azimuth.assert_eq(22, pounds)
  
  // 速度转换：公里/时转米/秒（1公里/时 ≈ 0.278米/秒，简化为1/4）
  let km_per_hour = 60
  let m_per_second = km_per_hour / 4  // 60 / 4 = 15
  @azimuth.assert_eq(15, m_per_second)
}

test "string_pattern_processing" {
  // 测试字符串模式处理
  // 测试各种字符串格式
  @azimuth.assert_eq_string("Hello, John_Doe!", @azimuth.greet("John_Doe"))
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, API_KEY!", @azimuth.greet("API_KEY"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, a+b=c!", @azimuth.greet("a+b=c"))
  @azimuth.assert_eq_string("Hello, $100!", @azimuth.greet("$100"))
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 计算等差数列：3, 7, 11, 15, 19（公差为4）
  let a1 = 3
  let d = 4
  let n = 5
  let an = @azimuth.add(a1, @azimuth.multiply(d, n - 1))
  @azimuth.assert_eq(19, an)
  
  // 计算等差数列前n项和
  let sum_arithmetic = @azimuth.multiply(n, @azimuth.add(a1, an)) / 2
  @azimuth.assert_eq(55, sum_arithmetic)
  
  // 计算等比数列：2, 6, 18, 54（公比为3）
  let g1 = 2
  let r = 3
  let gm = @azimuth.multiply(g1, @azimuth.multiply(r, @azimuth.multiply(r, r)))
  @azimuth.assert_eq(54, gm)
  
  // 计算等比数列前m项和（简化计算）
  let sum_geometric = @azimuth.add(@azimuth.add(@azimuth.add(2, 6), 18), 54)
  @azimuth.assert_eq(80, sum_geometric)
}
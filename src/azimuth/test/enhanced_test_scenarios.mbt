// 增强测试场景
// 包含10个新的MoonBit测试用例，覆盖各种实际应用场景
test "arithmetic_progression_sum" {
  // 计算等差数列的和：2 + 5 + 8 + 11 + 14
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 2
  let last = 14
  
  let sum = azimuth::@azimuth.multiply(n, azimuth::@azimuth.add(first, last)) / 2
  azimuth::@azimuth.assert_eq(40, sum)  // 5 × (2 + 14) / 2 = 40
}
test "geometric_progression_product" {
  // 计算等比数列的积：2 × 4 × 8 × 16
  // 通过连续相乘实现
  let term1 = 2
  let term2 = 4
  let term3 = 8
  let term4 = 16
  
  let product = azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(term1, term2), azimuth::@azimuth.multiply(term3, term4))
  azimuth::@azimuth.assert_eq(1024, product)  // 2 × 4 × 8 × 16 = 1024
}
test "bank_interest_calculation" {
  // 银行利息计算：本金 × 利率 × 时间
  let principal = 10000  // 本金10000元
  let rate = 3  // 年利率3%
  let years = 5  // 5年
  
  // 计算总利息：10000 × 3% × 5 = 1500
  let total_interest = azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(principal, rate), years) / 100
  // 计算本息和：10000 + 1500 = 11500
  let total_amount = azimuth::@azimuth.add(principal, total_interest)
  
  azimuth::@azimuth.assert_eq(11500, total_amount)
}
test "discount_price_calculation" {
  // 折扣价格计算：原价 × (1 - 折扣率)
  let original_price = 200  // 原价200元
  let discount_rate = 15  // 15%折扣
  
  // 计算折扣金额：200 × 15% = 30
  let discount_amount = azimuth::@azimuth.multiply(original_price, discount_rate) / 100
  // 计算折后价格：200 - 30 = 170
  let discounted_price = azimuth::@azimuth.add(original_price, -discount_amount)
  
  azimuth::@azimuth.assert_eq(170, discounted_price)
}
test "temperature_conversion_extended" {
  // 扩展的温度转换测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数近似：F ≈ C × 2 + 30
  
  // 测试多个温度点
  let temps_celsius = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  let i = 0
  while (i < 5) {
    let c = temps_celsius[i]
    let expected = expected_fahrenheit[i]
    let actual = azimuth::@azimuth.add(azimuth::@azimuth.multiply(c, 2), 30)
    azimuth::@azimuth.assert_eq(expected, actual)
    i = azimuth::@azimuth.add(i, 1)
  }
}
test "volume_calculation" {
  // 体积计算测试
  // 长方体体积：长 × 宽 × 高
  let length = 10
  let width = 5
  let height = 3
  
  let base_area = azimuth::@azimuth.multiply(length, width)
  let volume = azimuth::@azimuth.multiply(base_area, height)
  azimuth::@azimuth.assert_eq(150, volume)  // 10 × 5 × 3 = 150
  
  // 圆柱体体积（简化）：底面积 × 高
  let radius = 4
  let cylinder_height = 6
  let base_area2 = azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(radius, radius), 3)  // 近似π≈3
  let volume2 = azimuth::@azimuth.multiply(base_area2, cylinder_height)
  azimuth::@azimuth.assert_eq(288, volume2)  // 4² × 3 × 6 = 288
}
test "speed_distance_time" {
  // 速度、距离、时间关系测试
  // 距离 = 速度 × 时间
  
  // 场景1：计算距离
  let speed1 = 60  // 60公里/小时
  let time1 = 2.5  // 2.5小时（使用整数近似：2）
  let distance1 = azimuth::@azimuth.multiply(speed1, time1)
  azimuth::@azimuth.assert_eq(120, distance1)  // 60 × 2 = 120公里
  
  // 场景2：计算时间
  let distance2 = 180  // 180公里
  let speed2 = 60  // 60公里/小时
  let time2 = distance2 / speed2
  azimuth::@azimuth.assert_eq(3, time2)  // 180 ÷ 60 = 3小时
  
  // 场景3：计算速度
  let distance3 = 240  // 240公里
  let time3 = 4  // 4小时
  let speed3 = distance3 / time3
  azimuth::@azimuth.assert_eq(60, speed3)  // 240 ÷ 4 = 60公里/小时
}
test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试各种特殊情况的字符串
  
  // 包含空格的字符串
  let spaced_name = "  John Doe  "
  azimuth::@azimuth.assert_eq_string("Hello,   John Doe  !", azimuth::@azimuth.greet(spaced_name))
  
  // 包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  azimuth::@azimuth.assert_eq_string("Hello, !@#$%^&*()!", azimuth::@azimuth.greet(special_chars))
  
  // 包含数字的字符串
  let numeric_string = "User2023"
  azimuth::@azimuth.assert_eq_string("Hello, User2023!", azimuth::@azimuth.greet(numeric_string))
  
  // 包含Unicode字符的字符串
  let unicode_string = "测试用户"
  azimuth::@azimuth.assert_eq_string("Hello, 测试用户!", azimuth::@azimuth.greet(unicode_string))
}
test "combinatorial_calculations" {
  // 组合数学计算测试
  // 计算排列数：P(n, k) = n × (n-1) × (n-2) × ... × (n-k+1)
  
  // 计算P(5, 3) = 5 × 4 × 3 = 60
  let n = 5
  let k = 3
  let p1 = n
  let p2 = azimuth::@azimuth.add(n, -1)
  let p3 = azimuth::@azimuth.add(n, -2)
  let permutation = azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(p1, p2), p3)
  azimuth::@azimuth.assert_eq(60, permutation)
  
  // 计算组合数：C(n, k) = P(n, k) / k!
  // C(5, 3) = P(5, 3) / 3! = 60 / (3 × 2 × 1) = 10
  let factorial_k = azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(k, azimuth::@azimuth.add(k, -1)), 1)
  let combination = permutation / factorial_k
  azimuth::@azimuth.assert_eq(10, combination)
}
test "financial_investment_analysis" {
  // 投资分析测试
  // 计算投资回报率：(最终价值 - 初始投资) / 初始投资 × 100%
  
  let initial_investment = 10000  // 初始投资10000元
  let final_value = 12000  // 最终价值12000元
  let investment_period = 2  // 投资期限2年
  
  // 计算总收益
  let total_return = azimuth::@azimuth.add(final_value, -initial_investment)
  // 计算年化收益率
  let annual_return_rate = azimuth::@azimuth.multiply(total_return, 100) / initial_investment / investment_period
  
  azimuth::@azimuth.assert_eq(10, annual_return_rate)  // (12000-10000)/10000/2×100% = 10%
  
  // 计算复合年增长率（简化版）
  let growth_factor = azimuth::@azimuth.multiply(final_value, 100) / initial_investment
  let simple_cagr = azimuth::@azimuth.add(growth_factor, -100) / investment_period
  azimuth::@azimuth.assert_eq(10, simple_cagr)  // 简化计算也是10%
}
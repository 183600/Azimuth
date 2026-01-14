// 增强的 MoonBit 测试用例
// 测试 Azimuth 项目的各种功能和边界情况

// 测试用例1: 复杂的数学运算序列
test "complex_mathematical_sequence" {
  // 计算 (a + b) * (c - d) + e * f
  let a = 15
  let b = 25
  let c = 100
  let d = 40
  let e = 8
  let f = 12
  
  let sum_ab = @azimuth.add(a, b)  // 40
  let diff_cd = @azimuth.add(c, -d)  // 60
  let product_ab_cd = @azimuth.multiply(sum_ab, diff_cd)  // 2400
  let product_ef = @azimuth.multiply(e, f)  // 96
  let result = @azimuth.add(product_ab_cd, product_ef)  // 2496
  
  @azimuth.assert_eq(2496, result)
}

// 测试用例2: 温度转换计算
test "temperature_conversion_calculations" {
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 简化为整数计算：F = C * 2 + 30 (近似值)
  
  // 测试0°C
  let temp1 = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, temp1)
  
  // 测试20°C (室温)
  let temp2 = @azimuth.add(@azimuth.multiply(20, 2), 30)
  @azimuth.assert_eq(70, temp2)
  
  // 测试40°C (高温)
  let temp3 = @azimuth.add(@azimuth.multiply(40, 2), 30)
  @azimuth.assert_eq(110, temp3)
}

// 测试用例3: 购物车总价计算
test "shopping_cart_total_calculation" {
  // 商品价格和数量
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let item3_price = 99
  let item3_quantity = 3
  
  // 计算每种商品的小计
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)  // 398
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)  // 299
  let subtotal3 = @azimuth.multiply(item3_price, item3_quantity)  // 297
  
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal1, subtotal2), subtotal3)
  
  @azimuth.assert_eq(994, total)
}

// 测试用例4: 字符串处理边界情况
test "string_processing_boundary_cases" {
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringThatContainsManyCharactersAndSymbols1234567890!@#$%^&*()"
  let expected_long = "Hello, " + long_string + "!"
  @azimuth.assert_eq_string(expected_long, @azimuth.greet(long_string))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, 中文测试123!", @azimuth.greet("中文测试123"))
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏!", @azimuth.greet("🌍🌎🌏"))
}

// 测试用例5: 数列和级数计算
test "series_and_progression_calculations" {
  // 计算等差数列前n项和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 10
  let sum_of_odds = @azimuth.multiply(n, n)  // 10² = 100
  @azimuth.assert_eq(100, sum_of_odds)
  
  // 计算等比数列前n项和：2 + 4 + 8 + ... + 2^n
  // 简化计算：2 * (2^n - 1)
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2)  // 2^4 = 16
  let geometric_sum = @azimuth.multiply(2, @azimuth.add(power_of_2, -1))  // 2 * (16 - 1) = 30
  @azimuth.assert_eq(30, geometric_sum)
}

// 测试用例6: 面积和体积计算
test "area_and_volume_calculations" {
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
  
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 6
  let bottom_base = 10
  let trapezoid_height = 4
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), trapezoid_height) / 2
  @azimuth.assert_eq(32, trapezoid_area)
}

// 测试用例7: 金融计算场景
test "financial_calculation_scenarios" {
  // 简单贷款计算：月供 = (本金 + 利息) / 期数
  let principal = 100000  // 10万元
  let annual_rate = 6     // 年利率6%
  let years = 3           // 3年
  let months = @azimuth.multiply(years, 12)  // 36个月
  
  // 计算总利息：本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  // 总还款额
  let total_payment = @azimuth.add(principal, total_interest)
  // 月供
  let monthly_payment = total_payment / months
  
  @azimuth.assert_eq(2777, monthly_payment)  // 向下取整
}

// 测试用例8: 极值边界测试
test "extreme_value_boundary_tests" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 测试最小值的各种运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试极值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

// 测试用例9: 游戏得分计算
test "game_score_calculations" {
  // 基础得分计算
  let base_score = 1000
  let level_bonus = 200
  let combo_multiplier = 3
  let time_bonus = 150
  let penalty = 50
  
  // 计算最终得分
  let level_total = @azimuth.add(base_score, level_bonus)
  let combo_total = @azimuth.multiply(level_total, combo_multiplier)
  let time_total = @azimuth.add(combo_total, time_bonus)
  let final_score = @azimuth.add(time_total, -penalty)
  
  @azimuth.assert_eq(3750, final_score)
  
  // 计算排名得分（前10%额外加分）
  let rank_bonus_percent = 10
  let rank_bonus = @azimuth.multiply(final_score, rank_bonus_percent) / 100
  let rank_total = @azimuth.add(final_score, rank_bonus)
  
  @azimuth.assert_eq(4125, rank_total)
}

// 测试用例10: 数据统计计算
test "data_statistics_calculations" {
  // 计算数据集的基本统计量
  let data1 = 85
  let data2 = 92
  let data3 = 78
  let data4 = 96
  let data5 = 89
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  @azimuth.assert_eq(440, sum)
  
  // 计算平均值
  let average = sum / 5
  @azimuth.assert_eq(88, average)
  
  // 计算最大值和最小值的差（极差）
  let max_val = 96
  let min_val = 78
  let range = @azimuth.add(max_val, -min_val)
  @azimuth.assert_eq(18, range)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(data1, -average)
  let diff2 = @azimuth.add(data2, -average)
  let diff3 = @azimuth.add(data3, -average)
  let diff4 = @azimuth.add(data4, -average)
  let diff5 = @azimuth.add(data5, -average)
  
  let squared_diff1 = @azimuth.multiply(diff1, diff1)
  let squared_diff2 = @azimuth.multiply(diff2, diff2)
  let squared_diff3 = @azimuth.multiply(diff3, diff3)
  let squared_diff4 = @azimuth.multiply(diff4, diff4)
  let squared_diff5 = @azimuth.multiply(diff5, diff5)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(squared_diff1, squared_diff2), squared_diff3), squared_diff4), squared_diff5) / 5
  @azimuth.assert_eq(34, variance)
}
// Azimuth 项目新增综合测试用例
// 使用标准 MoonBit 测试语法，涵盖多种测试场景

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let result1 = add(2147483647, 0)  // 最大整数加零
  let result2 = add(-2147483648, 0)  // 最小整数加零
  let result3 = add(1000000, 1000000)  // 大数相加
  let result4 = add(-1000000, 1000000)  // 相反数相加
  
  assert_eq(2147483647, result1)
  assert_eq(-2147483648, result2)
  assert_eq(2000000, result3)
  assert_eq(0, result4)
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let result1 = multiply(2147483647, 1)  // 最大整数乘以1
  let result2 = multiply(-2147483648, 1)  // 最小整数乘以1
  let result3 = multiply(1000, 1000)  // 大数相乘
  let result4 = multiply(12345, 0)  // 乘以零
  
  assert_eq(2147483647, result1)
  assert_eq(-2147483648, result2)
  assert_eq(1000000, result3)
  assert_eq(0, result4)
}

test "greet_function_multilingual" {
  // 测试多语言问候功能
  let result1 = greet("世界")
  let result2 = greet("World")
  let result3 = greet("🌍")
  let result4 = greet("123")
  
  assert_eq_string("Hello, 世界!", result1)
  assert_eq_string("Hello, World!", result2)
  assert_eq_string("Hello, 🌍!", result3)
  assert_eq_string("Hello, 123!", result4)
}

test "complex_mathematical_operations" {
  // 测试复杂数学运算
  let a = 12
  let b = 8
  let c = 5
  
  // 测试组合运算：(a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(100, result1)
  
  // 测试分配律：a * b + a * c
  let result2 = add(multiply(a, b), multiply(a, c))
  assert_eq(156, result2)
  
  // 测试嵌套运算：(a - b) * (a + b)
  let result3 = multiply(add(a, -b), add(a, b))
  assert_eq(80, result3)
}

test "business_calculation_scenarios" {
  // 测试商业计算场景
  let unit_price = 199
  let quantity = 5
  let discount_rate = 10  // 10% 折扣
  let tax_rate = 8        // 8% 税率
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  assert_eq(995, subtotal)
  
  // 计算折扣
  let discount = multiply(subtotal, discount_rate) / 100
  assert_eq(99, discount)
  
  // 计算折后价
  let discounted_price = add(subtotal, -discount)
  assert_eq(896, discounted_price)
  
  // 计算税费
  let tax = multiply(discounted_price, tax_rate) / 100
  assert_eq(71, tax)
  
  // 计算总价
  let total = add(discounted_price, tax)
  assert_eq(967, total)
}

test "scientific_calculations" {
  // 测试科学计算
  let base = 10
  let exponent = 3
  let coefficient = 2
  
  // 计算 2 * 10^3 (简化为乘法)
  let result1 = multiply(coefficient, multiply(base, multiply(base, base)))
  assert_eq(2000, result1)
  
  // 计算平方差公式：a^2 - b^2 = (a - b)(a + b)
  let a = 15
  let b = 8
  let result2 = multiply(add(a, -b), add(a, b))
  assert_eq(161, result2)
  
  // 计算立方和：a^3 + b^3 (简化)
  let result3 = add(multiply(a, multiply(a, a)), multiply(b, multiply(b, b)))
  assert_eq(4607, result3)
}

test "date_and_time_calculations" {
  // 测试日期时间计算
  let days_in_week = 7
  let weeks_in_month = 4
  let months_in_year = 12
  
  // 计算一年中的天数（简化）
  let days_in_year = multiply(days_in_week, multiply(weeks_in_month, months_in_year))
  assert_eq(336, days_in_year)
  
  // 计算半年天数
  let half_year_days = multiply(days_in_year, 50) / 100
  assert_eq(168, half_year_days)
  
  // 计算季度天数
  let quarter_days = multiply(days_in_year, 25) / 100
  assert_eq(84, quarter_days)
}

test "statistical_calculations" {
  // 测试统计计算
  let values = [65, 78, 92, 85, 70]
  let count = 5
  
  // 计算总和
  let sum = add(add(add(add(values[0], values[1]), values[2]), values[3]), values[4])
  assert_eq(390, sum)
  
  // 计算平均值
  let average = sum / count
  assert_eq(78, average)
  
  // 计算最大值与最小值的差
  let max_val = 92
  let min_val = 65
  let range = add(max_val, -min_val)
  assert_eq(27, range)
}

test "unit_conversion_calculations" {
  // 测试单位转换计算
  let meters = 100
  let kilometers = multiply(meters, 1) / 1000  // 米转公里
  let centimeters = multiply(meters, 100)     // 米转厘米
  
  assert_eq(0, kilometers)  // 整数除法
  assert_eq(10000, centimeters)
  
  // 测试重量转换
  let kilograms = 5
  let grams = multiply(kilograms, 1000)  // 公斤转克
  let tons = multiply(kilograms, 1) / 1000  // 公斤转吨
  
  assert_eq(5000, grams)
  assert_eq(0, tons)  // 整数除法
}

test "game_score_calculations" {
  // 测试游戏得分计算
  let base_score = 100
  let level_multiplier = 3
  let combo_bonus = 500
  let penalty = 150
  
  // 计算等级得分
  let level_score = multiply(base_score, level_multiplier)
  assert_eq(300, level_score)
  
  // 计算连击得分
  let combo_score = add(level_score, combo_bonus)
  assert_eq(800, combo_score)
  
  // 计算最终得分
  let final_score = add(combo_score, -penalty)
  assert_eq(650, final_score)
  
  // 计算排名加成（前20%加成10%）
  let rank_bonus = multiply(final_score, 10) / 100
  let ranked_score = add(final_score, rank_bonus)
  assert_eq(715, ranked_score)
}
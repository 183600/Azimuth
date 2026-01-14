// 额外的 MoonBit 测试用例
// 导入 azimuth 包以使用其中的函数
test "advanced_arithmetic_operations" {
  // 测试高级算术运算
  // 计算复利：本金 × (1 + 利率)^年数
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 简化的复利计算
  let growth = azimuth::@azimuth.multiply(principal, azimuth::@azimuth.multiply(rate, years))
  let total = azimuth::@azimuth.add(principal, growth)
  @azimuth.assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000
  
  // 计算平方和：a² + b²
  let a = 3
  let b = 4
  let a_squared = azimuth::@azimuth.multiply(a, a)
  let b_squared = azimuth::@azimuth.multiply(b, b)
  let sum_of_squares = azimuth::@azimuth.add(a_squared, b_squared)
  @azimuth.assert_eq(25, sum_of_squares)  // 3² + 4² = 9 + 16 = 25
}
test "string_manipulation_advanced" {
  // 测试高级字符串操作
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = azimuth::@azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result1)
  
  // 测试包含Unicode字符的字符串
  let unicode_chars = "αβγδεζηθ"
  let result2 = azimuth::@azimuth.greet(unicode_chars)
  @azimuth.assert_eq_string("Hello, αβγδεζηθ!", result2)
  
  // 测试包含表情符号的字符串
  let emoji_string = "🎉🚀🌟💻"
  let result3 = azimuth::@azimuth.greet(emoji_string)
  @azimuth.assert_eq_string("Hello, 🎉🚀🌟💻!", result3)
}
test "mathematical_series_calculations" {
  // 测试数学序列计算
  // 计算斐波那契数列前几项的和：1 + 1 + 2 + 3 + 5 + 8 = 20
  let fib1 = 1
  let fib2 = 1
  let fib3 = azimuth::@azimuth.add(fib1, fib2)  // 2
  let fib4 = azimuth::@azimuth.add(fib2, fib3)  // 3
  let fib5 = azimuth::@azimuth.add(fib3, fib4)  // 5
  let fib6 = azimuth::@azimuth.add(fib4, fib5)  // 8
  
  let fib_sum = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(fib1, fib2), azimuth::@azimuth.add(fib3, fib4)), azimuth::@azimuth.add(fib5, fib6))
  @azimuth.assert_eq(20, fib_sum)
  
  // 计算等比数列：2 + 4 + 8 + 16 = 30
  let geo1 = 2
  let geo2 = azimuth::@azimuth.multiply(geo1, 2)  // 4
  let geo3 = azimuth::@azimuth.multiply(geo2, 2)  // 8
  let geo4 = azimuth::@azimuth.multiply(geo3, 2)  // 16
  
  let geo_sum = azimuth::@azimuth.add(azimuth::@azimuth.add(geo1, geo2), azimuth::@azimuth.add(geo3, geo4))
  @azimuth.assert_eq(30, geo_sum)
}
test "business_logic_scenarios" {
  // 测试业务逻辑场景
  // 计算折扣价格：原价 × (1 - 折扣率)
  let original_price = 200
  let discount_rate = 20  // 20%
  let discount_amount = azimuth::@azimuth.multiply(original_price, discount_rate) / 100
  let discounted_price = azimuth::@azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(160, discounted_price)  // 200 - 200*20% = 200 - 40 = 160
  
  // 计算利润率：(售价 - 成本) / 成本 × 100%
  let cost = 100
  let selling_price = 150
  let profit = azimuth::@azimuth.add(selling_price, -cost)
  let profit_rate = azimuth::@azimuth.multiply(profit, 100) / cost
  @azimuth.assert_eq(50, profit_rate)  // (150 - 100) / 100 × 100% = 50%
}
test "geometry_calculations" {
  // 测试几何计算
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 6
  let bottom_base = 10
  let height = 4
  
  let sum_of_bases = azimuth::@azimuth.add(top_base, bottom_base)
  let trapezoid_area = azimuth::@azimuth.multiply(sum_of_bases, height) / 2
  @azimuth.assert_eq(32, trapezoid_area)  // (6 + 10) × 4 ÷ 2 = 32
  
  // 计算圆柱体积：底面积 × 高 = π × r² × h（简化π为3）
  let radius = 5
  let cylinder_height = 10
  let base_area = azimuth::@azimuth.multiply(3, azimuth::@azimuth.multiply(radius, radius))
  let cylinder_volume = azimuth::@azimuth.multiply(base_area, cylinder_height)
  @azimuth.assert_eq(750, cylinder_volume)  // 3 × 5² × 10 = 750
}
test "time_and_date_calculations" {
  // 测试时间和日期计算
  // 计算两个时间点之间的分钟数
  let start_hour = 9
  let start_minute = 30
  let end_hour = 14
  let end_minute = 45
  
  let start_total_minutes = azimuth::@azimuth.add(azimuth::@azimuth.multiply(start_hour, 60), start_minute)
  let end_total_minutes = azimuth::@azimuth.add(azimuth::@azimuth.multiply(end_hour, 60), end_minute)
  let duration_minutes = azimuth::@azimuth.add(end_total_minutes, -start_total_minutes)
  @azimuth.assert_eq(315, duration_minutes)  // (14×60+45) - (9×60+30) = 885 - 570 = 315
  
  // 计算天数：小时数 ÷ 24
  let hours = 72
  let days = hours / 24
  @azimuth.assert_eq(3, days)
}
test "error_boundary_testing" {
  // 测试错误边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续溢出操作
  let overflow1 = azimuth::@azimuth.add(max_val, 1)
  let overflow2 = azimuth::@azimuth.add(overflow1, 1)
  @azimuth.assert_eq(max_val, overflow2)
  
  // 测试连续下溢操作
  let underflow1 = azimuth::@azimuth.add(min_val, -1)
  let underflow2 = azimuth::@azimuth.add(underflow1, -1)
  @azimuth.assert_eq(min_val, underflow2)
  
  // 测试极值乘法
  let extreme_mult1 = azimuth::@azimuth.multiply(max_val, max_val)
  let extreme_mult2 = azimuth::@azimuth.multiply(min_val, min_val)
  @azimuth.assert_eq(max_val, extreme_mult1)
  @azimuth.assert_eq(max_val, extreme_mult2)  // 负负得正，但会溢出
}
test "algorithm_implementation" {
  // 测试简单算法实现
  // 计算最大公约数（简化版，使用已知的GCD值）
  let a = 48
  let b = 18
  let gcd_ab = 6  // GCD(48, 18) = 6
  
  // 验证欧几里得算法的性质：a × b = GCD(a, b) × LCM(a, b)
  // 因此 LCM(a, b) = (a × b) / GCD(a, b)
  let product_ab = azimuth::@azimuth.multiply(a, b)
  let lcm_ab = product_ab / gcd_ab
  @azimuth.assert_eq(144, lcm_ab)  // (48 × 18) / 6 = 864 / 6 = 144
  
  // 测试素数性质（使用已知素数）
  let prime = 7
  let next_prime = 11
  let prime_sum = azimuth::@azimuth.add(prime, next_prime)
  let prime_product = azimuth::@azimuth.multiply(prime, next_prime)
  @azimuth.assert_eq(18, prime_sum)     // 7 + 11 = 18
  @azimuth.assert_eq(77, prime_product) // 7 × 11 = 77
}
test "statistical_calculations" {
  // 测试统计计算
  // 计算一组数的方差（简化版）
  // 数据：10, 20, 30, 40, 50
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  // 计算平均值
  let sum = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(data1, data2), data3), data4), data5)
  let mean = sum / 5
  @azimuth.assert_eq(30, mean)  // (10+20+30+40+50)/5 = 150/5 = 30
  
  // 计算方差（简化）：Σ(x - mean)² / n
  let diff1 = azimuth::@azimuth.add(data1, -mean)  // -20
  let diff2 = azimuth::@azimuth.add(data2, -mean)  // -10
  let diff3 = azimuth::@azimuth.add(data3, -mean)  // 0
  let diff4 = azimuth::@azimuth.add(data4, -mean)  // 10
  let diff5 = azimuth::@azimuth.add(data5, -mean)  // 20
  
  let sq_diff1 = azimuth::@azimuth.multiply(diff1, diff1)  // 400
  let sq_diff2 = azimuth::@azimuth.multiply(diff2, diff2)  // 100
  let sq_diff3 = azimuth::@azimuth.multiply(diff3, diff3)  // 0
  let sq_diff4 = azimuth::@azimuth.multiply(diff4, diff4)  // 100
  let sq_diff5 = azimuth::@azimuth.multiply(diff5, diff5)  // 400
  
  let sum_sq_diffs = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(sq_diff1, sq_diff2), sq_diff3), sq_diff4), sq_diff5)
  let variance = sum_sq_diffs / 5
  @azimuth.assert_eq(200, variance)  // (400+100+0+100+400)/5 = 1000/5 = 200
}
test "complex_composite_scenarios" {
  // 测试复杂复合场景
  // 场景：计算一个项目的总成本
  let labor_cost_per_hour = 50
  let labor_hours = 160
  let material_cost_per_unit = 25
  let material_units = 100
  let overhead_rate = 20  // 20%
  let profit_margin = 15  // 15%
  
  // 计算人工成本
  let labor_cost = azimuth::@azimuth.multiply(labor_cost_per_hour, labor_hours)
  
  // 计算材料成本
  let material_cost = azimuth::@azimuth.multiply(material_cost_per_unit, material_units)
  
  // 计算直接成本
  let direct_cost = azimuth::@azimuth.add(labor_cost, material_cost)
  
  // 计算管理费用
  let overhead_cost = azimuth::@azimuth.multiply(direct_cost, overhead_rate) / 100
  
  // 计算总成本
  let total_cost = azimuth::@azimuth.add(direct_cost, overhead_cost)
  
  // 计算利润
  let profit = azimuth::@azimuth.multiply(total_cost, profit_margin) / 100
  
  // 计算项目价格
  let project_price = azimuth::@azimuth.add(total_cost, profit)
  
  // 验证计算结果
  @azimuth.assert_eq(8000, labor_cost)      // 50 × 160 = 8000
  @azimuth.assert_eq(2500, material_cost)   // 25 × 100 = 2500
  @azimuth.assert_eq(10500, direct_cost)    // 8000 + 2500 = 10500
  @azimuth.assert_eq(2100, overhead_cost)   // 10500 × 20% = 2100
  @azimuth.assert_eq(12600, total_cost)     // 10500 + 2100 = 12600
  @azimuth.assert_eq(1890, profit)          // 12600 × 15% = 1890
  @azimuth.assert_eq(14490, project_price)  // 12600 + 1890 = 14490
}
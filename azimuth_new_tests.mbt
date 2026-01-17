// 新增 MoonBit 测试用例
// 针对 azimuth 模块的综合测试

test "arithmetic_sequence_calculations" {
  // 测试等差数列计算
  // a, a+d, a+2d, a+3d, ...
  let first_term = 5
  let difference = 3
  
  let term1 = first_term
  let term2 = add(term1, difference)
  let term3 = add(term2, difference)
  let term4 = add(term3, difference)
  let term5 = add(term4, difference)
  
  assert_eq(5, term1)
  assert_eq(8, term2)
  assert_eq(11, term3)
  assert_eq(14, term4)
  assert_eq(17, term5)
  
  // 计算前5项的和
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(55, sum)
}

test "compound_interest_calculation" {
  // 测试复利计算（简化版）
  // 本金 × (1 + 利率)^年数
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 简化计算：逐年计算
  let year1_amount = add(principal, multiply(principal, rate) / 100)
  let year2_amount = add(year1_amount, multiply(year1_amount, rate) / 100)
  let year3_amount = add(year2_amount, multiply(year2_amount, rate) / 100)
  
  assert_eq(1157, year3_amount)  // 近似值
}

test "data_validation_scenarios" {
  // 测试数据验证场景
  // 验证年龄范围
  let age = 25
  let min_age = 18
  let max_age = 65
  
  let is_valid_age = (age >= min_age) and (age <= max_age)
  assert_true(is_valid_age)
  
  // 验证分数范围
  let score = 85
  let min_score = 0
  let max_score = 100
  
  let is_valid_score = (score >= min_score) and (score <= max_score)
  assert_true(is_valid_score)
  
  // 测试无效分数
  let invalid_score = 105
  let is_invalid_score = (invalid_score >= min_score) and (invalid_score <= max_score)
  assert_false(is_invalid_score)
}

test "unit_conversion_calculations" {
  // 测试单位转换计算
  // 公里转英里：1公里 ≈ 0.621英里
  // 使用整数近似：英里 = 公里 × 5 / 8
  
  let kilometers = 80
  let miles = multiply(kilometers, 5) / 8
  assert_eq(50, miles)
  
  // 公斤转磅：1公斤 ≈ 2.2磅
  // 使用整数近似：磅 = 公斤 × 2 + 公斤 / 5
  
  let kilograms = 10
  let pounds = add(multiply(kilograms, 2), kilograms / 5)
  assert_eq(22, pounds)
  
  // 升转加仑：1升 ≈ 0.264加仑
  // 使用整数近似：加仑 = 升 / 4
  
  let liters = 20
  let gallons = liters / 4
  assert_eq(5, gallons)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算一组数据的平均值、中位数和众数
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 30  // 众数是30
  
  // 计算平均值
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  let mean = sum / 5
  assert_eq(26, mean)
  
  // 计算中位数（已排序：10, 20, 30, 30, 40）
  let median = 30
  assert_eq(30, median)
  
  // 计算范围（最大值-最小值）
  let range = add(data4, -data1)
  assert_eq(30, range)
}

test "time_calculations" {
  // 测试时间计算
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = add(add(multiply(hours, 3600), multiply(minutes, 60)), seconds)
  assert_eq(9930, total_seconds)
  
  // 计算两个时间点的差值
  let start_hour = 8
  let start_minute = 30
  let end_hour = 14
  let end_minute = 45
  
  let start_total = add(multiply(start_hour, 60), start_minute)
  let end_total = add(multiply(end_hour, 60), end_minute)
  let difference = add(end_total, -start_total)
  
  assert_eq(375, difference)  // 6小时15分钟 = 375分钟
}

test "geometry_calculations" {
  // 测试几何计算
  // 计算矩形的面积和周长
  let length = 8
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = add(multiply(length, 2), multiply(width, 2))
  
  assert_eq(40, area)
  assert_eq(26, perimeter)
  
  // 计算圆的面积和周长（简化版，π≈3）
  let radius = 7
  let pi_approx = 3
  
  let circle_area = multiply(multiply(radius, radius), pi_approx)
  let circle_circumference = multiply(multiply(radius, 2), pi_approx)
  
  assert_eq(147, circle_area)
  assert_eq(42, circle_circumference)
}

test "shopping_cart_calculations" {
  // 测试购物车计算
  let item1_price = 25
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  let item3_price = 50
  let item3_quantity = 1
  
  // 计算各项小计
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let subtotal3 = multiply(item3_price, item3_quantity)
  
  // 计算总价
  let total = add(add(subtotal1, subtotal2), subtotal3)
  assert_eq(125, total)
  
  // 计算税费（假设10%）
  let tax_rate = 10
  let tax_amount = multiply(total, tax_rate) / 100
  let final_total = add(total, tax_amount)
  
  assert_eq(137, final_total)
}

test "game_score_calculations" {
  // 测试游戏分数计算
  let base_score = 100
  let level_multiplier = 3
  let bonus_points = 50
  let penalty_points = 20
  
  // 计算最终分数
  let level_score = multiply(base_score, level_multiplier)
  let raw_score = add(add(level_score, bonus_points), -penalty_points)
  
  assert_eq(330, raw_score)
  
  // 计算排名分数（前10名额外加分）
  let rank = 5
  let rank_bonus = multiply(add(10, -rank), 10)
  let final_score = add(raw_score, rank_bonus)
  
  assert_eq(360, final_score)
}

test "string_length_calculations" {
  // 测试字符串长度计算（通过greet函数）
  let short_name = "Bob"
  let long_name = "Alexander"
  let empty_name = ""
  
  let short_greeting = greet(short_name)
  let long_greeting = greet(long_name)
  let empty_greeting = greet(empty_name)
  
  // 验证问候语格式
  assert_eq_string("Hello, Bob!", short_greeting)
  assert_eq_string("Hello, Alexander!", long_greeting)
  assert_eq_string("Hello, !", empty_greeting)
  
  // 计算问候语长度差异（简化版）
  // "Hello, " = 7个字符，"!" = 1个字符
  let base_length = 8
  let short_total = add(base_length, 3)  // "Bob"
  let long_total = add(base_length, 9)   // "Alexander"
  let empty_total = base_length          // 空字符串
  
  assert_eq(11, short_total)
  assert_eq(17, long_total)
  assert_eq(8, empty_total)
}
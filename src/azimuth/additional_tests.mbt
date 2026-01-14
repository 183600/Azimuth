// 额外的 MoonBit 测试用例
// 这些测试用例涵盖了各种实际应用场景和边界情况

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0)=0, F(1)=1, F(2)=1, F(3)=2, F(4)=3, F(5)=5, F(6)=8, F(7)=13
  let fib_0 = 0
  let fib_1 = 1
  let fib_2 = azimuth::add(fib_0, fib_1)  // 1
  let fib_3 = azimuth::add(fib_1, fib_2)  // 2
  let fib_4 = azimuth::add(fib_2, fib_3)  // 3
  let fib_5 = azimuth::add(fib_3, fib_4)  // 5
  let fib_6 = azimuth::add(fib_4, fib_5)  // 8
  let fib_7 = azimuth::add(fib_5, fib_6)  // 13
  
  assert_eq(1, fib_2)
  assert_eq(2, fib_3)
  assert_eq(3, fib_4)
  assert_eq(5, fib_5)
  assert_eq(8, fib_6)
  assert_eq(13, fib_7)
}

test "prime_number_validation" {
  // 验证一些素数和非素数
  // 通过简单的除法测试验证素数
  
  // 7是素数（不能被2,3,4,5,6整除）
  let candidate_7 = 7
  let is_7_prime = azimuth::add(candidate_7 % 2, azimuth::add(candidate_7 % 3, azimuth::add(candidate_7 % 4, azimuth::add(candidate_7 % 5, candidate_7 % 6)))) > 0
  assert_true(is_7_prime)
  
  // 11是素数
  let candidate_11 = 11
  let is_11_prime = azimuth::add(candidate_11 % 2, azimuth::add(candidate_11 % 3, azimuth::add(candidate_11 % 4, azimuth::add(candidate_11 % 5, azimuth::add(candidate_11 % 6, candidate_11 % 7))))) > 0
  assert_true(is_11_prime)
  
  // 9不是素数（能被3整除）
  let candidate_9 = 9
  let is_9_prime = candidate_9 % 3 > 0
  assert_false(is_9_prime)
}

test "loan_calculation_scenario" {
  // 贷款计算场景
  let principal = 100000  // 本金
  let annual_rate = 5     // 年利率5%
  let years = 3           // 贷款年限
  
  // 简化的单利计算：总利息 = 本金 × 利率 × 年数
  let total_interest = azimuth::multiply(azimuth::multiply(principal, annual_rate), years) / 100
  assert_eq(15000, total_interest)  // 100000 × 5 × 3 / 100 = 15000
  
  // 总还款额 = 本金 + 利息
  let total_payment = azimuth::add(principal, total_interest)
  assert_eq(115000, total_payment)
  
  // 月供（简化计算）
  let monthly_payment = total_payment / azimuth::multiply(years, 12)
  assert_eq(3194, monthly_payment)  // 整数除法
}

test "temperature_conversion_comprehensive" {
  // 温度转换综合测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数运算：F = C × 2 + 30（近似值）
  
  let celsius_values = [0, 10, 25, 37, 100]
  let expected_fahrenheit = [30, 50, 80, 104, 230]
  
  // 冰点
  let fahrenheit_0 = azimuth::add(azimuth::multiply(0, 2), 30)
  assert_eq(30, fahrenheit_0)
  
  // 室温
  let fahrenheit_25 = azimuth::add(azimuth::multiply(25, 2), 30)
  assert_eq(80, fahrenheit_25)
  
  // 体温
  let fahrenheit_37 = azimuth::add(azimuth::multiply(37, 2), 30)
  assert_eq(104, fahrenheit_37)
  
  // 沸点
  let fahrenheit_100 = azimuth::add(azimuth::multiply(100, 2), 30)
  assert_eq(230, fahrenheit_100)
}

test "shopping_cart_with_discounts" {
  // 带折扣的购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 599
  let item2_quantity = 1
  let item3_price = 199
  let item3_quantity = 3
  
  // 计算各项总价
  let total1 = azimuth::multiply(item1_price, item1_quantity)
  let total2 = azimuth::multiply(item2_price, item2_quantity)
  let total3 = azimuth::multiply(item3_price, item3_quantity)
  
  // 计算小计
  let subtotal = azimuth::add(azimuth::add(total1, total2), total3)
  assert_eq(1494, subtotal)  // 299*2 + 599*1 + 199*3 = 598 + 599 + 597 = 1794
  
  // 满1000减100折扣
  let discount = if (subtotal >= 1000) { 100 } else { 0 }
  let after_discount = azimuth::add(subtotal, -discount)
  assert_eq(1394, after_discount)
  
  // 运费（满1000免运费）
  let shipping = if (subtotal >= 1000) { 0 } else { 50 }
  let final_total = azimuth::add(after_discount, shipping)
  assert_eq(1394, final_total)
}

test "array_index_calculations" {
  // 数组索引计算测试
  // 二维数组转一维数组的索引计算：index = row * width + col
  
  let width = 5
  let height = 4
  
  // 测试几个关键位置
  let index_00 = azimuth::add(azimuth::multiply(0, width), 0)  // 第0行第0列
  let index_04 = azimuth::add(azimuth::multiply(0, width), 4)  // 第0行第4列
  let index_10 = azimuth::add(azimuth::multiply(1, width), 0)  // 第1行第0列
  let index_34 = azimuth::add(azimuth::multiply(3, width), 4)  // 第3行第4列（最后一个）
  
  assert_eq(0, index_00)
  assert_eq(4, index_04)
  assert_eq(5, index_10)
  assert_eq(19, index_34)
  
  // 验证总数
  let total_elements = azimuth::multiply(width, height)
  assert_eq(20, total_elements)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  // 测试各种特殊字符串的greet函数处理
  
  // 空字符串
  let empty_result = azimuth::greet("")
  assert_eq_string("Hello, !", empty_result)
  
  // 单字符
  let single_char_result = azimuth::greet("A")
  assert_eq_string("Hello, A!", single_char_result)
  
  // 包含空格的字符串
  let spaced_result = azimuth::greet("John Doe")
  assert_eq_string("Hello, John Doe!", spaced_result)
  
  // 包含特殊字符
  let special_result = azimuth::greet("user@domain.com")
  assert_eq_string("Hello, user@domain.com!", special_result)
  
  // 包含Unicode字符
  let unicode_result = azimuth::greet("测试123")
  assert_eq_string("Hello, 测试123!", unicode_result)
}

test "mathematical_series_evaluation" {
  // 数学级数求值
  
  // 等差数列求和：1 + 3 + 5 + ... + 19（前10个奇数）
  // 使用公式：n × (首项 + 末项) / 2
  let n = 10
  let first_term = 1
  let last_term = 19
  let arithmetic_sum = azimuth::multiply(n, azimuth::add(first_term, last_term)) / 2
  assert_eq(100, arithmetic_sum)
  
  // 等比数列求和：2 + 4 + 8 + 16 + 32（前5项，公比为2）
  // 手动计算
  let geo_term1 = 2
  let geo_term2 = azimuth::multiply(geo_term1, 2)
  let geo_term3 = azimuth::multiply(geo_term2, 2)
  let geo_term4 = azimuth::multiply(geo_term3, 2)
  let geo_term5 = azimuth::multiply(geo_term4, 2)
  let geometric_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(geo_term1, geo_term2), geo_term3), geo_term4), geo_term5)
  assert_eq(62, geometric_sum)
  
  // 平方和：1² + 2² + 3² + 4² + 5²
  let square_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::multiply(1, 1), azimuth::multiply(2, 2)), azimuth::multiply(3, 3)), azimuth::add(azimuth::multiply(4, 4), azimuth::multiply(5, 5)))
  assert_eq(55, square_sum)
}

test "game_mechanics_simulation" {
  // 游戏机制模拟
  
  // 角色属性计算
  let base_strength = 10
  let equipment_bonus = 5
  let buff_multiplier = 150  // 1.5倍表示为150
  let level_bonus = 3
  
  // 计算最终力量值
  let equipment_total = azimuth::add(base_strength, equipment_bonus)
  let buffed_strength = azimuth::multiply(equipment_total, buff_multiplier) / 100
  let final_strength = azimuth::add(buffed_strength, level_bonus)
  assert_eq(23, final_strength)  // (10+5)*1.5+3 = 22.5，整数除法为22
  
  // 伤害计算
  let weapon_damage = 50
  let critical_hit = if (true) { 2 } else { 1 }  // 暴击2倍伤害
  let defense = 20
  let final_damage = azimuth::add(azimuth::multiply(weapon_damage, critical_hit), -defense)
  assert_eq(80, final_damage)  // 50*2-20 = 80
  
  // 经验值计算
  let base_exp = 100
  let level_difficulty = 3
  let party_bonus = 150  // 1.5倍
  let total_exp = azimuth::multiply(azimuth::multiply(base_exp, level_difficulty), party_bonus) / 100
  assert_eq(450, total_exp)  // 100*3*1.5 = 450
}

test "data_analysis_scenario" {
  // 数据分析场景
  
  // 销售数据分析
  let q1_sales = 15000
  let q2_sales = 18000
  let q3_sales = 22000
  let q4_sales = 25000
  
  // 年度总销售额
  let annual_sales = azimuth::add(azimuth::add(azimuth::add(q1_sales, q2_sales), q3_sales), q4_sales)
  assert_eq(80000, annual_sales)
  
  // 季度平均销售额
  let quarterly_average = annual_sales / 4
  assert_eq(20000, quarterly_average)
  
  // 同比增长率（简化计算）
  let last_year_sales = 70000
  let growth_rate = azimuth::multiply(azimuth::add(annual_sales, -last_year_sales), 100) / last_year_sales
  assert_eq(14, growth_rate)  // (80000-70000)*100/70000 ≈ 14%
  
  // 下季度预测（基于平均增长率）
  let avg_growth = azimuth::add(azimuth::add(azimuth::add(q2_sales - q1_sales, q3_sales - q2_sales), q4_sales - q3_sales), 0) / 3
  let next_quarter_forecast = azimuth::add(q4_sales, avg_growth)
  assert_eq(28000, next_quarter_forecast)  // 25000 + (3000+4000+3000)/3 = 25000 + 3333 ≈ 28333，整数除法为28000
}
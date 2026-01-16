// Azimuth 项目的新增 MoonBit 测试用例
// 包含多种场景的测试，使用标准 MoonBit 测试语法

// 测试用例 1: 复杂数学运算场景
test "complex_mathematical_scenarios" {
  // 测试多项式计算: 3x² + 2x + 1，其中 x = 5
  let x = 5
  let x_squared = azimuth::multiply(x, x)  // x² = 25
  let term1 = azimuth::multiply(3, x_squared)  // 3x² = 75
  let term2 = azimuth::multiply(2, x)  // 2x = 10
  let result = azimuth::add(azimuth::add(term1, term2), 1)  // 75 + 10 + 1 = 86
  assert_eq(result, 86)
}

// 测试用例 2: 字符串处理边界情况
test "string_processing_edge_cases" {
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result = azimuth::greet(special_chars)
  assert_eq(result, "Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!")
  
  // 测试包含数字的字符串
  let numeric_string = "1234567890"
  let result2 = azimuth::greet(numeric_string)
  assert_eq(result2, "Hello, 1234567890!")
}

// 测试用例 3: 实际应用场景 - 购物车计算
test "shopping_cart_calculations" {
  // 模拟购物车计算: 商品价格 × 数量 + 运费 - 折扣
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let discount = 50
  
  let subtotal = azimuth::multiply(item_price, quantity)  // 199 × 3 = 597
  let total_before_discount = azimuth::add(subtotal, shipping)  // 597 + 20 = 617
  let final_total = azimuth::add(total_before_discount, -discount)  // 617 - 50 = 567
  
  assert_eq(final_total, 567)
}

// 测试用例 4: 边界值组合测试
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  let sum_result = azimuth::add(max_val, min_val)
  assert_eq(sum_result, -1)
  
  // 测试最大值与1的乘法
  let max_mul_result = azimuth::multiply(max_val, 1)
  assert_eq(max_mul_result, max_val)
  
  // 测试最小值与1的乘法
  let min_mul_result = azimuth::multiply(min_val, 1)
  assert_eq(min_mul_result, min_val)
}

// 测试用例 5: 温度转换计算
test "temperature_conversion" {
  // 摄氏度转华氏度: F = C × 9/5 + 32
  // 使用整数运算近似: F = C × 2 + 30
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(fahrenheit, 80)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth::add(azimuth::multiply(freezing_celsius, 2), 30)
  assert_eq(freezing_fahrenheit, 30)
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = azimuth::add(azimuth::multiply(boiling_celsius, 2), 30)
  assert_eq(boiling_fahrenheit, 230)
}

// 测试用例 6: 数列计算
test "sequence_calculations" {
  // 计算等差数列前n项和: 1 + 3 + 5 + ... + 19 (前10个奇数)
  let n = 10
  let first_term = 1
  let last_term = 19
  
  // 使用公式: n × (首项 + 末项) / 2
  let sum = azimuth::multiply(n, azimuth::add(first_term, last_term)) / 2
  assert_eq(sum, 100)
  
  // 计算等比数列: 2 + 4 + 8 + 16
  let term1 = 2
  let term2 = azimuth::multiply(term1, 2)  // 4
  let term3 = azimuth::multiply(term2, 2)  // 8
  let term4 = azimuth::multiply(term3, 2)  // 16
  let geometric_sum = azimuth::add(azimuth::add(azimuth::add(term1, term2), term3), term4)
  assert_eq(geometric_sum, 30)
}

// 测试用例 7: 面积和体积计算
test "area_and_volume_calculations" {
  // 计算矩形面积: 长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = azimuth::multiply(length, width)
  assert_eq(rectangle_area, 96)
  
  // 计算三角形面积: 底 × 高 / 2
  let base = 10
  let height = 6
  let triangle_area = azimuth::multiply(base, height) / 2
  assert_eq(triangle_area, 30)
  
  // 计算立方体体积: 边长³
  let side = 5
  let cube_volume = azimuth::multiply(azimuth::multiply(side, side), side)
  assert_eq(cube_volume, 125)
}

// 测试用例 8: 金融计算场景
test "financial_calculations" {
  // 简单利息计算: 本金 × (1 + 利率 × 时间)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  let interest_rate = azimuth::multiply(rate, years)  // 5 × 3 = 15
  let growth_factor = azimuth::add(1, interest_rate)  // 1 + 15 = 16
  let total_amount = azimuth::multiply(principal, growth_factor)
  assert_eq(total_amount, 160000)
  
  // 复利计算(简化版): 本金 × (1 + 利率)^年数
  // 简化为: 本金 × (1 + 利率 × 年数)
  let compound_total = azimuth::multiply(principal, azimuth::add(1, azimuth::multiply(rate, years)))
  assert_eq(compound_total, 160000)
}

// 测试用例 9: 游戏得分计算
test "game_score_calculations" {
  // 基础得分计算: 基础分 × 等级倍数 + 奖励分 - 惩罚分
  let base_score = 1000
  let level_multiplier = 3
  let bonus_points = 500
  let penalty_points = 200
  
  let level_score = azimuth::multiply(base_score, level_multiplier)  // 1000 × 3 = 3000
  let score_with_bonus = azimuth::add(level_score, bonus_points)  // 3000 + 500 = 3500
  let final_score = azimuth::add(score_with_bonus, -penalty_points)  // 3500 - 200 = 3300
  
  assert_eq(final_score, 3300)
  
  // 排名加分: 前10%额外加10%
  let top_percentage = 10
  let rank_bonus = azimuth::multiply(final_score, top_percentage) / 100
  let rank_total = azimuth::add(final_score, rank_bonus)
  assert_eq(rank_total, 3630)
}

// 测试用例 10: 时间和日期计算
test "time_and_date_calculations" {
  // 时间计算: 小时转分钟和秒
  let hours = 2
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = azimuth::add(azimuth::multiply(hours, 60), minutes)
  assert_eq(total_minutes, 150)
  
  // 转换为总秒数
  let total_seconds = azimuth::multiply(total_minutes, 60)
  assert_eq(total_seconds, 9000)
  
  // 计算时间差
  let start_hour = 8   // 8:00 AM
  let end_hour = 17    // 5:00 PM
  let duration_hours = azimuth::add(end_hour, -start_hour)
  let duration_minutes = azimuth::multiply(duration_hours, 60)
  assert_eq(duration_minutes, 540)
}
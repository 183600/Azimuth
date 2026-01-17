// Azimuth 项目新的综合测试用例
// 使用标准 MoonBit 测试语法，提供全面的测试覆盖

test "basic_addition_edge_cases" {
  // 基本加法边界情况测试
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.add(1, 0), 1)
  @azimuth.assert_eq(@azimuth.add(-1, 0), -1)
  @azimuth.assert_eq(@azimuth.add(1000, 0), 1000)
  @azimuth.assert_eq(@azimuth.add(-1000, 0), -1000)
}

test "basic_multiplication_edge_cases" {
  // 基本乘法边界情况测试
  @azimuth.assert_eq(@azimuth.multiply(0, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(1, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(-1, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(1, 1), 1)
  @azimuth.assert_eq(@azimuth.multiply(-1, -1), 1)
  @azimuth.assert_eq(@azimuth.multiply(1, -1), -1)
}

test "greet_function_special_characters" {
  // 问候函数特殊字符测试
  @azimuth.assert_eq_string(@azimuth.greet("Line\nBreak"), "Hello, Line\nBreak!")
  @azimuth.assert_eq_string(@azimuth.greet("Tab\tCharacter"), "Hello, Tab\tCharacter!")
  @azimuth.assert_eq_string(@azimuth.greet("Quote\"Mark"), "Hello, Quote\"Mark!")
  @azimuth.assert_eq_string(@azimuth.greet("Apostrophe'Mark"), "Hello, Apostrophe'Mark!")
  @azimuth.assert_eq_string(@azimuth.greet("Back\\Slash"), "Hello, Back\\Slash!")
}

test "mathematical_identity_properties" {
  // 数学恒等性质测试
  
  // 加法恒等律: a + 0 = a
  @azimuth.assert_eq(@azimuth.add(1, 0), 1)
  @azimuth.assert_eq(@azimuth.add(-1, 0), -1)
  @azimuth.assert_eq(@azimuth.add(42, 0), 42)
  @azimuth.assert_eq(@azimuth.add(-100, 0), -100)
  @azimuth.assert_eq(@azimuth.add(1000, 0), 1000)
  
  // 乘法恒等律: a * 1 = a
  @azimuth.assert_eq(@azimuth.multiply(1, 1), 1)
  @azimuth.assert_eq(@azimuth.multiply(-1, 1), -1)
  @azimuth.assert_eq(@azimuth.multiply(42, 1), 42)
  @azimuth.assert_eq(@azimuth.multiply(-100, 1), -100)
  @azimuth.assert_eq(@azimuth.multiply(1000, 1), 1000)
  
  // 乘法零律: a * 0 = 0
  @azimuth.assert_eq(@azimuth.multiply(1, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(-1, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(42, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(-100, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(1000, 0), 0)
}

test "complex_arithmetic_expressions" {
  // 复杂算术表达式测试
  
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 2
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试结合律: (a + b) + c = a + (b + c)
  let sum1 = @azimuth.add(@azimuth.add(a, b), c)
  let sum2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(sum1, sum2)
  
  // 测试交换律: a + b = b + a
  let comm1 = @azimuth.add(a, b)
  let comm2 = @azimuth.add(b, a)
  @azimuth.assert_eq(comm1, comm2)
}

test "practical_business_calculations" {
  // 实际业务计算测试
  
  // 计算总价: 单价 × 数量 + 税费
  let unit_price = 25
  let quantity = 8
  let tax_rate = 10  // 10%
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let tax_amount = @azimuth.multiply(subtotal, tax_rate) / 100
  let total_price = @azimuth.add(subtotal, tax_amount)
  @azimuth.assert_eq(220, total_price)
  
  // 计算折扣价: 原价 × (1 - 折扣率)
  let original_price = 120
  let discount = 15  // 15%
  let discount_amount = @azimuth.multiply(original_price, discount) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(102, final_price)
}

test "scientific_calculations" {
  // 科学计算测试
  
  // 计算欧拉数近似值: e ≈ 1 + 1/1! + 1/2! + 1/3!
  // 这里使用整数近似
  let term1 = 1
  let term2 = 1  // 1/1!
  let term3 = 1  // 1/2! ≈ 1 (整数近似)
  let term4 = 1  // 1/3! ≈ 1 (整数近似)
  let euler_approx = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(4, euler_approx)
  
  // 计算圆的周长近似值: 2 × π × r (使用 π ≈ 3)
  let radius = 5
  let pi_approx = 3
  let circumference_approx = @azimuth.multiply(@azimuth.multiply(2, pi_approx), radius)
  @azimuth.assert_eq(30, circumference_approx)
}

test "game_mechanics_calculations" {
  // 游戏机制计算测试
  
  // 计算经验值和等级
  let base_xp = 100
  let level_multiplier = 15
  let player_level = 3
  let xp_for_level = @azimuth.add(base_xp, @azimuth.multiply(level_multiplier, player_level))
  @azimuth.assert_eq(145, xp_for_level)
  
  // 计算伤害值: 基础伤害 × (1 + 暴击率)
  let base_damage = 50
  let crit_bonus = 20  // 20%
  let crit_multiplier = @azimuth.add(100, crit_bonus) / 100
  let total_damage = @azimuth.multiply(base_damage, crit_multiplier)
  @azimuth.assert_eq(60, total_damage)
  
  // 计算团队总得分
  let score1 = 120
  let score2 = 85
  let score3 = 95
  let score4 = 110
  let team_score = @azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4)
  @azimuth.assert_eq(410, team_score)
}

test "time_and_date_calculations" {
  // 时间和日期计算测试
  
  // 计算总秒数: 小时 × 3600 + 分钟 × 60 + 秒
  let hours = 2
  let minutes = 30
  let seconds = 45
  let total_seconds = @azimuth.add(@azimuth.add(@azimuth.multiply(hours, 3600), @azimuth.multiply(minutes, 60)), seconds)
  @azimuth.assert_eq(9045, total_seconds)
  
  // 计算工作日收入: 日薪 × 工作日数
  let daily_salary = 150
  let work_days = 22
  let monthly_salary = @azimuth.multiply(daily_salary, work_days)
  @azimuth.assert_eq(3300, monthly_salary)
  
  // 计算项目总工时: 每日工时 × 天数
  let daily_hours = 8
  let project_days = 15
  let total_hours = @azimuth.multiply(daily_hours, project_days)
  @azimuth.assert_eq(120, total_hours)
}

test "statistical_calculations" {
  // 统计计算测试
  
  // 计算平均值: (a + b + c + d) / 4
  let value1 = 85
  let value2 = 92
  let value3 = 78
  let value4 = 95
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(value1, value2), value3), value4)
  let average = sum / 4
  @azimuth.assert_eq(87, average)
  
  // 计算加权平均: (value1 × weight1 + value2 × weight2) / (weight1 + weight2)
  let val1 = 80
  let weight1 = 3
  let val2 = 90
  let weight2 = 7
  let weighted_sum = @azimuth.add(@azimuth.multiply(val1, weight1), @azimuth.multiply(val2, weight2))
  let total_weight = @azimuth.add(weight1, weight2)
  let weighted_average = weighted_sum / total_weight
  @azimuth.assert_eq(87, weighted_average)
  
  // 计算百分比变化: (new - old) / old × 100
  let old_value = 200
  let new_value = 250
  let difference = @azimuth.add(new_value, -old_value)
  let percentage_change = @azimuth.multiply(difference, 100) / old_value
  @azimuth.assert_eq(25, percentage_change)
}
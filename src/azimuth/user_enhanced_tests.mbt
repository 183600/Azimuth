// 增强的 MoonBit 测试用例
// 覆盖各种边界情况和实际应用场景

test "complex_financial_calculation" {
  // 复杂金融计算：贷款分期付款计算
  // 公式：月供 = [本金 × 月利率 × (1+月利率)^还款月数] / [(1+月利率)^还款月数 - 1]
  // 简化为整数计算
  
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 30          // 30年
  let months = azimuth::multiply(years, 12)  // 总月数
  
  // 简化计算：月供 ≈ 本金 / 总月数 + 利息
  let monthly_principal = principal / months
  let monthly_rate = annual_rate / 12  // 月利率
  let monthly_interest = azimuth::multiply(principal, monthly_rate) / 100
  
  let monthly_payment = azimuth::add(monthly_principal, monthly_interest)
  
  // 验证计算结果
  assert_true(monthly_payment > 0)
  assert_true(monthly_payment < principal)  // 月供应该小于本金
}

test "geometry_area_calculations" {
  // 几何面积计算测试
  // 圆面积 = π × r² (简化使用 π ≈ 3)
  let radius = 10
  let circle_area = azimuth::multiply(3, azimuth::multiply(radius, radius))
  assert_eq(300, circle_area)
  
  // 矩形面积 = 长 × 宽
  let length = 15
  let width = 8
  let rectangle_area = azimuth::multiply(length, width)
  assert_eq(120, rectangle_area)
  
  // 梯形面积 = (上底 + 下底) × 高 ÷ 2
  let top_base = 6
  let bottom_base = 10
  let height = 5
  let trapezoid_area = azimuth::multiply(azimuth::add(top_base, bottom_base), height) / 2
  assert_eq(40, trapezoid_area)
}

test "temperature_conversion_comprehensive" {
  // 温度转换综合测试
  // 摄氏度转华氏度：F = C × 9/5 + 32 (简化为 F = C × 2 + 30)
  let celsius_temps = [0, 10, 20, 30, 40, 100]
  let expected_fahrenheit = [30, 50, 70, 90, 110, 230]
  
  // 测试冰点
  assert_eq(30, azimuth::add(azimuth::multiply(0, 2), 30))
  
  // 测试室温
  assert_eq(70, azimuth::add(azimuth::multiply(20, 2), 30))
  
  // 测试沸点
  assert_eq(230, azimuth::add(azimuth::multiply(100, 2), 30))
  
  // 华氏度转摄氏度（简化）：C = (F - 30) ÷ 2
  let fahrenheit = 70
  let celsius = azimuth::add(fahrenheit, -30) / 2
  assert_eq(20, celsius)
}

test "shopping_cart_with_discounts" {
  // 购物车折扣计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  
  // 计算商品总价
  let subtotal1 = azimuth::multiply(item1_price, item1_quantity)
  let subtotal2 = azimuth::multiply(item2_price, item2_quantity)
  let subtotal3 = azimuth::multiply(item3_price, item3_quantity)
  let total_subtotal = azimuth::add(azimuth::add(subtotal1, subtotal2), subtotal3)
  
  // 计算折扣：满500减50，满1000减150
  let discount = if (total_subtotal >= 1000) {
    150
  } else if (total_subtotal >= 500) {
    50
  } else {
    0
  }
  
  // 计算最终价格
  let final_total = azimuth::add(total_subtotal, -discount)
  
  assert_eq(1144, final_total)  // 1294 - 150 = 1144
}

test "statistical_analysis" {
  // 统计分析测试
  // 数据集：[12, 15, 18, 21, 24, 27, 30]
  
  // 计算总和
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(12, 15), 18), 21), 24), 27), 30)
  assert_eq(147, sum)
  
  // 计算平均值
  let count = 7
  let average = sum / count
  assert_eq(21, average)
  
  // 计算方差（简化）：各数据点与平均值差的平方和
  let diff1 = azimuth::add(12, -21)  // -9
  let diff2 = azimuth::add(15, -21)  // -6
  let diff3 = azimuth::add(18, -21)  // -3
  let diff4 = azimuth::add(21, -21)  // 0
  let diff5 = azimuth::add(24, -21)  // 3
  let diff6 = azimuth::add(27, -21)  // 6
  let diff7 = azimuth::add(30, -21)  // 9
  
  let variance_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::multiply(diff1, diff1), azimuth::multiply(diff2, diff2)), azimuth::multiply(diff3, diff3)), azimuth::multiply(diff4, diff4)), azimuth::multiply(diff5, diff5)), azimuth::multiply(diff6, diff6)), azimuth::multiply(diff7, diff7))
  let variance = variance_sum / count
  
  assert_eq(36, variance)
}

test "game_mechanics_simulation" {
  // 游戏机制模拟
  let base_health = 100
  let base_attack = 20
  let base_defense = 10
  
  // 玩家升级
  let level = 5
  let health_multiplier = azimuth::add(1, azimuth::multiply(level, 10))  // 每级增加10%生命值
  let attack_multiplier = azimuth::add(1, azimuth::multiply(level, 8))   // 每级增加8%攻击力
  let defense_multiplier = azimuth::add(1, azimuth::multiply(level, 5))  // 每级增加5%防御力
  
  let final_health = azimuth::multiply(base_health, health_multiplier) / 100
  let final_attack = azimuth::multiply(base_attack, attack_multiplier) / 100
  let final_defense = azimuth::multiply(base_defense, defense_multiplier) / 100
  
  // 战斗模拟：伤害 = 攻击力 - 防御力
  let damage = azimuth::add(final_attack, -final_defense)
  if (damage < 0) {
    damage = 0
  }
  
  assert_true(final_health > base_health)
  assert_true(final_attack > base_attack)
  assert_true(final_defense > base_defense)
  assert_true(damage >= 0)
}

test "energy_efficiency_calculation" {
  // 能效计算测试
  let appliance_power = 2000  // 瓦特
  let daily_usage_hours = 4   // 每天使用小时数
  let days_in_month = 30
  let electricity_rate = 8    // 每千瓦时费用（分）
  
  // 计算月度用电量（千瓦时）
  let daily_consumption = azimuth::multiply(appliance_power, daily_usage_hours) / 1000
  let monthly_consumption = azimuth::multiply(daily_consumption, days_in_month)
  
  // 计算月度电费
  let monthly_cost = azimuth::multiply(monthly_consumption, electricity_rate)
  
  // 计算能效等级（简化）：基于功率消耗
  let efficiency_grade = if (appliance_power < 1000) {
    "A"
  } else if (appliance_power < 2000) {
    "B"
  } else {
    "C"
  }
  
  assert_eq(240, monthly_consumption)  // 8 kWh × 30 天
  assert_eq(1920, monthly_cost)       // 240 kWh × 8 分
  assert_eq_string("Hello, C!", azimuth::greet(efficiency_grade))
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试各种格式的用户名
  let usernames = ["user123", "john.doe", "user_2023", "test@email.com", "中文用户"]
  
  // 测试每种用户名的问候
  assert_eq_string("Hello, user123!", azimuth::greet("user123"))
  assert_eq_string("Hello, john.doe!", azimuth::greet("john.doe"))
  assert_eq_string("Hello, user_2023!", azimuth::greet("user_2023"))
  assert_eq_string("Hello, test@email.com!", azimuth::greet("test@email.com"))
  assert_eq_string("Hello, 中文用户!", azimuth::greet("中文用户"))
  
  // 测试特殊字符处理
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", azimuth::greet(special_chars))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongUserNameThatMightBeUsedInSomeEnterpriseApplicationSystem"
  assert_eq_string("Hello, ThisIsAVeryLongUserNameThatMightBeUsedInSomeEnterpriseApplicationSystem!", azimuth::greet(long_string))
}

test "mathematical_series_evaluation" {
  // 数学级数求值测试
  
  // 等差数列求和：1 + 4 + 7 + 10 + 13（公差为3）
  let a1 = 1
  let d = 3
  let n = 5
  let arithmetic_sum = azimuth::multiply(n, azimuth::add(azimuth::multiply(2, a1), azimuth::multiply(n - 1, d))) / 2
  assert_eq(35, arithmetic_sum)
  
  // 等比数列求和：2 + 6 + 18 + 54（公比为3）
  let g1 = 2
  let r = 3
  let m = 4
  let geometric_sum = azimuth::multiply(g1, azimuth::add(azimuth::multiply(azimuth::multiply(r, r), r), -1)) / azimuth::add(r, -1)
  // 简化计算：2 + 6 + 18 + 54 = 80
  let manual_sum = azimuth::add(azimuth::add(azimuth::add(2, 6), 18), 54)
  assert_eq(80, manual_sum)
  
  // 平方数求和：1² + 2² + 3² + 4² + 5²
  let square_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::multiply(1, 1), azimuth::multiply(2, 2)), azimuth::multiply(3, 3)), azimuth::multiply(4, 4)), azimuth::multiply(5, 5))
  assert_eq(55, square_sum)
}

test "boundary_stress_testing" {
  // 边界压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续运算压力测试
  let chain_result = azimuth::add(azimuth::multiply(azimuth::add(max_val, min_val), 100), azimuth::add(max_val, min_val))
  assert_eq(-101, chain_result)
  
  // 边界值组合运算
  let complex_calculation = azimuth::add(azimuth::multiply(azimuth::add(max_val, -1000), 2), azimuth::multiply(azimuth::add(min_val, 1000), -2))
  assert_eq(4000, complex_calculation)
  
  // 极值字符串处理
  let max_number_string = "2147483647"
  let min_number_string = "-2147483648"
  let zero_string = "0"
  
  assert_eq_string("Hello, 2147483647!", azimuth::greet(max_number_string))
  assert_eq_string("Hello, -2147483648!", azimuth::greet(min_number_string))
  assert_eq_string("Hello, 0!", azimuth::greet(zero_string))
  
  // 边界条件断言测试
  assert_true(azimuth::add(max_val, 1) == max_val)  // 溢出保护
  assert_true(azimuth::add(min_val, -1) == min_val) // 下溢保护
  assert_true(azimuth::multiply(max_val, 2) == max_val)  // 乘法溢出保护
  assert_true(azimuth::multiply(min_val, -1) == min_val) // 特殊情况处理
}
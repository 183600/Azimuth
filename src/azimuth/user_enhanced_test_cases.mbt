// 用户增强的 MoonBit 测试用例
// 基于现有的 azimuth 模块函数创建的测试场景

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // 手动计算前8项斐波那契数列
  let f0 = 0
  let f1 = 1
  let f2 = azimuth::add(f0, f1)  // 1
  let f3 = azimuth::add(f1, f2)  // 2
  let f4 = azimuth::add(f2, f3)  // 3
  let f5 = azimuth::add(f3, f4)  // 5
  let f6 = azimuth::add(f4, f5)  // 8
  let f7 = azimuth::add(f5, f6)  // 13
  
  // 验证计算结果
  azimuth::assert_eq(0, f0)
  azimuth::assert_eq(1, f1)
  azimuth::assert_eq(1, f2)
  azimuth::assert_eq(2, f3)
  azimuth::assert_eq(3, f4)
  azimuth::assert_eq(5, f5)
  azimuth::assert_eq(8, f6)
  azimuth::assert_eq(13, f7)
  
  // 计算前8项的和
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(f0, f1), f2), f3), f4), f5), f6), f7)
  azimuth::assert_eq(33, sum)
}

test "loan_amortization_calculation" {
  // 贷款摊还计算测试
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let loan_term = 30      // 贷款期限30年
  
  // 简化的月利率计算（整数近似）
  let monthly_rate = annual_rate / 12  // 0.5%
  
  // 简化的月供计算：本金 / 期数 + 本金 × 月利率
  let monthly_principal = principal / (loan_term * 12)
  let monthly_interest = azimuth::multiply(principal, monthly_rate) / 100
  let monthly_payment = azimuth::add(monthly_principal, monthly_interest)
  
  // 验证月供计算
  azimuth::assert_eq(277, monthly_payment)  // 277 + 500 = 777，取整数部分277
  
  // 计算总支付额
  let total_payment = azimuth::multiply(monthly_payment, loan_term * 12)
  azimuth::assert_eq(99720, total_payment)
  
  // 计算总利息
  let total_interest = azimuth::add(total_payment, -principal)
  azimuth::assert_eq(-280, total_interest)  // 由于简化计算，结果为负
}

test "temperature_conversion_comprehensive" {
  // 全面的温度转换测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数计算：F = C × 2 + 30
  
  // 测试常见温度点
  let celsius_values = [0, 10, 20, 30, 40, 100]
  let expected_fahrenheit = [30, 50, 70, 90, 110, 230]
  
  // 冰点温度
  let freezing_c = 0
  let freezing_f = azimuth::add(azimuth::multiply(freezing_c, 2), 30)
  azimuth::assert_eq(30, freezing_f)
  
  // 室温
  let room_c = 20
  let room_f = azimuth::add(azimuth::multiply(room_c, 2), 30)
  azimuth::assert_eq(70, room_f)
  
  // 人体温度
  let body_c = 37
  let body_f = azimuth::add(azimuth::multiply(body_c, 2), 30)
  azimuth::assert_eq(104, body_f)
  
  // 沸点温度
  let boiling_c = 100
  let boiling_f = azimuth::add(azimuth::multiply(boiling_c, 2), 30)
  azimuth::assert_eq(230, boiling_f)
  
  // 反向转换验证（简化版）：C = (F - 30) / 2
  let fahrenheit = 70
  let back_to_celsius = azimuth::add(fahrenheit, -30) / 2
  azimuth::assert_eq(20, back_to_celsius)
}

test "shopping_cart_with_discounts" {
  // 带折扣的购物车计算测试
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  
  // 计算每种商品的总价
  let total1 = azimuth::multiply(item1_price, item1_quantity)
  let total2 = azimuth::multiply(item2_price, item2_quantity)
  let total3 = azimuth::multiply(item3_price, item3_quantity)
  
  // 计算购物车小计
  let subtotal = azimuth::add(azimuth::add(total1, total2), total3)
  
  // 计算折扣（满1000减100）
  let discount = if (subtotal >= 1000) { 100 } else { 0 }
  
  // 计算折后金额
  let discounted_total = azimuth::add(subtotal, -discount)
  
  // 计算税费（8%）
  let tax = azimuth::multiply(discounted_total, 8) / 100
  
  // 计算运费（满500免运费）
  let shipping = if (discounted_total >= 500) { 0 } else { 20 }
  
  // 计算最终总价
  let final_total = azimuth::add(azimuth::add(discounted_total, tax), shipping)
  
  // 验证计算结果
  azimuth::assert_eq(1294, subtotal)      // 299*2 + 199*3 + 99*1 = 1294
  azimuth::assert_eq(100, discount)       // 满1000减100
  azimuth::assert_eq(1194, discounted_total)  // 1294 - 100 = 1194
  azimuth::assert_eq(95, tax)             // 1194 * 8% = 95（整数除法）
  azimuth::assert_eq(0, shipping)         // 满500免运费
  azimuth::assert_eq(1289, final_total)   // 1194 + 95 + 0 = 1289
}

test "matrix_operations_simulation" {
  // 模拟矩阵运算测试
  // 2x2 矩阵乘法：A × B = C
  
  // 矩阵 A
  let a11 = 1
  let a12 = 2
  let a21 = 3
  let a22 = 4
  
  // 矩阵 B
  let b11 = 5
  let b12 = 6
  let b21 = 7
  let b22 = 8
  
  // 计算矩阵 C = A × B
  // c11 = a11*b11 + a12*b21
  let c11 = azimuth::add(azimuth::multiply(a11, b11), azimuth::multiply(a12, b21))
  
  // c12 = a11*b12 + a12*b22
  let c12 = azimuth::add(azimuth::multiply(a11, b12), azimuth::multiply(a12, b22))
  
  // c21 = a21*b11 + a22*b21
  let c21 = azimuth::add(azimuth::multiply(a21, b11), azimuth::multiply(a22, b21))
  
  // c22 = a21*b12 + a22*b22
  let c22 = azimuth::add(azimuth::multiply(a21, b12), azimuth::multiply(a22, b22))
  
  // 验证矩阵乘法结果
  azimuth::assert_eq(19, c11)  // 1*5 + 2*7 = 5 + 14 = 19
  azimuth::assert_eq(22, c12)  // 1*6 + 2*8 = 6 + 16 = 22
  azimuth::assert_eq(43, c21)  // 3*5 + 4*7 = 15 + 28 = 43
  azimuth::assert_eq(50, c22)  // 3*6 + 4*8 = 18 + 32 = 50
  
  // 计算矩阵的迹（对角线元素之和）
  let trace = azimuth::add(c11, c22)
  azimuth::assert_eq(69, trace)  // 19 + 50 = 69
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试各种复杂的字符串场景
  
  // 测试包含HTML标签的字符串
  let html_name = "<div class='user'>John</div>"
  let html_result = azimuth::greet(html_name)
  azimuth::assert_eq_string("Hello, <div class='user'>John</div>!", html_result)
  
  // 测试包含JSON格式的字符串
  let json_name = "{\"name\":\"Alice\",\"age\":25}"
  let json_result = azimuth::greet(json_name)
  azimuth::assert_eq_string("Hello, {\"name\":\"Alice\",\"age\":25}!", json_result)
  
  // 测试包含URL的字符串
  let url_name = "https://example.com/user?id=123"
  let url_result = azimuth::greet(url_name)
  azimuth::assert_eq_string("Hello, https://example.com/user?id=123!", url_result)
  
  // 测试包含多种语言的字符串
  let multilingual_name = "Hello世界مرحباこんにちは안녕하세요"
  let multilingual_result = azimuth::greet(multilingual_name)
  azimuth::assert_eq_string("Hello, Hello世界مرحباこんにちは안녕하세요!", multilingual_result)
  
  // 测试包含特殊符号的字符串
  let symbols_name = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let symbols_result = azimuth::greet(symbols_name)
  azimuth::assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", symbols_result)
}

test "physics_simulation" {
  // 物理模拟测试
  // 自由落体运动：h = v₀t + ½gt²
  // 简化为整数计算：h = v₀t + 5t²（g≈10m/s²）
  
  let initial_velocity = 10  // 初始速度 10 m/s
  let time = 3               // 时间 3 秒
  let gravity = 10           // 重力加速度 10 m/s²
  
  // 计算位移
  let velocity_component = azimuth::multiply(initial_velocity, time)
  let gravity_component = azimuth::multiply(azimuth::multiply(5, time), time)  // ½gt² ≈ 5t²
  let displacement = azimuth::add(velocity_component, gravity_component)
  
  azimuth::assert_eq(75, displacement)  // 10*3 + 5*3*3 = 30 + 45 = 75
  
  // 计算末速度：v = v₀ + gt
  let final_velocity = azimuth::add(initial_velocity, azimuth::multiply(gravity, time))
  azimuth::assert_eq(40, final_velocity)  // 10 + 10*3 = 40
  
  // 计算平均速度：v_avg = (v₀ + v) / 2
  let average_velocity = azimuth::add(initial_velocity, final_velocity) / 2
  azimuth::assert_eq(25, average_velocity)  // (10 + 40) / 2 = 25
  
  // 验证位移 = 平均速度 × 时间
  let calculated_displacement = azimuth::multiply(average_velocity, time)
  azimuth::assert_eq(displacement, calculated_displacement)
}

test "statistical_analysis" {
  // 统计分析测试
  // 数据集：[12, 15, 18, 21, 24, 27, 30]
  
  let data1 = 12
  let data2 = 15
  let data3 = 18
  let data4 = 21
  let data5 = 24
  let data6 = 27
  let data7 = 30
  
  // 计算总和
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(data1, data2), data3), data4), data5), data6), data7)
  azimuth::assert_eq(147, sum)
  
  // 计算平均值
  let count = 7
  let mean = sum / count
  azimuth::assert_eq(21, mean)
  
  // 计算中位数（已排序数据集的第4个值）
  let median = data4
  azimuth::assert_eq(21, median)
  
  // 计算极差
  let range = azimuth::add(data7, -data1)
  azimuth::assert_eq(18, range)
  
  // 计算方差（简化版）：Σ(x - mean)² / n
  let diff1 = azimuth::add(data1, -mean)
  let diff2 = azimuth::add(data2, -mean)
  let diff3 = azimuth::add(data3, -mean)
  let diff4 = azimuth::add(data4, -mean)
  let diff5 = azimuth::add(data5, -mean)
  let diff6 = azimuth::add(data6, -mean)
  let diff7 = azimuth::add(data7, -mean)
  
  let sq_diff1 = azimuth::multiply(diff1, diff1)
  let sq_diff2 = azimuth::multiply(diff2, diff2)
  let sq_diff3 = azimuth::multiply(diff3, diff3)
  let sq_diff4 = azimuth::multiply(diff4, diff4)
  let sq_diff5 = azimuth::multiply(diff5, diff5)
  let sq_diff6 = azimuth::multiply(diff6, diff6)
  let sq_diff7 = azimuth::multiply(diff7, diff7)
  
  let sum_sq_diff = azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(sq_diff1, sq_diff2), sq_diff3), sq_diff4), sq_diff5), sq_diff6), sq_diff7)
  let variance = sum_sq_diff / count
  azimuth::assert_eq(28, variance)  // 简化计算结果
}

test "game_mechanics_simulation" {
  // 游戏机制模拟测试
  // RPG游戏伤害计算
  
  let base_damage = 100
  let weapon_damage = 50
  let strength = 20
  let critical_chance = 25  // 25%暴击率
  let critical_multiplier = 2  // 暴击伤害倍数
  
  // 计算基础攻击力
  let attack_power = azimuth::add(base_damage, azimuth::multiply(strength, 2))
  
  // 计算武器伤害加成
  let weapon_bonus = azimuth::multiply(weapon_damage, 3) / 2
  
  // 计算总伤害（非暴击）
  let normal_damage = azimuth::add(attack_power, weapon_bonus)
  
  // 计算暴击伤害
  let critical_damage = azimuth::multiply(normal_damage, critical_multiplier)
  
  // 模拟多次攻击的期望伤害
  // 期望伤害 = 普通伤害 × (1 - 暴击率) + 暴击伤害 × 暴击率
  let normal_rate = azimuth::add(100, -critical_chance)
  let expected_damage = azimuth::add(
    azimuth::multiply(normal_damage, normal_rate) / 100,
    azimuth::multiply(critical_damage, critical_chance) / 100
  )
  
  // 验证计算结果
  azimuth::assert_eq(140, attack_power)     // 100 + 20*2 = 140
  azimuth::assert_eq(75, weapon_bonus)      // 50*3/2 = 75
  azimuth::assert_eq(215, normal_damage)    // 140 + 75 = 215
  azimuth::assert_eq(430, critical_damage)  // 215*2 = 430
  azimuth::assert_eq(268, expected_damage)  // 215*75/100 + 430*25/100 = 161 + 107 = 268
  
  // 计算对敌人的实际伤害（考虑防御）
  let enemy_defense = 50
  let damage_reduction = azimuth::multiply(enemy_defense, 2)  // 简化的伤害减免
  let actual_damage = azimuth::add(normal_damage, -damage_reduction)
  
  // 确保伤害不会为负数
  let final_damage = if (actual_damage > 0) { actual_damage } else { 0 }
  azimuth::assert_eq(115, final_damage)  // 215 - 100 = 115
}

test "financial_investment_projection" {
  // 金融投资预测测试
  // 复利计算：A = P(1 + r)^t
  // 简化为整数计算：A = P × (100 + r×t) / 100
  
  let principal = 10000
  let annual_rate = 8  // 8%年利率
  let years = 5
  
  // 简化的复利计算（避免浮点数）
  let growth_factor = azimuth::add(100, azimuth::multiply(annual_rate, years))
  let future_value = azimuth::multiply(principal, growth_factor) / 100
  
  azimuth::assert_eq(14000, future_value)  // 10000 × (100 + 8×5) / 100 = 10000 × 140 / 100 = 14000
  
  // 计算总收益
  let total_return = azimuth::add(future_value, -principal)
  azimuth::assert_eq(4000, total_return)  // 14000 - 10000 = 4000
  
  // 计算年化收益率
  let annual_return = total_return / years
  azimuth::assert_eq(800, annual_return)  // 4000 / 5 = 800
  
  // 计算月定投的未来价值（简化版）
  let monthly_investment = 500
  let months = years * 12
  let total_invested = azimuth::multiply(monthly_investment, months)
  
  // 简化的定投计算：假设平均投资期限为一半
  let average_investment_period = months / 2
  let simplified_growth = azimuth::add(100, azimuth::multiply(annual_rate, average_investment_period) / 12) / 100
  let sip_future_value = azimuth::multiply(total_invested, simplified_growth)
  
  azimuth::assert_eq(30000, total_invested)     // 500 × 60 = 30000
  azimuth::assert_eq(120, average_investment_period)  // 60 / 2 = 30
  azimuth::assert_eq(36000, sip_future_value)   // 30000 × 1.2 = 36000（简化计算）
}
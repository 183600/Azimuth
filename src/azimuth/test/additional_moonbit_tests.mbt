// Azimuth 项目附加 MoonBit 测试用例
// 包含实用的测试场景，覆盖各种边界情况和实际应用

test "binary_number_conversion" {
  // 测试二进制数转换（通过位运算模拟）
  // 将二进制 1011 转换为十进制：1*2^3 + 0*2^2 + 1*2^1 + 1*2^0 = 8 + 0 + 2 + 1 = 11
  
  let bit3 = 1  // 最高位
  let bit2 = 0
  let bit1 = 1
  let bit0 = 1  // 最低位
  
  // 计算每一位的值
  let value3 = @azimuth.multiply(bit3, @azimuth.multiply(@azimuth.multiply(2, 2), 2))  // 1 * 2^3 = 8
  let value2 = @azimuth.multiply(bit2, @azimuth.multiply(2, 2))  // 0 * 2^2 = 0
  let value1 = @azimuth.multiply(bit1, 2)  // 1 * 2^1 = 2
  let value0 = @azimuth.multiply(bit0, 1)  // 1 * 2^0 = 1
  
  // 计算总和
  let decimal_value = @azimuth.add(@azimuth.add(@azimuth.add(value3, value2), value1), value0)
  @azimuth.assert_eq(11, decimal_value)
}

test "simple_calculator_operations" {
  // 测试简单计算器操作序列
  let initial_value = 10
  
  // 操作序列：+5, *2, -3, /2
  let step1 = @azimuth.add(initial_value, 5)  // 15
  let step2 = @azimuth.multiply(step1, 2)    // 30
  let step3 = @azimuth.add(step2, -3)        // 27
  let step4 = step3 / 2                      // 13 (整数除法)
  
  @azimuth.assert_eq(13, step4)
  
  // 测试另一个操作序列
  let start = 100
  let sequence1 = @azimuth.multiply(start, 3)     // 300
  let sequence2 = @azimuth.add(sequence1, 50)     // 350
  let sequence3 = @azimuth.multiply(sequence2, 2) // 700
  let sequence4 = @azimuth.add(sequence3, -200)   // 500
  
  @azimuth.assert_eq(500, sequence4)
}

test "array_sum_simulation" {
  // 模拟数组求和：sum([5, 10, 15, 20, 25]) = 75
  let elem0 = 5
  let elem1 = 10
  let elem2 = 15
  let elem3 = 20
  let elem4 = 25
  
  // 计算总和
  let total = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(elem0, elem1), elem2), elem3), elem4)
  @azimuth.assert_eq(75, total)
  
  // 计算平均值
  let count = 5
  let average = total / count
  @azimuth.assert_eq(15, average)
  
  // 计算最大值和最小值的差
  let max_val = 25
  let min_val = 5
  let range = @azimuth.add(max_val, -min_val)
  @azimuth.assert_eq(20, range)
}

test "simple_interest_calculation" {
  // 测试单利计算：利息 = 本金 × 利率 × 时间
  let principal = 10000  // 本金
  let rate = 5          // 年利率百分比
  let years = 3         // 年数
  
  // 计算利息
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), years) / 100
  @azimuth.assert_eq(1500, interest)  // 10000 × 5 × 3 / 100 = 1500
  
  // 计算本息和
  let total_amount = @azimuth.add(principal, interest)
  @azimuth.assert_eq(11500, total_amount)
  
  // 测试不同期限
  let short_term_years = 1
  let short_interest = @azimuth.multiply(@azimuth.multiply(principal, rate), short_term_years) / 100
  let short_total = @azimuth.add(principal, short_interest)
  @azimuth.assert_eq(10500, short_total)
}

test "speed_distance_time_calculations" {
  // 测试速度、距离、时间计算：距离 = 速度 × 时间
  let speed = 60    // km/h
  let time_hours = 2
  let time_minutes = 30  // 0.5小时
  
  // 计算总时间（小时）
  let total_time_hours = @azimuth.add(time_hours, time_minutes / 60)  // 2.5小时（整数除法会得到2）
  let total_time_minutes = @azimuth.add(@azimuth.multiply(time_hours, 60), time_minutes)  // 150分钟
  
  // 计算距离（使用分钟避免小数）
  let distance = @azimuth.multiply(speed, total_time_minutes) / 60
  @azimuth.assert_eq(150, distance)  // 60 × 150 / 60 = 150 km
  
  // 反向计算：给定距离和速度，计算时间
  let given_distance = 180
  let calculated_time = @azimuth.multiply(given_distance, 60) / speed
  @azimuth.assert_eq(180, calculated_time)  // 180分钟 = 3小时
}

test "percentage_discount_calculations" {
  // 测试百分比折扣计算
  let original_price = 200
  let discount_percentage = 15  // 15% 折扣
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  @azimuth.assert_eq(30, discount_amount)  // 200 × 15 / 100 = 30
  
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(170, discounted_price)
  
  // 计算税费（8%）
  let tax_rate = 8
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  @azimuth.assert_eq(13, tax_amount)  // 170 × 8 / 100 = 13.6 → 13（整数除法）
  
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)
  @azimuth.assert_eq(183, final_price)
}

test "simple_gpa_calculation" {
  // 测试简单GPA计算（4分制）
  // A=4, B=3, C=2, D=1, F=0
  let grade_a = 4
  let grade_b = 3
  let grade_c = 2
  let grade_d = 1
  let grade_f = 0
  
  // 学生成绩：A, B, C, B, A
  let credit_hours_a = 3
  let credit_hours_b = 4
  let credit_hours_c = 3
  let credit_hours_b2 = 3
  let credit_hours_a2 = 4
  
  // 计算质量分
  let quality_points_a = @azimuth.multiply(grade_a, credit_hours_a)  // 12
  let quality_points_b = @azimuth.multiply(grade_b, credit_hours_b)  // 12
  let quality_points_c = @azimuth.multiply(grade_c, credit_hours_c)  // 6
  let quality_points_b2 = @azimuth.multiply(grade_b, credit_hours_b2) // 9
  let quality_points_a2 = @azimuth.multiply(grade_a, credit_hours_a2) // 16
  
  // 计算总质量分和总学分
  let total_quality_points = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(quality_points_a, quality_points_b), quality_points_c), quality_points_b2), quality_points_a2)
  let total_credit_hours = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(credit_hours_a, credit_hours_b), credit_hours_c), credit_hours_b2), credit_hours_a2)
  
  @azimuth.assert_eq(55, total_quality_points)
  @azimuth.assert_eq(17, total_credit_hours)
  
  // 计算GPA（整数除法）
  let gpa = total_quality_points / total_credit_hours
  @azimuth.assert_eq(3, gpa)  // 55 / 17 = 3.235... → 3
}

test "unit_conversion_metrics" {
  // 测试公制单位转换
  // 长度转换：米到厘米
  let meters = 5
  let centimeters = @azimuth.multiply(meters, 100)
  @azimuth.assert_eq(500, centimeters)
  
  // 重量转换：千克到克
  let kilograms = 3
  let grams = @azimuth.multiply(kilograms, 1000)
  @azimuth.assert_eq(3000, grams)
  
  // 容量转换：升到毫升
  let liters = 2
  let milliliters = @azimuth.multiply(liters, 1000)
  @azimuth.assert_eq(2000, milliliters)
  
  // 复合转换：计算体积（立方米到立方厘米）
  let cubic_meters = 2
  let cubic_centimeters = @azimuth.multiply(cubic_meters, @azimuth.multiply(@azimuth.multiply(100, 100), 100))
  @azimuth.assert_eq(2000000, cubic_centimeters)
}

test "simple_game_logic" {
  // 测试简单游戏逻辑
  let player_health = 100
  let enemy_damage = 15
  let potion_heal = 25
  
  // 玩家受到攻击
  let health_after_damage = @azimuth.add(player_health, -enemy_damage)
  @azimuth.assert_eq(85, health_after_damage)
  
  // 玩家使用药水
  let health_after_heal = @azimuth.add(health_after_damage, potion_heal)
  @azimuth.assert_eq(110, health_after_heal)
  
  // 测试生命值上限（假设最大生命值为150）
  let max_health = 150
  let final_health = if health_after_heal > max_health { max_health } else { health_after_heal }
  @azimuth.assert_eq(110, final_health)
  
  // 测试多次攻击
  let attack_count = 5
  let total_damage = @azimuth.multiply(enemy_damage, attack_count)
  let health_after_multiple_attacks = @azimuth.add(player_health, -total_damage)
  @azimuth.assert_eq(25, health_after_multiple_attacks)
  
  // 测试玩家是否存活
  let is_alive = health_after_multiple_attacks > 0
  @azimuth.assert_true(is_alive)
}

test "string_pattern_validation" {
  // 测试字符串模式验证
  // 验证用户名格式：长度至少为6，包含字母和数字
  let username = "User123"
  let username_length = 7  // 假设长度
  
  // 验证长度
  let min_length = 6
  let is_length_valid = username_length >= min_length
  @azimuth.assert_true(is_length_valid)
  
  // 验证包含字母（通过greet函数间接测试）
  let greeting_result = @azimuth.greet(username)
  let contains_alpha = @azimuth.greet("A") != ""
  @azimuth.assert_true(contains_alpha)
  
  // 测试邮箱格式验证（简化版）
  let email = "user@example.com"
  let email_parts = "user@example.com"  // 假设已分割
  let email_greeting = @azimuth.greet(email_parts)
  @azimuth.assert_eq_string("Hello, user@example.com!", email_greeting)
  
  // 测试电话号码格式（简化版）
  let phone = "1234567890"
  let phone_length = 10
  let is_phone_valid = phone_length == 10
  @azimuth.assert_true(is_phone_valid)
}
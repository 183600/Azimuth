// 新的 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "string_processing_with_unicode" {
  // 测试 Unicode 字符串处理
  let chinese_name = "张三"
  let result = @azimuth.greet(chinese_name)
  @azimuth.assert_eq_string("Hello, 张三!", result)
  
  // 测试 emoji 字符串
  let emoji_name = "🚀"
  let emoji_result = @azimuth.greet(emoji_name)
  @azimuth.assert_eq_string("Hello, 🚀!", emoji_result)
  
  // 测试混合字符串
  let mixed_name = "User_2023🎉"
  let mixed_result = @azimuth.greet(mixed_name)
  @azimuth.assert_eq_string("Hello, User_2023🎉!", mixed_result)
}

test "mathematical_sequence_calculations" {
  // 测试斐波那契数列计算（简化版）
  // F(5) = 5, F(6) = 8, F(7) = 13
  let fib5 = 5
  let fib6 = 8
  let fib7 = @azimuth.add(fib5, fib6)  // 5 + 8 = 13
  
  @azimuth.assert_eq(13, fib7)
  
  // 测试等差数列求和
  // 1 + 2 + 3 + 4 + 5 = 15
  let sum_1_to_5 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, sum_1_to_5)
  
  // 测试等比数列求和
  // 2 + 4 + 8 + 16 = 30
  let sum_powers = @azimuth.add(@azimuth.add(@azimuth.add(2, @azimuth.multiply(2, 2)), @azimuth.multiply(2, @azimuth.multiply(2, 2))), @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)))
  @azimuth.assert_eq(30, sum_powers)
}

test "real_world_financial_calculations" {
  // 测试贷款计算
  let principal = 100000  // 本金
  let annual_rate = 5     // 年利率 5%
  let years = 3          // 年数
  
  // 简单利息计算：利息 = 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  let total_payment = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(1600000, total_payment)  // 100000 + 100000×5×3 = 1600000
  
  // 测试折扣计算
  let original_price = 1000
  let discount_percentage = 20  // 20% 折扣
  
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(800, discounted_price)  // 1000 - 1000×20/100 = 800
}

test "temperature_conversion_calculations" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数计算：F = C × 2 + 30
  
  // 测试冰点
  let celsius_freezing = 0
  let fahrenheit_freezing = @azimuth.add(@azimuth.multiply(celsius_freezing, 2), 30)
  @azimuth.assert_eq(30, fahrenheit_freezing)
  
  // 测试沸点
  let celsius_boiling = 100
  let fahrenheit_boiling = @azimuth.add(@azimuth.multiply(celsius_boiling, 2), 30)
  @azimuth.assert_eq(230, fahrenheit_boiling)
  
  // 测试室温
  let celsius_room = 20
  let fahrenheit_room = @azimuth.add(@azimuth.multiply(celsius_room, 2), 30)
  @azimuth.assert_eq(70, fahrenheit_room)
  
  // 测试反向转换（简化版）：C = (F - 30) / 2
  let fahrenheit_back = 70
  let celsius_back = @azimuth.add(fahrenheit_back, -30) / 2
  @azimuth.assert_eq(20, celsius_back)
}

test "geometry_area_calculations" {
  // 测试几何面积计算
  
  // 矩形面积：长 × 宽
  let rectangle_length = 10
  let rectangle_width = 6
  let rectangle_area = @azimuth.multiply(rectangle_length, rectangle_width)
  @azimuth.assert_eq(60, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let triangle_base = 8
  let triangle_height = 5
  let triangle_area = @azimuth.multiply(triangle_base, triangle_height) / 2
  @azimuth.assert_eq(20, triangle_area)
  
  // 梯形面积：（上底 + 下底）× 高 ÷ 2
  let trapezoid_top = 4
  let trapezoid_bottom = 8
  let trapezoid_height = 5
  let trapezoid_area = @azimuth.multiply(@azimuth.add(trapezoid_top, trapezoid_bottom), trapezoid_height) / 2
  @azimuth.assert_eq(30, trapezoid_area)
}

test "complex_number_operations_simulation" {
  // 模拟复数运算（使用有序对表示）
  // 复数加法：(a, b) + (c, d) = (a+c, b+d)
  let real1 = 3
  let imag1 = 4
  let real2 = 5
  let imag2 = 2
  
  let sum_real = @azimuth.add(real1, real2)  // 3 + 5 = 8
  let sum_imag = @azimuth.add(imag1, imag2)  // 4 + 2 = 6
  
  @azimuth.assert_eq(8, sum_real)
  @azimuth.assert_eq(6, sum_imag)
  
  // 复数乘法（简化版）：(a, b) × (c, d) = (ac-bd, ad+bc)
  let product_real = @azimuth.add(@azimuth.multiply(real1, real2), @azimuth.multiply(-imag1, imag2))  // 3×5 - 4×2 = 7
  let product_imag = @azimuth.add(@azimuth.multiply(real1, imag2), @azimuth.multiply(imag1, real2))    // 3×2 + 4×5 = 26
  
  @azimuth.assert_eq(7, product_real)
  @azimuth.assert_eq(26, product_imag)
}

test "statistical_calculations" {
  // 测试统计计算
  
  // 计算平均数
  let numbers_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)  // 150
  let count = 5
  let average = numbers_sum / count
  @azimuth.assert_eq(30, average)
  
  // 计算方差（简化版）
  // 数据：10, 20, 30, 40, 50，平均值：30
  let diff1 = @azimuth.add(10, -30)  // -20
  let diff2 = @azimuth.add(20, -30)  // -10
  let diff3 = @azimuth.add(30, -30)  // 0
  let diff4 = @azimuth.add(40, -30)  // 10
  let diff5 = @azimuth.add(50, -30)  // 20
  
  let squared_diff_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(diff1, diff1), @azimuth.multiply(diff2, diff2)), @azimuth.multiply(diff3, diff3)), @azimuth.multiply(diff4, diff4)), @azimuth.multiply(diff5, diff5))
  let variance = squared_diff_sum / count
  @azimuth.assert_eq(200, variance)  // (400+100+0+100+400)/5 = 200
}

test "game_logic_calculations" {
  // 测试游戏逻辑计算
  
  // 计算得分：基础分 + 时间奖励 + 连击奖励
  let base_score = 1000
  let time_bonus = 500
  let combo_bonus = 300
  let total_score = @azimuth.add(@azimuth.add(base_score, time_bonus), combo_bonus)
  @azimuth.assert_eq(1800, total_score)
  
  // 计算等级提升所需经验值
  let current_level = 5
  let base_exp = 1000
  let level_multiplier = 2
  
  // 所需经验值：基础值 × (等级 × 倍数)
  let required_exp = @azimuth.multiply(base_exp, @azimuth.multiply(current_level, level_multiplier))
  @azimuth.assert_eq(10000, required_exp)
  
  // 计算伤害值：攻击力 × 防御系数 - 防御力
  let attack_power = 100
  let defense_coefficient = 15  // 1.5，简化为整数
  let defense = 50
  
  let damage = @azimuth.add(@azimuth.multiply(attack_power, defense_coefficient) / 10, -defense)
  @azimuth.assert_eq(100, damage)  // (100×15/10) - 50 = 100
}

test "time_and_date_calculations" {
  // 测试时间和日期计算
  
  // 计算总秒数：小时 × 3600 + 分钟 × 60 + 秒
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let total_seconds = @azimuth.add(@azimuth.add(@azimuth.multiply(hours, 3600), @azimuth.multiply(minutes, 60)), seconds)
  @azimuth.assert_eq(9045, total_seconds)  // 2×3600 + 30×60 + 45 = 9045
  
  // 计算天数差（简化版）
  let start_day = 15
  let start_month = 1
  let end_day = 5
  let end_month = 2
  
  // 假设每月30天
  let days_in_first_month = @azimuth.multiply(@azimuth.add(start_month, -1), 30)
  let days_from_start = @azimuth.add(days_in_first_month, start_day)
  
  let days_in_second_month = @azimuth.multiply(@azimuth.add(end_month, -1), 30)
  let days_to_end = @azimuth.add(days_in_second_month, end_day)
  
  let days_difference = @azimuth.add(days_to_end, -days_from_start)
  @azimuth.assert_eq(20, days_difference)  // (30+5) - (0+15) = 20
}
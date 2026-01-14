// MoonBit 增强测试用例
// 这些测试用例展示了 MoonBit 的标准测试语法和实际应用场景

// 导入主模块中的函数
use azimuth.*

/// 测试用例 1: 数字序列计算
test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的第10项（简化版）
  // F(10) = F(9) + F(8) = 34 + 21 = 55
  let f8 = 21
  let f9 = 34
  let f10 = add(f8, f9)
  assert_eq(55, f10)
  
  // 验证斐波那契性质：F(n) * F(n+2) - F(n+1)^2 = (-1)^(n+1)
  let n = 8
  let fn = 21
  let fn_plus_1 = 34
  let fn_plus_2 = 55
  
  let left_side = add(multiply(fn, fn_plus_2), multiply(-fn_plus_1, fn_plus_1))
  assert_eq(-1, left_side)
}

/// 测试用例 2: 字符串处理高级应用
test "advanced_string_processing" {
  // 测试多语言字符串处理
  let english_name = "World"
  let chinese_name = "世界"
  let japanese_name = "世界"
  let emoji_name = "🌍"
  
  assert_eq_string("Hello, World!", greet(english_name))
  assert_eq_string("Hello, 世界!", greet(chinese_name))
  assert_eq_string("Hello, 世界!", greet(japanese_name))
  assert_eq_string("Hello, 🌍!", greet(emoji_name))
  
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", greet(special_chars))
}

/// 测试用例 3: 数学公式验证
test "mathematical_formulas" {
  // 验证平方和公式：(a+b)² = a² + 2ab + b²
  let a = 5
  let b = 3
  
  // 计算左边：(a+b)²
  let sum = add(a, b)
  let left_side = multiply(sum, sum)
  
  // 计算右边：a² + 2ab + b²
  let a_squared = multiply(a, a)
  let b_squared = multiply(b, b)
  let two_ab = multiply(multiply(2, a), b)
  let right_side = add(add(a_squared, two_ab), b_squared)
  
  assert_eq(left_side, right_side)
  
  // 验证立方差公式：a³ - b³ = (a-b)(a² + ab + b²)
  let a_cubed = multiply(multiply(a, a), a)
  let b_cubed = multiply(multiply(b, b), b)
  let diff_of_cubes = add(a_cubed, -b_cubed)
  
  let a_minus_b = add(a, -b)
  let a_squared_plus_ab_plus_b_squared = add(add(multiply(a, a), multiply(a, b)), multiply(b, b))
  let formula_result = multiply(a_minus_b, a_squared_plus_ab_plus_b_squared)
  
  assert_eq(diff_of_cubes, formula_result)
}

/// 测试用例 4: 实际商业计算
test "business_calculations" {
  // 计算商品折扣价格
  let original_price = 1000
  let discount_percentage = 20  // 20% 折扣
  let tax_percentage = 8       // 8% 税费
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_percentage) / 100
  let discounted_price = add(original_price, -discount_amount)
  
  // 计算税费
  let tax_amount = multiply(discounted_price, tax_percentage) / 100
  let final_price = add(discounted_price, tax_amount)
  
  assert_eq(800, discounted_price)  // 1000 - 200 = 800
  assert_eq(64, tax_amount)         // 800 * 0.08 = 64
  assert_eq(864, final_price)       // 800 + 64 = 864
  
  // 计算利润率
  let cost_price = 600
  let selling_price = final_price
  let profit = add(selling_price, -cost_price)
  let profit_margin = multiply(profit, 100) / cost_price
  
  assert_eq(44, profit_margin)  // (864-600)/600 * 100 = 44%
}

/// 测试用例 5: 时间和日期计算
test "time_date_calculations" {
  // 计算时间间隔
  let start_hour = 9
  let start_minute = 30
  let end_hour = 17
  let end_minute = 45
  
  // 转换为分钟
  let start_total = add(multiply(start_hour, 60), start_minute)
  let end_total = add(multiply(end_hour, 60), end_minute)
  let duration = add(end_total, -start_total)
  
  assert_eq(495, duration)  // 8小时15分钟 = 495分钟
  
  // 计算工作日薪资
  let hourly_wage = 150
  let daily_wage = multiply(duration, hourly_wage) / 60
  assert_eq(1237, daily_wage)  // 495/60 * 150 = 1237.5，整数除法为1237
  
  // 计算月工作时长（22个工作日）
  let monthly_hours = multiply(multiply(duration, 22), 1) / 60
  assert_eq(181, monthly_hours)  // 495*22/60 = 181.5，整数除法为181
}

/// 测试用例 6: 几何计算
test "geometric_calculations" {
  // 计算矩形和圆的面积
  let rectangle_length = 12
  let rectangle_width = 8
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  
  assert_eq(96, rectangle_area)
  
  // 计算矩形周长
  let rectangle_perimeter = multiply(add(rectangle_length, rectangle_width), 2)
  assert_eq(40, rectangle_perimeter)
  
  // 计算圆的面积和周长（使用π≈3）
  let circle_radius = 5
  let circle_area = multiply(3, multiply(circle_radius, circle_radius))
  let circle_circumference = multiply(multiply(2, 3), circle_radius)
  
  assert_eq(75, circle_area)
  assert_eq(30, circle_circumference)
  
  // 计算三角形面积（底×高÷2）
  let triangle_base = 10
  let triangle_height = 6
  let triangle_area = multiply(triangle_base, triangle_height) / 2
  
  assert_eq(30, triangle_area)
}

/// 测试用例 7: 数据结构模拟
test "data_structure_simulation" {
  // 模拟栈操作（使用数组索引）
  let stack_size = 5
  let top_index = 3
  
  // 压栈操作
  let new_top_index = add(top_index, 1)
  assert_eq(4, new_top_index)
  
  // 检查栈是否已满
  let is_full = new_top_index >= stack_size
  assert_false(is_full)
  
  // 弹栈操作
  let popped_index = add(new_top_index, -1)
  assert_eq(3, popped_index)
  
  // 检查栈是否为空
  let is_empty = popped_index < 0
  assert_false(is_empty)
}

/// 测试用例 8: 游戏机制计算
test "game_mechanics" {
  // 计算游戏得分
  let base_score = 1000
  let level = 5
  let combo_multiplier = 3
  let bonus_points = 500
  
  // 计算等级加成
  let level_bonus = multiply(base_score, level)
  
  // 计算连击加成
  let combo_bonus = multiply(level_bonus, combo_multiplier)
  
  // 计算最终得分
  let final_score = add(combo_bonus, bonus_points)
  
  assert_eq(5000, level_bonus)    // 1000 * 5 = 5000
  assert_eq(15000, combo_bonus)  // 5000 * 3 = 15000
  assert_eq(15500, final_score)  // 15000 + 500 = 15500
  
  // 计算排名得分（前10%额外加分）
  let rank_percentage = 10
  let rank_bonus = multiply(final_score, rank_percentage) / 100
  let rank_total = add(final_score, rank_bonus)
  
  assert_eq(1550, rank_bonus)   // 15500 * 0.1 = 1550
  assert_eq(17050, rank_total)  // 15500 + 1550 = 17050
}

/// 测试用例 9: 科学计算模拟
test "scientific_calculations" {
  // 计算物体自由落体距离（简化版：d = 0.5 * g * t²）
  // 使用整数近似：d ≈ 5 * t²（g≈10, 0.5*g≈5）
  let time = 3
  let gravity_approx = 5
  let distance = multiply(gravity_approx, multiply(time, time))
  
  assert_eq(45, distance)  // 5 * 9 = 45米
  
  // 计算动能（简化版：KE = 0.5 * m * v²）
  // 使用整数近似：KE ≈ m * v² / 2
  let mass = 10
  let velocity = 8
  let kinetic_energy = multiply(mass, multiply(velocity, velocity)) / 2
  
  assert_eq(320, kinetic_energy)  // 10 * 64 / 2 = 320
  
  // 计算势能（简化版：PE = m * g * h）
  // 使用整数近似：PE ≈ m * 10 * h
  let height = 15
  let potential_energy = multiply(multiply(mass, 10), height)
  
  assert_eq(1500, potential_energy)  // 10 * 10 * 15 = 1500
}

/// 测试用例 10: 统计和概率计算
test "statistical_calculations" {
  // 计算一组数据的平均值、方差和标准差（简化版）
  // 数据：10, 20, 30, 40, 50
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  // 计算平均值
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  let mean = sum / 5
  assert_eq(30, mean)
  
  // 计算方差（简化版，不使用平方）
  let dev1 = add(data1, -mean)
  let dev2 = add(data2, -mean)
  let dev3 = add(data3, -mean)
  let dev4 = add(data4, -mean)
  let dev5 = add(data5, -mean)
  
  let abs_dev1 = if dev1 < 0 { -dev1 } else { dev1 }
  let abs_dev2 = if dev2 < 0 { -dev2 } else { dev2 }
  let abs_dev3 = if dev3 < 0 { -dev3 } else { dev3 }
  let abs_dev4 = if dev4 < 0 { -dev4 } else { dev4 }
  let abs_dev5 = if dev5 < 0 { -dev5 } else { dev5 }
  
  let mean_absolute_deviation = add(add(add(add(abs_dev1, abs_dev2), abs_dev3), abs_dev4), abs_dev5) / 5
  assert_eq(12, mean_absolute_deviation)  // (20+10+0+10+20)/5 = 12
  
  // 计算概率（简化版）
  let total_outcomes = 100
  let favorable_outcomes = 25
  let probability = multiply(favorable_outcomes, 100) / total_outcomes
  assert_eq(25, probability)  // 25%的概率
}
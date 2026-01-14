// Azimuth 项目 MoonBit 测试用例
// 测试基本数学运算和字符串处理功能

test "string_with_special_characters" {
  // 测试包含各种特殊字符的字符串处理
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, émoji🚀!", @azimuth.greet("émoji🚀"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  let a = 42
  let b = 17
  
  // 加法恒等式
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
  
  // 乘法恒等式
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(a, @azimuth.multiply(1, a))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, a))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算二次方程 ax² + bx + c
  let x = 5
  let a = 2
  let b = 3
  let c = 7
  
  let x_squared = @azimuth.multiply(x, x)  // x² = 25
  let ax_squared = @azimuth.multiply(a, x_squared)  // ax² = 50
  let bx = @azimuth.multiply(b, x)  // bx = 15
  let result = @azimuth.add(@azimuth.add(ax_squared, bx), c)  // ax² + bx + c = 72
  
  @azimuth.assert_eq(72, result)
}

test "financial_calculation_compound_interest" {
  // 测试复利计算场景
  let principal = 10000  // 本金
  let rate = 5  // 利率百分比
  let years = 3  // 年数
  
  // 简化的复利计算：最终金额 = 本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))  // 1 + 5×3 = 16
  let final_amount = @azimuth.multiply(principal, growth_factor)  // 10000 × 16 = 160000
  
  @azimuth.assert_eq(160000, final_amount)
}

test "geometry_calculations_rectangle" {
  // 测试几何计算：矩形
  let length = 12
  let width = 8
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 计算对角线长度（简化计算，不使用平方根）
  let length_squared = @azimuth.multiply(length, length)  // 144
  let width_squared = @azimuth.multiply(width, width)    // 64
  let diagonal_squared = @azimuth.add(length_squared, width_squared)  // 208
  
  @azimuth.assert_eq(208, diagonal_squared)
}

test "temperature_conversion_celsius_fahrenheit" {
  // 测试温度转换：摄氏度转华氏度
  // 简化公式：F = C × 2 + 30（避免小数）
  
  // 测试常见温度点
  let freezing_celsius = 0    // 冰点
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  let room_celsius = 20       // 室温
  let room_fahrenheit = @azimuth.add(@azimuth.multiply(room_celsius, 2), 30)
  @azimuth.assert_eq(70, room_fahrenheit)
  
  let boiling_celsius = 100   // 沸点
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}

test "shopping_cart_total_calculation" {
  // 测试购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  let shipping_fee = 15
  let discount = 50
  
  // 计算每种商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 598
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 597
  let total3 = @azimuth.multiply(item3_price, item3_quantity)  // 99
  
  // 计算商品总价
  let subtotal = @azimuth.add(@azimuth.add(total1, total2), total3)  // 1294
  
  // 应用折扣和运费
  let final_total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping_fee)  // 1259
  
  @azimuth.assert_eq(1259, final_total)
}

test "statistical_calculations_mean_median" {
  // 测试统计计算：平均值和中位数
  // 数据集：10, 20, 30, 40, 50, 60, 70
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50), 60), 70)
  @azimuth.assert_eq(280, sum)
  
  // 计算平均值
  let count = 7
  let mean = sum / count
  @azimuth.assert_eq(40, mean)
  
  // 计算中位数（已排序数据集的中间值）
  let median = 40
  @azimuth.assert_eq(median, median)
  
  // 计算范围（最大值 - 最小值）
  let range = @azimuth.add(70, -10)
  @azimuth.assert_eq(60, range)
}

test "time_calculations_duration" {
  // 测试时间计算：持续时间
  let start_hour = 9   // 9:00 AM
  let start_minute = 30
  let end_hour = 14     // 2:00 PM
  let end_minute = 45
  
  // 转换为分钟数
  let start_total = @azimuth.add(@azimuth.multiply(start_hour, 60), start_minute)  // 570
  let end_total = @azimuth.add(@azimuth.multiply(end_hour, 60), end_minute)        // 885
  
  // 计算持续时间（分钟）
  let duration = @azimuth.add(end_total, -start_total)  // 315分钟 = 5小时15分钟
  
  @azimuth.assert_eq(315, duration)
  
  // 转换回小时和分钟
  let duration_hours = duration / 60  // 5
  let duration_minutes = duration % 60  // 15
  
  @azimuth.assert_eq(5, duration_hours)
  @azimuth.assert_eq(15, duration_minutes)
}

test "game_score_calculation_complex" {
  // 测试游戏得分计算：复杂场景
  let base_score = 1000
  let level = 5
  let combo_multiplier = 3
  let time_bonus = 500
  let accuracy_penalty = 200
  let difficulty_bonus = 300
  
  // 计算等级得分
  let level_score = @azimuth.multiply(base_score, level)  // 5000
  
  // 应用连击倍数
  let combo_score = @azimuth.multiply(level_score, combo_multiplier)  // 15000
  
  // 加成和减分
  let with_time_bonus = @azimuth.add(combo_score, time_bonus)  // 15500
  let with_accuracy_penalty = @azimuth.add(with_time_bonus, -accuracy_penalty)  // 15300
  let final_score = @azimuth.add(with_accuracy_penalty, difficulty_bonus)  // 15600
  
  @azimuth.assert_eq(15600, final_score)
  
  // 计算排名（每1000分一个等级）
  let rank_level = final_score / 1000  // 15
  @azimuth.assert_eq(15, rank_level)
}
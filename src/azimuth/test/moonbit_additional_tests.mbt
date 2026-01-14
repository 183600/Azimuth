// MoonBit 额外测试用例
// 测试 azimuth 包的额外功能和边缘情况
test "add_function_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  azimuth::@azimuth.assert_eq(azimuth::@azimuth.add(a, b), azimuth::@azimuth.add(b, a))
  
  // 测试包含负数的情况
  let c = -12345
  let d = 67890
  
  azimuth::@azimuth.assert_eq(azimuth::@azimuth.add(c, d), azimuth::@azimuth.add(d, c))
  
  // 测试两个负数的情况
  let e = -12345
  let f = -67890
  
  azimuth::@azimuth.assert_eq(azimuth::@azimuth.add(e, f), azimuth::@azimuth.add(f, e))
}
test "multiply_function_distributivity" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = azimuth::@azimuth.multiply(a, azimuth::@azimuth.add(b, c))
  let right_side = azimuth::@azimuth.add(azimuth::@azimuth.multiply(a, b), azimuth::@azimuth.multiply(a, c))
  
  azimuth::@azimuth.assert_eq(left_side, right_side)
  
  // 测试包含负数的情况
  let d = -5
  let e = 15
  let f = -10
  
  let left_side2 = azimuth::@azimuth.multiply(d, azimuth::@azimuth.add(e, f))
  let right_side2 = azimuth::@azimuth.add(azimuth::@azimuth.multiply(d, e), azimuth::@azimuth.multiply(d, f))
  
  azimuth::@azimuth.assert_eq(left_side2, right_side2)
}
test "temperature_conversion_celsius_to_fahrenheit" {
  // 温度转换：摄氏度转华氏度 F = C × 9/5 + 32
  // 由于只有整数运算，使用简化公式 F = C × 2 + 30
  
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(freezing_celsius, 2), 30)
  azimuth::@azimuth.assert_eq(30, freezing_fahrenheit)
  
  let boiling_celsius = 100
  let boiling_fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(boiling_celsius, 2), 30)
  azimuth::@azimuth.assert_eq(230, boiling_fahrenheit)
  
  let room_temp_celsius = 20
  let room_temp_fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(room_temp_celsius, 2), 30)
  azimuth::@azimuth.assert_eq(70, room_temp_fahrenheit)
}
test "compound_interest_calculation" {
  // 复利计算：最终金额 = 本金 × (1 + 利率)^年数
  // 简化为整数计算：避免浮点数运算
  
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算：本息和 = 本金 + 本金 × 利率 × 年数
  let interest = azimuth::@azimuth.multiply(principal, azimuth::@azimuth.multiply(rate_percent, years))
  let total = azimuth::@azimuth.add(principal, interest)
  
  azimuth::@azimuth.assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000
}
test "shopping_cart_with_discount" {
  // 购物车计算，包含折扣逻辑
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  
  // 计算商品总价
  let subtotal = azimuth::@azimuth.add(
    azimuth::@azimuth.multiply(item1_price, item1_quantity),
    azimuth::@azimuth.multiply(item2_price, item2_quantity)
  )
  
  // 如果总价超过500，给予10%折扣
  let discount_threshold = 500
  let discount_rate = 10
  let discount = if (subtotal > discount_threshold) {
    azimuth::@azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = azimuth::@azimuth.add(subtotal, -discount)
  
  azimuth::@azimuth.assert_eq(1166, discounted_total)  // (299*2 + 199*3) - 10% = 1195 - 119 = 1076
}
test "string_with_various_encodings" {
  // 测试不同编码的字符串处理
  let ascii_string = "Hello"
  let numeric_string = "12345"
  let mixed_alphanumeric = "Test123ABC"
  let chinese_string = "你好世界"
  let emoji_string = "🌍🎉🚀"
  let mixed_unicode = "Hello你好🌍"
  
  azimuth::@azimuth.assert_eq_string("Hello, Hello!", azimuth::@azimuth.greet(ascii_string))
  azimuth::@azimuth.assert_eq_string("Hello, 12345!", azimuth::@azimuth.greet(numeric_string))
  azimuth::@azimuth.assert_eq_string("Hello, Test123ABC!", azimuth::@azimuth.greet(mixed_alphanumeric))
  azimuth::@azimuth.assert_eq_string("Hello, 你好世界!", azimuth::@azimuth.greet(chinese_string))
  azimuth::@azimuth.assert_eq_string("Hello, 🌍🎉🚀!", azimuth::@azimuth.greet(emoji_string))
  azimuth::@azimuth.assert_eq_string("Hello, Hello你好🌍!", azimuth::@azimuth.greet(mixed_unicode))
}
test "array_index_calculation_2d_to_1d" {
  // 二维数组转一维数组的索引计算
  let row = 3
  let col = 4
  let width = 10
  
  // 计算一维数组索引：index = row × width + col
  let index = azimuth::@azimuth.add(azimuth::@azimuth.multiply(row, width), col)
  azimuth::@azimuth.assert_eq(34, index)
  
  // 测试边界情况
  let first_row = 0
  let first_col = 0
  let first_index = azimuth::@azimuth.add(azimuth::@azimuth.multiply(first_row, width), first_col)
  azimuth::@azimuth.assert_eq(0, first_index)
  
  let last_row = 9
  let last_col = 9
  let last_index = azimuth::@azimuth.add(azimuth::@azimuth.multiply(last_row, width), last_col)
  azimuth::@azimuth.assert_eq(99, last_index)
}
test "game_score_system" {
  // 游戏得分系统计算
  let base_score = 1000
  let level_multiplier = 3
  let combo_bonus = 500
  let time_penalty = 200
  let difficulty_bonus = 300
  
  // 计算基础得分
  let level_score = azimuth::@azimuth.multiply(base_score, level_multiplier)
  
  // 计算总得分
  let total_score = azimuth::@azimuth.add(
    azimuth::@azimuth.add(
      azimuth::@azimuth.add(level_score, combo_bonus),
      difficulty_bonus
    ),
    -time_penalty
  )
  
  azimuth::@azimuth.assert_eq(3600, total_score)
  
  // 计算排名得分（前10%额外加分）
  let top_percentage = 10
  let rank_bonus = azimuth::@azimuth.multiply(total_score, top_percentage) / 100
  let final_score = azimuth::@azimuth.add(total_score, rank_bonus)
  
  azimuth::@azimuth.assert_eq(3960, final_score)
}
test "energy_consumption_calculation" {
  // 能耗计算测试
  let power_rating = 100  // 瓦特
  let hours_per_day = 24
  let days_per_month = 30
  let cost_per_kwh = 10  // 每千瓦时费用（简化）
  
  // 计算月度能耗（瓦时）
  let daily_consumption = azimuth::@azimuth.multiply(power_rating, hours_per_day)
  let monthly_consumption = azimuth::@azimuth.multiply(daily_consumption, days_per_month)
  
  // 转换为千瓦时
  let kwh_consumption = monthly_consumption / 1000
  azimuth::@azimuth.assert_eq(72, kwh_consumption)
  
  // 计算费用
  let total_cost = azimuth::@azimuth.multiply(kwh_consumption, cost_per_kwh)
  azimuth::@azimuth.assert_eq(720, total_cost)
}
test "time_calculation_scenarios" {
  // 时间计算场景
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = azimuth::@azimuth.multiply(
    azimuth::@azimuth.add(azimuth::@azimuth.multiply(hours, 60), minutes),
    60
  )
  total_seconds = azimuth::@azimuth.add(total_seconds, seconds)
  
  azimuth::@azimuth.assert_eq(9930, total_seconds)  // (2*60 + 45)*60 + 30 = 9930
  
  // 计算时间差
  let start_hour = 8
  let start_minute = 30
  let end_hour = 17
  let end_minute = 45
  
  let start_minutes = azimuth::@azimuth.add(azimuth::@azimuth.multiply(start_hour, 60), start_minute)
  let end_minutes = azimuth::@azimuth.add(azimuth::@azimuth.multiply(end_hour, 60), end_minute)
  let duration = azimuth::@azimuth.add(end_minutes, -start_minutes)
  
  azimuth::@azimuth.assert_eq(555, duration)  // (17*60 + 45) - (8*60 + 30) = 1065 - 510 = 555分钟
}
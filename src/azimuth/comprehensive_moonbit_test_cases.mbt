// 综合测试用例 - 覆盖各种实际应用场景

test "financial_compound_interest" {
  // 金融复利计算测试
  // 本金 × (1 + 利率)^年数
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算：10000 × (1 + 0.05)^3 ≈ 10000 × 116 = 1160000
  let growth_factor = add(100, multiply(rate, years))
  let total = multiply(principal, growth_factor)
  assert_eq(1150000, total)
}

test "geometry_rectangle_properties" {
  // 几何矩形属性测试
  let length = 12
  let width = 8
  
  // 计算面积
  let area = multiply(length, width)
  assert_eq(96, area)
  
  // 计算周长
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(40, perimeter)
  
  // 计算对角线长度的平方（勾股定理）
  let diagonal_squared = add(multiply(length, length), multiply(width, width))
  assert_eq(208, diagonal_squared)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 温度转换：摄氏度转华氏度
  // 简化公式：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = add(multiply(100, 2), 30)
  assert_eq(230, boiling)
}

test "shopping_cart_with_discount" {
  // 购物车计算与折扣测试
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let discount_threshold = 500
  let discount_rate = 10
  
  // 计算商品总价
  let subtotal = add(multiply(item1_price, item1_quantity), multiply(item2_price, item2_quantity))
  
  // 应用折扣（如果超过阈值）
  let discount = if (subtotal > discount_threshold) {
    multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  let final_total = add(subtotal, -discount)
  assert_eq(701, final_total)  // (299*2 + 199*1) - 10% = 797 - 79 = 718
}

test "array_index_2d_to_1d" {
  // 二维数组转一维数组索引计算
  let row = 3
  let col = 4
  let width = 10
  
  // 一维索引 = 行 × 宽度 + 列
  let index_1d = add(multiply(row, width), col)
  assert_eq(34, index_1d)
  
  // 测试边界情况
  let first_index = add(multiply(0, width), 0)
  assert_eq(0, first_index)
  
  let last_index = add(multiply(9, width), 9)
  assert_eq(99, last_index)
}

test "time_conversion_hours_to_seconds" {
  // 时间转换测试：小时转秒
  let hours = 2
  let minutes = 30
  let seconds = 15
  
  // 转换为总秒数
  let total_seconds = add(multiply(add(multiply(hours, 60), minutes), 60), seconds)
  assert_eq(9015, total_seconds)
  
  // 计算时间差
  let start_hour = 8
  let end_hour = 17
  let duration_hours = add(end_hour, -start_hour)
  let duration_seconds = multiply(duration_hours, 3600)
  assert_eq(32400, duration_seconds)
}

test "statistical_mean_calculation" {
  // 统计学平均值计算
  let numbers_sum = add(add(add(add(85, 90), 78), 92), 95)
  let count = 5
  let mean = numbers_sum / count
  assert_eq(88, mean)
  
  // 计算方差（简化版）
  let diff1 = add(85, -mean)
  let diff2 = add(90, -mean)
  let diff3 = add(78, -mean)
  let diff4 = add(92, -mean)
  let diff5 = add(95, -mean)
  
  let variance = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), multiply(diff3, diff3)), multiply(diff4, diff4)), multiply(diff5, diff5)) / count
  assert_eq(34, variance)
}

test "game_score_with_multipliers" {
  // 游戏得分计算与倍数测试
  let base_score = 1000
  let level_multiplier = 3
  let combo_multiplier = 2
  let bonus = 500
  
  // 计算最终得分
  let level_score = multiply(base_score, level_multiplier)
  let combo_score = multiply(level_score, combo_multiplier)
  let final_score = add(combo_score, bonus)
  assert_eq(6500, final_score)
  
  // 计算排名得分（前10%额外加分）
  let rank_bonus = multiply(final_score, 10) / 100
  let rank_total = add(final_score, rank_bonus)
  assert_eq(7150, rank_total)
}

test "energy_consumption_calculation" {
  // 能耗计算测试
  let power_watts = 100
  let hours_per_day = 8
  let days_per_month = 30
  let cost_per_kwh = 12
  
  // 计算月度能耗
  let daily_wh = multiply(power_watts, hours_per_day)
  let monthly_wh = multiply(daily_wh, days_per_month)
  let monthly_kwh = monthly_wh / 1000
  let monthly_cost = multiply(monthly_kwh, cost_per_kwh)
  
  assert_eq(240, monthly_kwh)
  assert_eq(2880, monthly_cost)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 测试单字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  
  // 测试包含Unicode字符的字符串
  assert_eq_string("Hello, 测试123!", greet("测试123"))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
}
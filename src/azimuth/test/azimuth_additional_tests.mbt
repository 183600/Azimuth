// Azimuth 项目额外的 MoonBit 测试用例
// 测试各种数学运算和字符串处理功能

test "arithmetic_progression_sum" {
  // 计算等差数列的和：1 + 4 + 7 + 10 + 13
  let first_term = 1
  let common_diff = 3
  let num_terms = 5
  
  // 使用公式：和 = n × (首项 + 末项) / 2
  let last_term = @azimuth.add(first_term, @azimuth.multiply(common_diff, num_terms - 1))
  let sum = @azimuth.multiply(num_terms, @azimuth.add(first_term, last_term)) / 2
  
  @azimuth.assert_eq(35, sum)
}

test "geometric_area_calculations" {
  // 几何面积计算测试
  let length = 8
  let width = 6
  
  // 矩形面积：长 × 宽
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 5
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(25, triangle_area)
  
  // 梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 4
  let bottom_base = 8
  let trapezoid_height = 6
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), trapezoid_height) / 2
  @azimuth.assert_eq(36, trapezoid_area)
}

test "temperature_conversion_scenarios" {
  // 温度转换场景测试
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  
  let celsius_temp = 25
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temp, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_temp)
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
  
  // 测试负温度
  let negative_celsius = -10
  let negative_fahrenheit = @azimuth.add(@azimuth.multiply(negative_celsius, 2), 30)
  @azimuth.assert_eq(10, negative_fahrenheit)
}

test "shopping_cart_with_discount" {
  // 购物车折扣计算测试
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let discount_threshold = 500
  let discount_rate = 10
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  
  // 计算折扣（如果超过阈值）
  let discount_amount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算最终价格
  let final_total = @azimuth.add(subtotal, -discount_amount)
  
  @azimuth.assert_eq(698, final_total)
}

test "digital_time_conversion" {
  // 数字时间转换测试
  let hours = 14
  let minutes = 35
  
  // 转换为总分钟数
  let total_minutes = @azimuth.add(@azimuth.multiply(hours, 60), minutes)
  @azimuth.assert_eq(875, total_minutes)
  
  // 转换为总秒数
  let total_seconds = @azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(52500, total_seconds)
  
  // 12小时制转换
  let twelve_hour = if (hours > 12) {
    @azimuth.add(hours, -12)
  } else {
    hours
  }
  @azimuth.assert_eq(2, twelve_hour)
}

test "compound_interest_simple" {
  // 简单复利计算测试
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简化的复利计算：最终金额 = 本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let final_amount = @azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(160000, final_amount)
}

test "string_validation_scenarios" {
  // 字符串验证场景测试
  let valid_username = "user123"
  let invalid_username = ""
  let special_char_username = "user@123"
  let unicode_username = "用户123"
  
  // 测试各种用户名场景
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet(valid_username))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(invalid_username))
  @azimuth.assert_eq_string("Hello, user@123!", @azimuth.greet(special_char_username))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet(unicode_username))
}

test "array_index_calculation" {
  // 数组索引计算测试：二维数组转一维数组的索引计算
  let row = 2
  let col = 3
  let width = 5
  
  // 计算二维数组在一维数组中的索引：index = row * width + col
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(13, index)
  
  // 测试边界情况
  let first_row = 0
  let first_col = 0
  let first_index = @azimuth.add(@azimuth.multiply(first_row, width), first_col)
  @azimuth.assert_eq(0, first_index)
  
  let last_row = 4
  let last_col = 4
  let last_index = @azimuth.add(@azimuth.multiply(last_row, width), last_col)
  @azimuth.assert_eq(24, last_index)
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 800
  let percentage = 25
  
  // 计算百分比值：总数 × 百分比 / 100
  let value = @azimuth.multiply(total, percentage) / 100
  @azimuth.assert_eq(200, value)
  
  // 计算增加后的值：总数 + 百分比值
  let increased = @azimuth.add(total, value)
  @azimuth.assert_eq(1000, increased)
  
  // 计算减少后的值：总数 - 百分比值
  let decreased = @azimuth.add(total, -value)
  @azimuth.assert_eq(600, decreased)
}

test "game_score_calculations" {
  // 游戏得分计算测试
  let base_score = 1000
  let level_bonus = 200
  let level_multiplier = 3
  let combo_bonus = 500
  let penalty = 150
  
  // 计算等级得分
  let level_score = @azimuth.add(base_score, level_bonus)
  let multiplied_score = @azimuth.multiply(level_score, level_multiplier)
  
  // 计算最终得分
  let final_score = @azimuth.add(@azimuth.add(multiplied_score, combo_bonus), -penalty)
  
  @azimuth.assert_eq(3750, final_score)
}
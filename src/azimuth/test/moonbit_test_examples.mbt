// MoonBit 测试用例示例
// 包含10个标准测试用例，展示不同的测试场景

test "add_function_decimal_simulation" {
  // 模拟小数加法运算（通过整数运算）
  // 1.5 + 2.3 = 3.8，模拟为 15 + 23 = 38，然后除以10
  let num1_scaled = 15  // 代表1.5
  let num2_scaled = 23  // 代表2.3
  let sum_scaled = azimuth::add(num1_scaled, num2_scaled)
  let result = sum_scaled / 10
  
  assert_eq(3, result)  // 整数部分
}

test "multiply_function_percentage_calculation" {
  // 百分比计算测试：计算25%的200
  let percentage = 25
  let total = 200
  
  // 百分比计算：total * percentage / 100
  let result = azimuth::multiply(total, percentage) / 100
  assert_eq(50, result)  // 200 * 25 / 100 = 50
}

test "greet_function_with_multilingual_support" {
  // 多语言支持测试
  let english_name = "John"
  let chinese_name = "张三"
  let japanese_name = "田中"
  let korean_name = "김철수"
  
  assert_eq_string("Hello, John!", azimuth::greet(english_name))
  assert_eq_string("Hello, 张三!", azimuth::greet(chinese_name))
  assert_eq_string("Hello, 田中!", azimuth::greet(japanese_name))
  assert_eq_string("Hello, 김철수!", azimuth::greet(korean_name))
}

test "compound_interest_calculation" {
  // 复利计算测试（简化版）
  // 本金1000，年利率5%，3年后的本息和
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化复利计算：principal + principal * rate * years / 100
  let interest = azimuth::multiply(principal, azimuth::multiply(rate_percent, years)) / 100
  let total = azimuth::add(principal, interest)
  
  assert_eq(1150, total)  // 1000 + 1000*5*3/100 = 1150
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度测试（简化整数计算）
  // 公式：F = C * 9/5 + 32，简化为 F = C * 2 + 30
  let celsius = 20
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  
  assert_eq(70, fahrenheit)  // 20 * 2 + 30 = 70
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth::add(azimuth::multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
}

test "array_index_calculation_2d_to_1d" {
  // 二维数组转一维数组的索引计算
  // 公式：index = row * width + col
  let row = 3
  let col = 4
  let width = 5
  
  let index = azimuth::add(azimuth::multiply(row, width), col)
  assert_eq(19, index)  // 3 * 5 + 4 = 19
  
  // 测试边界情况
  let first_index = azimuth::add(azimuth::multiply(0, width), 0)
  assert_eq(0, first_index)
}

test "game_score_system" {
  // 游戏积分系统测试
  let base_score = 1000
  let level_multiplier = 3
  let combo_bonus = 500
  let penalty = 200
  
  // 计算最终得分
  let level_score = azimuth::multiply(base_score, level_multiplier)
  let total_with_bonus = azimuth::add(level_score, combo_bonus)
  let final_score = azimuth::add(total_with_bonus, -penalty)
  
  assert_eq(3300, final_score)  // 1000*3 + 500 - 200 = 3300
}

test "shopping_cart_with_discount" {
  // 购物车折扣计算测试
  let item_price = 199
  let quantity = 3
  let discount_threshold = 500
  let discount_rate = 10
  
  // 计算总价
  let subtotal = azimuth::multiply(item_price, quantity)
  
  // 计算折扣（如果超过阈值）
  let discount = if (subtotal > discount_threshold) {
    azimuth::multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算最终价格
  let final_price = azimuth::add(subtotal, -discount)
  assert_eq(537, final_price)  // 199*3 - 199*3*10/100 = 537
}

test "time_conversion_hours_to_seconds" {
  // 时间转换测试：小时转秒数
  let hours = 2
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = azimuth::add(azimuth::multiply(hours, 60), minutes)
  // 转换为总秒数
  let total_seconds = azimuth::multiply(total_minutes, 60)
  
  assert_eq(150, total_minutes)  // 2*60 + 30 = 150
  assert_eq(9000, total_seconds)  // 150*60 = 9000
}

test "geometric_area_calculation" {
  // 几何面积计算测试
  let rectangle_length = 8
  let rectangle_width = 5
  let triangle_base = 10
  let triangle_height = 6
  
  // 计算矩形面积
  let rectangle_area = azimuth::multiply(rectangle_length, rectangle_width)
  
  // 计算三角形面积（简化）：base * height / 2
  let triangle_area = azimuth::multiply(triangle_base, triangle_height) / 2
  
  // 计算总面积
  let total_area = azimuth::add(rectangle_area, triangle_area)
  
  assert_eq(40, rectangle_area)  // 8 * 5 = 40
  assert_eq(30, triangle_area)   // 10 * 6 / 2 = 30
  assert_eq(70, total_area)      // 40 + 30 = 70
}
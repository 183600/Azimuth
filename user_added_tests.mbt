// 用户添加的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法

test "add_negative_numbers" {
  // 测试负数加法的各种情况
  assert_eq(-10, azimuth::add(-3, -7))
  assert_eq(-100, azimuth::add(-50, -50))
  assert_eq(0, azimuth::add(-25, 25))
  assert_eq(5, azimuth::add(-10, 15))
}

test "multiply_by_negative_numbers" {
  // 测试与负数相乘的各种情况
  assert_eq(-20, azimuth::multiply(4, -5))
  assert_eq(30, azimuth::multiply(-6, -5))
  assert_eq(0, azimuth::multiply(-10, 0))
  assert_eq(-15, azimuth::multiply(3, -5))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串问候
  assert_eq_string("Hello, John-Doe!", azimuth::greet("John-Doe"))
  assert_eq_string("Hello, O'Reilly!", azimuth::greet("O'Reilly"))
  assert_eq_string("Hello, C++!", azimuth::greet("C++"))
  assert_eq_string("Hello, .NET!", azimuth::greet(".NET"))
}

test "calculate_rectangle_perimeter" {
  // 测试矩形周长计算：2 × (长 + 宽)
  let length = 8
  let width = 5
  let sum = azimuth::add(length, width)
  let perimeter = azimuth::multiply(2, sum)
  assert_eq(26, perimeter)
}

test "calculate_discounted_price" {
  // 测试折扣价格计算：原价 × (1 - 折扣率)
  let original_price = 200
  let discount_rate = 20  // 20% 折扣
  let discount_amount = azimuth::multiply(original_price, discount_rate) / 100
  let final_price = azimuth::add(original_price, -discount_amount)
  assert_eq(160, final_price)
}

test "calculate_simple_interest" {
  // 测试简单利息计算：本金 × 利率 × 时间
  let principal = 1000
  let rate = 5  // 5% 年利率
  let time = 3  // 3年
  let interest = azimuth::multiply(principal, azimuth::multiply(rate, time))
  let total_amount = azimuth::add(principal, interest)
  assert_eq(16000, total_amount)  // 1000 + (1000 × 5 × 3) = 16000
}

test "calculate_average_of_four_numbers" {
  // 测试四个数的平均值计算
  let num1 = 85
  let num2 = 90
  let num3 = 78
  let num4 = 92
  let sum = azimuth::add(azimuth::add(azimuth::add(num1, num2), num3), num4)
  let average = sum / 4
  assert_eq(86, average)  // (85 + 90 + 78 + 92) / 4 = 345 / 4 = 86.25，整数除法为86
}

test "calculate_celsius_to_fahrenheit" {
  // 测试摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数计算：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth::add(azimuth::multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
}

test "calculate_total_score_with_bonus" {
  // 测试总分加奖励计算
  let base_score = 1500
  let level_bonus = 300
  let achievement_bonus = 200
  let penalty = 100
  
  // 计算总分
  let total = azimuth::add(azimuth::add(azimuth::add(base_score, level_bonus), achievement_bonus), -penalty)
  assert_eq(1900, total)
}

test "calculate_age_in_days" {
  // 测试年龄转换为天数（简化计算）
  let age = 25
  let days_per_year = 365
  let total_days = azimuth::multiply(age, days_per_year)
  assert_eq(9125, total_days)  // 25 × 365 = 9125
}
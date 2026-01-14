// 额外的 MoonBit 测试用例
// 这些测试用例展示了对 azimuth 库函数的各种使用场景

test "string_length_validation" {
  // 测试不同长度字符串的处理
  let empty_string = ""
  let short_string = "Hi"
  let medium_string = "MoonBit"
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  
  azimuth::assert_eq_string("Hello, !", azimuth::greet(empty_string))
  azimuth::assert_eq_string("Hello, Hi!", azimuth::greet(short_string))
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet(medium_string))
  azimuth::assert_eq_string("Hello, ThisIsAVeryLongStringForTestingPurposes!", azimuth::greet(long_string))
}

test "mathematical_sequence_sum" {
  // 计算等差数列的和：2 + 4 + 6 + 8 + 10 = 30
  let term1 = 2
  let term2 = 4
  let term3 = 6
  let term4 = 8
  let term5 = 10
  
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(term1, term2), term3), term4), term5)
  azimuth::assert_eq(30, sum)
}

test "discount_calculation" {
  // 商业场景：折扣计算
  let original_price = 1000
  let discount_percentage = 20
  
  // 计算折扣金额
  let discount_amount = azimuth::multiply(original_price, discount_percentage) / 100
  // 计算折后价格
  let final_price = azimuth::add(original_price, -discount_amount)
  
  azimuth::assert_eq(200, discount_amount)
  azimuth::assert_eq(800, final_price)
}

test "temperature_conversion" {
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 15
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  
  azimuth::assert_eq(60, fahrenheit)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth::add(azimuth::multiply(freezing_celsius, 2), 30)
  azimuth::assert_eq(30, freezing_fahrenheit)
}

test "compound_interest_simple" {
  // 简单复利计算
  let principal = 5000
  let rate = 5  // 5%
  let years = 2
  
  // 简化计算：本金 + 本金 × 利率 × 年数
  let interest = azimuth::multiply(principal, azimuth::multiply(rate, years))
  let total = azimuth::add(principal, interest)
  
  azimuth::assert_eq(55000, total)  // 5000 + 5000×5×2 = 55000
}

test "area_perimeter_calculation" {
  // 几何计算：矩形的面积和周长
  let length = 8
  let width = 6
  
  // 面积 = 长 × 宽
  let area = azimuth::multiply(length, width)
  // 周长 = 2 × (长 + 宽)
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  
  azimuth::assert_eq(48, area)
  azimuth::assert_eq(28, perimeter)
}

test "score_statistics" {
  // 游戏得分统计
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 96
  let score5 = 89
  
  // 计算总分
  let total_score = azimuth::add(azimuth::add(azimuth::add(azimuth::add(score1, score2), score3), score4), score5)
  // 计算平均分
  let average_score = total_score / 5
  
  azimuth::assert_eq(440, total_score)
  azimuth::assert_eq(88, average_score)
}

test "time_conversion" {
  // 时间单位转换
  let hours = 3
  let minutes = 45
  
  // 转换为总分钟数
  let total_minutes = azimuth::add(azimuth::multiply(hours, 60), minutes)
  // 转换为总秒数
  let total_seconds = azimuth::multiply(total_minutes, 60)
  
  azimuth::assert_eq(225, total_minutes)
  azimuth::assert_eq(13500, total_seconds)
}

test "border_value_operations" {
  // 边界值运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与零的运算
  azimuth::assert_eq(max_val, azimuth::add(max_val, 0))
  azimuth::assert_eq(min_val, azimuth::add(min_val, 0))
  azimuth::assert_eq(0, azimuth::multiply(max_val, 0))
  azimuth::assert_eq(0, azimuth::multiply(min_val, 0))
  
  // 边界值与1的运算
  azimuth::assert_eq(max_val, azimuth::multiply(max_val, 1))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, 1))
}

test "string_special_characters" {
  // 特殊字符字符串测试
  let unicode_string = "测试"
  let emoji_string = "🌍"
  let mixed_string = "Hello测试123"
  
  azimuth::assert_eq_string("Hello, 测试!", azimuth::greet(unicode_string))
  azimuth::assert_eq_string("Hello, 🌍!", azimuth::greet(emoji_string))
  azimuth::assert_eq_string("Hello, Hello测试123!", azimuth::greet(mixed_string))
}
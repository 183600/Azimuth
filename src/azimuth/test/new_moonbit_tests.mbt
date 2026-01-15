// 新增的 MoonBit 测试用例
// 测试 azimuth 库的各种功能和场景

test "power_calculation" {
  // 测试幂运算计算：2^4 = 16
  let base = 2
  let exponent = 4
  let result = @azimuth.multiply(@azimuth.multiply(base, base), @azimuth.multiply(base, base))
  @azimuth.assert_eq(16, result)
}

test "geometric_area_calculation" {
  // 测试几何面积计算
  let length = 10
  let width = 5
  
  // 矩形面积 = 长 × 宽
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, rectangle_area)
  
  // 三角形面积 = 底 × 高 ÷ 2
  let base = 8
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(24, triangle_area)
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 计算最终总价
  let total = @azimuth.add(subtotal, shipping_fee)
  @azimuth.assert_eq(717, total)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）：摄氏度转华氏度 F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
}

test "loan_interest_calculation" {
  // 测试贷款利息计算
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简化利息计算：本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)
}

test "string_processing_with_special_chars" {
  // 测试包含特殊字符的字符串处理
  let email = "user@example.com"
  let username = "user_123"
  let hashtag = "#moonbit"
  
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet(email))
  @azimuth.assert_eq_string("Hello, user_123!", @azimuth.greet(username))
  @azimuth.assert_eq_string("Hello, #moonbit!", @azimuth.greet(hashtag))
}

test "statistical_calculations" {
  // 测试基础统计计算
  // 计算平均值：(10 + 20 + 30 + 40 + 50) ÷ 5 = 30
  let sum1 = @azimuth.add(10, 20)
  let sum2 = @azimuth.add(30, 40)
  let sum3 = @azimuth.add(sum1, sum2)
  let total_sum = @azimuth.add(sum3, 50)
  let average = total_sum / 5
  @azimuth.assert_eq(30, average)
}

test "game_score_system" {
  // 测试游戏得分系统
  let base_score = 1000
  let level_bonus = 500
  let combo_multiplier = 2
  let penalty = 200
  
  // 计算最终得分
  let level_score = @azimuth.add(base_score, level_bonus)
  let combo_score = @azimuth.multiply(level_score, combo_multiplier)
  let final_score = @azimuth.add(combo_score, @azimuth.multiply(-1, penalty))
  @azimuth.assert_eq(2800, final_score)
}
// Azimuth 项目新增标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

// 导入必要的模块
use azimuth

test "arithmetic_sequence_calculation" {
  // 等差数列计算测试
  // 计算前n个自然数的和：1 + 2 + 3 + ... + n = n × (n + 1) / 2
  let n = 10
  let sum = azimuth.multiply(n, azimuth.add(n, 1)) / 2
  @assertion.assert_eq(55, sum)?
  
  // 计算前n个偶数的和：2 + 4 + 6 + ... + 2n = n × (n + 1)
  let even_sum = azimuth.multiply(n, azimuth.add(n, 1))
  @assertion.assert_eq(110, even_sum)?
}

test "geometric_progression_calculation" {
  // 等比数列计算测试
  // 计算2^0 + 2^1 + 2^2 + 2^3 = 1 + 2 + 4 + 8 = 15
  let term1 = 1
  let term2 = azimuth.multiply(2, 1)
  let term3 = azimuth.multiply(2, term2)
  let term4 = azimuth.multiply(2, term3)
  let geo_sum = azimuth.add(azimuth.add(azimuth.add(term1, term2), term3), term4)
  @assertion.assert_eq(15, geo_sum)?
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度测试
  // 简化公式：F = C × 9/5 + 32，使用整数近似：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  @assertion.assert_eq(80, fahrenheit)?
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth.add(azimuth.multiply(freezing_celsius, 2), 30)
  @assertion.assert_eq(30, freezing_fahrenheit)?
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = azimuth.add(azimuth.multiply(boiling_celsius, 2), 30)
  @assertion.assert_eq(230, boiling_fahrenheit)?
}

test "simple_interest_calculation" {
  // 简单利息计算测试
  // 利息 = 本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  let interest = azimuth.multiply(principal, azimuth.multiply(rate, time))
  @assertion.assert_eq(150000, interest)?
  
  // 本息合计 = 本金 + 利息
  let total = azimuth.add(principal, interest)
  @assertion.assert_eq(160000, total)?
}

test "shopping_cart_total_calculation" {
  // 购物车总价计算测试
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount = 50
  
  // 计算每种商品的总价
  let total1 = azimuth.multiply(item1_price, item1_quantity)
  let total2 = azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价（应用折扣）
  let subtotal = azimuth.add(total1, total2)
  let final_total = azimuth.add(subtotal, -discount)
  
  @assertion.assert_eq(1094, final_total)?
}

test "perimeter_and_area_calculation" {
  // 周长和面积计算测试
  let length = 12
  let width = 8
  
  // 矩形周长 = 2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  @assertion.assert_eq(40, perimeter)?
  
  // 矩形面积 = 长 × 宽
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(96, area)?
  
  // 三角形面积 = 底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = azimuth.multiply(base, height) / 2
  @assertion.assert_eq(30, triangle_area)?
}

test "string_processing_with_special_characters" {
  // 特殊字符字符串处理测试
  let username = "user_2023"
  let email = "test@example.com"
  let phone = "+86-138-0013-8000"
  
  // 测试包含下划线的用户名
  let username_result = azimuth.greet(username)
  @assertion.assert_eq_string("Hello, user_2023!", username_result)?
  
  // 测试包含特殊符号的邮箱
  let email_result = azimuth.greet(email)
  @assertion.assert_eq_string("Hello, test@example.com!", email_result)?
  
  // 测试包含连字符的电话号码
  let phone_result = azimuth.greet(phone)
  @assertion.assert_eq_string("Hello, +86-138-0013-8000!", phone_result)?
}

test "boundary_value_combination_operations" {
  // 边界值组合运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  
  // 测试最大值与零的运算
  @assertion.assert_eq(max_val, azimuth.add(max_val, zero))?
  @assertion.assert_eq(zero, azimuth.multiply(max_val, zero))?
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, one))?
  
  // 测试最小值与零的运算
  @assertion.assert_eq(min_val, azimuth.add(min_val, zero))?
  @assertion.assert_eq(zero, azimuth.multiply(min_val, zero))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, one))?
  
  // 测试最大值与最小值的组合
  @assertion.assert_eq(-1, azimuth.add(max_val, min_val))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))?
}

test "game_score_system_calculation" {
  // 游戏积分系统计算测试
  let base_score = 100
  let level_bonus = 20
  let combo_multiplier = 3
  let time_bonus = 50
  let penalty = 30
  
  // 计算基础得分
  let basic_score = azimuth.add(base_score, level_bonus)
  
  // 应用连击倍数
  let combo_score = azimuth.multiply(basic_score, combo_multiplier)
  
  // 添加时间奖励
  let time_score = azimuth.add(combo_score, time_bonus)
  
  // 扣除惩罚分数
  let final_score = azimuth.add(time_score, -penalty)
  
  @assertion.assert_eq(380, final_score)?
}

test "currency_conversion_with_fees" {
  // 带手续费的货币转换测试
  let amount = 1000
  let exchange_rate = 7  // 1 USD = 7 CNY
  let transaction_fee_rate = 2  // 2% 手续费
  
  // 计算转换金额
  let converted_amount = azimuth.multiply(amount, exchange_rate)
  
  // 计算手续费
  let fee = azimuth.multiply(converted_amount, transaction_fee_rate) / 100
  
  // 计算最终到账金额
  let final_amount = azimuth.add(converted_amount, -fee)
  
  @assertion.assert_eq(6860, final_amount)?
}
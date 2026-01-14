// 实际应用场景的 MoonBit 测试用例
// 测试各种实际应用场景中的功能使用

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  // 计算每种商品的总价
  let total1 = azimuth::@azimuth.multiply(item1_price, item1_quantity)
  let total2 = azimuth::@azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = azimuth::@azimuth.add(total1, total2)
  
  // 验证总价：199*2 + 299*1 = 398 + 299 = 697
  @azimuth.assert_eq(697, cart_total)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C * 9/5 + 32，简化为 F = C * 2 + 30
  let celsius_temp = 20
  let fahrenheit_temp = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius_temp, 2), 30)
  @azimuth.assert_eq(70, fahrenheit_temp)  // 20 * 2 + 30 = 70
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
}

test "area_perimeter_calculation" {
  // 测试面积和周长计算
  let length = 10
  let width = 5
  
  // 计算矩形面积
  let area = azimuth::@azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
  
  // 计算矩形周长
  let perimeter = azimuth::@azimuth.multiply(azimuth::@azimuth.add(length, width), 2)
  @azimuth.assert_eq(30, perimeter)
}

test "grade_average_calculation" {
  // 测试成绩平均计算
  let math_score = 85
  let english_score = 90
  let science_score = 78
  let history_score = 92
  
  // 计算总分
  let total_score = azimuth::@azimuth.add(
    azimuth::@azimuth.add(math_score, english_score),
    azimuth::@azimuth.add(science_score, history_score)
  )
  
  // 计算平均分
  let average_score = total_score / 4
  @azimuth.assert_eq(86, average_score)
}

test "loan_interest_calculation" {
  // 测试贷款利息计算
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简化的利息计算：利息 = 本金 × 利率 × 年数
  let interest = azimuth::@azimuth.multiply(
    principal,
    azimuth::@azimuth.multiply(annual_rate, years)
  )
  
  // 计算总还款额
  let total_payment = azimuth::@azimuth.add(principal, interest)
  @azimuth.assert_eq(115000, total_payment)  // 10000 + 10000*5*3 = 115000
}

test "time_conversion" {
  // 测试时间单位转换
  let hours = 2
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = azimuth::@azimuth.add(
    azimuth::@azimuth.multiply(hours, 60),
    minutes
  )
  @azimuth.assert_eq(150, total_minutes)
  
  // 转换为总秒数
  let total_seconds = azimuth::@azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(9000, total_seconds)
}

test "discount_price_calculation" {
  // 测试折扣价格计算
  let original_price = 200
  let discount_percent = 15  // 15% 折扣
  
  // 计算折扣金额
  let discount_amount = azimuth::@azimuth.multiply(original_price, discount_percent) / 100
  
  // 计算折后价格
  let discounted_price = azimuth::@azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(170, discounted_price)
}

test "compound_interest_simplified" {
  // 测试简化的复利计算
  let principal = 5000
  let rate_percent = 4  // 4%
  let years = 2
  
  // 简化的复利计算：总额 = 本金 × (1 + 利率 × 年数)
  let growth_factor = azimuth::@azimuth.add(100, azimuth::@azimuth.multiply(rate_percent, years))
  let total_amount = azimuth::@azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(540000, total_amount)  // 5000 × (100 + 4×2) = 5000 × 108 = 540000
}

test "currency_conversion" {
  // 测试货币转换
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY (简化汇率)
  
  // 转换为人民币
  let cny_amount = azimuth::@azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(700, cny_amount)
  
  // 添加手续费 3%
  let fee = azimuth::@azimuth.multiply(cny_amount, 3) / 100
  let total_with_fee = azimuth::@azimuth.add(cny_amount, fee)
  @azimuth.assert_eq(721, total_with_fee)
}

test "greeting_multilingual" {
  // 测试多语言问候功能
  let english_name = "Alice"
  let chinese_name = "张三"
  let japanese_name = "田中"
  
  // 测试英文问候
  let english_greeting = azimuth::@azimuth.greet(english_name)
  @azimuth.assert_eq_string("Hello, Alice!", english_greeting)
  
  // 测试中文问候
  let chinese_greeting = azimuth::@azimuth.greet(chinese_name)
  @azimuth.assert_eq_string("Hello, 张三!", chinese_greeting)
  
  // 测试日文问候
  let japanese_greeting = azimuth::@azimuth.greet(japanese_name)
  @azimuth.assert_eq_string("Hello, 田中!", japanese_greeting)
}
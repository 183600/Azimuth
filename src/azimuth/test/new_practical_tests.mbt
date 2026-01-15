// 新的实用 MoonBit 测试用例
// 包含10个测试用例，覆盖各种实际应用场景

test "discount_calculation" {
  // 测试折扣计算场景
  let original_price = 1000
  let discount_percentage = 20
  let discounted_amount = multiply(original_price, discount_percentage) / 100
  let final_price = add(original_price, -discounted_amount)
  
  assert_eq(800, final_price)
}

test "age_calculation" {
  // 测试年龄计算场景
  let current_year = 2026
  let birth_year = 1990
  let age = add(current_year, -birth_year)
  
  assert_eq(36, age)
}

test "budget_allocation" {
  // 测试预算分配场景
  let total_budget = 10000
  let marketing_percentage = 30
  let development_percentage = 50
  let operations_percentage = 20
  
  let marketing_budget = multiply(total_budget, marketing_percentage) / 100
  let development_budget = multiply(total_budget, development_percentage) / 100
  let operations_budget = multiply(total_budget, operations_percentage) / 100
  
  assert_eq(3000, marketing_budget)
  assert_eq(5000, development_budget)
  assert_eq(2000, operations_budget)
}

test "speed_conversion" {
  // 测试速度转换场景：km/h 转 m/s
  let km_per_hour = 72
  // 转换公式：m/s = km/h × 1000 / 3600 ≈ km/h × 5 / 18
  let meters_per_second = multiply(km_per_hour, 5) / 18
  
  assert_eq(20, meters_per_second)
}

test "compound_interest_simple" {
  // 测试简单复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化复利公式：最终金额 = 本金 × (1 + 利率 × 年数)
  let growth_factor = add(100, multiply(rate_percent, years))
  let final_amount = multiply(principal, growth_factor)
  
  assert_eq(115000, final_amount)  // 1000 × 115 = 115000
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 测试温度转换：摄氏度转华氏度
  // 简化公式：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(80, fahrenheit)
}

test "area_perimeter_calculation" {
  // 测试面积和周长计算
  let length = 10
  let width = 5
  
  // 计算矩形面积
  let area = multiply(length, width)
  // 计算矩形周长
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(50, area)
  assert_eq(30, perimeter)
}

test "score_average_calculation" {
  // 测试分数平均计算
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let score4 = 80
  
  // 计算总分
  let total_score = add(add(add(score1, score2), score3), score4)
  // 计算平均分
  let average_score = total_score / 4
  
  assert_eq(350, total_score)
  assert_eq(87, average_score)  // 整数除法
}

test "time_conversion" {
  // 测试时间转换：小时和分钟转换为总分钟数
  let hours = 2
  let minutes = 45
  
  // 转换为总分钟数
  let total_minutes = add(multiply(hours, 60), minutes)
  
  assert_eq(165, total_minutes)
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let shipping_fee = 20
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let subtotal = add(multiply(item1_price, item1_quantity), multiply(item2_price, item2_quantity))
  // 计算税费
  let tax = multiply(subtotal, tax_rate) / 100
  // 计算最终总价
  let total = add(add(subtotal, tax), shipping_fee)
  
  assert_eq(1015, total)  // (299×2 + 199×1) + 8% + 20 = 797 + 63 + 20 = 880
}
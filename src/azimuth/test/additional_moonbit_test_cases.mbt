// 额外的 MoonBit 测试用例
// 包含各种实际应用场景和边界情况测试

test "banking_interest_calculation" {
  // 银行利息计算场景
  // 简单利息计算：本金 + 本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5% 利率
  let years = 3
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(total_amount, 160000)  // 10000 + 10000*5*3 = 160000
}

test "physics_velocity_calculation" {
  // 物理速度计算场景
  // 速度 = 距离 / 时间，简化为整数运算
  let distance = 300  // 米
  let time = 10       // 秒
  
  // 使用乘法模拟除法：如果 distance = velocity × time，则 velocity = distance / time
  // 这里我们验证：velocity × time = distance
  let velocity = 30  // 已知的速度值
  let calculated_distance = @azimuth.multiply(velocity, time)
  
  @azimuth.assert_eq(calculated_distance, distance)
}

test "array_index_calculation" {
  // 数组索引计算场景
  // 二维数组转一维数组的索引计算：index = row × columns + column
  let row = 3
  let column = 4
  let columns = 10  // 总列数
  
  let linear_index = @azimuth.add(@azimuth.multiply(row, columns), column)
  
  @azimuth.assert_eq(linear_index, 34)  // 3*10 + 4 = 34
}

test "temperature_conversion_enhanced" {
  // 增强的温度转换场景
  // 华氏度转摄氏度：C = (F - 32) × 5/9，简化为整数运算
  // 使用近似值：C ≈ (F - 30) / 2
  
  let fahrenheit_temp = 100
  let celsius_temp = @azimuth.multiply(@azimuth.add(fahrenheit_temp, -30), 5) / 10
  
  @azimuth.assert_eq(celsius_temp, 35)  // (100-30)*5/10 = 70*5/10 = 35
  
  // 测试负温度
  let negative_fahrenheit = -10
  let negative_celsius = @azimuth.multiply(@azimuth.add(negative_fahrenheit, -30), 5) / 10
  
  @azimuth.assert_eq(negative_celsius, -20)  // (-10-30)*5/10 = -40*5/10 = -20
}

test "shopping_cart_with_discount" {
  // 带折扣的购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 499
  let item2_quantity = 1
  let discount_rate = 10  // 10% 折扣
  let shipping = 20
  
  // 计算商品总价
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(subtotal1, subtotal2)
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(subtotal, discount_rate) / 100
  let discounted_total = @azimuth.add(subtotal, -discount_amount)
  
  // 加上运费
  let final_total = @azimuth.add(discounted_total, shipping)
  
  @azimuth.assert_eq(final_total, 1078)  // (299*2 + 499) - 10% + 20 = 1097 - 110 + 20 = 1007 + 20 = 1027
}

test "geometric_area_perimeter_calculation" {
  // 几何图形面积和周长计算
  // 圆形的近似计算（使用整数）
  let radius = 7
  let pi_approx = 22  // π ≈ 22/7
  
  // 圆周长：2 × π × r ≈ 2 × 22 × 7 / 7 = 44
  let circumference = @azimuth.multiply(@azimuth.multiply(2, pi_approx), radius) / 7
  
  // 圆面积：π × r² ≈ 22 × 49 / 7 = 154
  let area = @azimuth.multiply(pi_approx, @azimuth.multiply(radius, radius)) / 7
  
  @azimuth.assert_eq(circumference, 44)
  @azimuth.assert_eq(area, 154)
}

test "time_calculation_scenario" {
  // 时间计算场景
  // 计算总秒数：小时 × 3600 + 分钟 × 60 + 秒
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let total_seconds_from_hours = @azimuth.multiply(hours, 3600)
  let total_seconds_from_minutes = @azimuth.multiply(minutes, 60)
  let total_seconds = @azimuth.add(@azimuth.add(total_seconds_from_hours, total_seconds_from_minutes), seconds)
  
  @azimuth.assert_eq(total_seconds, 9045)  // 2*3600 + 30*60 + 45 = 7200 + 1800 + 45 = 9045
}

test "string_processing_with_numbers" {
  // 包含数字的字符串处理
  let user_id = "user12345"
  let product_code = "product67890"
  let mixed_content = "abc123def456"
  
  @azimuth.assert_eq_string(@azimuth.greet(user_id), "Hello, user12345!")
  @azimuth.assert_eq_string(@azimuth.greet(product_code), "Hello, product67890!")
  @azimuth.assert_eq_string(@azimuth.greet(mixed_content), "Hello, abc123def456!")
}

test "financial_compound_interest" {
  // 复利计算（简化版）
  // 本息和 = 本金 × (1 + 利率)^年数，简化为线性计算
  let principal = 5000
  let rate = 8  // 8% 利率
  
  // 简化计算：每年利息累加
  let year1_interest = @azimuth.multiply(principal, rate) / 100
  let year1_total = @azimuth.add(principal, year1_interest)
  
  let year2_interest = @azimuth.multiply(year1_total, rate) / 100
  let year2_total = @azimuth.add(year1_total, year2_interest)
  
  let year3_interest = @azimuth.multiply(year2_total, rate) / 100
  let year3_total = @azimuth.add(year2_total, year3_interest)
  
  let year4_interest = @azimuth.multiply(year3_total, rate) / 100
  let final_total = @azimuth.add(year3_total, year4_interest)
  
  @azimuth.assert_eq(final_total, 6802)  // 简化计算的近似值
}

test "game_score_calculation" {
  // 游戏得分计算场景
  // 基础分 + 连击奖励 - 时间惩罚
  let base_score = 1000
  let combo_multiplier = 3
  let combo_count = 5
  let time_penalty = 200
  
  // 计算连击奖励
  let combo_bonus = @azimuth.multiply(combo_multiplier, @azimuth.multiply(combo_count, 100))
  
  // 计算最终得分
  let final_score = @azimuth.add(@azimuth.add(base_score, combo_bonus), -time_penalty)
  
  @azimuth.assert_eq(final_score, 2300)  // 1000 + 3*5*100 - 200 = 1000 + 1500 - 200 = 2300
}
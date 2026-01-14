// MoonBit 测试用例示例
// 导入 azimuth 包以使用其中的函数
test "prime_number_check" {
  // 测试素数相关计算
  // 计算7的阶乘：7! = 7 × 6 × 5 × 4 × 3 × 2 × 1 = 5040
  let factorial_7 = azimuth::@azimuth.multiply(7, azimuth::@azimuth.multiply(6, azimuth::@azimuth.multiply(5, azimuth::@azimuth.multiply(4, azimuth::@azimuth.multiply(3, azimuth::@azimuth.multiply(2, 1))))))
  @azimuth.assert_eq(5040, factorial_7)
  
  // 验证7是素数（通过检查不能被2、3、5整除）
  let num = 7
  let is_not_divisible_by_2 = azimuth::@azimuth.multiply(num / 2, 2) != num
  let is_not_divisible_by_3 = azimuth::@azimuth.multiply(num / 3, 3) != num
  let is_not_divisible_by_5 = azimuth::@azimuth.multiply(num / 5, 5) != num
  let is_prime = is_not_divisible_by_2 && is_not_divisible_by_3 && is_not_divisible_by_5
  @azimuth.assert_true(is_prime)
}
test "fibonacci_sequence" {
  // 测试斐波那契数列
  // F(0)=0, F(1)=1, F(2)=1, F(3)=2, F(4)=3, F(5)=5, F(6)=8, F(7)=13, F(8)=21
  
  // 手动计算前几个斐波那契数
  let fib_0 = 0
  let fib_1 = 1
  let fib_2 = azimuth::@azimuth.add(fib_0, fib_1)  // 1
  let fib_3 = azimuth::@azimuth.add(fib_1, fib_2)  // 2
  let fib_4 = azimuth::@azimuth.add(fib_2, fib_3)  // 3
  let fib_5 = azimuth::@azimuth.add(fib_3, fib_4)  // 5
  let fib_6 = azimuth::@azimuth.add(fib_4, fib_5)  // 8
  let fib_7 = azimuth::@azimuth.add(fib_5, fib_6)  // 13
  let fib_8 = azimuth::@azimuth.add(fib_6, fib_7)  // 21
  
  @azimuth.assert_eq(21, fib_8)
  
  // 验证斐波那契性质：F(n) × F(n+1) = F(n)² + F(n) × F(n-1)
  let n = 5
  let fib_n = fib_5
  let fib_n_plus_1 = fib_6
  let fib_n_minus_1 = fib_4
  
  let left_side = azimuth::@azimuth.multiply(fib_n, fib_n_plus_1)
  let right_side = azimuth::@azimuth.add(azimuth::@azimuth.multiply(fib_n, fib_n), azimuth::@azimuth.multiply(fib_n, fib_n_minus_1))
  @azimuth.assert_eq(left_side, right_side)
}
test "geometry_calculations" {
  // 测试几何计算
  // 计算圆的面积和周长（简化π=3）
  let radius = 5
  
  // 周长 = 2 × π × r = 2 × 3 × 5 = 30
  let circumference = azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(30, circumference)
  
  // 面积 = π × r² = 3 × 25 = 75
  let area = azimuth::@azimuth.multiply(3, azimuth::@azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 8
  let height = 6
  let triangle_area = azimuth::@azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(24, triangle_area)
}
test "time_conversion" {
  // 测试时间转换
  // 将2小时30分钟转换为分钟
  let hours = 2
  let minutes = 30
  let total_minutes = azimuth::@azimuth.add(azimuth::@azimuth.multiply(hours, 60), minutes)
  @azimuth.assert_eq(150, total_minutes)
  
  // 转换为秒数
  let total_seconds = azimuth::@azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(9000, total_seconds)
  
  // 计算时间差：从8:00到17:30
  let start_hour = 8
  let start_minute = 0
  let end_hour = 17
  let end_minute = 30
  
  let start_total = azimuth::@azimuth.add(azimuth::@azimuth.multiply(start_hour, 60), start_minute)
  let end_total = azimuth::@azimuth.add(azimuth::@azimuth.multiply(end_hour, 60), end_minute)
  let duration = azimuth::@azimuth.add(end_total, -start_total)
  @azimuth.assert_eq(570, duration)  // 9小时30分钟 = 570分钟
}
test "currency_exchange" {
  // 测试货币兑换
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY（简化汇率）
  
  // 兑换为人民币
  let cny_amount = azimuth::@azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(700, cny_amount)
  
  // 添加手续费3%
  let fee_rate = 3
  let fee = azimuth::@azimuth.multiply(cny_amount, fee_rate) / 100
  let total_with_fee = azimuth::@azimuth.add(cny_amount, fee)
  @azimuth.assert_eq(721, total_with_fee)  // 700 + 700*3/100 = 700 + 21 = 721
  
  // 反向计算：从CNY换回USD
  let cny_to_convert = 721
  let usd_back = cny_to_convert / exchange_rate
  @azimuth.assert_eq(103, usd_back)  // 721 / 7 = 103（整数除法）
}
test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  
  // 计算每种商品总价
  let total1 = azimuth::@azimuth.multiply(item1_price, item1_quantity)
  let total2 = azimuth::@azimuth.multiply(item2_price, item2_quantity)
  let total3 = azimuth::@azimuth.multiply(item3_price, item3_quantity)
  
  // 计算购物车总价
  let cart_total = azimuth::@azimuth.add(azimuth::@azimuth.add(total1, total2), total3)
  @azimuth.assert_eq(1294, cart_total)  // 299*2 + 199*3 + 99*1 = 598 + 597 + 99 = 1294
  
  // 应用折扣：满1000减100
  let discount_threshold = 1000
  let discount_amount = 100
  let final_total = azimuth::@azimuth.add(cart_total, -discount_amount)
  @azimuth.assert_eq(1194, final_total)  // 1294 - 100 = 1194
}
test "statistics_calculations" {
  // 测试统计计算
  // 计算一组数据的平均值
  let data_sum = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(10, 20), 30), 40), 50)  // 150
  let data_count = 5
  let average = data_sum / data_count
  @azimuth.assert_eq(30, average)  // 150 / 5 = 30
  
  // 计算方差：每个数与平均值的差的平方的平均值
  let diff1 = azimuth::@azimuth.add(10, -30)  // -20
  let diff2 = azimuth::@azimuth.add(20, -30)  // -10
  let diff3 = azimuth::@azimuth.add(30, -30)  // 0
  let diff4 = azimuth::@azimuth.add(40, -30)  // 10
  let diff5 = azimuth::@azimuth.add(50, -30)  // 20
  
  let variance_numerator = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.multiply(diff1, diff1), azimuth::@azimuth.multiply(diff2, diff2)), azimuth::@azimuth.add(azimuth::@azimuth.multiply(diff3, diff3), azimuth::@azimuth.multiply(diff4, diff4))), azimuth::@azimuth.multiply(diff5, diff5))
  let variance = variance_numerator / data_count
  @azimuth.assert_eq(200, variance)  // (400+100+0+100+400)/5 = 1000/5 = 200
}
test "game_score_system" {
  // 测试游戏得分系统
  let base_score = 1000
  let level_multiplier = 3
  let combo_bonus = 500
  let time_penalty = 200
  
  // 计算基础得分
  let level_score = azimuth::@azimuth.multiply(base_score, level_multiplier)
  @azimuth.assert_eq(3000, level_score)
  
  // 添加连击奖励
  let score_with_combo = azimuth::@azimuth.add(level_score, combo_bonus)
  @azimuth.assert_eq(3500, score_with_combo)
  
  // 扣除时间惩罚
  let final_score = azimuth::@azimuth.add(score_with_combo, -time_penalty)
  @azimuth.assert_eq(3300, final_score)
  
  // 计算排名得分（前10%额外加成）
  let rank_bonus_rate = 10
  let rank_bonus = azimuth::@azimuth.multiply(final_score, rank_bonus_rate) / 100
  let total_score = azimuth::@azimuth.add(final_score, rank_bonus)
  @azimuth.assert_eq(3630, total_score)  // 3300 + 3300*10/100 = 3300 + 330 = 3630
}
test "energy_consumption" {
  // 测试能耗计算
  let power_rating = 100  // 瓦特
  let hours_per_day = 24
  let days_per_month = 30
  
  // 计算每日能耗
  let daily_consumption = azimuth::@azimuth.multiply(power_rating, hours_per_day)
  @azimuth.assert_eq(2400, daily_consumption)  // 100 * 24 = 2400 瓦时
  
  // 计算月度能耗
  let monthly_consumption = azimuth::@azimuth.multiply(daily_consumption, days_per_month)
  @azimuth.assert_eq(72000, monthly_consumption)  // 2400 * 30 = 72000 瓦时
  
  // 转换为千瓦时
  let kwh_consumption = monthly_consumption / 1000
  @azimuth.assert_eq(72, kwh_consumption)  // 72000 / 1000 = 72 千瓦时
  
  // 计算电费（每千瓦时0.5元）
  let rate_per_kwh = 50  // 分/千瓦时，避免小数
  let total_cost = azimuth::@azimuth.multiply(kwh_consumption, rate_per_kwh) / 100
  @azimuth.assert_eq(36, total_cost)  // 72 * 0.5 = 36 元
}
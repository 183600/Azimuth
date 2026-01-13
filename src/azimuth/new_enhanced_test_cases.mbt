// 新增的 MoonBit 测试用例
// 包含10个新的测试场景

test "bitwise_operations_simulation" {
  // 模拟位运算：使用加减乘除实现位运算效果
  let a = 8
  let b = 3
  
  // 左移运算模拟：a << 1 = a * 2
  let left_shift = azimuth::multiply(a, 2)
  assert_eq(16, left_shift)
  
  // 右移运算模拟：a >> 1 = a / 2
  let right_shift = a / 2
  assert_eq(4, right_shift)
  
  // 位或运算模拟（简化）：a | b = a + b
  let bit_or_sim = azimuth::add(a, b)
  assert_eq(11, bit_or_sim)
}

test "currency_conversion_practical" {
  // 实际货币转换场景
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY (简化汇率)
  
  // 转换为人民币
  let cny_amount = azimuth::multiply(usd_amount, exchange_rate)
  assert_eq(700, cny_amount)
  
  // 添加手续费 5%
  let fee = azimuth::multiply(cny_amount, 5) / 100
  let total_with_fee = azimuth::add(cny_amount, fee)
  assert_eq(735, total_with_fee)
}

test "geometry_calculations" {
  // 几何计算测试
  let radius = 5
  
  // 圆周长（简化计算）：2 × π × r ≈ 2 × 3 × r
  let circumference = azimuth::multiply(azimuth::multiply(2, 3), radius)
  assert_eq(30, circumference)
  
  // 圆面积（简化计算）：π × r² ≈ 3 × r²
  let area = azimuth::multiply(3, azimuth::multiply(radius, radius))
  assert_eq(75, area)
}

test "time_calculations" {
  // 时间计算测试
  let hours = 2
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = azimuth::add(azimuth::multiply(hours, 60), minutes)
  assert_eq(150, total_minutes)
  
  // 转换为总秒数
  let total_seconds = azimuth::multiply(total_minutes, 60)
  assert_eq(9000, total_seconds)
}

test "statistics_basic" {
  // 基础统计计算
  // 计算总和
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(10, 20), 30), 40), 50)
  assert_eq(150, sum)
  
  // 计算平均值
  let average = sum / 5
  assert_eq(30, average)
}

test "game_score_calculation" {
  // 游戏得分计算测试
  let base_score = 1000
  let level_multiplier = 3
  let bonus_points = 500
  let penalty = 200
  
  // 计算最终得分
  let level_score = azimuth::multiply(base_score, level_multiplier)
  let total_with_bonus = azimuth::add(level_score, bonus_points)
  let final_score = azimuth::add(total_with_bonus, -penalty)
  
  assert_eq(3300, final_score)
  
  // 计算排名得分（前10%额外加分）
  let top_percentage = 10
  let rank_bonus = azimuth::multiply(final_score, top_percentage) / 100
  let rank_total = azimuth::add(final_score, rank_bonus)
  
  assert_eq(3630, rank_total)
}

test "energy_consumption" {
  // 能耗计算测试
  let power_rating = 100  // 瓦特
  let hours_used = 24
  let days_used = 30
  let cost_per_kwh = 10  // 每千瓦时费用（简化）
  
  // 计算总能耗（瓦时）
  let daily_consumption = azimuth::multiply(power_rating, hours_used)
  let monthly_consumption = azimuth::multiply(daily_consumption, days_used)
  
  // 转换为千瓦时
  let kwh_consumption = monthly_consumption / 1000
  assert_eq(72, kwh_consumption)
  
  // 计算费用
  let total_cost = azimuth::multiply(kwh_consumption, cost_per_kwh)
  assert_eq(720, total_cost)
}

test "factorial_calculation" {
  // 计算5的阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let result = azimuth::multiply(n, azimuth::multiply(n - 1, azimuth::multiply(n - 2, azimuth::multiply(n - 3, n - 4))))
  assert_eq(120, result)
}

test "power_of_two_calculation" {
  // 计算2的幂：2^8 = 256
  // 通过连续相乘实现
  let result = azimuth::multiply(azimuth::multiply(azimuth::multiply(2, 2), azimuth::multiply(2, 2)), azimuth::multiply(azimuth::multiply(2, 2), azimuth::multiply(2, 2)))
  assert_eq(256, result)
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 500
  let percentage = 25  // 25%
  
  // 计算百分比值：总数 × 百分比 / 100
  let value = azimuth::multiply(total, percentage) / 100
  assert_eq(125, value)  // 500 × 25 / 100 = 125
  
  // 计算增加后的值：总数 + 百分比值
  let increased = azimuth::add(total, value)
  assert_eq(625, increased)  // 500 + 125 = 625
}
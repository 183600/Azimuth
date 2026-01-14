test "add_basic_functionality" {
  // 测试基本的加法功能
  let result1 = azimuth::add(2, 3)
  assert_eq(5, result1)
  
  let result2 = azimuth::add(-2, 2)
  assert_eq(0, result2)
  
  let result3 = azimuth::add(-2, -3)
  assert_eq(-5, result3)
}

test "add_zero_handling" {
  // 测试零值处理
  let result1 = azimuth::add(5, 0)
  assert_eq(5, result1)
  
  let result2 = azimuth::add(0, 5)
  assert_eq(5, result2)
  
  let result3 = azimuth::add(0, 0)
  assert_eq(0, result3)
}

test "add_boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  let result1 = azimuth::add(max_val, 1)
  assert_eq(max_val, result1)
  
  let result2 = azimuth::add(min_val, -1)
  assert_eq(min_val, result2)
  
  let result3 = azimuth::add(max_val, min_val)
  assert_eq(-1, result3)
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  let result1 = azimuth::multiply(2, 3)
  assert_eq(6, result1)
  
  let result2 = azimuth::multiply(-2, 3)
  assert_eq(-6, result2)
  
  let result3 = azimuth::multiply(-2, -3)
  assert_eq(6, result3)
}

test "multiply_zero_and_one" {
  // 测试零和一的乘法
  let result1 = azimuth::multiply(5, 0)
  assert_eq(0, result1)
  
  let result2 = azimuth::multiply(5, 1)
  assert_eq(5, result2)
  
  let result3 = azimuth::multiply(1, 5)
  assert_eq(5, result3)
}

test "multiply_boundary_values" {
  // 测试乘法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  let result1 = azimuth::multiply(max_val, 0)
  assert_eq(0, result1)
  
  let result2 = azimuth::multiply(min_val, -1)
  assert_eq(min_val, result2)
  
  let result3 = azimuth::multiply(max_val, 1)
  assert_eq(max_val, result3)
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  let result1 = azimuth::greet("World")
  assert_eq_string("Hello, World!", result1)
  
  let result2 = azimuth::greet("MoonBit")
  assert_eq_string("Hello, MoonBit!", result2)
}

test "greet_edge_cases" {
  // 测试问候函数的边界情况
  let result1 = azimuth::greet("")
  assert_eq_string("Hello, !", result1)
  
  let result2 = azimuth::greet("A")
  assert_eq_string("Hello, A!", result2)
  
  let result3 = azimuth::greet("123")
  assert_eq_string("Hello, 123!", result3)
}

test "complex_calculation" {
  // 测试复合计算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result1 = azimuth::multiply(azimuth::add(a, b), c)
  assert_eq(900, result1)
  
  // a * b + c
  let result2 = azimuth::add(azimuth::multiply(a, b), c)
  assert_eq(230, result2)
}

test "real_world_scenario" {
  // 测试实际应用场景
  // 计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = azimuth::multiply(item_price, quantity)
  let total = azimuth::add(subtotal, shipping)
  
  assert_eq(617, total)
}

test "add_edge_case_max_min_values" {
  // 测试最大值和最小值相加的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "add_consecutive_operations" {
  // 测试连续加法操作
  let result = azimuth::add(azimuth::add(1, 2), azimuth::add(3, 4))
  assert_eq(10, result)
  
  let result2 = azimuth::add(azimuth::add(-5, 10), azimuth::add(-3, 8))
  assert_eq(10, result2)
}

test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  assert_eq(8, azimuth::multiply(2, 4))
  assert_eq(16, azimuth::multiply(4, 4))
  assert_eq(1024, azimuth::multiply(32, 32))
  assert_eq(-1024, azimuth::multiply(-32, 32))
}

test "multiply_fractional_simulation" {
  // 通过整数运算模拟小数乘法（例如：计算百分比）
  // 25% of 400 = 400 * 25 / 100
  let value = 400
  let percentage = 25
  let result = azimuth::multiply(value, percentage) / 100
  assert_eq(100, result)
  
  // 75% of 200 = 200 * 75 / 100
  let value2 = 200
  let percentage2 = 75
  let result2 = azimuth::multiply(value2, percentage2) / 100
  assert_eq(150, result2)
}

test "greet_multilingual" {
  // 测试多语言问候
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, العربية!", azimuth::greet("العربية"))
  assert_eq_string("Hello, עברית!", azimuth::greet("עברית"))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, Line1\nLine2!", azimuth::greet("Line1\nLine2"))
  assert_eq_string("Hello, Tab\tSeparated!", azimuth::greet("Tab\tSeparated"))
  assert_eq_string("Hello, Emoji🌍!", azimuth::greet("Emoji🌍"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列
  // 计算：1 + 2*3 + 4*5*6
  let term1 = 1
  let term2 = azimuth::multiply(2, 3)
  let term3 = azimuth::multiply(azimuth::multiply(4, 5), 6)
  let result = azimuth::add(azimuth::add(term1, term2), term3)
  assert_eq(127, result)  // 1 + 6 + 120 = 127
}

// 新增的 10 个 MoonBit 测试用例
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
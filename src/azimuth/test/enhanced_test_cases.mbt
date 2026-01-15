// 增强的 MoonBit 测试用例
// 测试 Azimuth 项目的各种功能和边界情况

test "temperature_conversion_comprehensive" {
  // 温度转换综合测试：摄氏度转华氏度 F = C × 9/5 + 32
  // 使用整数近似：F = C × 2 + 30
  
  // 测试常见温度点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  let room_c = 20
  let room_f = @azimuth.add(@azimuth.multiply(room_c, 2), 30)
  @azimuth.assert_eq(70, room_f)
  
  let boiling_c = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling_c, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
  
  // 测试负温度
  let negative_c = -10
  let negative_f = @azimuth.add(@azimuth.multiply(negative_c, 2), 30)
  @azimuth.assert_eq(10, negative_f)
}

test "shopping_cart_total_calculation" {
  // 购物车总价计算测试
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 3
  let item3_price = 99
  let item3_qty = 1
  
  // 计算各项小计
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  let subtotal3 = @azimuth.multiply(item3_price, item3_qty)
  
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal1, subtotal2), subtotal3)
  
  // 验证：299*2 + 199*3 + 99*1 = 598 + 597 + 99 = 1294
  @azimuth.assert_eq(1294, total)
}

test "geometric_calculations" {
  // 几何计算测试
  let length = 8
  let width = 6
  
  // 矩形面积
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, rectangle_area)
  
  // 矩形周长
  let perimeter = @azimuth.multiply(@azimuth.add(length, width), 2)
  @azimuth.assert_eq(28, perimeter)
  
  // 三角形面积（底×高÷2）
  let base = 10
  let height = 5
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(25, triangle_area)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "mathematical_series_calculations" {
  // 数学级数计算测试
  // 等差数列求和：1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum_n = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum_n)
  
  // 等比数列求和：1 + 2 + 4 + 8 = 15
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 4), 8)
  @azimuth.assert_eq(15, geo_sum)
  
  // 平方和：1² + 2² + 3² + 4² = 30
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4))
  @azimuth.assert_eq(30, square_sum)
}

test "boundary_value_comprehensive" {
  // 边界值综合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 边界值附近的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "game_mechanics_simulation" {
  // 游戏机制模拟测试
  let base_score = 100
  let level = 5
  let multiplier = 2
  let bonus = 50
  
  // 计算等级加成分数
  let level_bonus = @azimuth.multiply(base_score, level)
  
  // 计算倍数加成
  let multiplier_bonus = @azimuth.multiply(level_bonus, multiplier)
  
  // 计算最终分数
  let final_score = @azimuth.add(multiplier_bonus, bonus)
  
  // 验证：(100 * 5) * 2 + 50 = 1050
  @azimuth.assert_eq(1050, final_score)
  
  // 计算排名奖励（前10名额外10%）
  let rank_bonus = @azimuth.multiply(final_score, 10) / 100
  let total_with_rank = @azimuth.add(final_score, rank_bonus)
  
  // 验证：1050 + 105 = 1155
  @azimuth.assert_eq(1155, total_with_rank)
}

test "time_conversion_utilities" {
  // 时间转换工具测试
  // 时间单位转换
  let hours = 2
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = @azimuth.add(@azimuth.multiply(hours, 60), minutes)
  @azimuth.assert_eq(150, total_minutes)
  
  // 转换为总秒数
  let total_seconds = @azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(9000, total_seconds)
  
  // 计算时间差
  let start_hour = 9   // 9:00 AM
  let end_hour = 17    // 5:00 PM
  let work_hours = @azimuth.add(end_hour, -start_hour)
  @azimuth.assert_eq(8, work_hours)
  
  // 计算工作分钟数
  let work_minutes = @azimuth.multiply(work_hours, 60)
  @azimuth.assert_eq(480, work_minutes)
}

test "financial_interest_calculations" {
  // 金融利息计算测试
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简单利息计算：利息 = 本金 × 利率 × 年数
  let simple_interest = @azimuth.multiply(principal, @azimuth.multiply(rate_percent, years))
  
  // 总金额 = 本金 + 利息
  let total_simple = @azimuth.add(principal, simple_interest)
  
  // 验证：10000 + 10000*5*3 = 10000 + 150000 = 160000
  @azimuth.assert_eq(160000, total_simple)
  
  // 复利简化计算：总金额 = 本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate_percent, years))
  let total_compound = @azimuth.multiply(principal, growth_factor)
  
  // 验证：10000 × (1 + 5×3) = 10000 × 16 = 160000
  @azimuth.assert_eq(160000, total_compound)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, max_val) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, min_val) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
  
  // 测试特殊值处理
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}
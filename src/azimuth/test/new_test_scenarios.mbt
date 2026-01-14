// 新的MoonBit测试场景
// 为azimuth项目添加额外的测试用例

test "temperature_conversion_celsius_to_fahrenheit" {
  // 温度转换：摄氏度转华氏度（简化公式：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = @azimuth.add(@azimuth.multiply(100, 2), 30)
  @azimuth.assert_eq(230, boiling)
}

test "simple_interest_calculation" {
  // 简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)  // 10000 + 10000×5×3 = 160000
}

test "array_index_calculation_2d_to_1d" {
  // 二维数组转一维数组的索引计算
  let row = 3
  let col = 4
  let width = 5
  
  // index = row × width + col
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(19, index)  // 3 × 5 + 4 = 19
  
  // 测试边界情况
  let first_index = @azimuth.add(@azimuth.multiply(0, 5), 0)
  @azimuth.assert_eq(0, first_index)
}

test "geometric_area_calculations" {
  // 几何面积计算
  let radius = 5
  
  // 圆面积（简化）：π × r² ≈ 3 × r²
  let circle_area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, circle_area)  // 3 × 25 = 75
  
  // 矩形面积：长 × 宽
  let length = 8
  let width = 6
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "game_score_system" {
  // 游戏得分系统
  let base_score = 1000
  let level_multiplier = 3
  let combo_bonus = 500
  let time_penalty = 200
  
  // 最终得分 = 基础分 × 等级倍数 + 连击奖励 - 时间惩罚
  let level_score = @azimuth.multiply(base_score, level_multiplier)
  let final_score = @azimuth.add(@azimuth.add(level_score, combo_bonus), -time_penalty)
  @azimuth.assert_eq(3300, final_score)  // 1000×3 + 500 - 200 = 3300
}

test "currency_exchange_calculation" {
  // 货币兑换计算
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY
  let fee_rate = 2  // 2% 手续费
  
  // 兑换金额
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(700, cny_amount)
  
  // 手续费
  let fee = @azimuth.multiply(cny_amount, fee_rate) / 100
  @azimuth.assert_eq(14, fee)
  
  // 实际到手金额
  let actual_amount = @azimuth.add(cny_amount, -fee)
  @azimuth.assert_eq(686, actual_amount)
}

test "time_conversion_calculations" {
  // 时间转换计算
  let hours = 2
  let minutes = 45
  
  // 转换为总分钟数
  let total_minutes = @azimuth.add(@azimuth.multiply(hours, 60), minutes)
  @azimuth.assert_eq(165, total_minutes)  // 2×60 + 45 = 165
  
  // 转换为总秒数
  let total_seconds = @azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(9900, total_seconds)  // 165 × 60 = 9900
}

test "shopping_cart_with_discount" {
  // 购物车计算（含折扣）
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  
  // 小计
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(subtotal1, subtotal2)
  
  // 折扣（满500减50）
  let discount = if (subtotal > 500) { 50 } else { 0 }
  let final_total = @azimuth.add(subtotal, -discount)
  
  @azimuth.assert_eq(797, final_total)  // (299×2 + 199×1) - 50 = 797
}

test "string_concatenation_edge_cases" {
  // 字符串拼接边界情况
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // 测试包含空格
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
}

test "mathematical_series_calculation" {
  // 数学级数计算
  // 等差数列求和：1 + 3 + 5 + ... + 19（前10个奇数）
  // 公式：n × (首项 + 末项) / 2
  let n = 10
  let first = 1
  let last = 19
  
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(100, sum)  // 10 × (1 + 19) / 2 = 100
  
  // 等比数列求和：2 + 4 + 8 + 16（前4项）
  // 手动计算：2 + 4 + 8 + 16 = 30
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geo_sum)
}
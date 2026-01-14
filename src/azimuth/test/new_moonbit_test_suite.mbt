// 新的 MoonBit 测试套件
// 包含10个实用的测试用例，使用标准的 MoonBit 测试语法

test "arithmetic_sequence_sum" {
  // 计算等差数列的和：2 + 5 + 8 + 11 + 14
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 2
  let last = 14
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(40, sum)
}

test "geometry_perimeter_calculation" {
  // 计算矩形周长：2 × (长 + 宽)
  let length = 8
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
}

test "string_with_unicode_chars" {
  // 测试包含 Unicode 字符的字符串处理
  let unicode_name = "张三李四"
  let result = @azimuth.greet(unicode_name)
  @azimuth.assert_eq_string("Hello, 张三李四!", result)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数计算：F = C × 2 + 30
  let celsius = 15
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(60, fahrenheit)
}

test "shopping_cart_total" {
  // 购物车总价计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  
  let total1 = @azimuth.multiply(item1_price, item1_qty)
  let total2 = @azimuth.multiply(item2_price, item2_qty)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(697, cart_total)
}

test "compound_interest_simple" {
  // 简单复利计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(160000, total)
}

test "array_index_2d_to_1d" {
  // 二维数组转一维数组的索引计算
  let row = 2
  let col = 3
  let width = 5
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(13, index)
}

test "boundary_value_addition" {
  // 边界值加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "game_score_with_bonus" {
  // 游戏得分计算（含奖励）
  let base_score = 1000
  let level_bonus = 500
  let combo_multiplier = 2
  let penalty = 200
  
  let final_score = @azimuth.add(@azimuth.multiply(@azimuth.add(base_score, level_bonus), combo_multiplier), -penalty)
  @azimuth.assert_eq(2800, final_score)
}

test "energy_consumption_cost" {
  // 能耗费用计算
  let power_watts = 100
  let hours_daily = 8
  let days_monthly = 30
  let price_per_kwh = 10
  
  let total_watt_hours = @azimuth.multiply(power_watts, @azimuth.multiply(hours_daily, days_monthly))
  let total_kwh = total_watt_hours / 1000
  let total_cost = @azimuth.multiply(total_kwh, price_per_kwh)
  
  @azimuth.assert_eq(240, total_cost)
}
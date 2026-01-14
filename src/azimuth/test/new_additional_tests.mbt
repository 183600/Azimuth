// 新增的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "arithmetic_sequence_calculation" {
  // 等差数列求和：1 + 3 + 5 + ... + 19（前10个奇数）
  // 公式：n × (首项 + 末项) / 2
  let n = 10
  let first = 1
  let last = 19
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(100, sum)
}

test "geometry_perimeter_calculation" {
  // 计算三角形周长
  let side_a = 7
  let side_b = 8
  let side_c = 9
  let perimeter = @azimuth.add(@azimuth.add(side_a, side_b), side_c)
  @azimuth.assert_eq(24, perimeter)
}

test "discount_price_calculation" {
  // 商品折扣计算
  let original_price = 299
  let discount_rate = 20  // 20% 折扣
  let discounted_price = original_price - @azimuth.multiply(original_price, discount_rate) / 100
  @azimuth.assert_eq(239, discounted_price)
}

test "temperature_conversion_fahrenheit_to_celsius" {
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  // 简化为整数计算：C = (F - 32) × 5 / 9
  let fahrenheit = 77
  let celsius = @azimuth.multiply(fahrenheit - 32, 5) / 9
  @azimuth.assert_eq(25, celsius)
}

test "compound_interest_simple" {
  // 简单复利计算：本金 × (1 + 利率 × 年数)
  let principal = 5000
  let rate = 6  // 6%
  let years = 4
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(125000, total)
}

test "string_greeting_with_special_cases" {
  // 测试各种特殊情况的字符串问候
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "area_calculation_composite_shape" {
  // 组合图形面积计算：矩形 + 半圆
  // 矩形：长 × 宽
  let rect_length = 10
  let rect_width = 6
  let rect_area = @azimuth.multiply(rect_length, rect_width)
  
  // 半圆面积（简化）：π × r² / 2 ≈ 3 × r² / 2
  let radius = 3
  let circle_area = @azimuth.multiply(3, @azimuth.multiply(radius, radius)) / 2
  
  let total_area = @azimuth.add(rect_area, circle_area)
  @azimuth.assert_eq(73, total_area)  // 60 + 13 = 73
}

test "speed_distance_time_calculation" {
  // 速度、距离、时间计算
  let distance = 300  // 公里
  let time_hours = 4
  let time_minutes = 30  // 0.5小时
  
  // 计算总时间（小时）
  let total_time = time_hours + time_minutes / 60
  
  // 计算速度：距离 / 时间
  let speed = distance / total_time
  @azimuth.assert_eq(66, speed)  // 300 / 4.5 ≈ 66.67，整数除法为66
}

test "array_index_2d_to_1d" {
  // 二维数组转一维数组的索引计算
  let row = 2
  let col = 3
  let width = 5
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(13, index)
}

test "shopping_cart_with_tax_and_shipping" {
  // 购物车计算（含税费和运费）
  let item_price = 159
  let quantity = 2
  let tax_rate = 8  // 8%
  let shipping = 15
  
  // 商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  // 最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(349, total)  // 318 + 25 + 15 = 358，但318*8/100=25.44，整数除法为25，所以318+25+15=358
  // 修正：159*2=318, 318*8/100=25(整数除法), 318+25+15=358
  @azimuth.assert_eq(358, total)
}
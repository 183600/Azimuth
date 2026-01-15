// 额外的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖项目的核心功能

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  @azimuth.assert_eq(result1, result2)
}

test "add_associative_property" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 1000
  let b = 2000
  let c = 3000
  
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 10
  let c = 15
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "extreme_boundary_operations" {
  // 测试极值边界运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result)
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric = "User123Test456"
  let result2 = @azimuth.greet(alphanumeric)
  @azimuth.assert_eq_string("Hello, User123Test456!", result2)
}

test "real_world_shopping_cart" {
  // 测试实际应用场景：购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount = 50  // 折扣金额
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价和应用折扣
  let subtotal = @azimuth.add(total1, total2)
  let final_total = @azimuth.add(subtotal, -discount)
  
  // 验证总价：299*2 + 199*3 - 50 = 598 + 597 - 50 = 1145
  @azimuth.assert_eq(1145, final_total)
}

test "compound_interest_calculation" {
  // 测试复利计算（整数简化版）
  let principal = 10000
  let rate_percent = 5  // 5%
  let years = 3
  
  // 简化的复利计算：本息和 = 本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate_percent, years))
  let total_amount = @azimuth.multiply(principal, growth_factor)
  
  // 验证结果：10000 × (1 + 5×3) = 10000 × 16 = 160000
  @azimuth.assert_eq(160000, total_amount)
}

test "geometric_area_calculations" {
  // 测试几何面积计算
  let radius = 7
  
  // 圆面积（简化计算）：π × r² ≈ 3 × r²
  let circle_area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  
  // 矩形面积
  let length = 10
  let width = 6
  let rectangle_area = @azimuth.multiply(length, width)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 8
  let height = 4
  let triangle_area = @azimuth.multiply(base, height) / 2
  
  @azimuth.assert_eq(147, circle_area)  // 3 × 7² = 3 × 49 = 147
  @azimuth.assert_eq(60, rectangle_area)  // 10 × 6 = 60
  @azimuth.assert_eq(16, triangle_area)  // 8 × 4 ÷ 2 = 16
}

test "time_conversion_calculations" {
  // 测试时间转换计算
  let days = 2
  let hours = 5
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = @azimuth.add(
    @azimuth.multiply(@azimuth.add(@azimuth.multiply(days, 24), hours), 60),
    minutes
  )
  
  // 验证结果：(2×24+5)×60+30 = (48+5)×60+30 = 53×60+30 = 3180+30 = 3210
  @azimuth.assert_eq(3210, total_minutes)
}

test "nested_calculation_scenario" {
  // 测试嵌套计算场景
  let base_value = 100
  let multiplier = 3
  let increment = 50
  let divisor = 2
  
  // 复杂计算：((base × multiplier) + increment) ÷ divisor
  let step1 = @azimuth.multiply(base_value, multiplier)  // 100 × 3 = 300
  let step2 = @azimuth.add(step1, increment)  // 300 + 50 = 350
  let final_result = step2 / divisor  // 350 ÷ 2 = 175
  
  @azimuth.assert_eq(175, final_result)
}
// 额外的 MoonBit 测试用例
// 覆盖各种边界情况和实际应用场景

test "arithmetics_associative_properties" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 15
  let b = 25
  let c = 35
  
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let d = -10
  let e = 20
  let f = -5
  
  let result3 = @azimuth.add(@azimuth.add(d, e), f)
  let result4 = @azimuth.add(d, @azimuth.add(e, f))
  @azimuth.assert_eq(result3, result4)
}

test "arithmetics_commutative_properties" {
  // 测试乘法交换律：a * b = b * a
  let a = 12
  let b = 17
  
  let result1 = @azimuth.multiply(a, b)
  let result2 = @azimuth.multiply(b, a)
  @azimuth.assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let c = -8
  let d = 13
  
  let result3 = @azimuth.multiply(c, d)
  let result4 = @azimuth.multiply(d, c)
  @azimuth.assert_eq(result3, result4)
}

test "extreme_boundary_conditions" {
  // 测试极端边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与接近最大值的数相加
  @azimuth.assert_eq(@azimuth.add(max_val, max_val), max_val)
  @azimuth.assert_eq(@azimuth.add(max_val, max_val - 1), max_val)
  
  // 测试最小值与接近最小值的数相加
  @azimuth.assert_eq(@azimuth.add(min_val, min_val), min_val)
  @azimuth.assert_eq(@azimuth.add(min_val, min_val + 1), min_val)
  
  // 测试大数相乘的溢出处理
  @azimuth.assert_eq(@azimuth.multiply(46341, 46341), max_val)
  @azimuth.assert_eq(@azimuth.multiply(-46341, 46341), min_val)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  @azimuth.assert_eq_string(@azimuth.greet("\"Quoted\""), "Hello, \"Quoted\"!")
  @azimuth.assert_eq_string(@azimuth.greet("Line1\nLine2"), "Hello, Line1\nLine2!")
  @azimuth.assert_eq_string(@azimuth.greet("Tab\tSeparated"), "Hello, Tab\tSeparated!")
  
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string(@azimuth.greet("🌍World"), "Hello, 🌍World!")
  @azimuth.assert_eq_string(@azimuth.greet("中文测试"), "Hello, 中文测试!")
  
  // 测试包含数字和字母混合的字符串
  @azimuth.assert_eq_string(@azimuth.greet("User123"), "Hello, User123!")
  @azimuth.assert_eq_string(@azimuth.greet("Test_456"), "Hello, Test_456!")
}

test "real_world_financial_calculations" {
  // 测试真实世界金融计算场景
  // 计算折扣价格：原价 * (1 - 折扣率)
  let original_price = 1000
  let discount_rate = 20  // 20%折扣
  let discount_amount = @azimuth.multiply(original_price, discount_rate)
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(discounted_price, 800)  // 1000 - 1000*20/100 = 800
  
  // 计算复利：本金 + 本金 * 利率 * 时间
  let principal = 5000
  let interest_rate = 5
  let time_years = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(interest_rate, time_years))
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(total_amount, 80000)  // 5000 + 5000*5*3 = 80000，表示8000元
}

test "geometric_calculations" {
  // 测试几何计算场景
  // 计算正方形面积：边长 * 边长
  let side_length = 7
  let square_area = @azimuth.multiply(side_length, side_length)
  @azimuth.assert_eq(square_area, 49)
  
  // 计算长方形周长：2 * (长 + 宽)
  let length = 12
  let width = 8
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(perimeter, 40)
  
  // 计算梯形面积：(上底 + 下底) * 高 / 2
  let top_base = 5
  let bottom_base = 9
  let height = 4
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), height) / 2
  @azimuth.assert_eq(trapezoid_area, 28)
}

test "temperature_conversion_calculations" {
  // 测试温度转换计算
  // 摄氏度转华氏度（简化版）：F = C * 9/5 + 32 ≈ C * 2 + 30
  let celsius_temp = 20
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temp, 2), 30)
  @azimuth.assert_eq(fahrenheit_temp, 70)  // 20 * 2 + 30 = 70
  
  // 测试极端温度
  let extreme_celsius = -40
  let extreme_fahrenheit = @azimuth.add(@azimuth.multiply(extreme_celsius, 2), 30)
  @azimuth.assert_eq(extreme_fahrenheit, -50)  // -40 * 2 + 30 = -50
  
  // 测试高温
  let high_celsius = 100
  let high_fahrenheit = @azimuth.add(@azimuth.multiply(high_celsius, 2), 30)
  @azimuth.assert_eq(high_fahrenheit, 230)  // 100 * 2 + 30 = 230
}

test "series_calculations" {
  // 测试数列计算
  // 计算等差数列前n项和：n/2 * (2a + (n-1)d)
  // 这里我们简化为：a + (a+d) + (a+2d) + ... + (a+(n-1)d)
  let first_term = 3
  let common_diff = 2
  let num_terms = 5
  
  // 手动计算：3 + 5 + 7 + 9 + 11 = 35
  let term1 = first_term
  let term2 = @azimuth.add(term1, common_diff)
  let term3 = @azimuth.add(term2, common_diff)
  let term4 = @azimuth.add(term3, common_diff)
  let term5 = @azimuth.add(term4, common_diff)
  
  let series_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(series_sum, 35)
  
  // 计算等比数列前n项和（简化版）：a + ar + ar² + ...
  let geo_first = 2
  let ratio = 3
  let geo_terms = 4
  
  // 手动计算：2 + 6 + 18 + 54 = 80
  let geo_term1 = geo_first
  let geo_term2 = @azimuth.multiply(geo_term1, ratio)
  let geo_term3 = @azimuth.multiply(geo_term2, ratio)
  let geo_term4 = @azimuth.multiply(geo_term3, ratio)
  
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(geo_term1, geo_term2), geo_term3), geo_term4)
  @azimuth.assert_eq(geo_sum, 80)
}

test "error_boundary_conditions" {
  // 测试错误边界条件和异常处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数的正确性
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_true(@azimuth.multiply(4, 6) == 24)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
  
  @azimuth.assert_false(@azimuth.add(5, 3) == 9)
  @azimuth.assert_false(@azimuth.multiply(4, 6) == 25)
  @azimuth.assert_false(@azimuth.greet("Test") == "Hi, Test!")
  
  // 测试边界条件的正确处理
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
}

test "complex_business_logic" {
  // 测试复杂业务逻辑场景
  // 场景：计算订单总价，包括商品价格、数量、税费和折扣
  
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let item3_price = 99
  let item3_quantity = 3
  
  // 计算各项商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let item3_total = @azimuth.multiply(item3_price, item3_quantity)
  
  // 计算小计
  let subtotal = @azimuth.add(@azimuth.add(item1_total, item2_total), item3_total)
  
  // 计算税费（假设10%）
  let tax_rate = 10
  let tax_amount = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算折扣（满1000减100）
  let discount_threshold = 1000
  let discount_amount = if (subtotal >= discount_threshold) { 100 } else { 0 }
  
  // 计算总价
  let total_with_tax = @azimuth.add(subtotal, tax_amount)
  let final_total = @azimuth.add(total_with_tax, -discount_amount)
  
  // 验证结果：
  // 小计：299*2 + 199*1 + 99*3 = 598 + 199 + 297 = 1094
  // 税费：1094 * 10% = 109.4 ≈ 109（整数除法）
  // 折扣：100（因为小计超过1000）
  // 总价：1094 + 109 - 100 = 1103
  @azimuth.assert_eq(1103, final_total)
}
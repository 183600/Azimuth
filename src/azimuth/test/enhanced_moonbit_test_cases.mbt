// 增强的 MoonBit 测试用例
// 包含多种场景的测试，验证 azimuth 模块的功能

// 测试数学运算的结合律
test "addition_associativity" {
  // 验证加法结合律: (a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  
  let left_associative = azimuth::@azimuth.add(azimuth::@azimuth.add(a, b), c)
  let right_associative = azimuth::@azimuth.add(a, azimuth::@azimuth.add(b, c))
  
  azimuth::@azimuth.assert_eq(left_associative, right_associative)
  
  // 测试负数情况
  let d = -150
  let e = 250
  let f = -100
  
  let left_neg = azimuth::@azimuth.add(azimuth::@azimuth.add(d, e), f)
  let right_neg = azimuth::@azimuth.add(d, azimuth::@azimuth.add(e, f))
  
  azimuth::@azimuth.assert_eq(left_neg, right_neg)
}

// 测试数学运算的交换律
test "multiplication_commutativity" {
  // 验证乘法交换律: a * b = b * a
  let a = 12
  let b = 15
  
  let result1 = azimuth::@azimuth.multiply(a, b)
  let result2 = azimuth::@azimuth.multiply(b, a)
  
  azimuth::@azimuth.assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let c = -20
  let d = 30
  
  let result3 = azimuth::@azimuth.multiply(c, d)
  let result4 = azimuth::@azimuth.multiply(d, c)
  
  azimuth::@azimuth.assert_eq(result3, result4)
}

// 测试字符串处理的边界情况
test "string_edge_cases" {
  // 测试空字符串
  azimuth::@azimuth.assert_eq_string("Hello, !", azimuth::@azimuth.greet(""))
  
  // 测试包含特殊字符的字符串
  let special_chars = "test@123#$%"
  azimuth::@azimuth.assert_eq_string("Hello, test@123#$%!", azimuth::@azimuth.greet(special_chars))
  
  // 测试包含Unicode字符的字符串
  let unicode_chars = "测试🌍"
  azimuth::@azimuth.assert_eq_string("Hello, 测试🌍!", azimuth::@azimuth.greet(unicode_chars))
}

// 测试实际应用场景：温度转换
test "temperature_conversion" {
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 简化为整数运算：F = C * 2 + 30
  
  let celsius_temp = 25
  let fahrenheit_temp = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius_temp, 2), 30)
  azimuth::@azimuth.assert_eq(80, fahrenheit_temp)
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(freezing_celsius, 2), 30)
  azimuth::@azimuth.assert_eq(30, freezing_fahrenheit)
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(boiling_celsius, 2), 30)
  azimuth::@azimuth.assert_eq(230, boiling_fahrenheit)
}

// 测试实际应用场景：订单计算
test "order_calculation" {
  let item_price = 199
  let quantity = 3
  let shipping_cost = 15
  let tax_rate = 10  // 10%
  
  // 计算商品总价
  let subtotal = azimuth::@azimuth.multiply(item_price, quantity)
  
  // 计算税费
  let tax = azimuth::@azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let total = azimuth::@azimuth.add(azimuth::@azimuth.add(subtotal, tax), shipping_cost)
  
  // 验证总价：(199 * 3) + (199 * 3 * 10 / 100) + 15 = 597 + 59 + 15 = 671
  azimuth::@azimuth.assert_eq(671, total)
}

// 测试极值处理
test "extreme_values_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法溢出
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 1))
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, max_val))
  
  // 测试最小值加法溢出
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, -1))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, min_val))
  
  // 测试最大值乘法溢出
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(max_val, 2))
  
  // 测试最小值乘法特殊情况
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, -1))
}

// 测试复杂数学运算
test "complex_mathematical_operations" {
  // 计算 (a + b) * (c + d) - e * f
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 8
  let f = 12
  
  let sum1 = azimuth::@azimuth.add(a, b)  // 30
  let sum2 = azimuth::@azimuth.add(c, d)  // 20
  let product1 = azimuth::@azimuth.multiply(sum1, sum2)  // 600
  let product2 = azimuth::@azimuth.multiply(e, f)  // 96
  let result = azimuth::@azimuth.add(product1, -product2)  // 504
  
  azimuth::@azimuth.assert_eq(504, result)
}

// 测试数组索引计算
test "array_index_calculation" {
  // 二维数组转一维数组的索引计算：index = row * width + col
  let row = 3
  let col = 4
  let width = 5
  
  let index = azimuth::@azimuth.add(azimuth::@azimuth.multiply(row, width), col)
  azimuth::@azimuth.assert_eq(19, index)
  
  // 测试边界情况
  let first_row = 0
  let first_col = 0
  let first_index = azimuth::@azimuth.add(azimuth::@azimuth.multiply(first_row, width), first_col)
  azimuth::@azimuth.assert_eq(0, first_index)
}

// 测试游戏得分计算
test "game_score_calculation" {
  let base_score = 1000
  let level_multiplier = 3
  let bonus_points = 500
  let penalty = 200
  
  // 计算最终得分
  let level_score = azimuth::@azimuth.multiply(base_score, level_multiplier)
  let total_with_bonus = azimuth::@azimuth.add(level_score, bonus_points)
  let final_score = azimuth::@azimuth.add(total_with_bonus, -penalty)
  
  azimuth::@azimuth.assert_eq(3300, final_score)
  
  // 计算排名得分（前10%额外加分）
  let top_percentage = 10
  let rank_bonus = azimuth::@azimuth.multiply(final_score, top_percentage) / 100
  let rank_total = azimuth::@azimuth.add(final_score, rank_bonus)
  
  azimuth::@azimuth.assert_eq(3630, rank_total)
}

// 测试几何计算
test "geometry_calculations" {
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  
  let rectangle_area = azimuth::@azimuth.multiply(length, width)
  azimuth::@azimuth.assert_eq(96, rectangle_area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  
  let triangle_area = azimuth::@azimuth.multiply(base, height) / 2
  azimuth::@azimuth.assert_eq(30, triangle_area)
  
  // 计算圆周长（简化计算）：2 × π × r ≈ 2 × 3 × r
  let radius = 5
  let circumference = azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(2, 3), radius)
  azimuth::@azimuth.assert_eq(30, circumference)
}
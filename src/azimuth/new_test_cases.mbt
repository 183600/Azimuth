// 新的 MoonBit 测试用例
// 这些测试用例提供额外的覆盖范围，专注于实际应用场景和边界情况

// 测试用例 1: 数字序列累加
test "number_sequence_accumulation" {
  // 测试斐波那契数列前几项的和
  // 1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  // 前10项的和 = 143
  
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  let fib7 = @azimuth.add(fib5, fib6)  // 13
  let fib8 = @azimuth.add(fib6, fib7)  // 21
  let fib9 = @azimuth.add(fib7, fib8)  // 34
  let fib10 = @azimuth.add(fib8, fib9) // 55
  
  let sum_fib = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(fib1, fib2), fib3), fib4), fib5), fib6)
  let sum_fib_rest = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(fib7, fib8), fib9), fib10), 0)
  let total_sum = @azimuth.add(sum_fib, sum_fib_rest)
  
  @azimuth.assert_eq(143, total_sum)
}

// 测试用例 2: 几何计算场景
test "geometric_calculations" {
  // 计算立方体的表面积和体积
  // 边长为6的立方体
  let side_length = 6
  
  // 一个面的面积 = 边长 * 边长
  let face_area = @azimuth.multiply(side_length, side_length)  // 36
  
  // 表面积 = 6 * 一个面的面积
  let surface_area = @azimuth.multiply(6, face_area)  // 216
  
  // 体积 = 边长 * 边长 * 边长
  let volume = @azimuth.multiply(face_area, side_length)  // 216
  
  @azimuth.assert_eq(216, surface_area)
  @azimuth.assert_eq(216, volume)
}

// 测试用例 3: 时间计算场景
test "time_calculations" {
  // 计算总秒数
  // 2小时15分钟30秒 = 总秒数
  let hours = 2
  let minutes = 15
  let seconds = 30
  
  // 小时转秒：2 * 60 * 60 = 7200
  let hours_to_seconds = @azimuth.multiply(@azimuth.multiply(hours, 60), 60)
  
  // 分钟转秒：15 * 60 = 900
  let minutes_to_seconds = @azimuth.multiply(minutes, 60)
  
  // 总秒数：7200 + 900 + 30 = 8130
  let total_seconds = @azimuth.add(@azimuth.add(hours_to_seconds, minutes_to_seconds), seconds)
  
  @azimuth.assert_eq(8130, total_seconds)
}

// 测试用例 4: 负数运算场景
test "negative_number_operations" {
  // 测试负数的各种运算
  let neg_a = -15
  let neg_b = -8
  let pos_c = 12
  
  // 负数相加：-15 + (-8) = -23
  let sum_negatives = @azimuth.add(neg_a, neg_b)
  @azimuth.assert_eq(-23, sum_negatives)
  
  // 负数与正数相加：-15 + 12 = -3
  let sum_mixed = @azimuth.add(neg_a, pos_c)
  @azimuth.assert_eq(-3, sum_mixed)
  
  // 负数相乘：-15 * -8 = 120
  let product_negatives = @azimuth.multiply(neg_a, neg_b)
  @azimuth.assert_eq(120, product_negatives)
  
  // 负数与正数相乘：-15 * 12 = -180
  let product_mixed = @azimuth.multiply(neg_a, pos_c)
  @azimuth.assert_eq(-180, product_mixed)
}

// 测试用例 5: 字符串长度和格式化
test "string_formatting_scenarios" {
  // 测试不同类型的字符串格式化
  
  // 测试包含空格的字符串
  let spaced_name = "  Alice Bob  "
  @azimuth.assert_eq_string("Hello,   Alice Bob  !", @azimuth.greet(spaced_name))
  
  // 测试包含制表符的字符串
  let tabbed_name = "Bob\tAlice"
  @azimuth.assert_eq_string("Hello, Bob\tAlice!", @azimuth.greet(tabbed_name))
  
  // 测试数字字符串
  let numeric_name = "1234567890"
  @azimuth.assert_eq_string("Hello, 1234567890!", @azimuth.greet(numeric_name))
  
  // 测试混合符号字符串
  let symbolic_name = "user_name@example.com"
  @azimuth.assert_eq_string("Hello, user_name@example.com!", @azimuth.greet(symbolic_name))
}

// 测试用例 6: 数值范围验证
test "numeric_range_validation" {
  // 测试数值范围验证
  
  let max_val = 2147483647
  let min_val = -2147483648
  let mid_val = 0
  
  // 测试极值之间的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(mid_val, @azimuth.add(max_val, @azimuth.add(min_val, 1)))
  
  // 测试极值与普通值的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  
  // 测试极值的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

// 测试用例 7: 实际商业计算场景
test "business_calculation_scenario" {
  // 模拟订单计算
  // 商品A：单价100元，数量3件
  // 商品B：单价250元，数量2件
  // 商品C：单价50元，数量5件
  // 运费：15元
  // 折扣：满500减30
  
  let price_a = 100
  let qty_a = 3
  let price_b = 250
  let qty_b = 2
  let price_c = 50
  let qty_c = 5
  let shipping = 15
  
  // 计算各商品小计
  let subtotal_a = @azimuth.multiply(price_a, qty_a)  // 300
  let subtotal_b = @azimuth.multiply(price_b, qty_b)  // 500
  let subtotal_c = @azimuth.multiply(price_c, qty_c)  // 250
  
  // 计算商品总计
  let items_total = @azimuth.add(@azimuth.add(subtotal_a, subtotal_b), subtotal_c)  // 1050
  
  // 加运费
  let total_with_shipping = @azimuth.add(items_total, shipping)  // 1065
  
  // 应用折扣（满500减30）
  let final_total = @azimuth.add(total_with_shipping, -30)  // 1035
  
  @azimuth.assert_eq(1035, final_total)
}

// 测试用例 8: 复合数学表达式
test "compound_mathematical_expressions" {
  // 测试复合数学表达式：(a + b) * (c - d) + e * f
  
  let a = 12
  let b = 8
  let c = 20
  let d = 5
  let e = 3
  let f = 7
  
  // 计算 (a + b) = 20
  let sum_ab = @azimuth.add(a, b)
  
  // 计算 (c - d) = 15
  let diff_cd = @azimuth.add(c, -d)
  
  // 计算 (a + b) * (c - d) = 20 * 15 = 300
  let product_part = @azimuth.multiply(sum_ab, diff_cd)
  
  // 计算 e * f = 3 * 7 = 21
  let product_ef = @azimuth.multiply(e, f)
  
  // 计算最终结果：300 + 21 = 321
  let final_result = @azimuth.add(product_part, product_ef)
  
  @azimuth.assert_eq(321, final_result)
}

// 测试用例 9: 边界条件和特殊值
test "boundary_conditions_and_special_values" {
  // 测试各种边界条件和特殊值
  
  // 测试连续运算的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  
  // 测试连续减法（通过加负数实现）
  let start_value = 100
  let result1 = @azimuth.add(start_value, -10)  // 90
  let result2 = @azimuth.add(result1, -20)      // 70
  let result3 = @azimuth.add(result2, -30)      // 40
  
  @azimuth.assert_eq(40, result3)
  
  // 测试连续除法（通过乘法近似）
  let dividend = 1000
  let divisor = 10
  
  // 1000 / 10 ≈ 100（使用整数除法）
  let quotient = @azimuth.multiply(dividend, 1) / divisor
  @azimuth.assert_eq(100, quotient)
}

// 测试用例 10: 字符串处理和验证
test "string_processing_and_validation" {
  // 测试字符串处理和验证场景
  
  // 测试用户名验证
  let valid_username = "user123"
  let invalid_username = ""  // 空用户名
  
  // 验证有效用户名
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet(valid_username))
  
  // 验证空用户名
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(invalid_username))
  
  // 测试包含SQL注入尝试的字符串（应该被正确处理）
  let sql_injection_attempt = "'; DROP TABLE users; --"
  @azimuth.assert_eq_string("Hello, '; DROP TABLE users; --!", @azimuth.greet(sql_injection_attempt))
  
  // 测试超长用户名
  let very_long_username = "ThisIsAVeryLongUsernameThatMightExceedTypicalDatabaseLimitsButShouldBeHandledGracefully"
  @azimuth.assert_eq_string("Hello, " + very_long_username + "!", @azimuth.greet(very_long_username))
}
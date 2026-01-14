// 标准 MoonBit 测试套件
// 包含10个标准测试用例，覆盖各种场景

// 测试用例1: 边界值组合测试
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的运算
  assert_eq(max_val, azimuth_lib::add(max_val, 0))
  assert_eq(max_val, azimuth_lib::multiply(max_val, 1))
  
  // 测试最小值的运算
  assert_eq(min_val, azimuth_lib::add(min_val, 0))
  assert_eq(min_val, azimuth_lib::multiply(min_val, 1))
  
  // 测试溢出保护
  assert_eq(max_val, azimuth_lib::add(max_val, 1))
  assert_eq(min_val, azimuth_lib::add(min_val, -1))
  assert_eq(max_val, azimuth_lib::multiply(max_val, 2))
  assert_eq(min_val, azimuth_lib::multiply(min_val, -1))
}

// 测试用例2: 字符串处理边界测试
test "string_boundary_tests" {
  // 测试空字符串
  assert_eq_string("Hello, !", azimuth_lib::greet(""))
  
  // 测试单字符字符串
  assert_eq_string("Hello, A!", azimuth_lib::greet("A"))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, 🌍!", azimuth_lib::greet("🌍"))
  assert_eq_string("Hello, \n\t!", azimuth_lib::greet("\n\t"))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringThatTestsTheBoundaryOfStringProcessingInMoonBit"
  assert_eq_string("Hello, " + long_string + "!", azimuth_lib::greet(long_string))
}

// 测试用例3: 数学运算性质验证
test "mathematical_properties" {
  let a = 12345
  let b = 67890
  let c = 11111
  
  // 加法交换律
  assert_eq(azimuth_lib::add(a, b), azimuth_lib::add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth_lib::multiply(a, b), azimuth_lib::multiply(b, a))
  
  // 分配律
  let left = azimuth_lib::multiply(a, azimuth_lib::add(b, c))
  let right = azimuth_lib::add(azimuth_lib::multiply(a, b), azimuth_lib::multiply(a, c))
  assert_eq(left, right)
  
  // 结合律
  let add_assoc1 = azimuth_lib::add(azimuth_lib::add(a, b), c)
  let add_assoc2 = azimuth_lib::add(a, azimuth_lib::add(b, c))
  assert_eq(add_assoc1, add_assoc2)
  
  let mul_assoc1 = azimuth_lib::multiply(azimuth_lib::multiply(a, b), c)
  let mul_assoc2 = azimuth_lib::multiply(a, azimuth_lib::multiply(b, c))
  assert_eq(mul_assoc1, mul_assoc2)
}

// 测试用例4: 实际应用场景 - 购物车计算
test "shopping_cart_calculations" {
  // 商品价格和数量
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 3
  let item3_price = 99
  let item3_qty = 1
  
  // 计算每种商品总价
  let total1 = azimuth_lib::multiply(item1_price, item1_qty)
  let total2 = azimuth_lib::multiply(item2_price, item2_qty)
  let total3 = azimuth_lib::multiply(item3_price, item3_qty)
  
  // 计算购物车总价
  let cart_total = azimuth_lib::add(azimuth_lib::add(total1, total2), total3)
  
  // 验证结果: 299*2 + 199*3 + 99*1 = 598 + 597 + 99 = 1294
  assert_eq(1294, cart_total)
  
  // 计算折扣后价格（假设9折）
  let discount_rate = 10
  let discount_amount = azimuth_lib::multiply(cart_total, discount_rate) / 100
  let discounted_total = azimuth_lib::add(cart_total, -discount_amount)
  
  // 验证折扣价格: 1294 - 129 = 1165
  assert_eq(1165, discounted_total)
}

// 测试用例5: 实际应用场景 - 温度转换
test "temperature_conversion" {
  // 摄氏度转华氏度（简化版: F = C × 2 + 30）
  let celsius_temps = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = azimuth_lib::add(azimuth_lib::multiply(freezing_c, 2), 30)
  assert_eq(30, freezing_f)
  
  // 测试沸点
  let boiling_c = 100
  let boiling_f = azimuth_lib::add(azimuth_lib::multiply(boiling_c, 2), 30)
  assert_eq(230, boiling_f)
  
  // 测试室温
  let room_c = 25
  let room_f = azimuth_lib::add(azimuth_lib::multiply(room_c, 2), 30)
  assert_eq(80, room_f)
}

// 测试用例6: 性能相关测试 - 大数运算
test "large_number_operations" {
  // 测试大数运算性能
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_num3 = 3000000
  
  // 大数加法
  let sum_result = azimuth_lib::add(azimuth_lib::add(large_num1, large_num2), large_num3)
  assert_eq(6000000, sum_result)
  
  // 大数乘法
  let product_result = azimuth_lib::multiply(large_num1, 3)
  assert_eq(3000000, product_result)
  
  // 复合运算
  let complex_result = azimuth_lib::multiply(
    azimuth_lib::add(large_num1, large_num2),
    azimuth_lib::add(large_num3, -large_num1)
  )
  // (1000000 + 2000000) × (3000000 - 1000000) = 3000000 × 2000000 = 6000000000000
  assert_eq(2147483647, complex_result) // 溢出保护
}

// 测试用例7: 错误处理和边界条件
test "error_handling_and_edge_cases" {
  // 测试断言函数
  azimuth_lib::assert_true(true)
  azimuth_lib::assert_false(false)
  azimuth_lib::assert_eq(42, 42)
  azimuth_lib::assert_eq_string("test", "test")
  
  // 测试除零保护（通过乘法模拟）
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  azimuth_lib::assert_true(azimuth_lib::add(max_val, 1) == max_val)
  azimuth_lib::assert_true(azimuth_lib::add(min_val, -1) == min_val)
  azimuth_lib::assert_true(azimuth_lib::multiply(max_val, 2) == max_val)
  azimuth_lib::assert_true(azimuth_lib::multiply(min_val, -1) == min_val)
}

// 测试用例8: 数列和级数计算
test "series_and_progressions" {
  // 计算等差数列前n项和: 1 + 3 + 5 + ... + 19 (前10个奇数)
  let n = 10
  let first_term = 1
  let last_term = 19
  
  // 使用公式: n × (首项 + 末项) / 2
  let arithmetic_sum = azimuth_lib::multiply(n, azimuth_lib::add(first_term, last_term)) / 2
  assert_eq(100, arithmetic_sum)
  
  // 计算等比数列前n项和: 2 + 4 + 8 + 16 + 32 (前5项)
  let geo_sum = azimuth_lib::add(azimuth_lib::add(azimuth_lib::add(azimuth_lib::add(2, 4), 8), 16), 32)
  assert_eq(62, geo_sum)
  
  // 计算平方和: 1² + 2² + 3² + 4² + 5²
  let square_sum = azimuth_lib::add(azimuth_lib::add(azimuth_lib::add(azimuth_lib::add(
    azimuth_lib::multiply(1, 1),
    azimuth_lib::multiply(2, 2)
  ), azimuth_lib::multiply(3, 3)), azimuth_lib::multiply(4, 4)), azimuth_lib::multiply(5, 5))
  assert_eq(55, square_sum)
}

// 测试用例9: 实际应用场景 - 贷款计算
test "loan_calculation" {
  // 贷款计算: 本金 × (1 + 利率 × 年数)
  let principal = 100000  // 10万元
  let annual_rate = 6     // 6%年利率
  let years = 3           // 3年
  
  // 计算总利息
  let total_interest = azimuth_lib::multiply(principal, azimuth_lib::multiply(annual_rate, years))
  
  // 计算总还款额
  let total_payment = azimuth_lib::add(principal, total_interest)
  
  // 验证结果: 100000 + 100000 × 6 × 3 = 100000 + 1800000 = 1900000
  assert_eq(1900000, total_payment)
  
  // 计算月供（简化版）
  let monthly_payment = total_payment / azimuth_lib::multiply(years, 12)
  assert_eq(52777, monthly_payment) // 整数除法
}

// 测试用例10: 综合应用场景
test "comprehensive_application" {
  // 复杂应用场景: 计算订单总价，包含商品、税费、运费和折扣
  
  // 商品信息
  let products = [(100, 2), (200, 3), (50, 1)] // (价格, 数量)
  
  // 计算商品总价
  let product1_total = azimuth_lib::multiply(100, 2)  // 200
  let product2_total = azimuth_lib::multiply(200, 3)  // 600
  let product3_total = azimuth_lib::multiply(50, 1)   // 50
  let subtotal = azimuth_lib::add(azimuth_lib::add(product1_total, product2_total), product3_total)
  
  assert_eq(850, subtotal)
  
  // 计算折扣（满500减50）
  let discount = if subtotal > 500 { 50 } else { 0 }
  let discounted_total = azimuth_lib::add(subtotal, -discount)
  
  assert_eq(800, discounted_total)
  
  // 计算税费（8%）
  let tax_rate = 8
  let tax_amount = azimuth_lib::multiply(discounted_total, tax_rate) / 100
  let total_with_tax = azimuth_lib::add(discounted_total, tax_amount)
  
  assert_eq(864, total_with_tax)
  
  // 加运费
  let shipping = 20
  let final_total = azimuth_lib::add(total_with_tax, shipping)
  
  assert_eq(884, final_total)
  
  // 生成订单摘要
  let order_summary = azimuth_lib::greet("Order #12345")
  assert_eq_string("Hello, Order #12345!", order_summary)
}
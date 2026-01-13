// MoonBit 新测试用例示例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, azimuth::add(3, 7))
  assert_eq(21, azimuth::multiply(3, 7))
  assert_eq(0, azimuth::add(0, 0))
  assert_eq(0, azimuth::multiply(0, 123))
}

test "string_greeting_functions" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, 12345!", azimuth::greet("12345"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  // (a + b) * c - d
  let a = 15
  let b = 10
  let c = 4
  let d = 20
  
  let sum = azimuth::add(a, b)
  let product = azimuth::multiply(sum, c)
  let result = azimuth::add(product, -d)
  
  assert_eq(80, result)  // (15 + 10) * 4 - 20 = 100 - 20 = 80
}

test "boundary_value_testing" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(max_val, azimuth::multiply(max_val, 2))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
}

test "sequential_calculation_patterns" {
  // 测试连续计算模式
  // 1 + 2 + 3 + 4 + 5 = 15
  let step1 = azimuth::add(1, 2)
  let step2 = azimuth::add(step1, 3)
  let step3 = azimuth::add(step2, 4)
  let final_sum = azimuth::add(step3, 5)
  
  assert_eq(15, final_sum)
  
  // 2 * 3 * 4 * 5 = 120
  let product1 = azimuth::multiply(2, 3)
  let product2 = azimuth::multiply(product1, 4)
  let final_product = azimuth::multiply(product2, 5)
  
  assert_eq(120, final_product)
}

test "real_world_application_scenarios" {
  // 测试真实世界应用场景
  // 购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let shipping_fee = 20
  
  let subtotal1 = azimuth::multiply(item1_price, item1_quantity)
  let subtotal2 = azimuth::multiply(item2_price, item2_quantity)
  let items_total = azimuth::add(subtotal1, subtotal2)
  let final_total = azimuth::add(items_total, shipping_fee)
  
  assert_eq(1394, final_total)  // 299*2 + 199*3 + 20 = 598 + 597 + 20 = 1394
  
  // 温度转换计算
  let celsius_temp = 37
  let fahrenheit_temp = azimuth::add(azimuth::multiply(celsius_temp, 2), 30)
  
  assert_eq(104, fahrenheit_temp)  // 37 * 2 + 30 = 104
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保边界值运算不会出错
  assert_eq(-1, azimuth::add(max_val, min_val))
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(0, azimuth::multiply(min_val, 0))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 等差数列：3 + 6 + 9 + 12 = 30
  let term1 = 3
  let term2 = azimuth::add(term1, 3)
  let term3 = azimuth::add(term2, 3)
  let term4 = azimuth::add(term3, 3)
  
  let arithmetic_sum = azimuth::add(azimuth::add(term1, term2), azimuth::add(term3, term4))
  assert_eq(30, arithmetic_sum)
  
  // 等比数列：3 + 6 + 12 + 24 = 45
  let geo_term1 = 3
  let geo_term2 = azimuth::multiply(geo_term1, 2)
  let geo_term3 = azimuth::multiply(geo_term2, 2)
  let geo_term4 = azimuth::multiply(geo_term3, 2)
  
  let geometric_sum = azimuth::add(azimuth::add(geo_term1, geo_term2), azimuth::add(geo_term3, geo_term4))
  assert_eq(45, geometric_sum)
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello,   !", azimuth::greet("  "))
  assert_eq_string("Hello, A!", azimuth::greet("A"))
  assert_eq_string("Hello, verylongname!", azimuth::greet("verylongname"))
  assert_eq_string("Hello, @#$%^&*()!", azimuth::greet("@#$%^&*()"))
}

test "advanced_calculation_scenarios" {
  // 测试高级计算场景
  // 计算矩形周长：2 * (长 + 宽)
  let length = 12
  let width = 8
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  assert_eq(40, perimeter)  // 2 * (12 + 8) = 40
  
  // 计算平均数：(a + b + c) / 3
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let sum = azimuth::add(azimuth::add(num1, num2), num3)
  let average = sum / 3
  assert_eq(90, average)  // (85 + 90 + 95) / 3 = 270 / 3 = 90
}
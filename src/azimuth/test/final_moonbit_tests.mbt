// 最终的 MoonBit 测试用例示例

test "basic_arithmetic_test" {
  // 测试基本算术运算
  assert_eq(15, azimuth::add(8, 7))
  assert_eq(30, azimuth::multiply(5, 6))
  assert_eq(0, azimuth::add(0, 0))
  assert_eq(0, azimuth::multiply(0, 999))
}

test "string_greeting_test" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
}

test "complex_expression_test" {
  // 测试复杂表达式
  // (a + b) * c - d
  let a = 12
  let b = 8
  let c = 3
  let d = 15
  
  let sum = azimuth::add(a, b)
  let product = azimuth::multiply(sum, c)
  let result = azimuth::add(product, -d)
  
  assert_eq(45, result)  // (12 + 8) * 3 - 15 = 60 - 15 = 45
}

test "boundary_value_test" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(max_val, azimuth::multiply(max_val, 2))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
}

test "sequential_operations_test" {
  // 测试连续运算
  // 2 + 4 + 6 + 8 = 20
  let step1 = azimuth::add(2, 4)
  let step2 = azimuth::add(step1, 6)
  let final_sum = azimuth::add(step2, 8)
  
  assert_eq(20, final_sum)
  
  // 3 * 4 * 5 = 60
  let product1 = azimuth::multiply(3, 4)
  let final_product = azimuth::multiply(product1, 5)
  
  assert_eq(60, final_product)
}

test "shopping_cart_test" {
  // 测试购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 15
  
  let subtotal1 = azimuth::multiply(item1_price, item1_quantity)
  let subtotal2 = azimuth::multiply(item2_price, item2_quantity)
  let items_total = azimuth::add(subtotal1, subtotal2)
  let final_total = azimuth::add(items_total, shipping)
  
  assert_eq(712, final_total)  // 199*2 + 299*1 + 15 = 398 + 299 + 15 = 712
}

test "temperature_conversion_test" {
  // 测试温度转换
  // 摄氏度转华氏度（简化版）：F = C * 2 + 30
  let celsius_temp = 25
  let fahrenheit_temp = azimuth::add(azimuth::multiply(celsius_temp, 2), 30)
  
  assert_eq(80, fahrenheit_temp)  // 25 * 2 + 30 = 80
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth::add(azimuth::multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
}

test "mathematical_sequence_test" {
  // 测试数学序列
  // 等差数列：4 + 7 + 10 + 13 = 34
  let term1 = 4
  let term2 = azimuth::add(term1, 3)
  let term3 = azimuth::add(term2, 3)
  let term4 = azimuth::add(term3, 3)
  
  let sum = azimuth::add(azimuth::add(term1, term2), azimuth::add(term3, term4))
  assert_eq(34, sum)
  
  // 等比数列：2 + 4 + 8 + 16 = 30
  let geo_term1 = 2
  let geo_term2 = azimuth::multiply(geo_term1, 2)
  let geo_term3 = azimuth::multiply(geo_term2, 2)
  let geo_term4 = azimuth::multiply(geo_term3, 2)
  
  let geo_sum = azimuth::add(azimuth::add(geo_term1, geo_term2), azimuth::add(geo_term3, geo_term4))
  assert_eq(30, geo_sum)
}
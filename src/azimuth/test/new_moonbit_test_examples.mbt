// 新的 MoonBit 测试用例示例
test "mathematical_operations_test" {
  // 测试基本数学运算
  @azimuth.assert_eq(15, azimuth::@azimuth.add(7, 8))
  @azimuth.assert_eq(42, azimuth::@azimuth.multiply(6, 7))
  @azimuth.assert_eq(0, azimuth::@azimuth.add(0, 0))
  @azimuth.assert_eq(0, azimuth::@azimuth.multiply(0, 123))
}
test "string_operations_test" {
  // 测试字符串操作
  @azimuth.assert_eq_string("Hello, MoonBit!", azimuth::@azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", azimuth::@azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 12345!", azimuth::@azimuth.greet("12345"))
}
test "complex_calculations_test" {
  // 测试复杂计算
  // (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum = azimuth::@azimuth.add(a, b)
  let product = azimuth::@azimuth.multiply(sum, c)
  let result = azimuth::@azimuth.add(product, -d)
  
  @azimuth.assert_eq(38, result)  // (10 + 5) * 3 - 7 = 45 - 7 = 38
}
test "boundary_values_test" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  @azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, -1))
  @azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, -1))
}
test "sequential_operations_test" {
  // 测试连续运算
  // 1 + 2 + 3 + 4 = 10
  let step1 = azimuth::@azimuth.add(1, 2)
  let step2 = azimuth::@azimuth.add(step1, 3)
  let final_sum = azimuth::@azimuth.add(step2, 4)
  
  @azimuth.assert_eq(10, final_sum)
  
  // 2 * 3 * 4 = 24
  let product1 = azimuth::@azimuth.multiply(2, 3)
  let final_product = azimuth::@azimuth.multiply(product1, 4)
  
  @azimuth.assert_eq(24, final_product)
}
test "real_world_scenarios_test" {
  // 测试真实世界场景
  // 购物车计算
  let item_price = 99
  let quantity = 5
  let shipping = 10
  
  let subtotal = azimuth::@azimuth.multiply(item_price, quantity)
  let total = azimuth::@azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(505, total)  // 99 * 5 + 10 = 495 + 10 = 505
  
  // 温度转换（简化版）
  let celsius = 30
  let fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(90, fahrenheit)  // 30 * 2 + 30 = 90
}
test "error_handling_test" {
  // 测试错误处理场景
  // 确保边界值被正确处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 这些应该不会溢出
  @azimuth.assert_eq(-1, azimuth::@azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, azimuth::@azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, azimuth::@azimuth.multiply(min_val, 0))
}
test "mathematical_sequences_test" {
  // 测试数学序列
  // 等差数列：2 + 4 + 6 + 8 = 20
  let term1 = 2
  let term2 = azimuth::@azimuth.add(term1, 2)
  let term3 = azimuth::@azimuth.add(term2, 2)
  let term4 = azimuth::@azimuth.add(term3, 2)
  
  let sum = azimuth::@azimuth.add(azimuth::@azimuth.add(term1, term2), azimuth::@azimuth.add(term3, term4))
  @azimuth.assert_eq(20, sum)
}
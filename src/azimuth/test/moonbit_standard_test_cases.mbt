// MoonBit 标准测试用例
// 测试 azimuth 包的核心功能
test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  azimuth::@azimuth.assert_eq(5, azimuth::@azimuth.add(2, 3))
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.add(0, 0))
  azimuth::@azimuth.assert_eq(-5, azimuth::@azimuth.add(-2, -3))
  azimuth::@azimuth.assert_eq(1, azimuth::@azimuth.add(10, -9))
}
test "add_function_boundary_values" {
  // 测试 add 函数的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 1))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, -1))
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.add(max_val, min_val))
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, max_val))
}
test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  azimuth::@azimuth.assert_eq(6, azimuth::@azimuth.multiply(2, 3))
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(5, 0))
  azimuth::@azimuth.assert_eq(-6, azimuth::@azimuth.multiply(2, -3))
  azimuth::@azimuth.assert_eq(1, azimuth::@azimuth.multiply(1, 1))
}
test "multiply_function_boundary_values" {
  // 测试 multiply 函数的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, -1))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, 1))
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(max_val, 1))
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(max_val, 0))
}
test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  azimuth::@azimuth.assert_eq_string("Hello, World!", azimuth::@azimuth.greet("World"))
  azimuth::@azimuth.assert_eq_string("Hello, !", azimuth::@azimuth.greet(""))
  azimuth::@azimuth.assert_eq_string("Hello, 123!", azimuth::@azimuth.greet("123"))
  azimuth::@azimuth.assert_eq_string("Hello, 测试!", azimuth::@azimuth.greet("测试"))
}
test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形的面积和周长
  let length = 10
  let width = 5
  
  let area = azimuth::@azimuth.multiply(length, width)
  let perimeter = azimuth::@azimuth.add(azimuth::@azimuth.multiply(2, length), azimuth::@azimuth.multiply(2, width))
  
  azimuth::@azimuth.assert_eq(50, area)
  azimuth::@azimuth.assert_eq(30, perimeter)
}
test "financial_calculation_example" {
  // 测试金融计算示例
  // 计算折扣后的价格
  let original_price = 1000
  let discount_percentage = 20
  let discount_amount = azimuth::@azimuth.multiply(original_price, discount_percentage) / 100
  let final_price = azimuth::@azimuth.add(original_price, -discount_amount)
  
  azimuth::@azimuth.assert_eq(800, final_price)
}
test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  let special_chars = "!@#$%^&*()"
  let unicode_chars = "🌍🎉"
  let mixed_input = "Test123!@#"
  
  azimuth::@azimuth.assert_eq_string("Hello, !@#$%^&*()!", azimuth::@azimuth.greet(special_chars))
  azimuth::@azimuth.assert_eq_string("Hello, 🌍🎉!", azimuth::@azimuth.greet(unicode_chars))
  azimuth::@azimuth.assert_eq_string("Hello, Test123!@#!", azimuth::@azimuth.greet(mixed_input))
}
test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算等差数列的和：1 + 2 + 3 + 4 + 5 = 15
  let n = 5
  let sum = azimuth::@azimuth.multiply(n, azimuth::@azimuth.add(1, n)) / 2
  azimuth::@azimuth.assert_eq(15, sum)
  
  // 计算等比数列的和：2 + 4 + 8 + 16 = 30
  let geo_sum = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(2, 4), 8), 16)
  azimuth::@azimuth.assert_eq(30, geo_sum)
}
test "real_world_application_scenario" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let tax_rate = 10  // 10%
  let shipping = 20
  
  // 计算商品总价
  let subtotal = azimuth::@azimuth.add(azimuth::@azimuth.multiply(item1_price, item1_quantity), azimuth::@azimuth.multiply(item2_price, item2_quantity))
  // 计算税费
  let tax = azimuth::@azimuth.multiply(subtotal, tax_rate) / 100
  // 计算最终总价
  let total = azimuth::@azimuth.add(azimuth::@azimuth.add(subtotal, tax), shipping)
  
  azimuth::@azimuth.assert_eq(786, total)  // (199*2 + 299*1) + 10% + 20 = 697 + 69 + 20 = 786
}
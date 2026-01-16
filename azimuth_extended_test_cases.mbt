// Azimuth 项目扩展测试用例
// 覆盖更多边界条件和实际应用场景

test "add_large_numbers" {
  // 测试大数加法
  assert_eq(azimuth::add(999999, 1), 1000000)
  assert_eq(azimuth::add(500000, 500000), 1000000)
  assert_eq(azimuth::add(-999999, 1), -999998)
}

test "add_boundary_values" {
  // 测试边界值加法
  assert_eq(azimuth::add(2147483647, 0), 2147483647)
  assert_eq(azimuth::add(-2147483648, 0), -2147483648)
  assert_eq(azimuth::add(2147483647, -2147483647), 0)
}

test "multiply_large_numbers" {
  // 测试大数乘法
  assert_eq(azimuth::multiply(10000, 1000), 10000000)
  assert_eq(azimuth::multiply(12345, 10), 123450)
  assert_eq(azimuth::multiply(-10000, 1000), -10000000)
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  assert_eq(azimuth::multiply(2147483647, 1), 2147483647)
  assert_eq(azimuth::multiply(-2147483648, 1), -2147483648)
  assert_eq(azimuth::multiply(2147483647, 0), 0)
}

test "greet_various_string_formats" {
  // 测试各种字符串格式的问候
  assert_eq(azimuth::greet("123"), "Hello, 123!")
  assert_eq(azimuth::greet("!@#$%"), "Hello, !@#$%!")
  assert_eq(azimuth::greet("mixedCASE123"), "Hello, mixedCASE123!")
  assert_eq(azimuth::greet("   leading"), "Hello,    leading!")
  assert_eq(azimuth::greet("trailing   "), "Hello, trailing   !")
}

test "mathematical_series" {
  // 测试数学级数计算
  
  // 等差数列求和: 1+3+5+7+9 = 25
  let arithmetic_series = azimuth::add(azimuth::add(azimuth::add(azimuth::add(1, 3), 5), 7), 9)
  assert_eq(25, arithmetic_series)
  
  // 等比数列求和: 2+4+8+16 = 30
  let geometric_series = azimuth::add(azimuth::add(azimuth::add(2, 4), 8), 16)
  assert_eq(30, geometric_series)
}

test "real_world_calculations" {
  // 测试实际世界计算场景
  
  // 计算购物折扣: 原价 × 折扣率
  let original_price = 120
  let discount_rate = 8  // 80% 折扣，用整数表示
  let discounted_price = azimuth::multiply(original_price, discount_rate)
  assert_eq(960, discounted_price)
  
  // 计算团队总工时: 人数 × 工时
  let team_size = 15
  let hours_per_person = 8
  let total_hours = azimuth::multiply(team_size, hours_per_person)
  assert_eq(120, total_hours)
}

test "complex_business_logic" {
  // 测试复杂业务逻辑
  
  // 计算利润: 收入 - 成本
  let revenue = 5000
  let cost = 3000
  let profit = azimuth::add(revenue, -cost)
  assert_eq(2000, profit)
  
  // 计算税费: 利润 × 税率
  let tax_rate = 2  // 20% 税率，用整数表示
  let tax = azimuth::multiply(profit, tax_rate)
  assert_eq(4000, tax)
  
  // 计算净利润: 利润 - 税费
  let net_profit = azimuth::add(profit, -tax)
  assert_eq(-2000, net_profit)
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  
  // 测试非常长的名称
  let long_name = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ"
  assert_eq(azimuth::greet(long_name), "Hello, " + long_name + "!")
  
  // 测试包含特殊字符的名称
  let special_chars = "中文测试🌟emoji"
  assert_eq(azimuth::greet(special_chars), "Hello, " + special_chars + "!")
}

test "error_prone_scenarios" {
  // 测试容易出错的场景
  
  // 连续加法运算
  let result = azimuth::add(azimuth::add(azimuth::add(10, 20), 30), 40)
  assert_eq(100, result)
  
  // 连续乘法运算
  let product = azimuth::multiply(azimuth::multiply(azimuth::multiply(2, 3), 4), 5)
  assert_eq(120, product)
  
  // 混合运算
  let mixed = azimuth::add(azimuth::multiply(10, 5), azimuth::multiply(3, 4))
  assert_eq(62, mixed)
}
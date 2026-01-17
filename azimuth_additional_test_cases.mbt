// Azimuth 项目额外测试用例
// 覆盖更多边界情况和实际应用场景

test "extreme_values_addition" {
  // 测试极大值和极小值加法
  assert_eq(azimuth::add(2147483647, 0), 2147483647)
  assert_eq(azimuth::add(-2147483648, 0), -2147483648)
  assert_eq(azimuth::add(1000000, 1000000), 2000000)
  assert_eq(azimuth::add(-1000000, -1000000), -2000000)
}

test "extreme_values_multiplication" {
  // 测试极大值和极小值乘法
  assert_eq(azimuth::multiply(46340, 46340), 2147395956)
  assert_eq(azimuth::multiply(-46340, 46340), -2147395956)
  assert_eq(azimuth::multiply(1000, 1000), 1000000)
  assert_eq(azimuth::multiply(-1000, 1000), -1000000)
}

test "greet_edge_cases" {
  // 测试更多字符串边界情况
  assert_eq(azimuth::greet("a"), "Hello, a!")
  assert_eq(azimuth::greet("A"), "Hello, A!")
  assert_eq(azimuth::greet("123"), "Hello, 123!")
  assert_eq(azimuth::greet("!@#$"), "Hello, !@#$!")
}

test "mathematical_series" {
  // 测试数学级数计算
  
  // 等差数列求和: 2 + 4 + 6 + 8 + 10 = 30
  let arithmetic_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(2, 4), 6), 8), 10)
  assert_eq(30, arithmetic_sum)
  
  // 等比数列求和: 2 + 4 + 8 + 16 = 30
  let geometric_sum = azimuth::add(azimuth::add(azimuth::add(2, 4), 8), 16)
  assert_eq(30, geometric_sum)
}

test "business_calculations" {
  // 测试商业计算场景
  
  // 计算折扣价: 原价 * 折扣率
  let original_price = 200
  let discount_rate = 8  // 80% 折扣，用整数表示
  let discounted_price = azimuth::multiply(original_price, discount_rate)
  assert_eq(1600, discounted_price)  // 实际应用中需要除以100
  
  // 计算总价: 单价 * 数量 + 运费
  let unit_price = 50
  let quantity = 3
  let shipping = 20
  let total = azimuth::add(azimuth::multiply(unit_price, quantity), shipping)
  assert_eq(170, total)
}

test "scientific_calculations" {
  // 测试科学计算场景
  
  // 计算正方形面积: 边长 * 边长
  let side_length = 7
  let square_area = azimuth::multiply(side_length, side_length)
  assert_eq(49, square_area)
  
  // 计算立方体表面积: 6 * 边长 * 边长
  let cube_surface_area = azimuth::multiply(6, square_area)
  assert_eq(294, cube_surface_area)
}

test "pattern_calculations" {
  // 测试模式计算
  
  // 斐波那契数列前几项的和: 1 + 1 + 2 + 3 + 5 = 12
  let fibonacci_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(1, 1), 2), 3), 5)
  assert_eq(12, fibonacci_sum)
  
  // 平方数序列: 1² + 2² + 3² + 4² = 1 + 4 + 9 + 16 = 30
  let square_sum = azimuth::add(azimuth::add(azimuth::add(1, 4), 9), 16)
  assert_eq(30, square_sum)
}

test "complex_greeting_scenarios" {
  // 测试复杂问候场景
  
  // 测试包含各种特殊字符的名称
  assert_eq(azimuth::greet("John-Doe_O'Reilly"), "Hello, John-Doe_O'Reilly!")
  assert_eq(azimuth::greet("中文测试"), "Hello, 中文测试!")
  assert_eq(azimuth::greet("España"), "Hello, España!")
  assert_eq(azimuth::greet("Москва"), "Hello, Москва!")
}

test "algebraic_expressions" {
  // 测试代数表达式
  
  // 计算 (x + y)² = x² + 2xy + y²
  let x = 3
  let y = 4
  let left_side = azimuth::multiply(azimuth::add(x, y), azimuth::add(x, y))
  let right_side = azimuth::add(azimuth::add(azimuth::multiply(x, x), azimuth::multiply(2, azimuth::multiply(x, y))), azimuth::multiply(y, y))
  assert_eq(left_side, right_side)
  
  // 计算 (x - y)² = x² - 2xy + y²
  let diff_left = azimuth::multiply(azimuth::add(x, -y), azimuth::add(x, -y))
  let diff_right = azimuth::add(azimuth::add(azimuth::multiply(x, x), azimuth::multiply(-2, azimuth::multiply(x, y))), azimuth::multiply(y, y))
  assert_eq(diff_left, diff_right)
}

test "performance_oriented_calculations" {
  // 测试性能相关的计算
  
  // 大量重复计算
  let base = 5
  let result1 = azimuth::add(base, azimuth::add(base, azimuth::add(base, azimuth::add(base, base))))
  assert_eq(25, result1)
  
  // 嵌套乘法计算
  let nested_result = azimuth::multiply(azimuth::multiply(2, 3), azimuth::multiply(4, 5))
  assert_eq(120, nested_result)
  
  // 混合运算链
  let chain_result = azimuth::add(azimuth::multiply(10, 2), azimuth::multiply(azimuth::add(5, 3), 4))
  assert_eq(52, chain_result)
}
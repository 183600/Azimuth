// Azimuth 项目新测试场景
// 使用标准 MoonBit 测试语法，包含不超过10个测试用例

test "add_zero_values" {
  // 测试加零操作
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::add(42, 0), 42)
  assert_eq(azimuth::add(0, 42), 42)
  assert_eq(azimuth::add(-100, 0), -100)
  assert_eq(azimuth::add(0, -100), -100)
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(azimuth::add(-5, -10), -15)
  assert_eq(azimuth::add(-100, 50), -50)
  assert_eq(azimuth::add(75, -25), 50)
  assert_eq(azimuth::add(-1000, 1000), 0)
}

test "multiply_zero_values" {
  // 测试乘零操作
  assert_eq(azimuth::multiply(0, 0), 0)
  assert_eq(azimuth::multiply(42, 0), 0)
  assert_eq(azimuth::multiply(0, 42), 0)
  assert_eq(azimuth::multiply(-100, 0), 0)
  assert_eq(azimuth::multiply(0, -100), 0)
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(azimuth::multiply(-5, -10), 50)
  assert_eq(azimuth::multiply(-5, 10), -50)
  assert_eq(azimuth::multiply(5, -10), -50)
  assert_eq(azimuth::multiply(-1, 100), -100)
}

test "greet_special_characters" {
  // 测试特殊字符问候
  assert_eq(azimuth::greet("123"), "Hello, 123!")
  assert_eq(azimuth::greet("user@domain.com"), "Hello, user@domain.com!")
  assert_eq(azimuth::greet("Café"), "Hello, Café!")
  assert_eq(azimuth::greet("🌟"), "Hello, 🌟!")
}

test "combined_operations_business_logic" {
  // 测试业务逻辑组合运算
  // 计算总价: (单价 * 数量) + 税费 - 折扣
  let unit_price = 100
  let quantity = 5
  let tax = 50
  let discount = 30
  
  let subtotal = azimuth::multiply(unit_price, quantity)
  let total = azimuth::add(azimuth::add(subtotal, tax), -discount)
  assert_eq(total, 520)
  
  // 计算利润: 收入 - 成本
  let revenue = 1000
  let cost = 750
  let profit = azimuth::add(revenue, -cost)
  assert_eq(profit, 250)
}

test "combined_operations_scientific_calculations" {
  // 测试科学计算组合运算
  // 计算平均数
  let a = 85
  let b = 90
  let c = 95
  let sum = azimuth::add(azimuth::add(a, b), c)
  let average = sum / 3
  assert_eq(average, 90)
  
  // 计算百分比
  let part = 25
  let whole = 100
  let percentage = azimuth::multiply(part, 100) / whole
  assert_eq(percentage, 25)
}

test "edge_case_large_number_operations" {
  // 测试大数运算边界情况
  let large_num = 1000000
  
  // 大数加法
  assert_eq(azimuth::add(large_num, large_num), 2000000)
  
  // 大数乘法（结果在合理范围内）
  assert_eq(azimuth::multiply(large_num, 2), 2000000)
  assert_eq(azimuth::multiply(large_num, 10), 10000000)
}

test "sequential_operations" {
  // 测试连续运算
  let start_value = 10
  
  // 连续加法
  let result1 = azimuth::add(start_value, 5)
  let result2 = azimuth::add(result1, 10)
  let result3 = azimuth::add(result2, 15)
  assert_eq(result3, 40)
  
  // 连续乘法
  let mult_result1 = azimuth::multiply(2, 3)
  let mult_result2 = azimuth::multiply(mult_result1, 4)
  let mult_result3 = azimuth::multiply(mult_result2, 5)
  assert_eq(mult_result3, 120)
}

test "mixed_arithmetic_operations" {
  // 测试混合算术运算
  let x = 10
  let y = 5
  let z = 2
  
  // (x + y) * z
  let result1 = azimuth::multiply(azimuth::add(x, y), z)
  assert_eq(result1, 30)
  
  // x * y + z
  let result2 = azimuth::add(azimuth::multiply(x, y), z)
  assert_eq(result2, 52)
  
  // (x * z) + (y * z)
  let result3 = azimuth::add(azimuth::multiply(x, z), azimuth::multiply(y, z))
  assert_eq(result3, 30)
}
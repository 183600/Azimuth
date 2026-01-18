// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法，包含10个测试用例

// 导入 azimuth 模块
use azimuth

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(8, azimuth.add(5, 3))
  assert_eq(0, azimuth.add(-5, 5))
  assert_eq(-8, azimuth.add(-5, -3))
  
  assert_eq(15, azimuth.multiply(3, 5))
  assert_eq(-15, azimuth.multiply(-3, 5))
  assert_eq(15, azimuth.multiply(-3, -5))
  assert_eq(0, azimuth.multiply(5, 0))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
  assert_eq_string("Hello, 🌍!", azimuth.greet("🌍"))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值加法
  assert_eq(max_val, azimuth.add(max_val, 0))
  assert_eq(min_val, azimuth.add(min_val, 0))
  assert_eq(-1, azimuth.add(max_val, min_val))
  
  // 测试边界值乘法
  assert_eq(0, azimuth.multiply(max_val, 0))
  assert_eq(0, azimuth.multiply(min_val, 0))
  assert_eq(max_val, azimuth.multiply(max_val, 1))
  assert_eq(min_val, azimuth.multiply(min_val, 1))
}

test "overflow_protection" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(max_val, azimuth.add(max_val, max_val))
  assert_eq(min_val, azimuth.add(min_val, -1))
  assert_eq(min_val, azimuth.add(min_val, min_val))
  
  // 测试乘法溢出保护
  assert_eq(max_val, azimuth.multiply(max_val, 2))
  assert_eq(min_val, azimuth.multiply(min_val, -1))
  assert_eq(min_val, azimuth.multiply(min_val, 2))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 7
  
  // 测试加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 测试乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 测试分配律
  let c = 5
  assert_eq(
    azimuth.multiply(a, azimuth.add(b, c)),
    azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  )
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形周长：2 × (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  assert_eq(30, perimeter)
  
  // 计算矩形面积：长 × 宽
  let area = azimuth.multiply(length, width)
  assert_eq(50, area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 8
  let height = 6
  let triangle_area = azimuth.multiply(base, height) / 2
  assert_eq(24, triangle_area)
}

test "financial_calculations" {
  // 测试金融计算场景
  // 简单利息计算：本金 × 利率 × 年数
  let principal = 10000
  let rate = 5
  let years = 3
  let interest = azimuth.multiply(principal, azimuth.multiply(rate, years))
  assert_eq(150000, interest)
  
  // 本息和：本金 + 利息
  let total = azimuth.add(principal, interest)
  assert_eq(160000, total)
  
  // 复利计算（简化）：本金 × (1 + 利率 × 年数)
  let compound_total = azimuth.multiply(principal, azimuth.add(1, azimuth.multiply(rate, years)))
  assert_eq(160000, compound_total)
}

test "assertion_functionality" {
  // 测试断言功能
  // 测试 assert_true 和 assert_false
  azimuth.assert_true(5 > 3)
  azimuth.assert_false(5 < 3)
  azimuth.assert_true(5 == 5)
  azimuth.assert_false(5 == 3)
  
  // 测试 assert_eq
  azimuth.assert_eq(10, azimuth.add(6, 4))
  azimuth.assert_eq(25, azimuth.multiply(5, 5))
  
  // 测试 assert_eq_string
  azimuth.assert_eq_string("Hello, Test!", azimuth.greet("Test"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
}

test "real_world_applications" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  let total1 = azimuth.multiply(item1_price, item1_quantity)
  let total2 = azimuth.multiply(item2_price, item2_quantity)
  let subtotal = azimuth.add(total1, total2)
  let final_total = azimuth.add(subtotal, shipping)
  
  assert_eq(717, final_total)
  
  // 温度转换（简化版）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "error_handling_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值的处理
  assert_eq(0, azimuth.multiply(0, max_val))
  assert_eq(0, azimuth.multiply(0, min_val))
  assert_eq(max_val, azimuth.multiply(max_val, 1))
  assert_eq(min_val, azimuth.multiply(min_val, 1))
  assert_eq(min_val, azimuth.multiply(min_val, -1))
  
  // 测试连续运算的稳定性
  let chain_result = azimuth.add(
    azimuth.multiply(5, 10),
    azimuth.multiply(3, 4)
  )
  assert_eq(62, chain_result)
  
  // 测试字符串边界情况
  assert_eq_string("Hello, \"John\"!", azimuth.greet("\"John\""))
  assert_eq_string("Hello,   !", azimuth.greet("  "))
}
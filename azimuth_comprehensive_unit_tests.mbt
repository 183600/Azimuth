// Azimuth 项目综合单元测试
// 使用标准 MoonBit 测试语法，覆盖核心功能

// 导入必要的模块
use azimuth

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, azimuth.add(2, 3))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(-5, azimuth.add(-2, -3))
  assert_eq(10, azimuth.add(15, -5))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, azimuth.multiply(2, 3))
  assert_eq(0, azimuth.multiply(0, 5))
  assert_eq(-6, azimuth.multiply(-2, 3))
  assert_eq(15, azimuth.multiply(-3, -5))
}

test "greet_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
}

test "add_boundary_values" {
  // 测试加法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(max_val, azimuth.add(max_val, max_val))
  
  // 测试最小值边界
  assert_eq(min_val, azimuth.add(min_val, -1))
  assert_eq(min_val, azimuth.add(min_val, min_val))
  
  // 测试最大值与最小值相加
  assert_eq(-1, azimuth.add(max_val, min_val))
}

test "multiply_boundary_values" {
  // 测试乘法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, azimuth.multiply(max_val, 1))
  assert_eq(max_val, azimuth.multiply(max_val, 0))
  
  // 测试最小值边界
  assert_eq(min_val, azimuth.multiply(min_val, 1))
  assert_eq(0, azimuth.multiply(min_val, 0))
  
  // 测试特殊值
  assert_eq(min_val, azimuth.multiply(min_val, -1))
}

test "complex_calculations" {
  // 测试复合计算
  let a = 10
  let b = 20
  let c = 5
  
  // 测试 (a + b) * c
  let sum = azimuth.add(a, b)
  let product = azimuth.multiply(sum, c)
  assert_eq(150, product)
  
  // 测试 a * b + c
  let product2 = azimuth.multiply(a, b)
  let sum2 = azimuth.add(product2, c)
  assert_eq(205, sum2)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 测试空字符串
  assert_eq_string("Hello, !", azimuth.greet(""))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @#$%!", azimuth.greet("@#$%"))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", azimuth.greet("John Doe"))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  assert_eq_string("Hello, ThisIsAVeryLongStringForTestingPurposes!", azimuth.greet(long_string))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 测试加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 测试乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 测试分配律
  let c = 3
  let left = azimuth.multiply(a, azimuth.add(b, c))
  let right = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  assert_eq(left, right)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let total = azimuth.multiply(item_price, quantity)
  assert_eq(597, total)
  
  // 温度转换（简化版）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 简单利息计算
  let principal = 1000
  let rate = 5
  let years = 3
  let interest = azimuth.multiply(principal, azimuth.multiply(rate, years))
  let total_amount = azimuth.add(principal, interest)
  assert_eq(16000, total_amount)
}

test "error_handling" {
  // 测试错误处理
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_true(azimuth.add(max_val, 1) == max_val)
  assert_true(azimuth.add(min_val, -1) == min_val)
  assert_true(azimuth.multiply(max_val, 2) == max_val)
  assert_true(azimuth.multiply(min_val, -1) == min_val)
}
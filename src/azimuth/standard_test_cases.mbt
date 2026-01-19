// 标准 MoonBit 测试用例
// 测试 azimuth 项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, @azimuth.add(7, 3))
  assert_eq(-2, @azimuth.add(5, -7))
  assert_eq(15, @azimuth.multiply(3, 5))
  assert_eq(-12, @azimuth.multiply(-3, 4))
  assert_eq(0, @azimuth.multiply(0, 100))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", @azimuth.greet(""))
  assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  assert_eq(@azimuth.multiply(a, @azimuth.add(b, 5)), 
             @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 5)))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, @azimuth.add(max_val, 1))
  assert_eq(min_val, @azimuth.add(min_val, -1))
  assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, @azimuth.multiply(max_val, 0))
  assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 8
  let b = 7
  let c = 3
  let d = 12
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum_ab, c)  // 45
  let result = @azimuth.add(product, -d)  // 33
  
  assert_eq(33, result)
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景
  // 计算贷款本息和：本金 + 本金 × 利率 × 年数
  let principal = 10000
  let rate = 6  // 6%
  let years = 3
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  assert_eq(190000, total)  // 10000 + 10000*6*3 = 190000
}

test "assertion_functions_validation" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("moonbit", "moonbit")
}

test "edge_case_string_processing" {
  // 测试边缘情况的字符串处理
  let special_chars = "!@#$%^&*()"
  let unicode = "🌙"
  let mixed = "Test123!@#"
  
  assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet(special_chars))
  assert_eq_string("Hello, 🌙!", @azimuth.greet(unicode))
  assert_eq_string("Hello, Test123!@#!", @azimuth.greet(mixed))
}

test "sequential_operations" {
  // 测试连续运算
  // 计算 1 + 2 + 3 + 4 + 5
  let sum1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  let total_sum = @azimuth.add(sum1, 5)
  assert_eq(15, total_sum)
  
  // 计算 2 * 3 * 4
  let product1 = @azimuth.multiply(2, 3)
  let total_product = @azimuth.multiply(product1, 4)
  assert_eq(24, total_product)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景：摄氏度转华氏度（简化版）
  // F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)  // 0 × 2 + 30 = 30
}
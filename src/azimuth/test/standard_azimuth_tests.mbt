// Azimuth 项目标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试数学运算和字符串处理功能

test "azimuth_basic_addition" {
  // 测试基本加法运算
  let result1 = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, result1)
  
  let result2 = @azimuth.add(-5, 15)
  @azimuth.assert_eq(10, result2)
  
  let result3 = @azimuth.add(0, 100)
  @azimuth.assert_eq(100, result3)
}

test "azimuth_basic_multiplication" {
  // 测试基本乘法运算
  let result1 = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, result1)
  
  let result2 = @azimuth.multiply(-8, 4)
  @azimuth.assert_eq(-32, result2)
  
  let result3 = @azimuth.multiply(123, 0)
  @azimuth.assert_eq(0, result3)
}

test "azimuth_string_operations" {
  // 测试字符串操作
  let result1 = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result1)
  
  let result2 = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result2)
  
  let result3 = @azimuth.greet("测试123")
  @azimuth.assert_eq_string("Hello, 测试123!", result3)
}

test "azimuth_overflow_protection" {
  // 测试溢出保护功能
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  let overflow_add = @azimuth.add(max_val, 100)
  @azimuth.assert_eq(max_val, overflow_add)
  
  let underflow_add = @azimuth.add(min_val, -100)
  @azimuth.assert_eq(min_val, underflow_add)
  
  // 测试乘法溢出保护
  let overflow_mul = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, overflow_mul)
}

test "azimuth_financial_calculation" {
  // 测试金融计算场景
  let principal = 5000  // 本金
  let rate = 8  // 利率百分比
  let years = 4  // 年数
  
  // 简化的复利计算：最终金额 = 本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let final_amount = @azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(170000, final_amount)  // 5000 × (1 + 8×4) = 5000 × 33 = 165000
}

test "azimuth_geometry_calculations" {
  // 测试几何计算场景
  let length = 15
  let width = 10
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(150, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(50, perimeter)
}

test "azimuth_temperature_conversion" {
  // 测试温度转换场景
  // 简化公式：F = C × 2 + 30
  
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  let celsius2 = -10
  let fahrenheit2 = @azimuth.add(@azimuth.multiply(celsius2, 2), 30)
  @azimuth.assert_eq(10, fahrenheit2)
}

test "azimuth_combination_operations" {
  // 测试组合运算场景
  let base = 100
  let multiplier = 3
  let addition = 50
  let subtraction = 25
  
  // 计算 (base × multiplier) + addition - subtraction
  let step1 = @azimuth.multiply(base, multiplier)  // 300
  let step2 = @azimuth.add(step1, addition)        // 350
  let result = @azimuth.add(step2, -subtraction)   // 325
  
  @azimuth.assert_eq(325, result)
}

test "azimuth_assertion_functions" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "azimuth_edge_cases" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}
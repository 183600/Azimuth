// Azimuth 项目综合标准测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "integer_addition_comprehensive" {
  // 整数加法综合测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(10, @azimuth.add(5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "integer_multiplication_comprehensive" {
  // 整数乘法综合测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "complex_arithmetic_expressions" {
  // 复杂算术表达式测试
  @azimuth.assert_eq(20, @azimuth.add(@azimuth.multiply(4, 3), 8))
  @azimuth.assert_eq(50, @azimuth.multiply(@azimuth.add(5, 5), 5))
  @azimuth.assert_eq(36, @azimuth.multiply(@azimuth.add(2, 4), 6))
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.multiply(2, 5), 5))
}

test "calculation_with_multiple_variables" {
  // 多变量计算测试
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  
  @azimuth.assert_eq(30, @azimuth.add(a, b))
  @azimuth.assert_eq(70, @azimuth.add(c, d))
  @azimuth.assert_eq(200, @azimuth.multiply(a, b))
  @azimuth.assert_eq(1200, @azimuth.multiply(c, d))
  @azimuth.assert_eq(100, @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(c, d)))
}

test "string_operations_and_concatenation" {
  // 字符串操作和连接测试
  @azimuth.assert_eq_string("Hello World", @azimuth.add("Hello", " World"))
  @azimuth.assert_eq_string("MoonBit Language", @azimuth.add("MoonBit", " Language"))
  @azimuth.assert_eq_string("Test Case 1", @azimuth.add("Test", " Case 1"))
  @azimuth.assert_eq_string("123456", @azimuth.add("123", "456"))
}

test "financial_calculations" {
  // 金融计算测试
  let principal = 1000
  let rate = 5
  let time = 2
  
  // 简单利息计算
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time)) / 100
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(100, interest)
  @azimuth.assert_eq(1100, total)
}

test "physics_formula_calculations" {
  // 物理公式计算测试
  let mass = 10
  let acceleration = 5
  
  // 力 = 质量 × 加速度
  let force = @azimuth.multiply(mass, acceleration)
  @azimuth.assert_eq(50, force)
  
  // 动能 = 0.5 × 质量 × 速度² (简化计算)
  let velocity = 10
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(velocity, velocity)) / 2
  @azimuth.assert_eq(500, kinetic_energy)
}

test "temperature_conversions" {
  // 温度转换测试 (简化公式)
  let celsius = 25
  
  // 摄氏度转华氏度: F = C × 9/5 + 32 (简化为 F = C × 2 + 30)
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度: C = (F - 32) × 5/9 (简化为 C = (F - 30) / 2)
  let back_to_celsius = (@azimuth.subtract(fahrenheit, 30)) / 2
  @azimuth.assert_eq(25, back_to_celsius)
}

test "sequence_and_pattern_calculations" {
  // 序列和模式计算测试
  // 等差数列: 2, 5, 8, 11, 14...
  let first_term = 2
  let common_difference = 3
  let term1 = first_term
  let term2 = @azimuth.add(term1, common_difference)
  let term3 = @azimuth.add(term2, common_difference)
  let term4 = @azimuth.add(term3, common_difference)
  let term5 = @azimuth.add(term4, common_difference)
  
  @azimuth.assert_eq(2, term1)
  @azimuth.assert_eq(5, term2)
  @azimuth.assert_eq(8, term3)
  @azimuth.assert_eq(11, term4)
  @azimuth.assert_eq(14, term5)
  
  // 数列和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(40, sum)
}

test "edge_cases_and_boundary_values" {
  // 边界值和特殊情况测试
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000000000, @azimuth.multiply(1000000, 1000000))
  
  // 零值运算
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  
  // 单位运算
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(2, @azimuth.add(1, 1))
}
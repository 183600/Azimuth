// 额外的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_commutativity" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(15, 25), @azimuth.add(25, 15))
  @azimuth.assert_eq(@azimuth.add(-10, 20), @azimuth.add(20, -10))
  @azimuth.assert_eq(@azimuth.add(0, 100), @azimuth.add(100, 0))
}

test "multiply_function_commutativity" {
  // 测试乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  @azimuth.assert_eq(@azimuth.multiply(-5, 8), @azimuth.multiply(8, -5))
  @azimuth.assert_eq(@azimuth.multiply(0, 50), @azimuth.multiply(50, 0))
}

test "add_function_associativity" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "greet_function_special_characters" {
  // 测试 greet 函数处理特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))
  @azimuth.assert_eq_string("Hello, line1\nline2!", @azimuth.greet("line1\nline2"))
  @azimuth.assert_eq_string("Hello, tab\ttest!", @azimuth.greet("tab\ttest"))
}

test "extreme_boundary_combinations" {
  // 测试极端边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值附近的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算矩形的面积和周长
  let length = 12
  let width = 8
  
  // 计算面积：长 × 宽
  let area = @azimuth.multiply(length, width)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  @azimuth.assert_eq(96, area)
  @azimuth.assert_eq(40, perimeter)
}

test "financial_calculation_scenario" {
  // 金融计算场景：简单利息计算
  let principal = 10000  // 本金
  let rate = 5           // 利率（百分比）
  let years = 3          // 年数
  
  // 计算利息：本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  
  // 计算总额：本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(150000, total)  // 10000 + 10000*5*3 = 160000
}

test "temperature_conversion_scenario" {
  // 温度转换场景：摄氏度转华氏度（简化版）
  // F = C × 2 + 30（简化公式，避免浮点数）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_eq(@azimuth.add(max_val, 1, true) == max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1, true) == min_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2, true) == max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1, true) == min_val)
  
  // 测试正常情况
  @azimuth.assert_eq(@azimuth.add(5, 3, true) == 8)
  @azimuth.assert_eq(@azimuth.multiply(4, 6, true) == 24)
  @azimuth.assert_eq(@azimuth.greet("Test", true) == "Hello, Test!")
}
// MoonBit 增强测试用例
// 测试核心功能和边界情况

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试零值
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊值
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
}

test "greet_function_various_inputs" {
  // 测试greet函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算二次方程 y = ax² + bx + c
  let x = 5
  let a = 2
  let b = 3
  let c = 7
  
  let x_squared = @azimuth.multiply(x, x)
  let ax_squared = @azimuth.multiply(a, x_squared)
  let bx = @azimuth.multiply(b, x)
  let result = @azimuth.add(@azimuth.add(ax_squared, bx), c)
  
  @azimuth.assert_eq(72, result)
}

test "financial_calculation_simple_interest" {
  // 测试金融计算场景：单利计算
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简化的单利计算：利息 = 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)
}

test "geometry_calculation_triangle" {
  // 测试几何计算：三角形面积和周长
  let base = 10
  let height = 6
  let side1 = 8
  let side2 = 8
  
  // 计算面积（底 × 高 ÷ 2）
  let area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.add(base, side1), side2)
  @azimuth.assert_eq(26, perimeter)
}

test "temperature_conversion_fahrenheit_to_celsius" {
  // 测试温度转换：华氏度转摄氏度（简化版）
  // 简化公式：C = (F - 30) ÷ 2（避免小数）
  
  let fahrenheit = 70
  let celsius = (@azimuth.add(fahrenheit, -30)) / 2
  @azimuth.assert_eq(20, celsius)
  
  let freezing_f = 30
  let freezing_c = (@azimuth.add(freezing_f, -30)) / 2
  @azimuth.assert_eq(0, freezing_c)
}
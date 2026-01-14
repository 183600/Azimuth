// 新的综合 MoonBit 测试用例
// 测试 azimuth 包的核心功能和边界情况

test "add_comprehensive_test" {
  // 综合测试加法功能
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_comprehensive_test" {
  // 综合测试乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(123, @azimuth.multiply(1, 123))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, -1))
}

test "greet_function_variations" {
  // 测试问候函数的各种情况
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "complex_mathematical_operations" {
  // 测试复杂数学运算
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(900, result)
  
  // 计算 a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(230, result2)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：贷款计算
  let principal = 10000  // 本金
  let rate = 5           // 利率 5%
  let years = 3          // 年数
  
  // 计算利息：本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  // 计算总额：本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
}

test "geometry_calculation_scenario" {
  // 测试几何计算场景：矩形周长和面积
  let length = 15
  let width = 10
  
  // 计算面积：长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(150, area)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(50, perimeter)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景：摄氏度转华氏度（简化版）
  let celsius = 25
  
  // 华氏度 = 摄氏度 × 2 + 30（简化公式）
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = @azimuth.add(@azimuth.multiply(100, 2), 30)
  @azimuth.assert_eq(230, boiling)
}

test "assert_functions_comprehensive" {
  // 综合测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  
  // 测试计算结果的断言
  let add_result = @azimuth.add(17, 25)
  @azimuth.assert_eq(42, add_result)
  
  let multiply_result = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, multiply_result)
  
  let greet_result = @azimuth.greet("Tester")
  @azimuth.assert_eq_string("Hello, Tester!", greet_result)
}
// 增强的 MoonBit 测试用例
// 包含10个标准测试用例，覆盖各种实际应用场景

test "add_edge_case_zero_handling" {
  // 测试加法中零值的处理
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
  @azimuth.assert_eq(100, @azimuth.add(0, 100))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-100, @azimuth.add(-100, 0))
  @azimuth.assert_eq(-100, @azimuth.add(0, -100))
}

test "multiply_edge_case_identity_elements" {
  // 测试乘法中单位元素的处理
  @azimuth.assert_eq(50, @azimuth.multiply(50, 1))
  @azimuth.assert_eq(50, @azimuth.multiply(1, 50))
  @azimuth.assert_eq(-50, @azimuth.multiply(-50, 1))
  @azimuth.assert_eq(-50, @azimuth.multiply(1, -50))
  @azimuth.assert_eq(0, @azimuth.multiply(50, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 50))
}

test "add_negative_number_scenarios" {
  // 测试负数加法场景
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(-5, @azimuth.add(10, -15))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(-20, @azimuth.add(-10, -10))
}

test "multiply_negative_number_scenarios" {
  // 测试负数乘法场景
  @azimuth.assert_eq(-50, @azimuth.multiply(10, -5))
  @azimuth.assert_eq(-50, @azimuth.multiply(-10, 5))
  @azimuth.assert_eq(50, @azimuth.multiply(-10, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-10, 0))
}

test "greet_function_various_inputs" {
  // 测试greet函数的各种输入
  @azimuth.assert_eq_string("Hello, John!", @azimuth.greet("John"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：(a + b) * (c - d)
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)
  let result = @azimuth.multiply(sum_ab, diff_cd)
  
  @azimuth.assert_eq(105, result)  // (10 + 5) * (20 - 8) = 15 * 12 = 180
}

test "financial_calculation_simple_interest" {
  // 测试金融计算：简单利息
  let principal = 1000
  let rate = 5
  let time = 3
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000
}

test "geometry_rectangle_calculations" {
  // 测试几何计算：矩形周长和面积
  let length = 8
  let width = 5
  
  // 周长 = 2 * (length + width)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  // 面积 = length * width
  let area = @azimuth.multiply(length, width)
  
  @azimuth.assert_eq(26, perimeter)  // 2 * (8 + 5) = 2 * 13 = 26
  @azimuth.assert_eq(40, area)       // 8 * 5 = 40
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 测试温度转换：摄氏度转华氏度（简化版）
  // F = C * 2 + 30（简化公式）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
}

test "string_processing_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  let name_with_spaces = "John Doe"
  let name_with_symbols = "John@Doe"
  let name_with_numbers = "John123"
  
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet(name_with_spaces))
  @azimuth.assert_eq_string("Hello, John@Doe!", @azimuth.greet(name_with_symbols))
  @azimuth.assert_eq_string("Hello, John123!", @azimuth.greet(name_with_numbers))
}
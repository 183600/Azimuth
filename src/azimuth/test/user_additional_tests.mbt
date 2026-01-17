// 用户额外添加的 MoonBit 测试用例
// 包含 10 个标准测试用例，覆盖项目的核心功能

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本情况
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "complex_calculation_area_perimeter" {
  // 测试复杂计算：面积和周长
  let length = 15
  let width = 10
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(150, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(50, perimeter)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
}

test "financial_calculation_simple_interest" {
  // 测试简单利息计算
  let principal = 5000
  let rate = 6  // 6% 利率
  let years = 2
  
  // 简单利息：本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let total_amount = @azimuth.multiply(principal, growth_factor)
  
  // 验证结果：5000 × (1 + 6 × 2) = 5000 × 13 = 65000
  @azimuth.assert_eq(65000, total_amount)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 测试温度转换：摄氏度转华氏度（简化版）
  // 公式：F = C × 2 + 30
  
  let celsius = 30
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  // 验证结果：30 × 2 + 30 = 90
  @azimuth.assert_eq(90, fahrenheit)
  
  // 测试其他温度点
  @azimuth.assert_eq(50, @azimuth.add(@azimuth.multiply(10, 2), 30))
  @azimuth.assert_eq(70, @azimuth.add(@azimuth.multiply(20, 2), 30))
}

test "string_handling_special_characters" {
  // 测试字符串特殊字符处理
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  @azimuth.assert_eq_string("Hello, C++!", @azimuth.greet("C++"))
  @azimuth.assert_eq_string("Hello, a+b=c!", @azimuth.greet("a+b=c"))
  @azimuth.assert_eq_string("Hello, 100%!", @azimuth.greet("100%"))
}
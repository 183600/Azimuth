// 用户请求的 MoonBit 测试用例
// 包含 10 个标准的测试用例，测试项目的核心功能

test "add_function_basic_positive_numbers" {
  // 测试正数相加
  assert_eq(15, add(7, 8))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "add_function_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-5, 5))
}

test "multiply_function_basic_tests" {
  // 测试乘法基本功能
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(7, multiply(7, 1))
  assert_eq(-8, multiply(-2, 4))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
}

test "boundary_value_maximum_tests" {
  // 测试最大值边界情况
  let max_val = 2147483647
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "boundary_value_minimum_tests" {
  // 测试最小值边界情况
  let min_val = -2147483648
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 计算面积
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 计算周长
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(26, perimeter)
}

test "financial_calculation_simple_interest" {
  // 测试金融计算：简单利息
  let principal = 1000
  let rate = 5
  let time = 3
  
  // 简单利息 = 本金 × 利率 × 时间
  let interest = multiply(principal, multiply(rate, time))
  let total = add(principal, interest)
  
  assert_eq(16000, total)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 测试温度转换：摄氏度转华氏度（简化版）
  // F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
}

test "string_processing_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  assert_eq_string("Hello, John@123!", greet("John@123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, user_2023!", greet("user_2023"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}
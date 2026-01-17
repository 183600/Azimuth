// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含10个精心设计的测试场景

test "add_function_basic_cases" {
  // 测试基本加法运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(0, multiply(123, 0))
  assert_eq(123, multiply(123, 1))
  assert_eq(-123, multiply(123, -1))
  assert_eq(123, multiply(-123, -1))
}

test "greet_function_various_inputs" {
  // 测试greet函数处理各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 张三!", greet("张三"))
}

test "overflow_protection_addition" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, min_val))
}

test "overflow_protection_multiplication" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：简单利息计算
  let principal = 10000
  let rate = 5
  let time = 3
  
  // 利息 = 本金 × 利率 × 时间
  let interest = multiply(principal, multiply(rate, time))
  assert_eq(150000, interest)
  
  // 总额 = 本金 + 利息
  let total = add(principal, interest)
  assert_eq(160000, total)
}

test "assert_functions_functionality" {
  // 测试断言函数的功能
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试复合条件
  let x = 10
  let y = 20
  assert_true(add(x, y) == 30)
  assert_false(multiply(x, y) == 150)
}

test "real_world_temperature_conversion" {
  // 测试实际应用：温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 30) ÷ 2（简化公式）
  let fahrenheit_back = 80
  let celsius_back = add(fahrenheit_back, -30) / 2
  assert_eq(25, celsius_back)
}
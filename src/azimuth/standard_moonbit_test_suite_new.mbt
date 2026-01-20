// 标准 MoonBit 测试套件 - 新增测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
}

test "add_function_edge_cases" {
  // 测试加法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(0, 100))
}

test "multiply_function_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}

test "financial_calculation" {
  // 测试金融计算场景：简单利息
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 利息 = 本金 × 利率 × 年数
  let interest = multiply(principal, multiply(rate, years))
  // 总额 = 本金 + 利息
  let total = add(principal, interest)
  
  assert_eq(160000, total)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版）
  // F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(80, fahrenheit)
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(0, divide_with_ceil(2, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}
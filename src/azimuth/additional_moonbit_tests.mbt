// 额外的 MoonBit 测试用例
// 测试基本功能函数

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
}

test "add_function_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-4, multiply(-2, 2))
  assert_eq(4, multiply(-2, -2))
}

test "multiply_function_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, Alice!", greet("Alice"))
}

test "greet_function_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算矩形的面积和周长
  let length = 10
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(50, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(30, perimeter)
}

test "financial_calculation" {
  // 测试金融计算场景
  // 简单的复利计算：本金 + 利息
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 利息 = 本金 × 利率 × 年数
  let interest = multiply(principal, multiply(rate, years))
  // 总金额 = 本金 + 利息
  let total = add(principal, interest)
  
  assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000
}

test "temperature_conversion" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
  
  // 测试冰点
  let freezing = 0
  let freezing_f = add(multiply(freezing, 2), 30)
  assert_eq(30, freezing_f)
}
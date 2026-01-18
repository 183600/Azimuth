// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
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
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(0, multiply(5, 0))
}

test "multiply_function_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(46341, 46341))
}

test "greet_function_variations" {
  // 测试greet函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
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

test "financial_calculation" {
  // 测试金融计算场景：简单利息计算
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 利息 = 本金 × 利率 × 年数
  let interest = multiply(principal, multiply(rate, years))
  // 总金额 = 本金 + 利息
  let total = add(principal, interest)
  
  assert_eq(160000, total)
}

test "temperature_conversion" {
  // 测试温度转换场景：摄氏度转华氏度（简化版）
  // F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
}

test "assert_functions_verification" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试实际计算结果
  let result = add(10, 20)
  assert_true(result == 30)
  assert_false(result == 25)
}
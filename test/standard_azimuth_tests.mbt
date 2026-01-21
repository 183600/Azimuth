// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 基本加法功能测试
  assert_eq(7, add(3, 4))
  assert_eq(0, add(-3, 3))
  assert_eq(-6, add(-3, -3))
  assert_eq(100, add(50, 50))
}

test "add_boundary_values" {
  // 边界值加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(0, 0))
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  assert_eq(12, multiply(3, 4))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
}

test "multiply_boundary_values" {
  // 边界值乘法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_functionality" {
  // greet 函数功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(4, divide_with_ceil(11, 3))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试：计算矩形面积和周长
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
  // 金融计算场景测试
  let principal = 5000
  let rate_percent = 8
  let years = 3
  
  // 简单利息计算：总额 = 本金 × (1 + 利率 × 年数)
  let total = multiply(principal, add(100, multiply(rate_percent, years))) / 100
  assert_eq(17000, total)
}

test "temperature_conversion_scenario" {
  // 温度转换场景测试（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30
  let celsius = 15
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(60, fahrenheit)
  
  // 华氏度转摄氏度（简化版）：C = (F - 30) ÷ 2
  let converted_back = add(fahrenheit, -30) / 2
  assert_eq(celsius, converted_back)
}

test "assert_functions_validation" {
  // 断言函数验证测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试数学运算结果
  assert_true(add(2, 3) == 5)
  assert_false(multiply(2, 3) == 7)
  assert_eq(6, multiply(2, 3))
  assert_eq_string("Hello, Test!", greet("Test"))
}
// MoonBit 测试套件 - 标准测试用例
// 测试 azimuth 模块的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, azimuth::add(7, 3))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-8, azimuth::add(-3, -5))
  assert_eq(100, azimuth::add(50, 50))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::add(max_val, 100))
  
  // 测试负数溢出
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::add(min_val, -100))
  
  // 测试边界值相加
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(12, azimuth::multiply(3, 4))
  assert_eq(-15, azimuth::multiply(3, -5))
  assert_eq(20, azimuth::multiply(-4, -5))
  assert_eq(0, azimuth::multiply(100, 0))
}

test "multiply_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与1和-1相乘
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  
  // 测试溢出保护
  assert_eq(max_val, azimuth::multiply(46341, 46341))
  assert_eq(min_val, azimuth::multiply(-46341, 46341))
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth::greet(""))
}

test "greet_function_special_chars" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  assert_eq_string("Hello, @#$!", azimuth::greet("@#$"))
  assert_eq_string("Hello,   !", azimuth::greet("  "))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 计算面积
  let area = azimuth::multiply(length, width)
  assert_eq(40, area)
  
  // 计算周长
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  assert_eq(26, perimeter)
}

test "financial_calculation" {
  // 测试金融计算场景：简单利息计算
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 计算利息：本金 × 利率 × 年数
  let interest = azimuth::multiply(principal, azimuth::multiply(rate, years))
  // 计算总额：本金 + 利息
  let total = azimuth::add(principal, interest)
  
  assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000
}

test "temperature_conversion" {
  // 测试温度转换场景：摄氏度转华氏度（简化版）
  // 使用近似公式：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  
  assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
  
  // 测试冰点
  let freezing = azimuth::add(azimuth::multiply(0, 2), 30)
  assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = azimuth::add(azimuth::multiply(100, 2), 30)
  assert_eq(230, boiling)
}

test "assertion_functions_test" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试计算结果的断言
  let add_result = azimuth::add(15, 25)
  assert_eq(40, add_result)
  
  let multiply_result = azimuth::multiply(6, 7)
  assert_eq(42, multiply_result)
  
  let greet_result = azimuth::greet("Test")
  assert_eq_string("Hello, Test!", greet_result)
}
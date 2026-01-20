// 标准 Azimuth 核心测试用例
// 测试基本数学运算和字符串处理功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-10, add(-5, -5))
  assert_eq(100, add(50, 50))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(3, -5))
  assert_eq(25, multiply(-5, -5))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(max_val, multiply(46341, 46341))
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_function_special_chars" {
  // 测试包含特殊字符的问候功能
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, user@123!", greet("user@123"))
  assert_eq_string("Hello,   spaced  !", greet("  spaced  "))
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
  let celsius = 25
  
  // 华氏度 = 摄氏度 × 2 + 30（简化公式）
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}
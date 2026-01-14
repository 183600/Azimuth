// 标准 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能和边界情况

test "add_basic_functionality" {
  // 测试基本加法功能
  let result = azimuth::add(10, 20)
  @azimuth.assert_eq(30, result)
  
  // 测试负数加法
  let negative_result = azimuth::add(-5, 15)
  @azimuth.assert_eq(10, negative_result)
}

test "add_overflow_protection" {
  // 测试正数溢出保护
  let max_val = 2147483647
  let overflow_result = azimuth::add(max_val, 1)
  @azimuth.assert_eq(max_val, overflow_result)
  
  // 测试负数溢出保护
  let min_val = -2147483648
  let underflow_result = azimuth::add(min_val, -1)
  @azimuth.assert_eq(min_val, underflow_result)
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  let result = azimuth::multiply(6, 7)
  @azimuth.assert_eq(42, result)
  
  // 测试与零相乘
  let zero_result = azimuth::multiply(123, 0)
  @azimuth.assert_eq(0, zero_result)
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let overflow_result = azimuth::multiply(max_val, 2)
  @azimuth.assert_eq(max_val, overflow_result)
  
  // 测试最小值乘法
  let min_val = -2147483648
  let min_multiply = azimuth::multiply(min_val, 1)
  @azimuth.assert_eq(min_val, min_multiply)
}

test "greet_function_variations" {
  // 测试基本问候功能
  let basic_greeting = azimuth::greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", basic_greeting)
  
  // 测试空字符串
  let empty_greeting = azimuth::greet("")
  @azimuth.assert_eq_string("Hello, !", empty_greeting)
  
  // 测试特殊字符
  let special_greeting = azimuth::greet("测试@123")
  @azimuth.assert_eq_string("Hello, 测试@123!", special_greeting)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 计算面积
  let area = azimuth::multiply(length, width)
  @azimuth.assert_eq(40, area)
  
  // 计算周长：2 × (length + width)
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  @azimuth.assert_eq(26, perimeter)
}

test "financial_calculation" {
  // 测试金融计算场景：简单利息计算
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 计算利息：principal × rate × years
  let interest = azimuth::multiply(principal, azimuth::multiply(rate, years))
  @azimuth.assert_eq(15000, interest)
  
  // 计算总额：principal + interest
  let total = azimuth::add(principal, interest)
  @azimuth.assert_eq(16000, total)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值相加
  let sum_result = azimuth::add(max_val, min_val)
  @azimuth.assert_eq(-1, sum_result)
  
  // 最大值与1相乘
  let max_multiply = azimuth::multiply(max_val, 1)
  @azimuth.assert_eq(max_val, max_multiply)
  
  // 最小值与1相乘
  let min_multiply = azimuth::multiply(min_val, 1)
  @azimuth.assert_eq(min_val, min_multiply)
}

test "assert_functions_verification" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试计算结果的断言
  let calc_result = azimuth::add(azimuth::multiply(3, 4), azimuth::multiply(2, 5))
  @azimuth.assert_eq(22, calc_result)  // 3*4 + 2*5 = 12 + 10 = 22
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameThatTestsStringHandlingCapabilities"
  let long_result = azimuth::greet(long_name)
  @azimuth.assert_eq_string("Hello, " + long_name + "!", long_result)
  
  // 测试包含空格的字符串
  let spaced_name = "  Leading and trailing spaces  "
  let spaced_result = azimuth::greet(spaced_name)
  @azimuth.assert_eq_string("Hello,   Leading and trailing spaces  !", spaced_result)
  
  // 测试Unicode字符
  let unicode_name = "🌍🚀"
  let unicode_result = azimuth::greet(unicode_name)
  @azimuth.assert_eq_string("Hello, 🌍🚀!", unicode_result)
}
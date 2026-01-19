// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，针对核心功能进行全面测试

// 导入必要的模块
use azimuth

test "add_function_basic_cases" {
  // 加法函数基本测试用例
  @assertion.assert_eq(10, azimuth.add(3, 7))?
  @assertion.assert_eq(0, azimuth.add(5, -5))?
  @assertion.assert_eq(-10, azimuth.add(-3, -7))?
  @assertion.assert_eq(100, azimuth.add(50, 50))?
}

test "add_function_edge_cases" {
  // 加法函数边界测试用例
  let max_int = 2147483647
  let min_int = -2147483648
  
  @assertion.assert_eq(max_int, azimuth.add(max_int, 1))?
  @assertion.assert_eq(min_int, azimuth.add(min_int, -1))?
  @assertion.assert_eq(-1, azimuth.add(max_int, min_int))?
  @assertion.assert_eq(0, azimuth.add(min_int, 1))?
}

test "multiply_function_basic_cases" {
  // 乘法函数基本测试用例
  @assertion.assert_eq(15, azimuth.multiply(3, 5))?
  @assertion.assert_eq(-15, azimuth.multiply(3, -5))?
  @assertion.assert_eq(0, azimuth.multiply(0, 12345))?
  @assertion.assert_eq(100, azimuth.multiply(10, 10))?
}

test "multiply_function_edge_cases" {
  // 乘法函数边界测试用例
  let max_int = 2147483647
  let min_int = -2147483648
  
  @assertion.assert_eq(max_int, azimuth.multiply(max_int, 1))?
  @assertion.assert_eq(min_int, azimuth.multiply(min_int, 1))?
  @assertion.assert_eq(min_int, azimuth.multiply(min_int, -1))?
  @assertion.assert_eq(0, azimuth.multiply(max_int, 0))?
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  @assertion.assert_eq_string("Hello, World!", azimuth.greet("World"))?
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
  @assertion.assert_eq_string("Hello, 123!", azimuth.greet("123"))?
  @assertion.assert_eq_string("Hello, 测试!", azimuth.greet("测试"))?
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(40, area)?
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  @assertion.assert_eq(26, perimeter)?
}

test "financial_calculation_test" {
  // 金融计算测试
  // 简单利息计算：本金 + 本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  let interest = azimuth.multiply(principal, azimuth.multiply(rate, time))
  let total = azimuth.add(principal, interest)
  
  @assertion.assert_eq(160000, total)?
}

test "temperature_conversion_test" {
  // 温度转换测试（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（整数近似）
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  
  @assertion.assert_eq(80, fahrenheit)?
  
  // 测试冰点
  let freezing = azimuth.add(azimuth.multiply(0, 2), 30)
  @assertion.assert_eq(30, freezing)?
}
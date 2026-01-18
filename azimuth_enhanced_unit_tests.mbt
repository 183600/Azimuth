// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，测试核心功能

// 导入必要的模块
use azimuth

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  @assertion.assert_eq(5, azimuth.add(2, 3))?
  @assertion.assert_eq(0, azimuth.add(0, 0))?
  @assertion.assert_eq(-3, azimuth.add(-1, -2))?
  @assertion.assert_eq(10, azimuth.add(0, 10))?
  @assertion.assert_eq(15, azimuth.add(15, 0))?
}

test "add_function_overflow_protection" {
  // 测试加法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @assertion.assert_eq(max_val, azimuth.add(max_val, 1))?
  @assertion.assert_eq(max_val, azimuth.add(max_val, max_val))?
  
  // 测试负数溢出
  @assertion.assert_eq(min_val, azimuth.add(min_val, -1))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, min_val))?
  
  // 测试混合情况
  @assertion.assert_eq(0, azimuth.add(max_val, min_val))?
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  @assertion.assert_eq(6, azimuth.multiply(2, 3))?
  @assertion.assert_eq(0, azimuth.multiply(0, 5))?
  @assertion.assert_eq(0, azimuth.multiply(5, 0))?
  @assertion.assert_eq(-6, azimuth.multiply(-2, 3))?
  @assertion.assert_eq(6, azimuth.multiply(-2, -3))?
  @assertion.assert_eq(5, azimuth.multiply(5, 1))?
  @assertion.assert_eq(5, azimuth.multiply(1, 5))?
}

test "multiply_function_overflow_protection" {
  // 测试乘法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 2))?
  @assertion.assert_eq(max_val, azimuth.multiply(46341, 46341))?
  
  // 测试负数溢出
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 2))?
  @assertion.assert_eq(min_val, azimuth.multiply(-46341, 46341))?
  
  // 测试最小值的特殊情况
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))?
  @assertion.assert_eq(0, azimuth.multiply(min_val, 0))?
}

test "greet_function_various_names" {
  // 测试问候函数的各种名字
  @assertion.assert_eq_string("Hello, World!", azimuth.greet("World"))?
  @assertion.assert_eq_string("Hello, 张三!", azimuth.greet("张三"))?
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
  @assertion.assert_eq_string("Hello, 123!", azimuth.greet("123"))?
  @assertion.assert_eq_string("Hello, 🌍!", azimuth.greet("🌍"))?
}

test "complex_mathematical_operations" {
  // 测试复杂的数学运算组合
  let a = 10
  let b = 20
  let c = 5
  
  // 测试 (a + b) * c
  let result1 = azimuth.multiply(azimuth.add(a, b), c)
  @assertion.assert_eq(150, result1)?
  
  // 测试 a * b + c
  let result2 = azimuth.add(azimuth.multiply(a, b), c)
  @assertion.assert_eq(205, result2)?
  
  // 测试 (a - b) * c (使用加负数实现减法)
  let result3 = azimuth.multiply(azimuth.add(a, -b), c)
  @assertion.assert_eq(-50, result3)?
}

test "edge_cases_and_special_values" {
  // 测试边界值和特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值和最小值的运算
  @assertion.assert_eq(max_val, azimuth.add(max_val, 0))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, 0))?
  @assertion.assert_eq(0, azimuth.multiply(max_val, 0))?
  @assertion.assert_eq(0, azimuth.multiply(min_val, 0))?
  
  // 测试1和-1的特殊情况
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 1))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 1))?
  @assertion.assert_eq(-max_val, azimuth.multiply(max_val, -1))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))?
}

test "real_world_scenarios" {
  // 测试真实世界场景
  // 购物车计算
  let price = 199
  let quantity = 3
  let discount = 50
  let subtotal = azimuth.multiply(price, quantity)
  let total = azimuth.add(subtotal, -discount)
  @assertion.assert_eq(547, total)?
  
  // 温度转换（简化版）：摄氏度转华氏度 F = C × 2 + 30
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  @assertion.assert_eq(80, fahrenheit)?
  
  // 面积计算
  let length = 12
  let width = 8
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(96, area)?
}

test "string_operations_and_combinations" {
  // 测试字符串操作和组合
  // 测试不同类型的字符串拼接
  let english = "Hello"
  let chinese = "世界"
  let number = "2023"
  let symbol = "!"
  
  let mixed = english + ", " + chinese + " " + number + symbol
  @assertion.assert_eq_string("Hello, 世界 2023!", mixed)?
  
  // 测试空字符串
  let empty = ""
  let with_empty = azimuth.greet(empty)
  @assertion.assert_eq_string("Hello, !", with_empty)?
  
  // 测试长字符串
  let long_name = "这是一个非常长的名字用来测试字符串处理功能"
  let long_greeting = azimuth.greet(long_name)
  @assertion.assert_eq_string("Hello, 这是一个非常长的名字用来测试字符串处理功能!", long_greeting)?
}
// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖项目主要功能

// 导入必要的模块
use azimuth

test "add_basic_functionality" {
  // 测试基本加法功能
  let result1 = azimuth.add(2, 3)
  @assertion.assert_eq(5, result1)?
  
  let result2 = azimuth.add(-5, 10)
  @assertion.assert_eq(5, result2)?
  
  let result3 = azimuth.add(0, 42)
  @assertion.assert_eq(42, result3)?
}

test "add_zero_handling" {
  // 测试加法中的零处理
  let result1 = azimuth.add(0, 0)
  @assertion.assert_eq(0, result1)?
  
  let result2 = azimuth.add(123, 0)
  @assertion.assert_eq(123, result2)?
  
  let result3 = azimuth.add(0, -456)
  @assertion.assert_eq(-456, result3)?
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  let result1 = azimuth.multiply(3, 4)
  @assertion.assert_eq(12, result1)?
  
  let result2 = azimuth.multiply(-5, 6)
  @assertion.assert_eq(-30, result2)?
  
  let result3 = azimuth.multiply(7, -8)
  @assertion.assert_eq(-56, result3)?
}

test "multiply_zero_and_one_handling" {
  // 测试乘法中的0和1处理
  let result1 = azimuth.multiply(0, 123)
  @assertion.assert_eq(0, result1)?
  
  let result2 = azimuth.multiply(456, 0)
  @assertion.assert_eq(0, result2)?
  
  let result3 = azimuth.multiply(1, 789)
  @assertion.assert_eq(789, result3)?
  
  let result4 = azimuth.multiply(-123, 1)
  @assertion.assert_eq(-123, result4)?
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  let result1 = azimuth.greet("World")
  @assertion.assert_eq_string("Hello, World!", result1)?
  
  let result2 = azimuth.greet("MoonBit")
  @assertion.assert_eq_string("Hello, MoonBit!", result2)?
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  let result1 = azimuth.greet("")
  @assertion.assert_eq_string("Hello, !", result1)?
  
  let result2 = azimuth.greet("123")
  @assertion.assert_eq_string("Hello, 123!", result2)?
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  let result1 = azimuth.add(max_val, 1)
  @assertion.assert_eq(max_val, result1)?
  
  // 测试负数溢出
  let result2 = azimuth.add(min_val, -1)
  @assertion.assert_eq(min_val, result2)?
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  let result1 = azimuth.multiply(max_val, 2)
  @assertion.assert_eq(max_val, result1)?
  
  // 测试最小值乘法
  let result2 = azimuth.multiply(min_val, -1)
  @assertion.assert_eq(min_val, result2)?
}

test "edge_cases_combination" {
  // 测试边界情况组合
  let result1 = azimuth.add(azimuth.multiply(2, 3), azimuth.multiply(4, 5))
  @assertion.assert_eq(26, result1)?
  
  let result2 = azimuth.multiply(azimuth.add(1, 2), azimuth.add(3, 4))
  @assertion.assert_eq(21, result2)?
}
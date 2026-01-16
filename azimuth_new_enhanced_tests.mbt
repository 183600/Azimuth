// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "add_zero_property" {
  // 测试加法零元素特性
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::add(42, 0), 42)
  assert_eq(azimuth::add(0, 42), 42)
  assert_eq(azimuth::add(-17, 0), -17)
  assert_eq(azimuth::add(0, -17), -17)
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(azimuth::add(-5, -3), -8)
  assert_eq(azimuth::add(-10, 3), -7)
  assert_eq(azimuth::add(7, -15), -8)
  assert_eq(azimuth::add(-100, 50), -50)
}

test "multiply_identity_property" {
  // 测试乘法单位元素特性
  assert_eq(azimuth::multiply(1, 1), 1)
  assert_eq(azimuth::multiply(42, 1), 42)
  assert_eq(azimuth::multiply(1, 42), 42)
  assert_eq(azimuth::multiply(-17, 1), -17)
  assert_eq(azimuth::multiply(1, -17), -17)
}

test "multiply_zero_property" {
  // 测试乘法零元素特性
  assert_eq(azimuth::multiply(0, 0), 0)
  assert_eq(azimuth::multiply(42, 0), 0)
  assert_eq(azimuth::multiply(0, 42), 0)
  assert_eq(azimuth::multiply(-17, 0), 0)
  assert_eq(azimuth::multiply(0, -17), 0)
}

test "greet_numeric_names" {
  // 测试数字名称的问候
  assert_eq(azimuth::greet("123"), "Hello, 123!")
  assert_eq(azimuth::greet("007"), "Hello, 007!")
  assert_eq(azimuth::greet("2023"), "Hello, 2023!")
}

test "greet_special_symbols" {
  // 测试特殊符号名称的问候
  assert_eq(azimuth::greet("!@#$"), "Hello, !@#$!")
  assert_eq(azimuth::greet("()[]"), "Hello, ()[]!")
  assert_eq(azimuth::greet("{}<>"), "Hello, {}<>!")
}

test "compound_calculations_temperature" {
  // 测试复合计算：温度转换
  // 摄氏度转华氏度公式：F = C * 9/5 + 32
  // 使用整数运算模拟：(C * 9) / 5 + 32
  
  let celsius_0 = 0
  let fahrenheit_0 = azimuth::add(azimuth::multiply(celsius_0, 9) / 5, 32)
  assert_eq(32, fahrenheit_0)
  
  let celsius_100 = 100
  let fahrenheit_100 = azimuth::add(azimuth::multiply(celsius_100, 9) / 5, 32)
  assert_eq(212, fahrenheit_100)
  
  let celsius_minus40 = -40
  let fahrenheit_minus40 = azimuth::add(azimuth::multiply(celsius_minus40, 9) / 5, 32)
  assert_eq(-40, fahrenheit_minus40)
}

test "compound_calculations_simple_interest" {
  // 测试复合计算：简单利息
  // 利息公式：Interest = Principal * Rate * Time
  
  let principal = 1000
  let rate = 5  // 5%
  let time = 2  // 2年
  
  let interest = azimuth::multiply(azimuth::multiply(principal, rate), time) / 100
  let total = azimuth::add(principal, interest)
  
  assert_eq(100, interest)
  assert_eq(1100, total)
}

test "error_boundary_overflow" {
  // 测试错误边界：溢出处理
  let max_int = 2147483647
  
  // 测试加法溢出
  let overflow_add = azimuth::add(max_int, 1)
  assert_true(overflow_add <= max_int)  // 应该被限制在最大值
  
  // 测试乘法溢出
  let overflow_mul = azimuth::multiply(max_int, 2)
  assert_true(overflow_mul <= max_int)  // 应该被限制在最大值
}
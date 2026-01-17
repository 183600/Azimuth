// 用户新增的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_function_basic_tests" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
}

test "multiply_function_basic_tests" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "greet_function_basic_tests" {
  // 测试 greet 函数的基本功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "mathematical_associative_law" {
  // 测试数学结合律: (a + b) + c = a + (b + c)
  let a = 5
  let b = 10
  let c = 15
  
  let left_side = @azimuth.add(@azimuth.add(a, b), c)
  let right_side = @azimuth.add(a, @azimuth.add(b, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "mathematical_identity_elements" {
  // 测试数学单位元
  // 加法单位元: a + 0 = a
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
  
  // 乘法单位元: a * 1 = a
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-42, @azimuth.multiply(-42, 1))
  
  // 乘法零元: a * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-42, 0))
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试最小值加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试最大值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 10
  let width = 5
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(length, 2), @azimuth.multiply(width, 2))
  @azimuth.assert_eq(30, perimeter)
}

test "string_processing_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  @azimuth.assert_eq_string("Hello, emoji😊!", @azimuth.greet("emoji😊"))
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景
  let principal = 1000  // 本金
  let rate = 5          // 利率 5%
  let years = 3         // 年数
  
  // 简化的利息计算：利息 = 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  // 总金额 = 本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000
}

test "temperature_conversion_calculation" {
  // 测试温度转换计算（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
}
// 用户自定义的标准 MoonBit 测试用例
// 包含10个测试用例，覆盖 azimuth 模块的核心功能

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试负数情况
  let c = -10
  let d = 20
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
}

test "add_associative_property" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 5
  let b = 10
  let c = 15
  
  let left = @azimuth.add(@azimuth.add(a, b), c)
  let right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left, right)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 3
  let b = 4
  let c = 5
  
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "multiply_commutative_property" {
  // 测试乘法交换律：a × b = b × a
  let a = 12
  let b = 8
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -6
  let d = 7
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, John@Doe!", @azimuth.greet("John@Doe"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))  // 10/3 = 3.33 → 4
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))  // 11/3 = 3.67 → 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 9/3 = 3 → 3
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2, 5))   // 2/5 = 0.4 → 1
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 计算面积：长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：简单利息计算
  let principal = 1000  // 本金
  let rate = 5          // 利率 5%
  let time = 3          // 时间 3年
  
  // 计算利息：本金 × 利率 × 时间
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  @azimuth.assert_eq(15000, interest)  // 1000 × 5 × 3 = 15000
  
  // 计算总额：本金 + 利息
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(16000, total)  // 1000 + 15000 = 16000
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景：摄氏度转华氏度（简化版）
  // F = C × 2 + 30 (简化公式，避免小数)
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)  // 20 × 2 + 30 = 70
  
  // 测试冰点
  let freezing = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing, 2), 30)
  @azimuth.assert_eq(30, freezing_f)  // 0 × 2 + 30 = 30
}

test "overflow_boundary_handling" {
  // 测试溢出边界处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试乘法溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}
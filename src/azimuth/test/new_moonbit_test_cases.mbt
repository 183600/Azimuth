// 新的 MoonBit 测试用例
// 测试 azimuth 包的额外功能和边界情况

test "add_function_large_numbers" {
  // 测试大数加法
  assert_eq(2000000000, azimuth::add(1000000000, 1000000000))
  assert_eq(-2000000000, azimuth::add(-1000000000, -1000000000))
  assert_eq(0, azimuth::add(1500000000, -1500000000))
}

test "multiply_function_large_numbers" {
  // 测试大数乘法
  assert_eq(1000000000, azimuth::multiply(10000, 100000))
  assert_eq(-1000000000, azimuth::multiply(-10000, 100000))
  assert_eq(1000000000, azimuth::multiply(-10000, -100000))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 123
  let b = 456
  
  // 加法交换律: a + b = b + a
  assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
}

test "mathematical_associativity" {
  // 测试数学结合律
  let a = 10
  let b = 20
  let c = 30
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_add = azimuth::add(azimuth::add(a, b), c)
  let right_add = azimuth::add(a, azimuth::add(b, c))
  assert_eq(left_add, right_add)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let left_mul = azimuth::multiply(azimuth::multiply(a, b), c)
  let right_mul = azimuth::multiply(a, azimuth::multiply(b, c))
  assert_eq(left_mul, right_mul)
}

test "distributive_property" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(left_side, right_side)
}

test "greet_function_edge_cases" {
  // 测试 greet 函数的边界情况
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", azimuth::greet("John Doe"))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, test@123#!", azimuth::greet("test@123#!"))
  
  // 测试Unicode字符
  assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
}

test "financial_calculation_scenario" {
  // 测试金融计算场景
  let principal = 10000  // 本金
  let rate = 5           // 利率 5%
  let years = 3          // 年数
  
  // 简单的利息计算: 本金 + 本金 * 利率 * 年数
  let interest = azimuth::multiply(principal, azimuth::multiply(rate, years))
  let total = azimuth::add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
}

test "geometry_calculations" {
  // 测试几何计算
  // 计算矩形面积: 长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = azimuth::multiply(length, width)
  assert_eq(96, rectangle_area)
  
  // 计算矩形周长: 2 × (长 + 宽)
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  assert_eq(40, perimeter)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度: F = C * 9/5 + 32
  // 简化为整数运算: F = C * 2 + 30
  
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth::add(azimuth::multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
}
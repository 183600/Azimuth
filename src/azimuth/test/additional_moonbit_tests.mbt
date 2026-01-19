// 额外的 MoonBit 测试用例
// 测试 azimuth 包中的更多功能

test "add_commutative_property" {
  // 测试加法交换律: a + b = b + a
  let a = 12345
  let b = 67890
  
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试负数的交换律
  let c = -123
  let d = 456
  
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
}

test "multiply_associative_property" {
  // 测试乘法结合律: (a * b) * c = a * (b * c)
  let a = 12
  let b = 15
  let c = 20
  
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  
  @azimuth.assert_eq(result1, result2)
}

test "distributive_property" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 13
  let c = 19
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 温度转换测试：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius_temps = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  // 测试 0°C = 30°F
  let f1 = @azimuth.add(@azimuth.multiply(celsius_temps[0], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[0], f1)
  
  // 测试 20°C = 70°F
  let f2 = @azimuth.add(@azimuth.multiply(celsius_temps[2], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[2], f2)
  
  // 测试 40°C = 110°F
  let f3 = @azimuth.add(@azimuth.multiply(celsius_temps[4], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[4], f3)
}

test "simple_interest_calculation" {
  // 简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 3   // 3年
  
  // 计算利息
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  
  // 计算总金额
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total_amount)  // 10000 + 10000*5*3 = 160000
}

test "area_perimeter_calculations" {
  // 面积和周长计算测试
  let length = 15
  let width = 8
  
  // 计算矩形面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(120, area)
  
  // 计算矩形周长
  let perimeter = @azimuth.add(@azimuth.multiply(length, 2), @azimuth.multiply(width, 2))
  @azimuth.assert_eq(46, perimeter)
  
  // 计算三角形面积（底×高÷2）
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "string_edge_cases" {
  // 字符串边界情况测试
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet(special_chars))
  
  // 测试包含数字的字符串
  let numeric_string = "123456789"
  @azimuth.assert_eq_string("Hello, 123456789!", @azimuth.greet(numeric_string))
  
  // 测试包含空格的字符串
  let spaced_string = "John Doe"
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet(spaced_string))
}

test "extreme_value_operations" {
  // 极值运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值加法
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 测试极值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "sequential_operations" {
  // 连续运算测试
  // 计算 ((a + b) * c) - d
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  
  let sum = @azimuth.add(a, b)           // 10 + 20 = 30
  let product = @azimuth.multiply(sum, c) // 30 * 5 = 150
  let result = @azimuth.add(product, -d)  // 150 - 15 = 135
  
  @azimuth.assert_eq(135, result)
  
  // 计算 (a * b) + (c * d)
  let result2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(c, d))
  @azimuth.assert_eq(350, result2)  // 10*20 + 5*15 = 200 + 75 = 275 (修正)
  @azimuth.assert_eq(275, result2)  // 10*20 + 5*15 = 200 + 75 = 275
}
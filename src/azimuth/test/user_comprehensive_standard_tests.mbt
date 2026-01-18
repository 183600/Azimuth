// 用户综合标准测试用例
// 使用标准的 MoonBit 测试语法，全面测试 azimuth 库的功能

test "addition_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 123
  let b = 456
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  @azimuth.assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let c = -100
  let d = 200
  
  let result3 = @azimuth.add(c, d)
  let result4 = @azimuth.add(d, c)
  @azimuth.assert_eq(result3, result4)
}

test "multiplication_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 10
  let c = 15
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "addition_associative_property" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 25
  let b = 35
  let c = 45
  
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
}

test "multiplication_identity_elements" {
  // 测试乘法单位元：a × 1 = a，a × 0 = 0
  let value = 789
  
  @azimuth.assert_eq(value, @azimuth.multiply(value, 1))
  @azimuth.assert_eq(value, @azimuth.multiply(1, value))
  @azimuth.assert_eq(0, @azimuth.multiply(value, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, value))
}

test "string_greeting_multilingual" {
  // 测试多语言问候功能
  let english_name = "Alice"
  let chinese_name = "张三"
  let japanese_name = "田中"
  let mixed_name = "User123测试"
  
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet(english_name))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(chinese_name))
  @azimuth.assert_eq_string("Hello, 田中!", @azimuth.greet(japanese_name))
  @azimuth.assert_eq_string("Hello, User123测试!", @azimuth.greet(mixed_name))
}

test "mathematical_sequence_sum" {
  // 测试数学序列求和：计算等差数列 1 + 2 + 3 + ... + 10
  let n = 10
  // 使用公式：n × (n + 1) / 2
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
  
  // 验证通过逐项相加
  let manual_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5), 6), 7), 8), 9), 10)
  @azimuth.assert_eq(sum, manual_sum)
}

test "complex_financial_calculation" {
  // 测试复杂金融计算：复利计算
  // 本金 × (1 + 利率)^年数（简化为整数计算）
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算：本金 + 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)  // 10000 + 10000×5×3 = 160000
}

test "geometric_calculations" {
  // 测试几何计算：矩形和三角形面积
  let rectangle_length = 12
  let rectangle_width = 8
  let triangle_base = 10
  let triangle_height = 6
  
  // 矩形面积 = 长 × 宽
  let rectangle_area = @azimuth.multiply(rectangle_length, rectangle_width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 三角形面积 = 底 × 高 ÷ 2
  let triangle_area = @azimuth.multiply(triangle_base, triangle_height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景：摄氏度转华氏度
  // 简化公式：F = C × 2 + 30（避免浮点数）
  let celsius_temperatures = [0, 10, 20, 30, 100]
  let expected_fahrenheit = [30, 50, 70, 90, 230]
  
  let c0 = celsius_temperatures[0]
  let f0 = @azimuth.add(@azimuth.multiply(c0, 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[0], f0)
  
  let c1 = celsius_temperatures[1]
  let f1 = @azimuth.add(@azimuth.multiply(c1, 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[1], f1)
  
  let c2 = celsius_temperatures[2]
  let f2 = @azimuth.add(@azimuth.multiply(c2, 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[2], f2)
}

test "boundary_value_comprehensive" {
  // 测试边界值的综合情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值的各种运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试极值之间的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}
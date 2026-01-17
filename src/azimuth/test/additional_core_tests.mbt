// 额外的核心功能测试
// 测试 azimuth 项目的主要功能和边界情况

test "add_with_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  @azimuth.assert_eq(0, @azimuth.add(1500000000, -1500000000))
}

test "multiply_with_large_numbers" {
  // 测试大数相乘
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
}

test "add_chained_operations" {
  // 测试链式加法运算
  let result = @azimuth.add(@azimuth.add(10, 20), @azimuth.add(30, 40))
  @azimuth.assert_eq(100, result)
}

test "multiply_chained_operations" {
  // 测试链式乘法运算
  let result = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result)
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \t\n!", @azimuth.greet("\t\n"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的周长
  let length = 15
  let width = 8
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(46, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：简化版复利计算
  let principal = 5000
  let rate = 6  // 6%
  let years = 4
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(125000, total)  // 5000 + 5000*6*4 = 125000
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景：摄氏度转华氏度（简化版）
  let celsius = 30
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(90, fahrenheit)  // 30 * 2 + 30 = 90
}

test "mixed_operations_test" {
  // 测试混合运算：((a + b) * c) - d
  let a = 12
  let b = 8
  let c = 5
  let d = 25
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(75, result)
}

test "edge_case_boundary_values" {
  // 测试边界值的边缘情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与负数相加
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(2147482647, @azimuth.add(max_val, -1000))
  
  // 最小值与正数相加
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(-2147482648, @azimuth.add(min_val, 1000))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}
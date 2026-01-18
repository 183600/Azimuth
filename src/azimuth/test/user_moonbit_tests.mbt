// 用户请求的 MoonBit 标准测试用例
// 包含10个测试用例，使用标准的 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 17
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "zero_value_properties" {
  // 测试零值特性
  let test_num = 42
  
  // 加法零元特性
  @azimuth.assert_eq(test_num, @azimuth.add(test_num, 0))
  @azimuth.assert_eq(test_num, @azimuth.add(0, test_num))
  
  // 乘法零元特性
  @azimuth.assert_eq(0, @azimuth.multiply(test_num, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, test_num))
}

test "unit_conversion_temperature" {
  // 测试单位转换：温度
  // 简化版摄氏度转华氏度: F = C * 2 + 30
  
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
}

test "financial_simple_interest" {
  // 测试简单利息计算
  let principal = 1000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  // 简单利息: 本金 + (本金 * 利率 * 时间)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000
}

test "geometry_rectangle_calculations" {
  // 测试几何计算：矩形
  let length = 10
  let width = 6
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(32, perimeter)
}

test "statistical_mean_calculation" {
  // 测试统计计算：平均值
  // 数据集: 15, 25, 35, 45, 55
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 25), 35), 45), 55)
  @azimuth.assert_eq(175, sum)
  
  let count = 5
  let mean = sum / count
  @azimuth.assert_eq(35, mean)
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值
  // 计算: (a + b) * c - d
  let a = 8
  let b = 7
  let c = 3
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum_ab, c)  // 45
  let result = @azimuth.add(product, -d)  // 35
  
  @azimuth.assert_eq(35, result)
}
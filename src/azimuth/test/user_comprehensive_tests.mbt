// 用户综合测试用例
// 覆盖 Azimuth 项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(-25, @azimuth.multiply(5, -5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
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

test "zero_value_operations" {
  // 测试零值运算
  let test_val = 42
  
  // 加法零值
  @azimuth.assert_eq(test_val, @azimuth.add(test_val, 0))
  @azimuth.assert_eq(test_val, @azimuth.add(0, test_val))
  
  // 乘法零值
  @azimuth.assert_eq(0, @azimuth.multiply(test_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, test_val))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 8
  let width = 6
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(28, perimeter)
}

test "financial_calculation_simple" {
  // 测试简单金融计算：总价计算
  let unit_price = 199
  let quantity = 3
  let discount = 50
  let tax_rate = 8
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 应用折扣
  let discounted_price = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax)
  
  @azimuth.assert_eq(565, final_price)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版 F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
}

test "statistical_calculation" {
  // 测试统计计算：平均值和范围
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let num4 = 80
  let num5 = 100
  
  // 计算总和
  let total = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  
  // 计算平均值
  let average = total / 5
  @azimuth.assert_eq(90, average)
  
  // 计算范围
  let range = @azimuth.add(100, -80)
  @azimuth.assert_eq(20, range)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试长字符串
  let long_name = "VeryLongNameWithManyCharactersForTesting"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  // 测试包含特殊字符的字符串
  let special_name = "test@example.com"
  @azimuth.assert_eq_string("Hello, " + special_name + "!", @azimuth.greet(special_name))
  
  // 测试包含空格的字符串
  let spaced_name = "John Doe"
  @azimuth.assert_eq_string("Hello, " + spaced_name + "!", @azimuth.greet(spaced_name))
}
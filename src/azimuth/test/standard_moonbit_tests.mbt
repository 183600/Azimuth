// 标准 MoonBit 测试用例集
// 覆盖基本功能和常见场景

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutative_property" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "mathematical_identity_elements" {
  // 测试数学单位元
  let test_value = 42
  
  // 加法单位元
  @azimuth.assert_eq(test_value, @azimuth.add(test_value, 0))
  @azimuth.assert_eq(test_value, @azimuth.add(0, test_value))
  
  // 乘法单位元
  @azimuth.assert_eq(test_value, @azimuth.multiply(test_value, 1))
  @azimuth.assert_eq(test_value, @azimuth.multiply(1, test_value))
}

test "calculation_area_of_rectangle" {
  // 测试矩形面积计算
  let length = 8
  let width = 6
  let expected_area = 48
  
  let actual_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(expected_area, actual_area)
}

test "calculation_perimeter_of_rectangle" {
  // 测试矩形周长计算
  let length = 10
  let width = 4
  let expected_perimeter = 28
  
  // 2 * (length + width)
  let sum_sides = @azimuth.add(length, width)
  let actual_perimeter = @azimuth.multiply(2, sum_sides)
  @azimuth.assert_eq(expected_perimeter, actual_perimeter)
}

test "simple_financial_calculation" {
  // 测试简单财务计算
  let price = 199
  let quantity = 3
  let tax_rate = 10  // 10%
  let expected_total = 656  // 199 * 3 + 10% tax
  
  // 计算小计
  let subtotal = @azimuth.multiply(price, quantity)
  
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算总计
  let total = @azimuth.add(subtotal, tax)
  @azimuth.assert_eq(expected_total, total)
}

test "temperature_conversion_simplified" {
  // 测试简化的温度转换
  // 使用简化公式：F = C * 2 + 30
  
  let celsius = 25
  let expected_fahrenheit = 80  // 25 * 2 + 30
  
  let actual_fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(expected_fahrenheit, actual_fahrenheit)
}

test "compound_interest_simple" {
  // 测试简化的复利计算
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 简化计算：最终金额 = 本金 * (1 + 利率 * 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let final_amount = @azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(16000, final_amount)
}
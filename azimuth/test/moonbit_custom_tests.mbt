// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "moon_basic_arithmetic" {
  // 基本算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
}

test "moon_division_operations" {
  // 除法运算测试
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
}

test "moon_string_processing" {
  // 字符串处理测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 2024!", @azimuth.greet("2024"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "moon_extreme_values" {
  // 极值处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "moon_complex_calculations" {
  // 复杂计算测试
  // 计算表达式：(a + b) * c - d / e
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  let e = 5
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let division = d / e
  let result = @azimuth.add(product, -division)
  
  @azimuth.assert_eq(87, result)
}

test "moon_financial_calculations" {
  // 金融计算测试
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简单利息计算：本金 + (本金 * 利率 * 年数)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)
}

test "moon_geometric_calculations" {
  // 几何计算测试
  let radius = 7
  
  // 圆周长（简化）：2 * π * r ≈ 2 * 3 * r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(42, circumference)
  
  // 圆面积（简化）：π * r² ≈ 3 * r²
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(147, area)
}

test "moon_temperature_conversion" {
  // 温度转换测试
  // 摄氏度转华氏度（简化）：F = C * 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度（简化）：C = (F - 30) / 2
  let fahrenheit_back = 80
  let celsius_back = @azimuth.add(fahrenheit_back, -30) / 2
  @azimuth.assert_eq(25, celsius_back)
}

test "moon_logical_operations" {
  // 逻辑运算测试
  let x = 15
  let y = 20
  let z = 25
  
  // 测试条件表达式
  let condition1 = x < y
  let result1 = if condition1 { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  @azimuth.assert_eq(35, result1)
  
  let condition2 = y > z
  let result2 = if condition2 { @azimuth.add(y, z) } else { @azimuth.multiply(y, z) }
  @azimuth.assert_eq(500, result2)
}

test "moon_data_validation" {
  // 数据验证测试
  let valid_numbers = [0, 1, -1, 100, -100, 2147483647, -2147483648]
  let valid_strings = ["", "a", "Hello", "测试", "123", "!@#$"]
  
  // 验证数字运算不溢出
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(2, @azimuth.add(1, 1))
  @azimuth.assert_eq(-2, @azimuth.add(-1, -1))
  
  // 验证字符串处理正确
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, Hello!", @azimuth.greet("Hello"))
}
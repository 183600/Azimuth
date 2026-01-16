// Azimuth 项目新增综合测试用例
// 使用标准 MoonBit 测试语法

test "add_zero_identity" {
  // 测试加法零恒等式
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
}

test "multiply_zero_property" {
  // 测试乘法零性质
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(-17, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -17))
}

test "multiply_identity" {
  // 测试乘法单位元
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(1, -17))
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-8, @azimuth.add(-5, -3))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(-5, @azimuth.add(7, -12))
  @azimuth.assert_eq(-50, @azimuth.add(-20, -30))
  @azimuth.assert_eq(0, @azimuth.add(-100, 100))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-5, -3))
  @azimuth.assert_eq(-50, @azimuth.multiply(-10, 5))
  @azimuth.assert_eq(-84, @azimuth.multiply(7, -12))
  @azimuth.assert_eq(600, @azimuth.multiply(-20, -30))
  @azimuth.assert_eq(0, @azimuth.multiply(-100, 0))
}

test "greet_special_characters" {
  // 测试特殊字符问候
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, hello_world!", @azimuth.greet("hello_world"))
  @azimuth.assert_eq_string("Hello, test-case!", @azimuth.greet("test-case"))
}

test "greet_multilingual" {
  // 测试多语言问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, العالم!", @azimuth.greet("العالم"))
  @azimuth.assert_eq_string("Hello, мир!", @azimuth.greet("мир"))
  @azimuth.assert_eq_string("Hello, monde!", @azimuth.greet("monde"))
  @azimuth.assert_eq_string("Hello, mundo!", @azimuth.greet("mundo"))
}

test "mathematical_series" {
  // 测试数学级数计算
  
  // 等差数列求和: 2+5+8+11+14 = 40
  let arithmetic_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(2, 5), 8), 11), 14)
  @azimuth.assert_eq(40, arithmetic_sum)
  
  // 平方数求和: 1²+2²+3²+4² = 1+4+9+16 = 30
  let squares_sum = @azimuth.add(@azimuth.add(@azimuth.add(1, 4), 9), 16)
  @azimuth.assert_eq(30, squares_sum)
}

test "financial_calculations" {
  // 测试金融计算场景
  
  // 计算复利: 本金 × (1 + 利率) ^ 期数
  // 简化为: 本金 + 本金 × 利率
  let principal = 1000
  let rate = 5  // 5%
  let interest = @azimuth.multiply(principal, rate) / 100
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(1050, total)
  
  // 计算折扣: 原价 × (1 - 折扣率)
  // 简化为: 原价 - 原价 × 折扣率
  let original_price = 200
  let discount_rate = 20  // 20%
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(160, final_price)
}

test "measurement_conversions" {
  // 测试计量单位转换
  
  // 英寸转厘米: 1英寸 = 2.54厘米，简化为整数运算
  let inches = 10
  let centimeters = @azimuth.multiply(inches, 254) / 100
  @azimuth.assert_eq(25, centimeters)  // 10 * 2.54 = 25.4，取整为25
  
  // 公斤转磅: 1公斤 = 2.20462磅，简化为整数运算
  let kilograms = 5
  let pounds = @azimuth.multiply(kilograms, 220) / 100
  @azimuth.assert_eq(11, pounds)  // 5 * 2.20 = 11.0
}
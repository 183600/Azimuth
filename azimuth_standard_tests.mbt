// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法

test "add_large_numbers" {
  // 测试大数加法
  assert_eq(azimuth::add(1000000, 2000000), 3000000)
  assert_eq(azimuth::add(999999, 1), 1000000)
  assert_eq(azimuth::add(500000, 500000), 1000000)
}

test "add_decimal_values" {
  // 测试小数值加法（通过整数模拟）
  assert_eq(azimuth::add(15, 25), 40)
  assert_eq(azimuth::add(123, 456), 579)
  assert_eq(azimuth::add(999, 1), 1000)
}

test "multiply_large_numbers" {
  // 测试大数乘法
  assert_eq(azimuth::multiply(1000, 1000), 1000000)
  assert_eq(azimuth::multiply(2500, 4), 10000)
  assert_eq(azimuth::multiply(123, 456), 56088)
}

test "multiply_fractions_simulation" {
  // 测试分数乘法模拟（通过整数除法）
  assert_eq(azimuth::multiply(10, 5) / 2, 25)
  assert_eq(azimuth::multiply(12, 3) / 4, 9)
  assert_eq(azimuth::multiply(15, 2) / 3, 10)
}

test "greet_international_names" {
  // 测试国际化名称问候
  assert_eq(azimuth::greet("张三"), "Hello, 张三!")
  assert_eq(azimuth::greet("José"), "Hello, José!")
  assert_eq(azimuth::greet("François"), "Hello, François!")
  assert_eq(azimuth::greet("Владимир"), "Hello, Владимир!")
}

test "greet_edge_cases" {
  // 测试边界情况问候
  assert_eq(azimuth::greet(""), "Hello, !")
  assert_eq(azimuth::greet(" "), "Hello,  !")
  assert_eq(azimuth::greet("!@#$%^&*()"), "Hello, !@#$%^&*()!")
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  // (a + b) * c
  assert_eq(azimuth::multiply(azimuth::add(5, 3), 2), 16)
  assert_eq(azimuth::multiply(azimuth::add(10, 15), 3), 75)
  
  // a * b + c * d
  assert_eq(azimuth::add(azimuth::multiply(2, 3), azimuth::multiply(4, 5)), 26)
  assert_eq(azimuth::add(azimuth::multiply(10, 2), azimuth::multiply(5, 3)), 35)
}

test "mathematical_sequences" {
  // 测试数学序列
  // 平方序列: 1² + 2² + 3²
  let squares_sum = azimuth::add(azimuth::add(azimuth::multiply(1, 1), azimuth::multiply(2, 2)), azimuth::multiply(3, 3))
  assert_eq(squares_sum, 14)
  
  // 立方序列: 1³ + 2³
  let cubes_sum = azimuth::add(azimuth::multiply(azimuth::multiply(1, 1), 1), azimuth::multiply(azimuth::multiply(2, 2), 2))
  assert_eq(cubes_sum, 9)
}

test "financial_calculations" {
  // 测试金融计算
  // 复利计算: 本金 * (1 + 利率)^时间
  let principal = 1000
  let rate = 5  // 5%
  let time = 2  // 2年
  
  // 简化版本: 本金 + 本金 * 利率 * 时间
  let simple_interest = azimuth::multiply(azimuth::multiply(principal, rate), time) / 100
  let total = azimuth::add(principal, simple_interest)
  assert_eq(total, 1100)
  
  // 折扣计算
  let original_price = 500
  let discount_rate = 20  // 20%
  let discount_amount = azimuth::multiply(original_price, discount_rate) / 100
  let final_price = azimuth::add(original_price, -discount_amount)
  assert_eq(final_price, 400)
}

test "measurement_conversions" {
  // 测试单位转换
  // 英寸转厘米: 英寸 * 2.54 (通过整数模拟)
  let inches = 10
  let centimeters = azimuth::multiply(inches, 254) / 100
  assert_eq(centimeters, 25)
  
  // 磅转千克: 磅 * 0.453592 (通过整数模拟)
  let pounds = 100
  let kilograms = azimuth::multiply(pounds, 45359) / 100000
  assert_eq(kilograms, 45)
  
  // 英里转公里: 英里 * 1.60934 (通过整数模拟)
  let miles = 10
  let kilometers = azimuth::multiply(miles, 160934) / 100000
  assert_eq(kilometers, 16)
}
// 用户自定义的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，测试 azimuth 模块的核心功能

test "add_identity_property" {
  // 测试加法单位元性质：a + 0 = a
  assert_eq(azimuth::add(42, 0), 42)
  assert_eq(azimuth::add(0, 42), 42)
  assert_eq(azimuth::add(-17, 0), -17)
  assert_eq(azimuth::add(0, -17), -17)
}

test "add_inverse_property" {
  // 测试加法逆元性质：a + (-a) = 0
  assert_eq(azimuth::add(15, -15), 0)
  assert_eq(azimuth::add(-23, 23), 0)
  assert_eq(azimuth::add(100, -100), 0)
}

test "multiply_identity_property" {
  // 测试乘法单位元性质：a * 1 = a
  assert_eq(azimuth::multiply(25, 1), 25)
  assert_eq(azimuth::multiply(1, 25), 25)
  assert_eq(azimuth::multiply(-8, 1), -8)
  assert_eq(azimuth::multiply(1, -8), -8)
}

test "multiply_zero_property" {
  // 测试乘法零元性质：a * 0 = 0
  assert_eq(azimuth::multiply(37, 0), 0)
  assert_eq(azimuth::multiply(0, 37), 0)
  assert_eq(azimuth::multiply(-12, 0), 0)
  assert_eq(azimuth::multiply(0, -12), 0)
}

test "greet_multilingual_support" {
  // 测试多语言支持的问候功能
  assert_eq_string(azimuth::greet("世界"), "Hello, 世界!")
  assert_eq_string(azimuth::greet("Monde"), "Hello, Monde!")
  assert_eq_string(azimuth::greet("Mundo"), "Hello, Mundo!")
  assert_eq_string(azimuth::greet("Мир"), "Hello, Мир!")
}

test "complex_arithmetic_expression" {
  // 测试复杂算术表达式：(a + b) * (c - d) + e
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  
  let sum_ab = azimuth::add(a, b)  // 15
  let diff_cd = azimuth::add(c, -d)  // 12
  let product = azimuth::multiply(sum_ab, diff_cd)  // 180
  let result = azimuth::add(product, e)  // 183
  
  assert_eq(183, result)
}

test "geometric_calculations" {
  // 测试几何计算
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 7
  let rectangle_area = azimuth::multiply(length, width)
  assert_eq(84, rectangle_area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = azimuth::multiply(base, height) / 2
  assert_eq(30, triangle_area)
  
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 5
  let bottom_base = 9
  let trapezoid_height = 4
  let trapezoid_area = azimuth::multiply(azimuth::add(top_base, bottom_base), trapezoid_height) / 2
  assert_eq(28, trapezoid_area)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景
  // 计算贷款月供：本金 × (1 + 利率 × 期数) ÷ 期数
  let principal = 12000
  let annual_rate = 6  // 6%
  let months = 12
  
  let monthly_rate = azimuth::divide_with_ceil(annual_rate, 12)  // 月利率
  let total_amount = azimuth::multiply(principal, azimuth::add(1, azimuth::multiply(monthly_rate, months)))
  let monthly_payment = azimuth::divide_with_ceil(total_amount, months)
  
  assert_eq(12700, monthly_payment)
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 摄氏度转华氏度：F = C × 9/5 + 32（简化为整数运算）
  let celsius_temperatures = [0, 10, 20, 30, 40]
  
  // 0°C = 32°F
  let temp0_f = azimuth::add(azimuth::multiply(0, 9) / 5, 32)
  assert_eq(32, temp0_f)
  
  // 100°C = 212°F
  let temp100_f = azimuth::add(azimuth::multiply(100, 9) / 5, 32)
  assert_eq(212, temp100_f)
  
  // -40°C = -40°F（唯一相等的点）
  let temp_minus40_f = azimuth::add(azimuth::multiply(-40, 9) / 5, 32)
  assert_eq(-40, temp_minus40_f)
}
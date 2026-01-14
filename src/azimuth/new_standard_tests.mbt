// 新增的标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = azimuth::add(a, b)
  let result2 = azimuth::add(b, a)
  azimuth::assert_eq(result1, result2)
  
  // 测试负数情况
  let c = -12345
  let d = 67890
  
  let result3 = azimuth::add(c, d)
  let result4 = azimuth::add(d, c)
  azimuth::assert_eq(result3, result4)
}

test "multiply_associative_property" {
  // 测试乘法结合律：(a * b) * c = a * (b * c)
  let a = 12
  let b = 15
  let c = 20
  
  let result1 = azimuth::multiply(azimuth::multiply(a, b), c)
  let result2 = azimuth::multiply(a, azimuth::multiply(b, c))
  azimuth::assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let x = -5
  let y = 8
  let z = -3
  
  let result3 = azimuth::multiply(azimuth::multiply(x, y), z)
  let result4 = azimuth::multiply(x, azimuth::multiply(y, z))
  azimuth::assert_eq(result3, result4)
}

test "add_distributive_property" {
  // 测试加法对乘法的分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 13
  let c = 19
  
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  azimuth::assert_eq(left_side, right_side)
}

test "greet_multilingual_support" {
  // 测试多语言字符串支持
  azimuth::assert_eq_string("Hello, English!", azimuth::greet("English"))
  azimuth::assert_eq_string("Hello, 中文!", azimuth::greet("中文"))
  azimuth::assert_eq_string("Hello, 日本語!", azimuth::greet("日本語"))
  azimuth::assert_eq_string("Hello, 한국어!", azimuth::greet("한국어"))
  azimuth::assert_eq_string("Hello, Español!", azimuth::greet("Español"))
}

test "extreme_boundary_values" {
  // 测试极值边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  azimuth::assert_eq(-1, azimuth::add(max_val, min_val))
  azimuth::assert_eq(max_val, azimuth::add(max_val, 0))
  azimuth::assert_eq(min_val, azimuth::add(min_val, 0))
  
  // 测试极值乘法
  azimuth::assert_eq(0, azimuth::multiply(max_val, 0))
  azimuth::assert_eq(0, azimuth::multiply(min_val, 0))
  azimuth::assert_eq(max_val, azimuth::multiply(max_val, 1))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, 1))
}

test "real_world_financial_calculation" {
  // 实际金融计算场景：贷款利息计算
  let principal = 100000  // 本金
  let annual_rate = 5     // 年利率5%
  let years = 3          // 年数
  
  // 简单利息计算：总利息 = 本金 × 利率 × 年数
  let total_interest = azimuth::multiply(principal, azimuth::multiply(annual_rate, years))
  // 总还款额 = 本金 + 总利息
  let total_payment = azimuth::add(principal, total_interest)
  
  // 验证计算结果：100000 + 100000 × 5 × 3 = 100000 + 1500000 = 1600000
  azimuth::assert_eq(1600000, total_payment)
}

test "temperature_conversion_calculation" {
  // 温度转换计算：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius_temperatures = [0, 10, 20, 30, 40, 100]
  let expected_fahrenheit = [30, 50, 70, 90, 110, 230]
  
  // 测试0°C
  let f1 = azimuth::add(azimuth::multiply(celsius_temperatures[0], 2), 30)
  azimuth::assert_eq(expected_fahrenheit[0], f1)
  
  // 测试20°C（室温）
  let f2 = azimuth::add(azimuth::multiply(celsius_temperatures[2], 2), 30)
  azimuth::assert_eq(expected_fahrenheit[2], f2)
  
  // 测试100°C（沸点）
  let f3 = azimuth::add(azimuth::multiply(celsius_temperatures[5], 2), 30)
  azimuth::assert_eq(expected_fahrenheit[5], f3)
}

test "complex_geometric_calculation" {
  // 复杂几何计算：圆柱体表面积和体积
  let radius = 5
  let height = 10
  let pi_approx = 3  // π的近似值
  
  // 圆柱体表面积 = 2 × π × r × (r + h)
  let surface_area = azimuth::multiply(2, azimuth::multiply(pi_approx, azimuth::multiply(radius, azimuth::add(radius, height))))
  // 预期：2 × 3 × 5 × (5 + 10) = 2 × 3 × 5 × 15 = 450
  azimuth::assert_eq(450, surface_area)
  
  // 圆柱体体积 = π × r² × h
  let volume = azimuth::multiply(pi_approx, azimuth::multiply(azimuth::multiply(radius, radius), height))
  // 预期：3 × 5² × 10 = 3 × 25 × 10 = 750
  azimuth::assert_eq(750, volume)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 测试空字符串
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  
  // 测试单字符
  azimuth::assert_eq_string("Hello, A!", azimuth::greet("A"))
  
  // 测试包含特殊字符的字符串
  azimuth::assert_eq_string("Hello, @#$%^&*()!", azimuth::greet("@#$%^&*()"))
  
  // 测试包含数字的字符串
  azimuth::assert_eq_string("Hello, 123456789!", azimuth::greet("123456789"))
  
  // 测试包含空格的字符串
  azimuth::assert_eq_string("Hello, John Doe!", azimuth::greet("John Doe"))
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列和等比数列
  // 等差数列求和：1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let arithmetic_sum = azimuth::multiply(n, azimuth::add(1, n)) / 2
  azimuth::assert_eq(55, arithmetic_sum)
  
  // 等比数列求和：2 + 2² + 2³ + 2⁴ = 2 + 4 + 8 + 16 = 30
  let geo_term1 = 2
  let geo_term2 = azimuth::multiply(2, 2)
  let geo_term3 = azimuth::multiply(azimuth::multiply(2, 2), 2)
  let geo_term4 = azimuth::multiply(azimuth::multiply(azimuth::multiply(2, 2), 2), 2)
  let geometric_sum = azimuth::add(azimuth::add(azimuth::add(geo_term1, geo_term2), geo_term3), geo_term4)
  azimuth::assert_eq(30, geometric_sum)
}
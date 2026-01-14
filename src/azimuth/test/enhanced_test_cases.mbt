// 增强的 MoonBit 测试用例
// 覆盖更多边界情况和实际应用场景

test "complex_arithmetic_operations" {
  // 测试复合算术运算：(a + b) * c - d
  let a = 15
  let b = 25
  let c = 4
  let d = 30
  
  let sum_ab = azimuth::add(a, b)
  let product = azimuth::multiply(sum_ab, c)
  let result = azimuth::add(product, -d)
  
  azimuth::assert_eq(130, result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, A!", azimuth::greet("A"))
  azimuth::assert_eq_string("Hello, 123!", azimuth::greet("123"))
  azimuth::assert_eq_string("Hello, 🌟!", azimuth::greet("🌟"))
}

test "mathematical_series_calculations" {
  // 测试数学数列计算
  // 计算等差数列前n项和：n * (first + last) / 2
  let n = 10
  let first = 1
  let last = 10
  
  let sum = azimuth::multiply(n, azimuth::add(first, last)) / 2
  azimuth::assert_eq(55, sum)
}

test "overflow_protection_mechanisms" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  azimuth::assert_eq(max_val, azimuth::add(max_val, 1000))
  azimuth::assert_eq(min_val, azimuth::add(min_val, -1000))
  
  // 测试乘法溢出保护
  azimuth::assert_eq(max_val, azimuth::multiply(max_val, 2))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, 2))
}

test "real_world_financial_calculations" {
  // 测试实际金融计算场景
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简单利息计算：本金 + 本金 * 利率 * 年数
  let interest = azimuth::multiply(principal, azimuth::multiply(rate, years))
  let total = azimuth::add(principal, interest)
  
  azimuth::assert_eq(160000, total)
}

test "geometric_calculations" {
  // 测试几何计算
  let length = 12
  let width = 8
  
  // 计算矩形面积
  let area = azimuth::multiply(length, width)
  azimuth::assert_eq(96, area)
  
  // 计算矩形周长：2 * (length + width)
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  azimuth::assert_eq(40, perimeter)
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化公式）：F = C * 2 + 30
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  azimuth::assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = azimuth::add(azimuth::multiply(0, 2), 30)
  azimuth::assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = azimuth::add(azimuth::multiply(100, 2), 30)
  azimuth::assert_eq(230, boiling)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数的正确性
  azimuth::assert_true(true)
  azimuth::assert_false(false)
  azimuth::assert_eq(42, 42)
  azimuth::assert_eq_string("test", "test")
  
  // 测试边界值的正确处理
  azimuth::assert_true(azimuth::add(max_val, 1) == max_val)
  azimuth::assert_true(azimuth::add(min_val, -1) == min_val)
}

test "performance_related_operations" {
  // 测试性能相关的操作
  let base = 100
  let multiplier = 1000
  
  // 测试大数运算
  let large_result = azimuth::multiply(base, multiplier)
  azimuth::assert_eq(100000, large_result)
  
  // 测试连续运算
  let consecutive_result = azimuth::add(azimuth::multiply(10, 20), azimuth::multiply(30, 40))
  azimuth::assert_eq(1400, consecutive_result)
}

test "data_validation_scenarios" {
  // 测试数据验证场景
  // 测试输入验证
  let valid_inputs = [0, 1, -1, 100, -100, 2147483647, -2147483648]
  
  // 验证每个输入与0的加法
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(1, azimuth::add(1, 0))
  azimuth::assert_eq(-1, azimuth::add(-1, 0))
  azimuth::assert_eq(100, azimuth::add(100, 0))
  azimuth::assert_eq(-100, azimuth::add(-100, 0))
  azimuth::assert_eq(2147483647, azimuth::add(2147483647, 0))
  azimuth::assert_eq(-2147483648, azimuth::add(-2147483648, 0))
  
  // 验证每个输入与1的乘法
  azimuth::assert_eq(0, azimuth::multiply(0, 1))
  azimuth::assert_eq(1, azimuth::multiply(1, 1))
  azimuth::assert_eq(-1, azimuth::multiply(-1, 1))
  azimuth::assert_eq(100, azimuth::multiply(100, 1))
  azimuth::assert_eq(-100, azimuth::multiply(-100, 1))
  azimuth::assert_eq(2147483647, azimuth::multiply(2147483647, 1))
  azimuth::assert_eq(-2147483648, azimuth::multiply(-2147483648, 1))
}
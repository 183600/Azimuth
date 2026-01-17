// 新增的 MoonBit 测试用例
// 测试 azimuth 项目中的核心功能

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
}

test "greet_multilingual" {
  // 测试多语言字符串
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "add_sequence_calculation" {
  // 测试序列计算：1+2+3+4+5 = 15
  let sum = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum)
}

test "multiply_exponential_calculation" {
  // 测试指数计算：2^4 = 16
  let result = multiply(multiply(2, 2), multiply(2, 2))
  assert_eq(16, result)
}

test "business_calculation_scenario" {
  // 测试商业计算场景
  let unit_price = 199
  let quantity = 5
  let discount = 50
  let tax_rate = 8
  
  // 计算总价
  let subtotal = multiply(unit_price, quantity)
  // 应用折扣
  let discounted = add(subtotal, -discount)
  // 计算税费
  let tax = multiply(discounted, tax_rate) / 100
  // 最终价格
  let final_price = add(discounted, tax)
  
  assert_eq(1052, final_price)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30
  let celsius = 15
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(60, fahrenheit)
  
  // 华氏度转摄氏度（简化版）：C = (F - 30) / 2
  let fahrenheit_back = 60
  let celsius_back = add(fahrenheit_back, -30) / 2
  assert_eq(15, celsius_back)
}

test "geometry_perimeter_calculation" {
  // 测试几何周长计算
  let length = 12
  let width = 8
  
  // 矩形周长：2 × (length + width)
  let perimeter = multiply(2, add(length, width))
  assert_eq(40, perimeter)
  
  // 正方形周长：4 × side
  let side = 7
  let square_perimeter = multiply(4, side)
  assert_eq(28, square_perimeter)
}

test "complex_expression_evaluation" {
  // 测试复杂表达式计算：(a + b) × (c - d) + e
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 15
  
  let sum_ab = add(a, b)
  let diff_cd = add(c, -d)
  let product = multiply(sum_ab, diff_cd)
  let result = add(product, e)
  
  assert_eq(205, result)
}

test "edge_case_zero_operations" {
  // 测试零的边缘情况
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, multiply(0, -2147483648))
  assert_eq(2147483647, add(0, 2147483647))
  assert_eq(-2147483648, add(0, -2147483648))
  assert_eq_string("Hello, !", greet(""))
}
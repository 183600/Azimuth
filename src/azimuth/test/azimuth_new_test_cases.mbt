// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，覆盖更多功能场景

test "advanced_addition_scenarios" {
  // 测试高级加法场景
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(-50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 100))  // 测试溢出保护
}

test "advanced_multiplication_scenarios" {
  // 测试高级乘法场景
  @azimuth.assert_eq(10000, @azimuth.multiply(100, 100))
  @azimuth.assert_eq(-10000, @azimuth.multiply(-100, 100))
  @azimuth.assert_eq(10000, @azimuth.multiply(-100, -100))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 2))  // 测试溢出保护
}

test "greeting_with_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, John_Doe!", @azimuth.greet("John_Doe"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "mathematical_expressions" {
  // 测试数学表达式组合
  let a = 10
  let b = 5
  let c = 3
  
  // 计算 (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(45, result1)
  
  // 计算 a * b + a * c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(80, result2)
}

test "division_simulation" {
  // 测试除法模拟（使用乘法和减法）
  // 模拟 10 / 2 = 5
  let dividend = 10
  let divisor = 2
  let mut quotient = 0
  let mut remainder = dividend
  
  // 简单的除法模拟
  while remainder >= divisor {
    remainder = @azimuth.add(remainder, -divisor)
    quotient = @azimuth.add(quotient, 1)
  }
  
  @azimuth.assert_eq(5, quotient)
  @azimuth.assert_eq(0, remainder)
}

test "business_calculation_scenario" {
  // 测试商业计算场景
  let unit_price = 199
  let quantity = 3
  let discount = 50
  let tax = 30
  
  // 计算总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 减去折扣
  let discounted = @azimuth.add(subtotal, -discount)
  // 加上税费
  let total = @azimuth.add(discounted, tax)
  
  @azimuth.assert_eq(637, total)
}

test "geometry_calculations" {
  // 测试几何计算
  let length = 8
  let width = 6
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)
  
  // 计算周长
  let perimeter = @azimuth.multiply(@azimuth.add(length, width), 2)
  @azimuth.assert_eq(28, perimeter)
  
  // 计算对角线长度的平方（勾股定理）
  let diagonal_squared = @azimuth.add(@azimuth.multiply(length, length), @azimuth.multiply(width, width))
  @azimuth.assert_eq(100, diagonal_squared)
}

test "series_calculations" {
  // 测试序列计算
  // 计算等差数列 2, 4, 6, 8, 10 的和
  let term1 = 2
  let term2 = @azimuth.add(term1, 2)
  let term3 = @azimuth.add(term2, 2)
  let term4 = @azimuth.add(term3, 2)
  let term5 = @azimuth.add(term4, 2)
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(30, sum)
}

test "string_processing_with_numbers" {
  // 测试包含数字的字符串处理
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
  @azimuth.assert_eq_string("Hello, Version1.0!", @azimuth.greet("Version1.0"))
  @azimuth.assert_eq_string("Hello, 123-456-7890!", @azimuth.greet("123-456-7890"))
}

test "complex_nested_calculations" {
  // 测试复杂嵌套计算
  let base = 5
  let multiplier = 3
  let offset = 10
  let divisor = 2
  
  // 计算 ((base * multiplier) + offset) / divisor
  let product = @azimuth.multiply(base, multiplier)
  let sum = @azimuth.add(product, offset)
  
  // 模拟整数除法
  let result = sum / divisor
  
  @azimuth.assert_eq(12, result)  // ((5*3)+10)/2 = 25/2 = 12 (整数除法)
}
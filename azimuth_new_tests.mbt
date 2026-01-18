// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含多样化测试场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, add(5, -7))
  assert_eq(0, multiply(10, 0))
}

test "string_concatenation" {
  // 测试字符串连接
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_expressions" {
  // 测试复杂表达式
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(16, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)
}

test "boundary_conditions" {
  // 测试边界条件
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 边界值加法
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
  
  // 边界值乘法
  assert_eq(0, multiply(max_int, 0))
  assert_eq(0, multiply(min_int, 0))
}

test "mathematical_series" {
  // 测试数学级数
  // 等差数列: 1 + 2 + 3 + 4 + 5 = 15
  let sum_5 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum_5)
  
  // 等比数列: 2 + 4 + 8 = 14
  let geo_sum = add(add(2, 4), 8)
  assert_eq(14, geo_sum)
}

test "financial_calculations" {
  // 测试金融计算
  let price = 100
  let quantity = 3
  let discount = 10
  let tax = 5
  
  let subtotal = multiply(price, quantity)
  let discounted = add(subtotal, -discount)
  let total = add(discounted, tax)
  
  assert_eq(295, total)
}

test "geometry_calculations" {
  // 测试几何计算
  let length = 8
  let width = 4
  
  // 面积
  let area = multiply(length, width)
  assert_eq(32, area)
  
  // 周长
  let perimeter = multiply(2, add(length, width))
  assert_eq(24, perimeter)
}

test "temperature_conversion" {
  // 测试温度转换
  let celsius = 25
  // 简化的华氏度转换: F = C * 2 + 30
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "time_calculations" {
  // 测试时间计算
  let hours = 2
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(150, total_minutes)
  
  // 转换为总秒数
  let total_seconds = multiply(total_minutes, 60)
  assert_eq(9000, total_seconds)
}

test "nested_operations" {
  // 测试嵌套运算
  let base = 5
  
  // ((base + 3) * 2) - 4
  let result1 = add(multiply(add(base, 3), 2), -4)
  assert_eq(12, result1)
  
  // (base * 2) + (base * 3)
  let result2 = add(multiply(base, 2), multiply(base, 3))
  assert_eq(25, result2)
}
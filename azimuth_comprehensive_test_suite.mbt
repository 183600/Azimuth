// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法，包含多样化测试场景

test "string_greeting_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
}

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(0, add(0, 0))
  assert_eq(10, add(5, 5))
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(100, add(50, 50))
  assert_eq(1000, add(999, 1))
  assert_eq(-1000, add(-500, -500))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(-25, multiply(5, -5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(100, multiply(10, 10))
}

test "complex_calculations" {
  // 测试复杂计算场景
  let a = 10
  let b = 5
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(30, result1)
  
  // a * b + a * c
  let result2 = add(multiply(a, b), multiply(a, c))
  assert_eq(70, result2)
  
  // (a - b) * (a + b)
  let result3 = multiply(add(a, -b), add(a, b))
  assert_eq(75, result3)
}

test "financial_calculations" {
  // 测试金融计算场景
  let price = 100
  let quantity = 3
  let tax_rate = 10
  let discount = 20
  
  // 计算总价（含税）
  let subtotal = multiply(price, quantity)
  let tax = multiply(subtotal, tax_rate) / 100
  let total = add(subtotal, tax)
  assert_eq(330, total)
  
  // 应用折扣
  let final_price = add(total, -discount)
  assert_eq(310, final_price)
}

test "geometry_calculations" {
  // 测试几何计算
  let length = 8
  let width = 6
  let height = 4
  
  // 计算矩形面积
  let area = multiply(length, width)
  assert_eq(48, area)
  
  // 计算矩形周长
  let perimeter = multiply(2, add(length, width))
  assert_eq(28, perimeter)
  
  // 计算长方体体积
  let volume = multiply(multiply(length, width), height)
  assert_eq(192, volume)
}

test "time_conversions" {
  // 测试时间转换计算
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 转换为总秒数
  let total_seconds = add(multiply(add(multiply(hours, 60), minutes), 60), seconds)
  assert_eq(9045, total_seconds)
  
  // 计算一天中的秒数
  let day_seconds = multiply(24, multiply(60, 60))
  assert_eq(86400, day_seconds)
}

test "sequence_calculations" {
  // 测试序列计算
  // 计算前5个自然数的和：1+2+3+4+5
  let sum1 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum1)
  
  // 计算前5个偶数的和：2+4+6+8+10
  let sum2 = add(add(add(add(2, 4), 6), 8), 10)
  assert_eq(30, sum2)
  
  // 计算阶乘 5! = 5*4*3*2*1
  let factorial = multiply(multiply(multiply(multiply(5, 4), 3), 2), 1)
  assert_eq(120, factorial)
}

test "temperature_conversions" {
  // 测试温度转换
  let celsius = 25
  
  // 摄氏度转华氏度: F = C * 9/5 + 32
  let fahrenheit = add(multiply(celsius, 9) / 5, 32)
  assert_eq(77, fahrenheit)
  
  // 摄氏度转开尔文: K = C + 273
  let kelvin = add(celsius, 273)
  assert_eq(298, kelvin)
  
  // 水的冰点测试
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 9) / 5, 32)
  assert_eq(32, freezing_fahrenheit)
}
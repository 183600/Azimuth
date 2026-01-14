// 新增的 MoonBit 测试用例
// 测试 azimuth 包的额外功能和边界情况

test "add_function_large_numbers" {
  // 测试大数加法
  assert_eq(2000000000, azimuth::add(1000000000, 1000000000))
  assert_eq(-2000000000, azimuth::add(-1000000000, -1000000000))
  assert_eq(0, azimuth::add(1500000000, -1500000000))
}

test "multiply_function_large_numbers" {
  // 测试大数乘法
  assert_eq(1000000, azimuth::multiply(1000, 1000))
  assert_eq(-1000000, azimuth::multiply(-1000, 1000))
  assert_eq(1000000, azimuth::multiply(-1000, -1000))
  assert_eq(4000000, azimuth::multiply(2000, 2000))
}

test "greet_function_unicode_and_emoji" {
  // 测试 Unicode 和 emoji 字符
  assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
  assert_eq_string("Hello, 👋!", azimuth::greet("👋"))
  assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列和：1 + 2 + 3 + 4 + 5 = 15
  let sum1 = azimuth::add(azimuth::add(1, 2), azimuth::add(3, 4))
  let total1 = azimuth::add(sum1, 5)
  assert_eq(15, total1)
  
  // 计算等比数列和：2 + 4 + 8 + 16 = 30
  let sum2 = azimuth::add(azimuth::add(2, 4), azimuth::add(8, 16))
  assert_eq(30, sum2)
}

test "complex_business_calculation" {
  // 测试复杂业务计算
  // 计算订单总价：商品价格 × 数量 + 税费 + 运费
  let unit_price = 299
  let quantity = 2
  let tax_rate = 8  // 8%
  let shipping = 15
  
  let subtotal = azimuth::multiply(unit_price, quantity)
  let tax = azimuth::multiply(subtotal, tax_rate) / 100
  let total = azimuth::add(azimuth::add(subtotal, tax), shipping)
  
  assert_eq(658, total)  // 299*2 + 299*2*8/100 + 15 = 598 + 47 + 15 = 660
}

test "temperature_conversion_calculation" {
  // 测试温度转换计算
  // 摄氏度转华氏度：F = C * 9/5 + 32 (简化为 F = C * 2 + 30)
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9 (简化为 C = (F - 30) / 2)
  let fahrenheit2 = 80
  let celsius2 = (azimuth::add(fahrenheit2, -30)) / 2
  assert_eq(25, celsius2)
}

test "geometric_area_calculations" {
  // 测试几何面积计算
  // 矩形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = azimuth::multiply(length, width)
  assert_eq(96, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = (azimuth::multiply(base, height)) / 2
  assert_eq(30, triangle_area)
  
  // 梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 6
  let bottom_base = 10
  let trapezoid_height = 4
  let trapezoid_area = (
    azimuth::multiply(azimuth::add(top_base, bottom_base), trapezoid_height)
  ) / 2
  assert_eq(32, trapezoid_area)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 空字符串
  assert_eq_string("Hello, !", azimuth::greet(""))
  
  // 单个字符
  assert_eq_string("Hello, A!", azimuth::greet("A"))
  
  // 包含空格的字符串
  assert_eq_string("Hello, John Doe!", azimuth::greet("John Doe"))
  
  // 包含特殊符号的字符串
  assert_eq_string("Hello, test@example.com!", azimuth::greet("test@example.com"))
  
  // 包含数字的字符串
  assert_eq_string("Hello, User123!", azimuth::greet("User123"))
}

test "overflow_and_underflow_protection" {
  // 测试溢出和下溢保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::add(max_val, max_val))
  
  // 加法下溢保护
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::add(min_val, min_val))
  
  // 乘法溢出保护
  assert_eq(max_val, azimuth::multiply(max_val, 2))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  
  // 乘法下溢保护
  assert_eq(min_val, azimuth::multiply(min_val, 2))
}

test "nested_function_calls" {
  // 测试嵌套函数调用
  // 计算 ((a + b) * (c + d)) - (e * f)
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 8
  let f = 12
  
  let sum1 = azimuth::add(a, b)  // 30
  let sum2 = azimuth::add(c, d)  // 20
  let product1 = azimuth::multiply(sum1, sum2)  // 600
  let product2 = azimuth::multiply(e, f)  // 96
  let result = azimuth::add(product1, -product2)  // 504
  
  assert_eq(504, result)
}
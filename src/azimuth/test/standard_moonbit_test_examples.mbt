// 标准 MoonBit 测试用例示例
// 测试 azimuth 包中的核心功能

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  assert_eq(5, azimuth::add(2, 3))
  assert_eq(0, azimuth::add(-2, 2))
  assert_eq(-5, azimuth::add(-2, -3))
  assert_eq(100, azimuth::add(50, 50))
}

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(max_val, azimuth::add(max_val, max_val))
  
  // 测试零值
  assert_eq(5, azimuth::add(5, 0))
  assert_eq(-5, azimuth::add(-5, 0))
  assert_eq(0, azimuth::add(0, 0))
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  assert_eq(6, azimuth::multiply(2, 3))
  assert_eq(-6, azimuth::multiply(2, -3))
  assert_eq(6, azimuth::multiply(-2, -3))
  assert_eq(0, azimuth::multiply(5, 0))
  assert_eq(5, azimuth::multiply(5, 1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, azimuth::multiply(max_val, 2))
  assert_eq(min_val, azimuth::multiply(min_val, 2))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  
  // 测试特殊值
  assert_eq(0, azimuth::multiply(0, max_val))
  assert_eq(0, azimuth::multiply(0, min_val))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 张三!", azimuth::greet("张三"))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  assert_eq_string("Hello, @#$!", azimuth::greet("@#$"))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  // (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum = azimuth::add(a, b)
  let product = azimuth::multiply(sum, c)
  let result = azimuth::add(product, -d)
  
  assert_eq(38, result)  // (10 + 5) * 3 - 7 = 38
}

test "real_world_shopping_cart" {
  // 测试真实世界场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  let subtotal1 = azimuth::multiply(item1_price, item1_quantity)
  let subtotal2 = azimuth::multiply(item2_price, item2_quantity)
  let items_total = azimuth::add(subtotal1, subtotal2)
  let final_total = azimuth::add(items_total, shipping_fee)
  
  assert_eq(717, final_total)  // 199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
}

test "temperature_conversion_simplified" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C * 2 + 30 (简化公式)
  let celsius_0 = 0
  let celsius_25 = 25
  let celsius_100 = 100
  
  let fahrenheit_0 = azimuth::add(azimuth::multiply(celsius_0, 2), 30)
  let fahrenheit_25 = azimuth::add(azimuth::multiply(celsius_25, 2), 30)
  let fahrenheit_100 = azimuth::add(azimuth::multiply(celsius_100, 2), 30)
  
  assert_eq(30, fahrenheit_0)    // 0 * 2 + 30 = 30
  assert_eq(80, fahrenheit_25)  // 25 * 2 + 30 = 80
  assert_eq(230, fahrenheit_100) // 100 * 2 + 30 = 230
}

test "mathematical_series_calculations" {
  // 测试数学序列计算
  // 等差数列求和：1 + 3 + 5 + 7 + 9 = 25
  let sum1 = azimuth::add(azimuth::add(1, 3), azimuth::add(5, 7))
  let total1 = azimuth::add(sum1, 9)
  assert_eq(25, total1)
  
  // 等比数列求和：2 + 4 + 8 = 14
  let sum2 = azimuth::add(azimuth::multiply(1, 2), azimuth::multiply(2, 2))
  let total2 = azimuth::add(sum2, azimuth::multiply(3, 2))
  assert_eq(14, total2)
}

test "financial_calculations" {
  // 测试金融计算
  // 简单利息计算：本金 + 利息
  let principal = 1000
  let rate = 5
  let years = 3
  
  let interest = azimuth::multiply(principal, azimuth::multiply(rate, years))
  let total = azimuth::add(principal, interest)
  
  assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000 (表示1600元)
}
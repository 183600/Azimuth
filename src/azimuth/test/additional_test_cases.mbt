// 新增的 MoonBit 测试用例
// 测试 azimuth 包中的函数

test "add_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值相加
  assert_eq(-1, azimuth::add(max_val, min_val))
  
  // 测试大数相加
  assert_eq(2000000000, azimuth::add(1000000000, 1000000000))
  assert_eq(-2000000000, azimuth::add(-1000000000, -1000000000))
  
  // 测试连续相加
  let sum1 = azimuth::add(1, 2)
  let sum2 = azimuth::add(3, 4)
  let sum3 = azimuth::add(sum1, sum2)
  assert_eq(15, azimuth::add(sum3, 5))
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘
  assert_eq(1000000, azimuth::multiply(1000, 1000))
  assert_eq(-1000000, azimuth::multiply(-1000, 1000))
  
  // 测试连续相乘
  assert_eq(24, azimuth::multiply(azimuth::multiply(2, 3), 4))
  
  // 测试最小值与-1相乘
  assert_eq(min_val, azimuth::multiply(min_val, -1))
}

test "greet_variations" {
  // 测试问候函数的各种情况
  // 测试空字符串
  assert_eq_string("Hello, !", azimuth::greet(""))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", azimuth::greet("John Doe"))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @#$%!", azimuth::greet("@#$%"))
  
  // 测试Unicode字符
  assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
}

test "complex_calculations" {
  // 测试复合计算
  // 计算 (a + b) * c
  let a = 10
  let b = 20
  let c = 3
  
  let sum = azimuth::add(a, b)
  let result = azimuth::multiply(sum, c)
  assert_eq(90, result)  // (10 + 20) * 3 = 90
  
  // 计算 a * b + c * d
  let d = 4
  let product1 = azimuth::multiply(a, b)
  let product2 = azimuth::multiply(c, d)
  let result2 = azimuth::add(product1, product2)
  assert_eq(212, result2)  // 10 * 20 + 3 * 4 = 200 + 12 = 212
}

test "mathematical_series" {
  // 测试数学序列计算
  // 计算等差数列前5项的和：1 + 3 + 5 + 7 + 9 = 25
  let sum1 = azimuth::add(azimuth::add(1, 3), azimuth::add(5, 7))
  let total1 = azimuth::add(sum1, 9)
  assert_eq(25, total1)
  
  // 计算等比数列前4项的和：2 + 4 + 8 + 16 = 30
  let term2 = azimuth::multiply(2, 2)
  let term3 = azimuth::multiply(term2, 2)
  let term4 = azimuth::multiply(term3, 2)
  let sum2 = azimuth::add(azimuth::add(2, term2), azimuth::add(term3, term4))
  assert_eq(30, sum2)
}

test "real_world_scenarios" {
  // 测试真实世界场景
  // 购物车计算：商品价格 * 数量 + 运费
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = azimuth::multiply(item_price, quantity)
  let total = azimuth::add(subtotal, shipping)
  assert_eq(612, total)  // 199 * 3 + 15 = 597 + 15 = 612
  
  // 温度转换：摄氏度转华氏度（简化版）
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
}

test "performance_oriented" {
  // 测试性能相关的计算
  // 批量计算：0 + 1 + 2 + ... + 9 = 45
  let sum1 = azimuth::add(0, 1)
  let sum2 = azimuth::add(sum1, 2)
  let sum3 = azimuth::add(sum2, 3)
  let sum4 = azimuth::add(sum3, 4)
  let sum5 = azimuth::add(sum4, 5)
  let sum6 = azimuth::add(sum5, 6)
  let sum7 = azimuth::add(sum6, 7)
  let sum8 = azimuth::add(sum7, 8)
  let sum9 = azimuth::add(sum8, 9)
  assert_eq(45, sum9)
  
  // 阶乘计算：5! = 120
  let factorial = azimuth::multiply(5, azimuth::multiply(4, azimuth::multiply(3, azimuth::multiply(2, 1))))
  assert_eq(120, factorial)
}

test "error_handling" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::add(max_val, 100))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::add(min_val, -100))
  
  // 测试乘法溢出
  assert_eq(max_val, azimuth::multiply(46341, 46341))
  assert_eq(min_val, azimuth::multiply(-46341, 46341))
}

test "string_operations" {
  // 测试字符串操作
  // 测试不同长度的字符串
  assert_eq_string("Hello, A!", azimuth::greet("A"))
  assert_eq_string("Hello, ABC!", azimuth::greet("ABC"))
  assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", azimuth::greet("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  
  // 测试包含数字的字符串
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  assert_eq_string("Hello, Test123!", azimuth::greet("Test123"))
  
  // 测试包含符号的字符串
  assert_eq_string("Hello, test@example.com!", azimuth::greet("test@example.com"))
  assert_eq_string("Hello, user_2023!", azimuth::greet("user_2023"))
}
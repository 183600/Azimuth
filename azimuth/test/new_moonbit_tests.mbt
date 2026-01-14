test "string_concatenation_with_unicode" {
  // 测试Unicode字符串拼接
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
}

test "mathematical_sequence_calculation" {
  // 计算斐波那契数列前几项的和
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)
  let fib4 = @azimuth.add(fib2, fib3)
  let fib5 = @azimuth.add(fib3, fib4)
  
  // 计算前5项的和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(fib1, fib2), fib3), fib4), fib5)
  @azimuth.assert_eq(12, sum)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度：F = C * 9/5 + 32，简化为整数版本：F = C * 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  // 测试沸点
  let boiling = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
}

test "shopping_cart_total_calculation" {
  // 购物车总价计算
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 1
  let item3_price = 99
  let item3_qty = 3
  
  // 计算每种商品总价
  let total1 = @azimuth.multiply(item1_price, item1_qty)
  let total2 = @azimuth.multiply(item2_price, item2_qty)
  let total3 = @azimuth.multiply(item3_price, item3_qty)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), total3)
  @azimuth.assert_eq(1094, cart_total)
}

test "compound_interest_calculation" {
  // 复利计算简化版：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5 // 5%
  let years = 3
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)
}

test "area_perimeter_calculation" {
  // 矩形面积和周长计算
  let length = 12
  let width = 8
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(40, perimeter)
}

test "number_palindrome_check" {
  // 检查数字是否为回文数（简化测试）
  let num1 = 121
  let num2 = 123
  
  // 模拟回文检查（简化版）
  let reversed_num1 = @azimuth.add(@azimuth.multiply(100, 1), @azimuth.add(@azimuth.multiply(10, 2), 1))
  let reversed_num2 = @azimuth.add(@azimuth.multiply(100, 3), @azimuth.add(@azimuth.multiply(10, 2), 1))
  
  @azimuth.assert_true(num1 == reversed_num1)  // 121是回文数
  @azimuth.assert_false(num2 == reversed_num2) // 123不是回文数
}

test "greeting_with_various_formats" {
  // 测试不同格式的问候字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, user_123!", @azimuth.greet("user_123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
}

test "extreme_boundary_operations" {
  // 极端边界值运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值加法
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试边界值乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}
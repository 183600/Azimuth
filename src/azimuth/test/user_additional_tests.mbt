// 用户额外的 MoonBit 测试用例
// 这些测试用例涵盖了 azimuth 库的各种功能场景

test "add_zero_and_negative" {
  // 测试零与负数相加
  @azimuth.assert_eq(@azimuth.add(0, -5), -5)
  @azimuth.assert_eq(@azimuth.add(-10, 0), -10)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
}

test "multiply_boundary_values" {
  // 测试边界值相乘
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(@azimuth.multiply(max_val, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 1), min_val)
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string(@azimuth.greet("测试@123"), "Hello, 测试@123!")
  @azimuth.assert_eq_string(@azimuth.greet("user@example.com"), "Hello, user@example.com!")
  @azimuth.assert_eq_string(@azimuth.greet("John_Doe"), "Hello, John_Doe!")
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c - a
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -a)
  
  @azimuth.assert_eq(result, 890)  // (10 + 20) * 30 - 10 = 900 - 10 = 890
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(@azimuth.add(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.add(max_val, 100), max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1), min_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -100), min_val)
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2), max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1), min_val)
  @azimuth.assert_eq(@azimuth.multiply(46341, 46341), max_val)
}

test "greet_with_unicode" {
  // 测试 Unicode 字符串处理
  @azimuth.assert_eq_string(@azimuth.greet("张三"), "Hello, 张三!")
  @azimuth.assert_eq_string(@azimuth.greet("🌍"), "Hello, 🌍!")
  @azimuth.assert_eq_string(@azimuth.greet("Café"), "Hello, Café!")
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), 
                     @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
}

test "real_world_scenario_shopping" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  // 计算总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(cart_total, 697)  // 199*2 + 299*1 = 398 + 299 = 697
}

test "temperature_conversion" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化版）：F = C * 2 + 30
  let celsius_temp = 25
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temp, 2), 30)
  
  @azimuth.assert_eq(fahrenheit_temp, 80)  // 25 * 2 + 30 = 80
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  
  @azimuth.assert_eq(freezing_fahrenheit, 30)  // 0 * 2 + 30 = 30
}
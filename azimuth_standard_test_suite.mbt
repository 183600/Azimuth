// 标准 MoonBit 测试套件
// 包含10个针对 azimuth 核心功能的测试用例

test "add_function_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  let result1 = azimuth.add(a, b)
  let result2 = azimuth.add(b, a)
  azimuth.assert_eq(result1, result2)
}

test "multiply_function_commutativity" {
  // 测试乘法交换律：a * b = b * a
  let a = -123
  let b = 456
  let result1 = azimuth.multiply(a, b)
  let result2 = azimuth.multiply(b, a)
  azimuth.assert_eq(result1, result2)
}

test "add_function_associativity" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 1000
  let b = 2000
  let c = 3000
  let result1 = azimuth.add(azimuth.add(a, b), c)
  let result2 = azimuth.add(a, azimuth.add(b, c))
  azimuth.assert_eq(result1, result2)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 15
  let b = 20
  let c = 25
  let left_side = azimuth.multiply(a, azimuth.add(b, c))
  let right_side = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  azimuth.assert_eq(left_side, right_side)
}

test "greet_function_multilingual" {
  // 测试多语言问候功能
  azimuth.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  azimuth.assert_eq_string("Hello, Москва!", azimuth.greet("Москва"))
  azimuth.assert_eq_string("Hello, العربية!", azimuth.greet("العربية"))
  azimuth.assert_eq_string("Hello, 🌎!", azimuth.greet("🌎"))
}

test "complex_arithmetic_expression" {
  // 测试复杂算术表达式：(a + b) * (c - d) + e * f
  let a = 10
  let b = 15
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = azimuth.add(a, b)
  let diff_cd = azimuth.add(c, -d)
  let product_ef = azimuth.multiply(e, f)
  let result = azimuth.add(azimuth.multiply(sum_ab, diff_cd), product_ef)
  
  azimuth.assert_eq(251, result) // (10+15)*(20-8) + 3*7 = 25*12 + 21 = 300 + 21 = 321
}

test "boundary_value_arithmetic" {
  // 测试边界值算术运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  azimuth.assert_eq(-1, azimuth.add(max_val, min_val))
  azimuth.assert_eq(min_val, azimuth.multiply(min_val, -1))
  azimuth.assert_eq(0, azimuth.multiply(max_val, 0))
  azimuth.assert_eq(0, azimuth.multiply(min_val, 0))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello,   !", azimuth.greet("  "))
  azimuth.assert_eq_string("Hello, \n!", azimuth.greet("\\n"))
  azimuth.assert_eq_string("Hello, \t!", azimuth.greet("\\t"))
  azimuth.assert_eq_string("Hello, \"quote\"!", azimuth.greet("\"quote\""))
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算：斐波那契数列前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  let f0 = 0
  let f1 = 1
  let f2 = azimuth.add(f0, f1)
  let f3 = azimuth.add(f1, f2)
  let f4 = azimuth.add(f2, f3)
  let f5 = azimuth.add(f3, f4)
  
  azimuth.assert_eq(1, f2)
  azimuth.assert_eq(2, f3)
  azimuth.assert_eq(3, f4)
  azimuth.assert_eq(5, f5)
}

test "real_world_application_scenario" {
  // 测试实际应用场景：计算购物车总价（含折扣和税费）
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount_threshold = 1000
  let discount_rate = 10
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal1 = azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = azimuth.multiply(item2_price, item2_quantity)
  let subtotal = azimuth.add(subtotal1, subtotal2)
  
  // 计算折扣（如果超过阈值）
  let discount = if (subtotal > discount_threshold) {
    azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价
  let final_total = azimuth.add(discounted_total, tax)
  
  azimuth.assert_eq(1206, final_total) // 299*2 + 199*3 = 598 + 597 = 1195
                                          // 折扣: 1195 * 10% = 119
                                          // 折后: 1195 - 119 = 1076
                                          // 税费: 1076 * 8% = 86
                                          // 总价: 1076 + 86 = 1162
}
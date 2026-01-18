// 用户最终的 MoonBit 测试用例
// 这些测试用例展示了 MoonBit 标准测试语法和最佳实践

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 123
  let b = 456
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试负数的交换律
  let c = -100
  let d = 200
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
}

test "multiply_associative_property" {
  // 测试乘法结合律：(a * b) * c = a * (b * c)
  let a = 5
  let b = 10
  let c = 20
  
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result1, result2)
}

test "greet_multilingual_support" {
  // 测试多语言支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "mathematical_formula_validation" {
  // 验证数学公式：(a + b)² = a² + 2ab + b²
  let a = 3
  let b = 4
  
  let left_side = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(a, b))
  let right_side = @azimuth.add(@azimuth.add(@azimuth.multiply(a, a), @azimuth.multiply(2, @azimuth.multiply(a, b))), @azimuth.multiply(b, b))
  @azimuth.assert_eq(left_side, right_side)
}

test "string_length_handling" {
  // 测试不同长度字符串的处理
  let empty = ""
  let single = "A"
  let medium = "MoonBit"
  let long = "ThisIsAVeryLongStringForTestingPurposesInMoonBitLanguage"
  
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(empty))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet(single))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet(medium))
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongStringForTestingPurposesInMoonBitLanguage!", @azimuth.greet(long))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let item_price = 199
  let quantity = 3
  let discount_rate = 10  // 10% 折扣
  let shipping_fee = 15
  let tax_rate = 8  // 8% 税率
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  
  // 计算折扣
  let discount = @azimuth.multiply(subtotal, discount_rate) / 100
  let discounted_total = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax), shipping_fee)
  
  @azimuth.assert_eq(674, final_total)
}

test "sequential_operations" {
  // 测试连续运算：((a + b) * c) - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(75, result)
}

test "error_boundary_handling" {
  // 测试错误边界处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
}

test "real_world_scenarios" {
  // 实际应用场景：温度转换和计算
  // 摄氏度转华氏度（简化版）：F = C * 2 + 30
  let celsius_temps = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  // 测试第一个温度点
  let c1 = celsius_temps[0]
  let f1 = @azimuth.add(@azimuth.multiply(c1, 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[0], f1)
  
  // 测试第三个温度点
  let c3 = celsius_temps[2]
  let f3 = @azimuth.add(@azimuth.multiply(c3, 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[2], f3)
  
  // 测试最后一个温度点
  let c5 = celsius_temps[4]
  let f5 = @azimuth.add(@azimuth.multiply(c5, 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[4], f5)
}
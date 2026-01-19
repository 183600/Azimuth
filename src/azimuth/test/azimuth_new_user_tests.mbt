// 用户请求的新增 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法
// 包含不超过10个测试用例，覆盖核心功能

test "mathematical_operations_combination" {
  // 测试数学运算的组合使用
  // 计算 (a + b) * c - d 的复合运算
  let a = 8
  let b = 7
  let c = 3
  let d = 10
  
  let sum = @azimuth.add(a, b)  // 8 + 7 = 15
  let product = @azimuth.multiply(sum, c)  // 15 * 3 = 45
  let result = @azimuth.add(product, -d)  // 45 - 10 = 35
  
  @azimuth.assert_eq(35, result)
}

test "string_processing_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  let name_with_symbols = "user@domain.com"
  let result1 = @azimuth.greet(name_with_symbols)
  @azimuth.assert_eq_string("Hello, user@domain.com!", result1)
  
  let name_with_spaces = "John Doe"
  let result2 = @azimuth.greet(name_with_spaces)
  @azimuth.assert_eq_string("Hello, John Doe!", result2)
  
  let name_with_unicode = "🌍World"
  let result3 = @azimuth.greet(name_with_unicode)
  @azimuth.assert_eq_string("Hello, 🌍World!", result3)
}

test "boundary_value_arithmetic" {
  // 测试边界值的算术运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与正数的运算
  let result1 = @azimuth.add(max_val, 100)
  @azimuth.assert_eq(max_val, result1)
  
  // 测试最小值与负数的运算
  let result2 = @azimuth.add(min_val, -100)
  @azimuth.assert_eq(min_val, result2)
  
  // 测试最大值与-1的乘法
  let result3 = @azimuth.multiply(max_val, -1)
  @azimuth.assert_eq(min_val, result3)
}

test "real_world_shopping_cart" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算商品总价
  let subtotal = @azimuth.add(total1, total2)
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(subtotal, shipping_fee)
  
  // 199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
  @azimuth.assert_eq(717, final_total)
}

test "temperature_conversion_scenario" {
  // 测试实际应用场景：温度转换
  // 摄氏度转华氏度（简化公式）：F = C * 2 + 30
  
  let celsius_temp = 25
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temp, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_temp)
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}

test "mathematical_properties_verification" {
  // 测试数学性质的验证
  let a = 12
  let b = 8
  let c = 5
  
  // 验证加法交换律：a + b = b + a
  let add_result1 = @azimuth.add(a, b)
  let add_result2 = @azimuth.add(b, a)
  @azimuth.assert_eq(add_result1, add_result2)
  
  // 验证乘法交换律：a * b = b * a
  let mul_result1 = @azimuth.multiply(a, b)
  let mul_result2 = @azimuth.multiply(b, a)
  @azimuth.assert_eq(mul_result1, mul_result2)
  
  // 验证分配律：a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "sequential_calculation_chain" {
  // 测试连续计算链
  // 计算 (((1 + 2) * 3) + 4) * 5
  let step1 = @azimuth.add(1, 2)  // 3
  let step2 = @azimuth.multiply(step1, 3)  // 9
  let step3 = @azimuth.add(step2, 4)  // 13
  let final_result = @azimuth.multiply(step3, 5)  // 65
  
  @azimuth.assert_eq(65, final_result)
}

test "error_handling_and_boundary_conditions" {
  // 测试错误处理和边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let overflow_result = @azimuth.add(max_val, 1)
  @azimuth.assert_true(overflow_result == max_val)
  
  let underflow_result = @azimuth.add(min_val, -1)
  @azimuth.assert_true(underflow_result == min_val)
}

test "string_edge_cases_and_formatting" {
  // 测试字符串边界情况和格式化
  // 测试空字符串
  let empty_result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", empty_result)
  
  // 测试单字符
  let single_char_result = @azimuth.greet("A")
  @azimuth.assert_eq_string("Hello, A!", single_char_result)
  
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  let long_result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongNameForTestingPurposes!", long_result)
  
  // 测试包含数字的字符串
  let numeric_name = "User123"
  let numeric_result = @azimuth.greet(numeric_name)
  @azimuth.assert_eq_string("Hello, User123!", numeric_result)
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 模拟贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate_percent = 5  // 5%
  let years = 3
  
  // 计算总利率
  let total_rate = @azimuth.multiply(rate_percent, years)
  // 计算 (1 + 总利率)
  let multiplier = @azimuth.add(1, total_rate)
  // 计算最终金额
  let total_amount = @azimuth.multiply(principal, multiplier)
  
  // 10000 × (1 + 5×3) = 10000 × 16 = 160000
  @azimuth.assert_eq(160000, total_amount)
  
  // 模拟折扣计算
  let original_price = 1000
  let discount_percentage = 20  // 20% 折扣
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(800, discounted_price)
}
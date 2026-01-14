// 新增的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_negative_numbers" {
  // 测试负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "multiply_large_positive_numbers" {
  // 测试大正数相乘
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(123456, @azimuth.multiply(123, 1004))
  @azimuth.assert_eq(99980001, @azimuth.multiply(9999, 9999))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-20, @azimuth.multiply(-4, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(-7, 0))
}

test "add_edge_cases" {
  // 测试加法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_unicode_characters" {
  // 测试包含Unicode字符的问候语
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c - (a * b)
  let sum = @azimuth.add(a, b)
  let product1 = @azimuth.multiply(sum, c)
  let product2 = @azimuth.multiply(a, b)
  let result = @azimuth.add(product1, -product2)
  
  @azimuth.assert_eq(700, result)
}

test "practical_business_calculation" {
  // 测试实际业务计算
  let unit_price = 199
  let quantity = 5
  let discount_rate = 10
  let tax_rate = 8
  
  // 计算总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 计算折扣
  let discount = @azimuth.multiply(subtotal, discount_rate) / 100
  // 计算折后价格
  let discounted_price = @azimuth.add(subtotal, -discount)
  // 计算税费
  let tax = @azimuth.multiply(discounted_price, tax_rate) / 100
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax)
  
  @azimuth.assert_eq(1026, final_price)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列 2 + 5 + 8 + 11 + 14
  let term1 = 2
  let diff = 3
  let n = 5
  
  let term2 = @azimuth.add(term1, diff)
  let term3 = @azimuth.add(term2, diff)
  let term4 = @azimuth.add(term3, diff)
  let term5 = @azimuth.add(term4, diff)
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(40, sum)
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含特殊符号的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}
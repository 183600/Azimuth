// 用户综合测试用例
// 测试 azimuth 库的核心功能和边界情况

test "add_function_comprehensive" {
  // 测试加法功能的多种情况
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
}

test "multiply_function_comprehensive" {
  // 测试乘法功能的多种情况
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(1000, @azimuth.multiply(100, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
}

test "greet_function_comprehensive" {
  // 测试问候函数的多种情况
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob123!", @azimuth.greet("Bob123"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_operations_combination" {
  // 测试数学运算的组合
  let a = 12
  let b = 8
  let c = 5
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(100, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(101, result2)
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "real_world_application" {
  // 测试实际应用场景：计算折扣价格
  let original_price = 1000
  let discount_percentage = 20
  let tax_percentage = 8
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_percentage) / 100
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  // 验证结果：1000 - 200 = 800，800 + 64 = 864
  @azimuth.assert_eq(864, final_price)
}

test "sequence_operations" {
  // 测试序列运算
  // 计算序列和：1 + 2 + 3 + 4 + 5 = 15
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, sum)
  
  // 计算序列积：1 × 2 × 3 × 4 × 5 = 120
  let product = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(1, 2), 3), 4), 5)
  @azimuth.assert_eq(120, product)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "error_handling_validation" {
  // 测试错误处理和验证
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "complex_business_logic" {
  // 测试复杂业务逻辑：计算贷款还款
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率（百分比）
  let years = 10          // 贷款年限
  
  // 简化的利息计算：总利息 = 本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(@azimuth.multiply(principal, annual_rate), years)
  
  // 总还款额 = 本金 + 总利息
  let total_payment = @azimuth.add(principal, total_interest)
  
  // 年还款额 = 总还款额 / 年数
  let annual_payment = total_payment / years
  
  // 验证结果：100000 + 100000*6*10 = 100000 + 6000000 = 6100000
  // 年还款额：6100000 / 10 = 610000
  @azimuth.assert_eq(6100000, total_payment)
  @azimuth.assert_eq(610000, annual_payment)
}
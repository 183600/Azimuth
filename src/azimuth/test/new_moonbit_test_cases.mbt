// 新增 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法和断言

test "addition_with_various_number_ranges" {
  // 测试不同数值范围的加法
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
  @azimuth.assert_eq(-1000, @azimuth.add(-500, -500))
  @azimuth.assert_eq(0, @azimuth.add(500, -500))
  @azimuth.assert_eq(1, @azimuth.add(2147483647, -2147483646))
}

test "multiplication_with_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46341, 46341))
}

test "string_greeting_with_special_characters" {
  // 测试包含特殊字符的字符串问候
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, 🚀🌟!", @azimuth.greet("🚀🌟"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  let a = 12
  let b = 8
  let c = 5
  let d = 3
  
  // 计算 (a + b) * (c - d)
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)
  let result = @azimuth.multiply(sum_ab, diff_cd)
  @azimuth.assert_eq(40, result)  // (12 + 8) * (5 - 3) = 20 * 2 = 40
  
  // 计算 a * b + c * d
  let product_ab = @azimuth.multiply(a, b)
  let product_cd = @azimuth.multiply(c, d)
  let result2 = @azimuth.add(product_ab, product_cd)
  @azimuth.assert_eq(111, result2)  // 12 * 8 + 5 * 3 = 96 + 15 = 111
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "real_world_financial_calculations" {
  // 测试实际金融计算场景
  let principal = 10000  // 本金
  let rate = 5           // 利率 5%
  let years = 3          // 年数
  
  // 简单利息计算：本金 + (本金 × 利率 × 年数)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
  
  // 分期付款计算：总价 / 期数
  let total_price = 3600
  let installments = 12
  let monthly_payment = total_price / installments
  @azimuth.assert_eq(300, monthly_payment)
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 计算等差数列前n项和：1 + 2 + 3 + 4 + 5
  let n1 = 1
  let n2 = 2
  let n3 = 3
  let n4 = 4
  let n5 = 5
  
  let sum1 = @azimuth.add(n1, @azimuth.add(n2, @azimuth.add(n3, @azimuth.add(n4, n5))))
  @azimuth.assert_eq(15, sum1)  // 1 + 2 + 3 + 4 + 5 = 15
  
  // 计算等比数列前n项积：2 × 4 × 8
  let product1 = @azimuth.multiply(2, @azimuth.multiply(4, 8))
  @azimuth.assert_eq(64, product1)  // 2 × 4 × 8 = 64
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试空字符串和单字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
  @azimuth.assert_eq_string("Hello, v1.0.0!", @azimuth.greet("v1.0.0"))
  
  // 测试包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出处理
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 验证正常运算
  @azimuth.assert_true(@azimuth.add(10, 20) == 30)
  @azimuth.assert_true(@azimuth.multiply(5, 6) == 30)
}

test "comprehensive_application_scenario" {
  // 综合应用场景测试：电商订单计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let item3_price = 99
  let item3_qty = 3
  let shipping_fee = 15
  let discount = 50
  
  // 计算各商品小计
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  let subtotal3 = @azimuth.multiply(item3_price, item3_qty)
  
  // 计算商品总价
  let items_total = @azimuth.add(@azimuth.add(subtotal1, subtotal2), subtotal3)
  
  // 应用折扣和运费
  let after_discount = @azimuth.add(items_total, -discount)
  let final_total = @azimuth.add(after_discount, shipping_fee)
  
  // 验证最终总价：(199*2) + (299*1) + (99*3) - 50 + 15 = 398 + 299 + 297 - 50 + 15 = 959
  @azimuth.assert_eq(959, final_total)
}
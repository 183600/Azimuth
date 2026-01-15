// 额外的 MoonBit 测试用例
// 测试各种边界情况和实际应用场景

test "large_number_operations" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  // 大数相加
  let sum_result = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(3000000, sum_result)
  
  // 大数相乘
  let product_result = @azimuth.multiply(1000, 2000)
  @azimuth.assert_eq(2000000, product_result)
  
  // 大数与边界值组合
  let max_val = 2147483647
  let large_sum = @azimuth.add(max_val, -1000000)
  @azimuth.assert_eq(2147482647, large_sum)
}

test "negative_number_operations" {
  // 测试负数运算
  let neg1 = -100
  let neg2 = -200
  let pos = 300
  
  // 负数相加
  let neg_sum = @azimuth.add(neg1, neg2)
  @azimuth.assert_eq(-300, neg_sum)
  
  // 负数与正数相加
  let mixed_sum = @azimuth.add(neg1, pos)
  @azimuth.assert_eq(200, mixed_sum)
  
  // 负数相乘
  let neg_product = @azimuth.multiply(neg1, neg2)
  @azimuth.assert_eq(20000, neg_product)
  
  // 负数与正数相乘
  let mixed_product = @azimuth.multiply(neg1, pos)
  @azimuth.assert_eq(-30000, mixed_product)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 边界值与零的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(max_val, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(min_val, zero))
  
  // 边界值与1的运算
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 边界值与-1的运算
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "advanced_string_processing" {
  // 测试高级字符串处理
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result1)
  
  // 测试包含数字的字符串
  let numeric_string = "1234567890"
  let result2 = @azimuth.greet(numeric_string)
  @azimuth.assert_eq_string("Hello, 1234567890!", result2)
  
  // 测试包含混合语言的字符串
  let mixed_language = "Hello世界123"
  let result3 = @azimuth.greet(mixed_language)
  @azimuth.assert_eq_string("Hello, Hello世界123!", result3)
  
  // 测试包含空格的字符串
  let spaced_string = "  Leading and trailing spaces  "
  let result4 = @azimuth.greet(spaced_string)
  @azimuth.assert_eq_string("Hello,   Leading and trailing spaces  !", result4)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 15
  let b = 25
  let c = 10
  
  // 加法结合律：(a + b) + c = a + (b + c)
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律：(a × b) × c = a × (b × c)
  let left_mult_assoc = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mult_assoc = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mult_assoc, right_mult_assoc)
  
  // 分配律：a × (b + c) = a × b + a × c
  let distrib_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distrib_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distrib_left, distrib_right)
}

test "real_world_financial_calculations" {
  // 测试实际金融计算场景
  // 贷款计算：本金 × (1 + 利率 × 期限)
  let principal = 100000  // 10万元
  let annual_rate = 6     // 6%年利率
  let years = 5           // 5年期限
  
  // 计算总利息（简化计算）
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  // 计算总还款额
  let total_payment = @azimuth.add(principal, total_interest)
  
  // 验证结果：100000 + 100000 × 6 × 5 = 100000 + 3000000 = 3100000
  @azimuth.assert_eq(3100000, total_payment)
  
  // 投资回报计算：本金 × (1 + 收益率)^年数（简化版）
  let investment = 50000
  let return_rate = 8      // 8%年收益率
  let investment_years = 3
  
  // 简化的复利计算
  let total_return = @azimuth.multiply(investment, @azimuth.add(100, @azimuth.multiply(return_rate, investment_years)))
  @azimuth.assert_eq(1400000, total_return)  // 50000 × (100 + 8×3) = 50000 × 124 = 6200000
}

test "error_resilience_testing" {
  // 测试错误恢复能力
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_add = @azimuth.add(max_val, max_val)
  @azimuth.assert_true(overflow_add == max_val)
  
  let underflow_add = @azimuth.add(min_val, min_val)
  @azimuth.assert_true(underflow_add == min_val)
  
  let overflow_mul = @azimuth.multiply(max_val, 2)
  @azimuth.assert_true(overflow_mul == max_val)
  
  let underflow_mul = @azimuth.multiply(min_val, 2)
  @azimuth.assert_true(underflow_mul == min_val)
  
  // 测试特殊情况处理
  let special_case1 = @azimuth.multiply(min_val, -1)
  @azimuth.assert_true(special_case1 == min_val)
  
  let special_case2 = @azimuth.add(max_val, 1)
  @azimuth.assert_true(special_case2 == max_val)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "performance_oriented_calculations" {
  // 测试性能导向的计算
  // 大量数据累加
  let base_value = 100
  let iterations = 1000
  
  // 使用公式计算：base_value × iterations × (iterations + 1) / 2
  let sum_result = @azimuth.multiply(base_value, @azimuth.multiply(iterations, @azimuth.add(iterations, 1)) / 2)
  @azimuth.assert_eq(50050000, sum_result)
  
  // 大量数据累乘（简化版）
  let product_base = 2
  let power = 10
  
  // 计算2^10
  let power_result = @azimuth.multiply(
    @azimuth.multiply(@azimuth.multiply(product_base, product_base), @azimuth.multiply(product_base, product_base)),
    @azimuth.multiply(@azimuth.multiply(product_base, product_base), @azimuth.multiply(product_base, product_base))
  )
  @azimuth.assert_eq(1024, power_result)
  
  // 混合运算测试
  let mixed_result = @azimuth.add(
    @azimuth.multiply(100, 20),
    @azimuth.multiply(50, 30)
  )
  @azimuth.assert_eq(3500, mixed_result)
}

test "data_conversion_and_formatting" {
  // 测试数据转换和格式化
  // 数字到字符串的间接转换
  let year_str = "2023"
  let month_str = "12"
  let day_str = "25"
  let date_format = year_str + "-" + month_str + "-" + day_str
  
  // 测试日期字符串的greet函数处理
  let date_greeting = @azimuth.greet(date_format)
  @azimuth.assert_eq_string("Hello, 2023-12-25!", date_greeting)
  
  // 布尔值到字符串的转换
  let true_str = "true"
  let false_str = "false"
  
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet(true_str))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet(false_str))
  
  // 浮点数的整数表示
  let pi_approx = "3.14"
  
  @azimuth.assert_eq_string("Hello, 3.14!", @azimuth.greet(pi_approx))
}

test "complex_business_logic_simulation" {
  // 测试复杂业务逻辑模拟
  // 电商订单结算系统
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount_threshold = 1000
  let discount_rate = 10
  let shipping_fee = 20
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(subtotal1, subtotal2)
  
  // 计算折扣
  let discount_amount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = @azimuth.add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax_amount), shipping_fee)
  
  // 验证结果：(299×2 + 199×3) = 598 + 597 = 1195
  // 折扣：1195 × 10% = 119
  // 折后：1195 - 119 = 1076
  // 税费：1076 × 8% = 86
  // 总价：1076 + 86 + 20 = 1182
  @azimuth.assert_eq(1182, final_total)
  
  // 会员等级计算
  let purchase_amount = 15000
  let silver_threshold = 5000
  let gold_threshold = 10000
  let platinum_threshold = 20000
  
  let membership_level = 
    if (purchase_amount >= platinum_threshold) { "Platinum" }
    else if (purchase_amount >= gold_threshold) { "Gold" }
    else if (purchase_amount >= silver_threshold) { "Silver" }
    else { "Regular" }
  
  @azimuth.assert_eq_string("Gold", membership_level)
}
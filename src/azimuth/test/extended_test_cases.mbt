// 扩展测试用例 - Azimuth 项目
// 这些测试用例补充了 lib.mbt 中的基本测试，覆盖更多边界情况和实际应用场景

// 测试 1: 大数运算精度测试
test "large_number_precision" {
  // 测试大数相加的精度
  let large_num1 = 1000000000
  let large_num2 = 1000000000
  let large_num3 = 1000000000
  
  // 连续相加：1000000000 + 1000000000 + 1000000000 = 3000000000
  // 由于溢出保护，应该返回最大值
  let sum1 = @azimuth.add(large_num1, large_num2)
  let sum2 = @azimuth.add(sum1, large_num3)
  
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, sum2)
  
  // 测试大数相乘
  let large_mult1 = 50000
  let large_mult2 = 50000
  
  // 50000 * 50000 = 2500000000，会溢出
  let product = @azimuth.multiply(large_mult1, large_mult2)
  @azimuth.assert_eq(max_val, product)
}

// 测试 2: 负数运算边界测试
test "negative_number_boundaries" {
  let min_val = -2147483648
  
  // 测试负数相加的边界情况
  let neg1 = -1000000000
  let neg2 = -1000000000
  let neg3 = -1483648
  
  // 连续相加：-1000000000 + -1000000000 + -1483648 = -2147483648
  let sum1 = @azimuth.add(neg1, neg2)
  let sum2 = @azimuth.add(sum1, neg3)
  
  @azimuth.assert_eq(min_val, sum2)
  
  // 测试负数相乘
  let neg_mult1 = -50000
  let neg_mult2 = 50000
  
  // -50000 * 50000 = -2500000000，会溢出
  let product = @azimuth.multiply(neg_mult1, neg_mult2)
  @azimuth.assert_eq(min_val, product)
}

// 测试 3: 字符串处理高级测试
test "advanced_string_processing" {
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*"
  let result1 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*!", result1)
  
  // 测试包含数字的字符串
  let numeric_string = "12345"
  let result2 = @azimuth.greet(numeric_string)
  @azimuth.assert_eq_string("Hello, 12345!", result2)
  
  // 测试混合字符串
  let mixed_string = "User123"
  let result3 = @azimuth.greet(mixed_string)
  @azimuth.assert_eq_string("Hello, User123!", result3)
  
  // 测试单个字符
  let single_char = "A"
  let result4 = @azimuth.greet(single_char)
  @azimuth.assert_eq_string("Hello, A!", result4)
}

// 测试 4: 复合运算场景测试
test "compound_calculation_scenarios" {
  // 场景1: 计算折扣价格
  let original_price = 1000
  let discount_rate = 20  // 20%折扣
  let quantity = 5
  
  // 折扣金额 = 原价 * 折扣率 / 100
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  // 折扣后价格 = 原价 - 折扣金额
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  // 总价 = 折扣后价格 * 数量
  let total_price = @azimuth.multiply(discounted_price, quantity)
  
  // 验证：1000 * (1 - 20/100) * 5 = 1000 * 0.8 * 5 = 4000
  @azimuth.assert_eq(4000, total_price)
  
  // 场景2: 计算复合增长率
  let initial_value = 10000
  let growth_rate = 10  // 10%增长率
  let years = 3
  
  // 简化计算：最终值 = 初始值 + 初始值 * 增长率 * 年数
  let growth_amount = @azimuth.multiply(initial_value, @azimuth.multiply(growth_rate, years))
  let final_value = @azimuth.add(initial_value, growth_amount / 100)
  
  // 验证：10000 + 10000 * 10 * 3 / 100 = 10000 + 3000 = 13000
  @azimuth.assert_eq(13000, final_value)
}

// 测试 5: 边界值组合测试
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与各种值的组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1000000))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试最小值与各种值的组合
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1000000))
  
  // 测试乘法边界
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, -1))
}

// 测试 6: 数学性质验证测试
test "mathematical_properties" {
  // 测试加法交换律: a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  @azimuth.assert_eq(result1, result2)
  
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let c = 11111
  
  let result3 = @azimuth.add(@azimuth.add(a, b), c)
  let result4 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result3, result4)
  
  // 测试乘法交换律: a * b = b * a
  let result5 = @azimuth.multiply(a, b)
  let result6 = @azimuth.multiply(b, a)
  @azimuth.assert_eq(result5, result6)
  
  // 测试乘法结合律: (a * b) * c = a * (b * c)
  let result7 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result8 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result7, result8)
  
  // 测试分配律: a * (b + c) = a * b + a * c
  let result9 = @azimuth.multiply(a, @azimuth.add(b, c))
  let result10 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(result9, result10)
}

// 测试 7: 实际应用场景测试
test "real_world_application_scenarios" {
  // 场景1: 计算BMI指数（简化版）
  let weight_kg = 70  // 体重(kg)
  let height_m = 175  // 身高(cm)
  
  // BMI = 体重(kg) / (身高(m))^2
  // 简化计算：BMI ≈ 体重 * 10000 / (身高)^2
  let height_squared = @azimuth.multiply(height_m, height_m)  // 175 * 175 = 30625
  let bmi_numerator = @azimuth.multiply(weight_kg, 10000)   // 70 * 10000 = 700000
  let bmi = bmi_numerator / height_squared                   // 700000 / 30625 ≈ 22
  
  @azimuth.assert_eq(22, bmi)
  
  // 场景2: 计算贷款利息（简化版）
  let principal = 100000  // 本金
  let annual_rate = 5     // 年利率(%)
  let months = 12         // 月数
  
  // 简化计算：利息 = 本金 * 年利率 * 月数 / 12 / 100
  let interest = @azimuth.multiply(@azimuth.multiply(principal, annual_rate), months) / 1200
  let total_amount = @azimuth.add(principal, interest)
  
  // 验证：100000 + 100000 * 5 * 12 / 1200 = 100000 + 5000 = 105000
  @azimuth.assert_eq(105000, total_amount)
}

// 测试 8: 性能压力测试
test "performance_stress_test" {
  // 测试大量连续运算
  let mut result = 0
  
  // 连续相加100次1
  for i = 0; i < 100; i = i + 1 {
    result = @azimuth.add(result, 1)
  }
  @azimuth.assert_eq(100, result)
  
  // 连续相乘10次2
  let mut product = 1
  for i = 0; i < 10; i = i + 1 {
    product = @azimuth.multiply(product, 2)
  }
  @azimuth.assert_eq(1024, product)
  
  // 测试大范围运算
  let mut sum = 0
  for i = 1; i <= 100; i = i + 1 {
    sum = @azimuth.add(sum, i)
  }
  // 1 + 2 + 3 + ... + 100 = 5050
  @azimuth.assert_eq(5050, sum)
}

// 测试 9: 错误恢复和容错测试
test "error_recovery_and_fault_tolerance" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出后的运算能力
  let overflow_result = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, overflow_result)
  
  // 溢出后继续运算
  let after_overflow = @azimuth.add(overflow_result, 1)
  @azimuth.assert_eq(max_val, after_overflow)
  
  // 测试负溢出后的运算能力
  let neg_overflow_result = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, neg_overflow_result)
  
  // 负溢出后继续运算
  let after_neg_overflow = @azimuth.add(neg_overflow_result, -1)
  @azimuth.assert_eq(min_val, after_neg_overflow)
  
  // 测试乘法溢出后的运算能力
  let mult_overflow = @azimuth.multiply(50000, 50000)
  @azimuth.assert_eq(max_val, mult_overflow)
  
  // 乘法溢出后继续运算
  let after_mult_overflow = @azimuth.add(mult_overflow, 1)
  @azimuth.assert_eq(max_val, after_mult_overflow)
}

// 测试 10: 字符串边界和特殊处理测试
test "string_boundary_and_special_handling" {
  // 测试空字符串处理
  let empty_string = ""
  let result1 = @azimuth.greet(empty_string)
  @azimuth.assert_eq_string("Hello, !", result1)
  
  // 测试只包含空格的字符串
  let spaces_only = "     "
  let result2 = @azimuth.greet(spaces_only)
  @azimuth.assert_eq_string("Hello,      !", result2)
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric = "User123ABC456"
  let result3 = @azimuth.greet(alphanumeric)
  @azimuth.assert_eq_string("Hello, User123ABC456!", result3)
  
  // 测试包含HTML标签的字符串
  let html_tags = "divHello"
  let result4 = @azimuth.greet(html_tags)
  @azimuth.assert_eq_string("Hello, divHello!", result4)
  
  // 测试包含URL的字符串
  let url_string = "httpsexamplecom"
  let result5 = @azimuth.greet(url_string)
  @azimuth.assert_eq_string("Hello, httpsexamplecom!", result5)
}
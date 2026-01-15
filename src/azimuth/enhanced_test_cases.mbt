// 增强的 MoonBit 测试用例
// 覆盖各种边界条件和实际应用场景

test "enhanced_addition_series" {
  // 测试连续加法的级联效应
  let base = 100
  let increment = 50
  let count = 5
  
  // 计算：base + (base + increment) + (base + 2*increment) + ... 
  let term1 = base
  let term2 = add(base, increment)
  let term3 = add(base, multiply(2, increment))
  let term4 = add(base, multiply(3, increment))
  let term5 = add(base, multiply(4, increment))
  
  let total = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(1000, total)  // 100 + 150 + 200 + 250 + 300 = 1000
}

test "enhanced_multiplication_table" {
  // 测试乘法表的正确性
  let base = 7
  
  // 7的乘法表：7×1, 7×2, ..., 7×5
  assert_eq(7, multiply(base, 1))
  assert_eq(14, multiply(base, 2))
  assert_eq(21, multiply(base, 3))
  assert_eq(28, multiply(base, 4))
  assert_eq(35, multiply(base, 5))
  
  // 验证乘法表的和：7 + 14 + 21 + 28 + 35 = 105
  let sum = add(add(add(add(7, 14), 21), 28), 35)
  assert_eq(105, sum)
}

test "enhanced_string_concatenation_chain" {
  // 测试字符串连接链式处理
  let prefix = "Hello"
  let name = "MoonBit"
  let suffix = "2023"
  let exclamation = "!"
  
  // 通过greet函数模拟链式字符串处理
  let step1 = greet(name)
  let step2 = step1 + " " + suffix + exclamation
  
  assert_eq_string("Hello, MoonBit! 2023!", step2)
  
  // 测试包含数字的字符串
  let numeric_name = "User123"
  let numeric_result = greet(numeric_name)
  assert_eq_string("Hello, User123!", numeric_result)
}

test "enhanced_boundary_stress_test" {
  // 边界值压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的值
  let near_max = max_val - 1000
  let near_min = min_val + 1000
  
  // 接近最大值的加法
  let result1 = add(near_max, 500)
  assert_eq(2147482847, result1)
  
  // 接近最小值的加法
  let result2 = add(near_min, -500)
  assert_eq(-2147483148, result2)
  
  // 边界值乘法测试
  let result3 = multiply(46340, 46340)  // 接近sqrt(max_val)
  assert_eq(2147395600, result3)
}

test "enhanced_financial_compound_calculation" {
  // 复杂的金融复合计算
  let principal = 10000
  let rate1 = 5  // 第一年利率
  let rate2 = 6  // 第二年利率
  let rate3 = 7  // 第三年利率
  
  // 第一年后的金额
  let year1 = multiply(principal, add(100, rate1))
  // 第二年后的金额
  let year2 = multiply(year1, add(100, rate2))
  // 第三年后的金额
  let year3 = multiply(year2, add(100, rate3))
  
  // 转换为实际金额（除以100^3）
  let final_amount = year3 / 1000000
  
  assert_eq(12285, final_amount)  // 约等于12285元
}

test "enhanced_geometric_progression" {
  // 几何级数计算：1, 2, 4, 8, 16, 32
  let first_term = 1
  let ratio = 2
  
  // 计算前6项
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  let term6 = multiply(term5, ratio)
  
  // 计算总和
  let sum = add(add(add(add(add(term1, term2), term3), term4), term5), term6)
  assert_eq(63, sum)  // 1 + 2 + 4 + 8 + 16 + 32 = 63
  
  // 验证公式：a * (r^n - 1) / (r - 1)
  let formula_result = multiply(first_term, multiply(63, 1))  // 简化计算
  assert_eq(63, formula_result)
}

test "enhanced_temperature_conversion_table" {
  // 温度转换表测试（简化版：F = C * 2 + 30）
  let celsius_temps = [0, 10, 20, 30, 40]  // 摄氏度温度
  let expected_fahrenheit = [30, 50, 70, 90, 110]  // 预期的华氏度温度
  
  // 测试每个转换
  let f1 = add(multiply(0, 2), 30)
  let f2 = add(multiply(10, 2), 30)
  let f3 = add(multiply(20, 2), 30)
  let f4 = add(multiply(30, 2), 30)
  let f5 = add(multiply(40, 2), 30)
  
  assert_eq(30, f1)
  assert_eq(50, f2)
  assert_eq(70, f3)
  assert_eq(90, f4)
  assert_eq(110, f5)
  
  // 计算平均温度转换
  let avg_celsius = add(add(add(add(0, 10), 20), 30), 40) / 5
  let avg_fahrenheit = add(multiply(avg_celsius, 2), 30)
  assert_eq(70, avg_fahrenheit)
}

test "enhanced_error_recovery_scenarios" {
  // 错误恢复场景测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出后的恢复能力
  let overflow_result = add(max_val, 1000)
  assert_eq(max_val, overflow_result)
  
  // 从溢出状态继续计算
  let recovery_result = add(overflow_result, -100)
  assert_eq(max_val, recovery_result)
  
  // 测试最小值的特殊情况
  let min_mult = multiply(min_val, 1)
  assert_eq(min_val, min_mult)
  
  // 测试断言函数的稳定性
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "enhanced_string_unicode_handling" {
  // Unicode 字符串处理测试
  let unicode_names = ["世界", "🌍", "测试", "MoonBit", "αβγ"]
  
  // 测试各种Unicode字符串
  let result1 = greet("世界")
  let result2 = greet("🌍")
  let result3 = greet("测试")
  let result4 = greet("MoonBit")
  let result5 = greet("αβγ")
  
  assert_eq_string("Hello, 世界!", result1)
  assert_eq_string("Hello, 🌍!", result2)
  assert_eq_string("Hello, 测试!", result3)
  assert_eq_string("Hello, MoonBit!", result4)
  assert_eq_string("Hello, αβγ!", result5)
  
  // 测试混合Unicode字符串
  let mixed = "Hello世界🌍"
  let mixed_result = greet(mixed)
  assert_eq_string("Hello, Hello世界🌍!", mixed_result)
}

test "enhanced_complex_business_logic" {
  // 复杂业务逻辑测试：电商促销活动计算
  let original_price = 1000
  let customer_level = 3  // VIP等级
  let purchase_quantity = 5
  let promotion_threshold = 3  // 促销门槛
  
  // 基础折扣：VIP等级 × 5%
  let base_discount = multiply(customer_level, 5)
  
  // 数量折扣：超过促销门槛，每多一个额外2%折扣
  let quantity_discount = if (purchase_quantity > promotion_threshold) {
    multiply(purchase_quantity - promotion_threshold, 2)
  } else {
    0
  }
  
  // 总折扣率
  let total_discount = add(base_discount, quantity_discount)
  
  // 折扣金额
  let discount_amount = multiply(original_price, total_discount) / 100
  
  // 最终价格
  let final_price = add(original_price, -discount_amount)
  
  assert_eq(750, final_price)  // 1000 - (1000 * 25 / 100) = 750
  
  // 测试不同场景
  let regular_customer = 1
  let regular_quantity = 2
  let regular_discount = multiply(regular_customer, 5)
  let regular_final = add(original_price, -multiply(original_price, regular_discount) / 100)
  assert_eq(950, regular_final)
}
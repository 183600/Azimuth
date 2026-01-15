// 新增的 MoonBit 测试用例
// 覆盖特殊场景和边界情况

test "numeric_range_validation" {
  // 测试数值范围验证
  let large_positive = 1000000
  let large_negative = -1000000
  let zero = 0
  
  // 大数相加
  assert_true(add(large_positive, large_positive) > 0)
  assert_true(add(large_negative, large_negative) < 0)
  assert_eq(0, add(large_positive, large_negative))
  
  // 大数相乘
  assert_true(multiply(large_positive, 2) > 0)
  assert_true(multiply(large_negative, 2) < 0)
  assert_eq(0, multiply(large_positive, 0))
}

test "string_boundary_conditions" {
  // 测试字符串边界条件
  // 测试单字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a b c!", greet("a b c"))
  
  // 测试包含特殊符号的字符串
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  // 计算：(a + b) * (c - d) + e * f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 4
  
  let sum_ab = add(a, b)  // 15
  let diff_cd = add(c, -d)  // 12
  let product_ef = multiply(e, f)  // 12
  let result = add(multiply(sum_ab, diff_cd), product_ef)  // 15*12 + 12 = 192
  
  assert_eq(192, result)
}

test "special_business_scenarios" {
  // 测试特殊业务场景：折扣计算
  let original_price = 1000
  let discount_percent = 20
  let tax_percent = 8
  let shipping = 50
  
  // 计算折扣价
  let discount_amount = multiply(original_price, discount_percent) / 100
  let discounted_price = add(original_price, -discount_amount)
  
  // 计算税费
  let tax_amount = multiply(discounted_price, tax_percent) / 100
  
  // 计算最终价格
  let final_price = add(add(discounted_price, tax_amount), shipping)
  
  // 验证：1000 - 200 + 64 + 50 = 914
  assert_eq(914, final_price)
}

test "performance_related_operations" {
  // 测试性能相关操作
  // 模拟大量数据的累加
  let base = 100
  let multiplier = 5
  let iterations = 10
  
  // 计算：base * (1 + 2 + ... + iterations)
  let sum_formula = multiply(base, multiply(iterations, add(iterations, 1)) / 2)
  
  // 验证：100 * (10 * 11 / 2) = 100 * 55 = 5500
  assert_eq(5500, sum_formula)
}

test "data_conversion_scenarios" {
  // 测试数据转换场景
  // 模拟布尔值到字符串的转换
  let bool_true = "true"
  let bool_false = "false"
  
  assert_eq_string("Hello, true!", greet(bool_true))
  assert_eq_string("Hello, false!", greet(bool_false))
  
  // 模拟数字字符串的处理
  let numeric_string = "12345"
  let floating_string = "3.14159"
  
  assert_eq_string("Hello, 12345!", greet(numeric_string))
  assert_eq_string("Hello, 3.14159!", greet(floating_string))
}

test "error_recovery_mechanisms" {
  // 测试错误恢复机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出后的状态恢复
  let overflow_result = add(max_val, max_val)
  assert_eq(max_val, overflow_result)
  
  // 验证后续计算不受影响
  let normal_calculation = add(overflow_result, 100)
  assert_eq(max_val, normal_calculation)
  
  // 测试下溢后的状态恢复
  let underflow_result = multiply(min_val, 2)
  assert_eq(min_val, underflow_result)
  
  // 验证后续计算不受影响
  let normal_calculation2 = add(underflow_result, -100)
  assert_eq(min_val, normal_calculation2)
}

test "conditional_logic_testing" {
  // 测试条件逻辑
  let x = 15
  let y = 20
  let z = 25
  
  // 测试嵌套条件：如果 x < y 且 y < z，则计算 x + y + z
  let condition1 = x < y
  let condition2 = y < z
  let result = if (condition1 && condition2) { 
    add(add(x, y), z) 
  } else { 
    0 
  }
  
  assert_eq(60, result)
  
  // 测试反向条件
  let condition3 = x > y
  let result2 = if (condition3) { 
    multiply(x, y) 
  } else { 
    add(x, y) 
  }
  
  assert_eq(35, result2)
}

test "function_composition_patterns" {
  // 测试函数组合模式
  let base_value = 100
  
  // 组合1：先乘后加
  let result1 = add(multiply(base_value, 3), 50)
  assert_eq(350, result1)
  
  // 组合2：先加后乘
  let result2 = multiply(add(base_value, 50), 3)
  assert_eq(450, result2)
  
  // 组合3：嵌套组合
  let result3 = add(multiply(add(base_value, 10), 2), multiply(base_value, 3))
  assert_eq(520, result3)
}

test "real_world_simulation" {
  // 测试实际世界模拟：银行利息计算
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  let compound_frequency = 12  // 月复利
  
  // 简化的复利计算（避免浮点数）
  // 使用公式：本金 × (1 + 利率 × 年数)
  let simple_interest = multiply(principal, add(1, multiply(annual_rate, years)))
  
  // 验证：10000 × (1 + 5 × 3) = 10000 × 16 = 160000
  assert_eq(160000, simple_interest)
  
  // 测试定期存款
  let monthly_deposit = 1000
  let months = 12
  
  // 简化的定期存款计算：月存款 × 月数 × (1 + 利率)
  let deposit_total = multiply(multiply(monthly_deposit, months), add(1, annual_rate))
  
  // 验证：1000 × 12 × 6 = 72000
  assert_eq(72000, deposit_total)
}
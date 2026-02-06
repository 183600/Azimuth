// 额外的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "arithmetic_sequence_sum" {
  // 等差数列求和测试
  // 数列: 3, 7, 11, 15, 19 (公差为4，共5项)
  let first_term = 3
  let common_diff = 4
  let num_terms = 5
  
  // 使用公式: S = n/2 * (2a + (n-1)d)
  let double_sum = multiply(num_terms, add(multiply(2, first_term), multiply(subtract(num_terms, 1), common_diff)))
  let sum = divide_with_ceil(double_sum, 2)
  
  // 手动验证
  let manual_sum = add(add(add(add(3, 7), 11), 15), 19)
  
  assert_eq(sum, manual_sum)
  assert_eq(sum, 55)
}

test "greeting_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string(greet("Alice"), "Hello, Alice!")
  assert_eq_string(greet("Bob"), "Hello, Bob!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
  assert_eq_string(greet("!@#$%"), "Hello, !@#$%!")
}

test "complex_mathematical_expression" {
  // 复杂数学表达式测试
  // 计算: (15 + 23) * (42 - 18) / 7
  let step1 = add(15, 23)  // 38
  let step2 = subtract(42, 18)  // 24
  let step3 = multiply(step1, step2)  // 912
  let result = divide_with_ceil(step3, 7)  // 向上取整
  
  assert_eq(step1, 38)
  assert_eq(step2, 24)
  assert_eq(step3, 912)
  assert_eq(result, 131)
}

test "negative_number_division" {
  // 负数除法测试
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(-10, -3), 4)
  assert_eq(divide_with_ceil(0, -5), 0)
  assert_eq(divide_with_ceil(-0, 5), 0)
}

test "calculator_simulation" {
  // 计算器模拟测试
  let accumulator = 0
  
  // 执行一系列计算
  accumulator = add(accumulator, 100)  // +100
  accumulator = multiply(accumulator, 2)  // *2
  accumulator = subtract(accumulator, 50)  // -50
  accumulator = divide_with_ceil(accumulator, 3)  // /3 (向上取整)
  
  assert_eq(accumulator, 50)
}

test "number_properties_check" {
  // 数字性质检查测试
  let num = 24
  
  // 检查因子关系
  let factor1 = 6
  let factor2 = 4
  let product_check = multiply(factor1, factor2)
  
  // 检查是否为偶数
  let is_even = divide_with_ceil(num, 2)
  let doubled_even = multiply(is_even, 2)
  
  // 检查是否为3的倍数
  let is_multiple_of_3 = divide_with_ceil(num, 3)
  let tripled_multiple = multiply(is_multiple_of_3, 3)
  
  assert_eq(product_check, num)
  assert_eq(doubled_even, num)
  assert_eq(tripled_multiple, 24)  // 24是3的倍数，向上取整后仍然正确
}

test "temperature_range_analysis" {
  // 温度范围分析测试
  let daily_highs = add(add(add(add(23, 25), 22), 26), 24)  // 5天最高温度总和
  let daily_lows = add(add(add(add(15, 17), 14), 18), 16)   // 5天最低温度总和
  
  let days = 5
  let avg_high = divide_with_ceil(daily_highs, days)
  let avg_low = divide_with_ceil(daily_lows, days)
  
  // 计算日平均温度
  let daily_avg_total = add(daily_highs, daily_lows)
  let overall_avg = divide_with_ceil(daily_avg_total, multiply(days, 2))
  
  // 计算温度范围
  let temp_range = subtract(avg_high, avg_low)
  
  assert_eq(daily_highs, 120)
  assert_eq(daily_lows, 80)
  assert_eq(avg_high, 24)
  assert_eq(avg_low, 16)
  assert_eq(overall_avg, 20)
  assert_eq(temp_range, 8)
}

test "financial_calculator" {
  // 金融计算器测试
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 计算简单利息
  let interest_per_year = divide_with_ceil(multiply(principal, rate_percent), 100)
  let total_interest = multiply(interest_per_year, years)
  let final_amount = add(principal, total_interest)
  
  // 计算复合利息（简化版）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  let compound_interest = subtract(year3_amount, principal)
  
  assert_eq(interest_per_year, 50)
  assert_eq(total_interest, 150)
  assert_eq(final_amount, 1150)
  assert_eq(year1_amount, 1050)
  assert_eq(year2_amount, 1103)
  assert_eq(year3_amount, 1158)
  assert_eq(compound_interest, 158)
}
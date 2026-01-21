// 综合新测试用例 - 为 azimuth 库添加的高质量测试
// 覆盖新的场景和边界情况，确保代码的健壮性

test "advanced_mathematical_series" {
  // 高级数学级数计算：等比数列求和
  let first_term = 2
  let ratio = 3
  let terms = 4
  
  // 计算 2 + 6 + 18 + 54 = 80
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  
  let series_sum = add(add(add(term1, term2), term3), term4)
  assert_eq(80, series_sum)
}

test "resource_optimization_scenario" {
  // 资源优化场景：计算最优资源分配
  let total_resources = 100
  let project_a_needs = 35
  let project_b_needs = 28
  let project_c_needs = 33
  
  let allocated_a = project_a_needs
  let allocated_b = project_b_needs
  let allocated_c = project_c_needs
  
  let total_allocated = add(add(allocated_a, allocated_b), allocated_c)
  let remaining = add(total_resources, -total_allocated)
  
  assert_eq(96, total_allocated)
  assert_eq(4, remaining)
}

test "time_management_complex" {
  // 复杂时间管理：计算项目时间线
  let tasks_per_sprint = 8
  let total_complex_tasks = 27
  let total_simple_tasks = 15
  
  let complex_sprints = divide_with_ceil(total_complex_tasks, tasks_per_sprint)
  let simple_sprints = divide_with_ceil(total_simple_tasks, tasks_per_sprint)
  
  let total_sprints = add(complex_sprints, simple_sprints)
  let total_weeks = multiply(total_sprints, 2)  // 每个sprint 2周
  
  assert_eq(4, complex_sprints)  // ceil(27/8) = 4
  assert_eq(2, simple_sprints)   // ceil(15/8) = 2
  assert_eq(6, total_sprints)    // 4 + 2 = 6
  assert_eq(12, total_weeks)     // 6 * 2 = 12
}

test "financial_compound_interest" {
  // 金融复利计算：简化版本
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let year1_total = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_total, rate_percent), 100)
  let year2_total = add(year1_total, year2_interest)
  
  // 第三年
  let year3_interest = divide_with_ceil(multiply(year2_total, rate_percent), 100)
  let final_amount = add(year2_total, year3_interest)
  
  assert_eq(1158, final_amount)  // 1000 -> 1050 -> 1103 -> 1158
}

test "inventory_batch_processing" {
  // 库存批处理：计算批次分配
  let total_inventory = 157
  let batch_size = 25
  let emergency_reserve = 20
  
  let available_for_batches = add(total_inventory, -emergency_reserve)
  let full_batches = divide_with_ceil(available_for_batches, batch_size)
  let items_in_last_batch = add(available_for_batches, -multiply(full_batches - 1, batch_size))
  
  assert_eq(6, full_batches)  // ceil(137/25) = 6
  assert_eq(12, items_in_last_batch)  // 137 - 5*25 = 12
}

test "unicode_greeting_comprehensive" {
  // 综合Unicode问候测试
  assert_eq_string("Hello, 🌟✨🚀!", greet("🌟✨🚀"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, こんにちは!", greet("こんにちは"))
  assert_eq_string("Hello, 🏠🌳🌺!", greet("🏠🌳🌺"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟：计算操作次数
  let n = 10
  
  // O(n) 操作
  let linear_ops = multiply(n, 3)
  
  // O(n²) 操作（简化计算）
  let quadratic_ops = multiply(n, n)
  
  // O(log n) 操作（简化为向上取整）
  let log_ops = divide_with_ceil(n, 3)
  
  let total_ops = add(add(linear_ops, quadratic_ops), log_ops)
  
  assert_eq(30, linear_ops)   // 10 * 3 = 30
  assert_eq(100, quadratic_ops)  // 10 * 10 = 100
  assert_eq(4, log_ops)      // ceil(10/3) = 4
  assert_eq(134, total_ops)  // 30 + 100 + 4 = 134
}

test "data_structure_capacity_planning" {
  // 数据结构容量规划
  let expected_records = 1000
  let growth_factor_percent = 25
  let safety_buffer = 100
  
  let growth_capacity = divide_with_ceil(multiply(expected_records, growth_factor_percent), 100)
  let base_capacity = expected_records
  let total_capacity = add(add(base_capacity, growth_capacity), safety_buffer)
  
  assert_eq(250, growth_capacity)  // ceil(1000*25/100) = 250
  assert_eq(1350, total_capacity)  // 1000 + 250 + 100 = 1350
}

test "multi_currency_calculation" {
  // 多货币计算模拟
  let base_currency_amount = 100
  let exchange_rate_a = 1.18  // 简化为整数计算
  let exchange_rate_b = 0.85  // 简化为整数计算
  
  // 简化的汇率计算（乘以100再除以100）
  let converted_a = divide_with_ceil(multiply(base_currency_amount, 118), 100)
  let converted_b = divide_with_ceil(multiply(base_currency_amount, 85), 100)
  
  let total_converted = add(converted_a, converted_b)
  
  assert_eq(118, converted_a)   // ceil(100*118/100) = 118
  assert_eq(85, converted_b)    // ceil(100*85/100) = 85
  assert_eq(203, total_converted)  // 118 + 85 = 203
}

test "extreme_value_robustness" {
  // 极端值健壮性测试
  // 大数值运算
  let large_num = 100000
  let another_large = 200000
  
  assert_eq(300000, add(large_num, another_large))
  assert_eq(20000000000, multiply(large_num, another_large))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(100000, divide_with_ceil(100000, 1))
  assert_eq(0, divide_with_ceil(100000, 0))
  
  // 极端字符串测试
  assert_eq_string("Hello, ThisIsAVeryLongStringWithNoSpacesThatMightCauseIssues!", greet("ThisIsAVeryLongStringWithNoSpacesThatMightCauseIssues"))
  assert_eq_string("Hello,   !", greet("   "))
}
// 新增的高质量 MoonBit 测试用例
// 专注于边缘情况、组合操作和实际应用场景

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
  assert_eq(0, subtract(0, 0))
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let initial = 15
  let step1 = multiply(initial, 2)      // 30
  let step2 = add(step1, 10)            // 40
  let step3 = divide_with_ceil(step2, 6) // 7
  let step4 = subtract(step3, 2)        // 5
  assert_eq(5, step4)
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  let total_items = 125
  let team_size = 8
  let items_per_person_per_day = 3
  
  // 计算每天总处理能力
  let daily_capacity = multiply(team_size, items_per_person_per_day)
  
  // 计算需要的天数
  let days_needed = divide_with_ceil(total_items, daily_capacity)
  
  // 验证结果
  assert_eq(24, daily_capacity)  // 8 * 3
  assert_eq(6, days_needed)      // ceil(125/24)
  
  // 验证总处理能力
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(144, total_capacity) // 6 * 24
}

test "mathematical_identities" {
  // 数学恒等式测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "divide_with_ceil_precision_edge_cases" {
  // 向上取整除法的精度边缘情况
  // 测试大数除法
  assert_eq(1000001, divide_with_ceil(1000001, 1))
  assert_eq(500001, divide_with_ceil(1000001, 2))
  assert_eq(334, divide_with_ceil(1000, 3))
  assert_eq(200001, divide_with_ceil(600000, 3))
  
  // 测试小数边界
  assert_eq(2, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(1, divide_with_ceil(99, 100))
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况
  // 测试空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 
	!", greet("
	"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, \'apostrophe\'!", greet("\'apostrophe\'"))
  
  // 测试长字符串
  assert_eq_string("Hello, very_long_string_with_multiple_words!", greet("very_long_string_with_multiple_words"))
}

test "negative_number_division_complex" {
  // 复杂负数除法测试
  // 混合符号的各种组合
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(11, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(4, divide_with_ceil(-11, -3))
  
  // 零的特殊情况
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(-0, 5))
}

test "business_calculation_workflows" {
  // 业务计算工作流测试
  // 场景：项目预算和资源分配
  
  let project_budget = 50000
  let team_members = 7
  let monthly_costs = 8000
  
  // 计算可支持的月数
  let months_supported = divide_with_ceil(project_budget, monthly_costs)
  assert_eq(7, months_supported) // ceil(50000/8000)
  
  // 计算每人每月预算
  let budget_per_person = divide_with_ceil(monthly_costs, team_members)
  assert_eq(1143, budget_per_person) // ceil(8000/7)
  
  // 计算总分配预算
  let total_allocated = multiply(months_supported, monthly_costs)
  assert_eq(56000, total_allocated) // 7 * 8000
  
  // 验证预算超支
  let budget_overrun = subtract(total_allocated, project_budget)
  assert_eq(6000, budget_overrun) // 56000 - 50000
}

test "performance_optimization_scenarios" {
  // 性能优化场景测试
  // 场景：批量处理优化
  
  let total_records = 987
  let batch_size = 50
  let processing_time_per_batch = 3
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_records, batch_size)
  assert_eq(20, batches_needed) // ceil(987/50)
  
  // 计算总处理时间
  let total_time = multiply(batches_needed, processing_time_per_batch)
  assert_eq(60, total_time) // 20 * 3
  
  // 计算实际处理容量
  let actual_capacity = multiply(batches_needed, batch_size)
  assert_eq(1000, actual_capacity) // 20 * 50
  
  // 计算剩余容量
  let remaining_capacity = subtract(actual_capacity, total_records)
  assert_eq(13, remaining_capacity) // 1000 - 987
  
  // 计算利用率
  let utilization_percent = divide_with_ceil(multiply(total_records, 100), actual_capacity)
  assert_eq(99, utilization_percent) // ceil(98700/1000)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试各种可能导致错误的输入组合
  
  // 除零的所有情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  assert_eq(0, divide_with_ceil(-1000000, 0))
  
  // 极值运算
  let large_num = 1000000
  let small_num = 1
  
  assert_eq(1000001, add(large_num, small_num))
  assert_eq(999999, subtract(large_num, small_num))
  assert_eq(1000000, multiply(large_num, small_num))
  assert_eq(1000000, divide_with_ceil(large_num, small_num))
  
  // 边界值测试
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(2, divide_with_ceil(-3, -2))
  assert_eq(-1, divide_with_ceil(-3, 2))
  assert_eq(-1, divide_with_ceil(3, -2))
}
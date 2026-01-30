// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加的高质量测试
// 包含核心功能测试、边界情况测试和实际应用场景

test "mathematical_identity_verification" {
  // 验证数学恒等式
  let x = 42
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "divide_with_ceil_precision_testing" {
  // 测试向上取整除法的精度
  // 测试小数部分的各种情况
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 -> 2
  assert_eq(3, divide_with_ceil(8, 3))    // 2.666... -> 3
  assert_eq(4, divide_with_ceil(11, 3))   // 3.666... -> 4
  assert_eq(7, divide_with_ceil(13, 2))   // 6.5 -> 7
  
  // 测试负数情况
  assert_eq(-1, divide_with_ceil(-3, 2))   // -1.5 -> -1
  assert_eq(-2, divide_with_ceil(-7, 3))   // -2.333... -> -2
}

test "greet_function_unicode_comprehensive" {
  // 全面的 Unicode 字符串测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
}

test "complex_business_workflow_calculation" {
  // 复杂业务工作流计算：订单处理
  let unit_price = 89
  let quantity = 7
  let tax_rate = 8
  let shipping_cost = 15
  let discount_threshold = 500
  let discount_amount = 30
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, tax_amount), shipping_cost)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(623, subtotal)
  assert_eq(50, tax_amount)
  assert_eq(688, pre_discount_total)
  assert_eq(658, final_total)
}

test "resource_allocation_optimization_scenario" {
  // 资源分配优化场景
  let total_budget = 10000
  let team_a_cost = 3200
  let team_b_cost = 2800
  let team_c_cost = 2100
  
  // 计算已分配预算
  let allocated_budget = add(add(team_a_cost, team_b_cost), team_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量（每个项目800）
  let small_projects_count = divide_with_ceil(remaining_budget, 800)
  
  assert_eq(8100, allocated_budget)
  assert_eq(1900, remaining_budget)
  assert_eq(3, small_projects_count)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 8
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证结果：5 + 8 + 11 + 14 + 17 + 20 + 23 + 26 = 124
  assert_eq(124, sum)
}

test "error_handling_robustness_comprehensive" {
  // 全面的错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极值运算测试
  let large_positive = 1000000
  let large_negative = -1000000
  
  assert_eq(2000000, add(large_positive, large_positive))
  assert_eq(-2000000, add(large_negative, large_negative))
  assert_eq(0, add(large_positive, large_negative))
}

test "time_and_project_management_calculation" {
  // 时间和项目管理计算
  let total_tasks = 87
  let team_members = 5
  let working_hours_per_day = 8
  let tasks_per_person_per_day = 3
  
  // 计算每日团队处理能力
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  
  // 计算需要的工作日数
  let workdays_needed = divide_with_ceil(total_tasks, daily_capacity)
  
  // 计算总工作小时数
  let total_work_hours = multiply(workdays_needed, working_hours_per_day)
  
  assert_eq(15, daily_capacity)
  assert_eq(6, workdays_needed)
  assert_eq(48, total_work_hours)
}

test "data_analysis_statistical_simulation" {
  // 数据分析统计模拟
  let data_points = 125
  let sample_size = 8
  let batch_processing_size = 15
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(data_points, batch_processing_size)
  
  // 计算样本数量占总数据的百分比
  let sample_percentage = divide_with_ceil(multiply(sample_size, 100), data_points)
  
  // 计算剩余未处理的数据点
  let processed_data = multiply(batches_needed - 1, batch_processing_size)
  let remaining_data = add(data_points, -processed_data)
  
  assert_eq(9, batches_needed)
  assert_eq(7, sample_percentage)
  assert_eq(20, remaining_data)
}

test "string_processing_advanced_scenarios" {
  // 高级字符串处理场景
  let first_name = "张"
  let last_name = "三"
  let company = "科技有限公司"
  let department = "研发部"
  
  // 构建完整的职业信息
  let full_name = first_name + last_name
  let professional_info = full_name + " - " + company + " " + department
  let greeting_result = greet(professional_info)
  
  assert_eq_string("Hello, 张三 - 科技有限公司 研发部!", greeting_result)
  
  // 测试特殊字符组合
  let complex_string = "User@2024#Project$Alpha"
  assert_eq_string("Hello, User@2024#Project$Alpha!", greet(complex_string))
}
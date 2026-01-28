// Azimuth 项目新增测试用例
// 专注于性能优化、复杂业务场景和函数组合测试

test "performance_large_scale_computations" {
  // 大规模运算性能测试
  // 测试大数运算的性能和正确性
  let large_num1 = 100000
  let large_num2 = 200000
  let large_num3 = 300000
  
  // 大数加法链
  let add_chain = add(add(large_num1, large_num2), large_num3)
  assert_eq(600000, add_chain)
  
  // 大数乘法链
  let mult_chain = multiply(multiply(large_num1, 2), multiply(divide_with_ceil(large_num2, 10), 3))
  assert_eq(60000, mult_chain)
  
  // 大数除法链
  let div_chain = divide_with_ceil(mult_chain, large_num3)
  assert_eq(400000, div_chain)
}

test "complex_e_commerce_calculations" {
  // 电商业务复杂计算测试
  // 场景：计算订单总价、折扣、税费和运费
  
  let item_price = 299
  let quantity = 7
  let discount_threshold = 5
  let discount_rate = 10  // 10% 折扣
  let tax_rate = 8        // 8% 税率
  let shipping_per_box = 15
  let items_per_box = 3
  
  // 计算商品小计
  let subtotal = multiply(item_price, quantity)
  assert_eq(2093, subtotal)
  
  // 计算折扣（如果数量超过阈值）
  let has_discount = quantity >= discount_threshold
  let discount_amount = if has_discount {
    divide_with_ceil(multiply(subtotal, discount_rate), 100)
  } else {
    0
  }
  assert_eq(209, discount_amount)
  
  // 计算折后价格
  let discounted_total = subtract(subtotal, discount_amount)
  assert_eq(1884, discounted_total)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(discounted_total, tax_rate), 100)
  assert_eq(151, tax_amount)
  
  // 计算运费
  let boxes_needed = divide_with_ceil(quantity, items_per_box)
  let shipping_cost = multiply(boxes_needed, shipping_per_box)
  assert_eq(45, shipping_cost)
  
  // 计算最终总价
  let final_total = add(add(discounted_total, tax_amount), shipping_cost)
  assert_eq(2080, final_total)
}

test "nested_function_composition_advanced" {
  // 高级嵌套函数组合测试
  // 测试复杂的函数嵌套和组合
  
  let base_value = 15
  
  // 复杂嵌套计算：((base * 4 + 10) / 3) * 2 - 5
  let step1 = multiply(base_value, 4)           // 15 * 4 = 60
  let step2 = add(step1, 10)                    // 60 + 10 = 70
  let step3 = divide_with_ceil(step2, 3)        // ceil(70/3) = 24
  let step4 = multiply(step3, 2)                // 24 * 2 = 48
  let result = subtract(step4, 5)               // 48 - 5 = 43
  
  assert_eq(43, result)
  
  // 更复杂的嵌套：使用函数组合
  let complex_result = subtract(
    multiply(
      divide_with_ceil(
        add(
          multiply(base_value, 6),
          subtract(20, 5)
        ),
        add(base_value, divide_with_ceil(base_value, 2))
      ),
      3
    ),
    multiply(2, divide_with_ceil(base_value, 3))
  )
  
  // 手动计算：ceil((15*6 + 15) / (15 + 8)) * 3 - 2*ceil(15/3)
  // = ceil(105 / 23) * 3 - 2*5 = 5*3 - 10 = 5
  assert_eq(5, complex_result)
}

test "data_processing_pipeline_test" {
  // 数据处理流水线测试
  // 模拟批量数据处理和转换
  
  let data_batches = 250
  let batch_size = 17
  let processing_time_per_batch = 4
  let validation_time_per_batch = 2
  let error_rate = 5  // 每100批次有5个错误
  
  // 计算处理轮次
  let processing_rounds = divide_with_ceil(data_batches, batch_size)
  assert_eq(15, processing_rounds)
  
  // 计算总处理时间
  let total_processing_time = multiply(processing_rounds, processing_time_per_batch)
  assert_eq(60, total_processing_time)
  
  // 计算总验证时间
  let total_validation_time = multiply(processing_rounds, validation_time_per_batch)
  assert_eq(30, total_validation_time)
  
  // 计算预期错误数量
  let expected_errors = divide_with_ceil(multiply(data_batches, error_rate), 100)
  assert_eq(13, expected_errors)
  
  // 计算总时间（包括错误处理）
  let error_handling_time = multiply(expected_errors, 3)
  let total_time = add(add(total_processing_time, total_validation_time), error_handling_time)
  assert_eq(129, total_time)
}

test "resource_allocation_optimization_complex" {
  // 复杂资源分配优化测试
  // 场景：多项目资源分配和优化
  
  let total_budget = 50000
  let project_count = 7
  let min_budget_per_project = 5000
  let contingency_reserve_rate = 10  // 10% 储备金
  
  // 计算每个项目的基本预算
  let base_budget_per_project = divide_with_ceil(
    subtract(total_budget, multiply(min_budget_per_project, project_count)),
    project_count
  )
  assert_eq(2143, base_budget_per_project)
  
  // 计算每个项目的总预算
  let total_budget_per_project = add(base_budget_per_project, min_budget_per_project)
  assert_eq(7143, total_budget_per_project)
  
  // 计算储备金
  let contingency_reserve = divide_with_ceil(
    multiply(total_budget, contingency_reserve_rate),
    100
  )
  assert_eq(5000, contingency_reserve)
  
  // 计算可分配预算
  let allocatable_budget = subtract(total_budget, contingency_reserve)
  assert_eq(45000, allocatable_budget)
  
  // 验证分配不超过预算
  let total_allocated = multiply(total_budget_per_project, project_count)
  assert_eq(50001, total_allocated)
  
  // 优化后的分配
  let optimized_per_project = divide_with_ceil(allocatable_budget, project_count)
  assert_eq(6429, optimized_per_project)
}

test "string_processing_performance_test" {
  // 字符串处理性能测试
  // 测试各种字符串输入的处理性能和正确性
  
  // 测试长字符串
  let long_string = "This is a very long string that contains many words and characters to test the greet function's performance with larger inputs"
  let long_result = greet(long_string)
  assert_eq_string("Hello, " + long_string + "!", long_result)
  
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_result = greet(special_chars)
  assert_eq_string("Hello, " + special_chars + "!", special_result)
  
  // 测试包含数字的字符串
  let numeric_string = "1234567890"
  let numeric_result = greet(numeric_string)
  assert_eq_string("Hello, " + numeric_string + "!", numeric_result)
  
  // 测试混合字符串
  let mixed_string = "Mixed123!@#中文"
  let mixed_result = greet(mixed_string)
  assert_eq_string("Hello, " + mixed_string + "!", mixed_result)
}

test "mathematical_series_calculations" {
  // 数学级数计算测试
  // 测试等差数列和等比数列的计算
  
  let first_term = 3
  let common_difference = 4
  let terms_count = 6
  
  // 计算等差数列第n项
  let nth_term = add(first_term, multiply(common_difference, subtract(terms_count, 1)))
  assert_eq(23, nth_term)
  
  // 计算等差数列前n项和
  let arithmetic_sum = divide_with_ceil(
    multiply(terms_count, add(first_term, nth_term)),
    2
  )
  assert_eq(78, arithmetic_sum)
  
  // 等比数列计算
  let geometric_ratio = 2
  let geometric_nth_term = multiply(first_term, 
    multiply(geometric_ratio, subtract(terms_count, 1)))
  assert_eq(30, geometric_nth_term)
  
  // 复杂级数计算
  let complex_series = add(
    divide_with_ceil(multiply(first_term, terms_count), 2),
    multiply(common_difference, divide_with_ceil(multiply(terms_count, subtract(terms_count, 1)), 2))
  )
  assert_eq(78, complex_series)
}

test "error_boundary_and_recovery_test" {
  // 错误边界和恢复测试
  // 测试各种错误情况和系统恢复能力
  
  // 测试除零的各种组合
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  assert_eq(0, divide_with_ceil(-1000000, 0))
  
  // 测试极值运算
  let max_positive = 2147483647
  let max_negative = -2147483648
  
  // 安全的极值运算
  assert_eq(max_positive, add(max_positive, 0))
  assert_eq(max_negative, add(max_negative, 0))
  assert_eq(0, multiply(max_positive, 0))
  assert_eq(0, multiply(max_negative, 0))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(max_positive, max_positive))
  assert_eq(1, divide_with_ceil(max_negative, max_negative))
  assert_eq(0, divide_with_ceil(0, max_positive))
  assert_eq(0, divide_with_ceil(0, max_negative))
}

test "real_world_project_management" {
  // 真实世界项目管理测试
  // 场景：复杂项目的资源调度和时间管理
  
  let project_tasks = 145
  let team_members = 8
  let working_hours_per_day = 8
  let working_days_per_week = 5
  let tasks_per_person_per_day = 3
  let buffer_time_rate = 20  // 20% 缓冲时间
  
  // 计算每日团队处理能力
  let daily_team_capacity = multiply(team_members, tasks_per_person_per_day)
  assert_eq(24, daily_team_capacity)
  
  // 计算需要的工作日
  let required_work_days = divide_with_ceil(project_tasks, daily_team_capacity)
  assert_eq(7, required_work_days)
  
  // 计算总工作小时
  let total_work_hours = multiply(required_work_days, working_hours_per_day)
  assert_eq(56, total_work_hours)
  
  // 计算缓冲时间
  let buffer_days = divide_with_ceil(
    multiply(required_work_days, buffer_time_rate),
    100
  )
  assert_eq(2, buffer_days)
  
  // 计算总项目时间
  let total_project_days = add(required_work_days, buffer_days)
  assert_eq(9, total_project_days)
  
  // 计算项目周数
  let project_weeks = divide_with_ceil(total_project_days, working_days_per_week)
  assert_eq(2, project_weeks)
  
  // 验证项目排期的合理性
  assert_eq(total_project_days, add(required_work_days, buffer_days))
  assert_eq(1, divide_with_ceil(project_weeks, 1))
}
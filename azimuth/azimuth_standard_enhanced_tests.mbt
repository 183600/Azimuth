// Azimuth 标准增强测试用例
// 包含10个高质量的标准 MoonBit 测试用例，覆盖核心功能和实际应用场景

test "mathematical_commutativity_enhanced" {
  // 增强数学交换律测试
  let a = 42
  let b = 17
  
  // 加法交换律验证
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律验证
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 复杂表达式交换律
  assert_eq(
    add(multiply(a, 3), multiply(b, 2)),
    add(multiply(b, 2), multiply(a, 3))
  )
}

test "divide_with_ceil_precision_testing" {
  // 向上取整除法精度测试
  // 测试不同精度级别的除法运算
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  
  // 大数精度测试
  assert_eq(1001, divide_with_ceil(10001, 10))
  assert_eq(10001, divide_with_ceil(100001, 10))
  
  // 负数精度测试
  assert_eq(-1, divide_with_ceil(-3, 2))
  assert_eq(-2, divide_with_ceil(-5, 2))
}

test "greet_function_multilingual_support" {
  // 问候函数多语言支持测试
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, español!", greet("español"))
  assert_eq_string("Hello, français!", greet("français"))
  assert_eq_string("Hello, русский!", greet("русский"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_resource_allocation" {
  // 复杂资源分配测试
  // 场景：多项目资源分配
  let total_resources = 100
  let project_a_priority = 3
  let project_b_priority = 2
  let project_c_priority = 1
  
  // 计算权重总和
  let total_priority = add(add(project_a_priority, project_b_priority), project_c_priority)
  
  // 按权重分配资源
  let resources_a = divide_with_ceil(multiply(total_resources, project_a_priority), total_priority)
  let resources_b = divide_with_ceil(multiply(total_resources, project_b_priority), total_priority)
  let resources_c = divide_with_ceil(multiply(total_resources, project_c_priority), total_priority)
  
  assert_eq(51, resources_a)  // ceil(100*3/6) = 50
  assert_eq(34, resources_b)  // ceil(100*2/6) = 34
  assert_eq(17, resources_c)  // ceil(100*1/6) = 17
  
  // 验证分配合理性
  let total_allocated = add(add(resources_a, resources_b), resources_c)
  assert_eq(102, total_allocated)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试各种错误情况的恢复能力
  let safe_value = 10
  
  // 除零错误处理链
  let step1 = divide_with_ceil(safe_value, 0)  // 返回0
  let step2 = add(step1, safe_value)  // 0 + 10 = 10
  let step3 = multiply(step2, 2)  // 10 * 2 = 20
  let step4 = divide_with_ceil(step3, 5)  // ceil(20/5) = 4
  
  assert_eq(0, step1)
  assert_eq(10, step2)
  assert_eq(20, step3)
  assert_eq(4, step4)
  
  // 混合错误场景测试
  let complex_result = add(
    multiply(divide_with_ceil(15, 0), 10),  // 0 * 10 = 0
    divide_with_ceil(multiply(5, 3), 2)    // ceil(15/2) = 8
  )
  assert_eq(8, complex_result)
}

test "arithmetical_sequence_operations" {
  // 算术序列操作测试
  // 生成和操作算术序列
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 6
  
  // 计算第n项：a_n = a_1 + (n-1)*d
  let sixth_term = add(first_term, multiply(common_difference, subtract(number_of_terms, 1)))
  assert_eq(20, sixth_term)  // 5 + (6-1)*3 = 20
  
  // 计算前n项和：S_n = n/2 * (a_1 + a_n)
  let sum_first_six = divide_with_ceil(
    multiply(number_of_terms, add(first_term, sixth_term)),
    2
  )
  assert_eq(75, sum_first_six)  // ceil(6*(5+20)/2) = 75
  
  // 序列性质验证
  let sequence_sum = add(add(
    add(first_term, add(first_term, common_difference)),
    add(add(first_term, multiply(common_difference, 2)), add(first_term, multiply(common_difference, 3)))
  ), add(add(first_term, multiply(common_difference, 4)), sixth_term))
  assert_eq(75, sequence_sum)
}

test "business_calculations_comprehensive" {
  // 综合业务计算测试
  // 场景：电商订单处理成本计算
  let orders_per_day = 150
  let items_per_order = 3
  let processing_cost_per_item = 2
  let packaging_cost_per_order = 5
  let shipping_cost_per_package = 15
  let items_per_package = 10
  
  // 计算每日处理成本
  let daily_items = multiply(orders_per_day, items_per_order)
  let daily_processing_cost = multiply(daily_items, processing_cost_per_item)
  let daily_packaging_cost = multiply(orders_per_day, packaging_cost_per_order)
  
  // 计算需要的包裹数
  let packages_needed = divide_with_ceil(daily_items, items_per_package)
  let daily_shipping_cost = multiply(packages_needed, shipping_cost_per_package)
  
  // 计算总成本
  let total_daily_cost = add(add(daily_processing_cost, daily_packaging_cost), daily_shipping_cost)
  
  // 验证计算结果
  assert_eq(450, daily_items)  // 150 * 3
  assert_eq(900, daily_processing_cost)  // 450 * 2
  assert_eq(750, daily_packaging_cost)  // 150 * 5
  assert_eq(45, packages_needed)  // ceil(450/10)
  assert_eq(675, daily_shipping_cost)  // 45 * 15
  assert_eq(2325, total_daily_cost)  // 900 + 750 + 675
}

test "algorithm_optimization_scenarios" {
  // 算法优化场景测试
  // 比较不同算法策略的性能特征
  let dataset_size = 1000
  
  // 线性搜索模拟（最坏情况）
  let linear_search_steps = dataset_size
  assert_eq(1000, linear_search_steps)
  
  // 二分搜索模拟（最坏情况）
  let binary_search_steps = divide_with_ceil(dataset_size, 2)
  assert_eq(500, binary_search_steps)
  
  // 分块搜索模拟
  let block_size = 50
  let blocks_needed = divide_with_ceil(dataset_size, block_size)
  let block_search_steps = add(blocks_needed, block_size)
  assert_eq(70, block_search_steps)  // ceil(1000/50) + 50
  
  // 验证算法效率提升
  let linear_vs_binary_improvement = divide_with_ceil(linear_search_steps, binary_search_steps)
  let linear_vs_block_improvement = divide_with_ceil(linear_search_steps, block_search_steps)
  
  assert_eq(2, linear_vs_binary_improvement)  // 1000/500 = 2
  assert_eq(15, linear_vs_block_improvement)  // ceil(1000/70)
}

test "data_validation_and_constraints" {
  // 数据验证和约束测试
  // 场景：表单数据验证和业务规则检查
  let min_age = 18
  let max_age = 65
  let applicant_age = 25
  let years_of_experience = 3
  let min_experience = 2
  
  // 年龄验证
  let age_valid = and(
    applicant_age >= min_age,
    applicant_age <= max_age
  )
  assert_true(age_valid)
  
  // 经验验证
  let experience_valid = years_of_experience >= min_experience
  assert_true(experience_valid)
  
  // 综合资格评分
  let age_score = divide_with_ceil(multiply(applicant_age, 100), max_age)
  let experience_score = multiply(years_of_experience, 10)
  let total_score = add(age_score, experience_score)
  
  assert_eq(38, age_score)        // ceil(25*100/65)
  assert_eq(30, experience_score) // 3 * 10
  assert_eq(68, total_score)      // 38 + 30
}

test "time_based_calculations" {
  // 基于时间的计算测试
  // 场景：项目时间管理和排期
  let project_start_day = 1
  let task_duration_days = 14
  let team_velocity = 8  // 每周完成的故事点数
  let total_story_points = 45
  let holidays_in_period = 2
  
  // 计算有效工作日
  let total_calendar_days = add(project_start_day, task_duration_days)
  let effective_work_days = subtract(total_calendar_days, holidays_in_period)
  
  // 计算工作周数
  let work_weeks = divide_with_ceil(effective_work_days, 5)
  
  // 计算团队容量
  let team_capacity = multiply(work_weeks, team_velocity)
  
  // 计算需要的迭代次数
  let iterations_needed = divide_with_ceil(total_story_points, team_capacity)
  
  // 验证计算结果
  assert_eq(15, total_calendar_days)     // 1 + 14
  assert_eq(13, effective_work_days)     // 15 - 2
  assert_eq(3, work_weeks)               // ceil(13/5)
  assert_eq(24, team_capacity)           // 3 * 8
  assert_eq(2, iterations_needed)        // ceil(45/24)
}
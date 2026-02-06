// Azimuth 综合新测试用例 - 10个标准 MoonBit 测试
// 提供新的测试场景和边界情况覆盖

test "advanced_arithmetic_combinations" {
  // 高级算术组合测试 - 测试多个函数的复合使用
  let base_value = 10
  let multiplier = 3
  let addition = 5
  let divisor = 4
  
  // 复合计算: (base * multiplier) + addition
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.add(step1, addition)
  @azimuth.assert_eq(35, step2)  // (10 * 3) + 5 = 35
  
  // 复合计算: ceil(result / divisor)
  let final_result = @azimuth.divide_with_ceil(step2, divisor)
  @azimuth.assert_eq(9, final_result)  // ceil(35/4) = 9
}

test "financial_calculations" {
  // 金融计算场景测试
  let principal = 10000
  let rate = 5
  let years = 3
  let fee = -50
  
  // 计算简单利息: principal + (principal * rate / 100) * years
  let interest_per_year = @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate), 100)
  let total_interest = @azimuth.multiply(interest_per_year, years)
  let amount_before_fee = @azimuth.add(principal, total_interest)
  let final_amount = @azimuth.add(amount_before_fee, fee)
  
  @azimuth.assert_eq(11500, final_amount)  // 10000 + (10000*5/100)*3 - 50 = 11500
}

test "inventory_optimization" {
  // 库存优化场景测试
  let demand_forecast = 157
  let safety_stock = 20
  let package_size = 12
  
  // 计算需要订购的总数量
  let total_needed = @azimuth.add(demand_forecast, safety_stock)
  
  // 计算包装数量
  let packages_needed = @azimuth.divide_with_ceil(total_needed, package_size)
  let total_order = @azimuth.multiply(packages_needed, package_size)
  
  // 计算剩余空间
  let remaining_space = @azimuth.subtract(total_order, total_needed)
  
  @azimuth.assert_eq(15, packages_needed)
  @azimuth.assert_eq(180, total_order)
  @azimuth.assert_eq(3, remaining_space)
}

test "unicode_and_internationalization" {
  // Unicode和国际化的扩展测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, 🏳️‍🌈!", @azimuth.greet("🏳️‍🌈"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, עברית!", @azimuth.greet("עברית"))
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 测试除零的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试极值运算
  let large_positive = 1000000
  let large_negative = -1000000
  
  @azimuth.assert_eq(2000000, @azimuth.add(large_positive, large_positive))
  @azimuth.assert_eq(-2000000, @azimuth.add(large_negative, large_negative))
  @azimuth.assert_eq(0, @azimuth.add(large_positive, large_negative))
  
  @azimuth.assert_eq(100000000000, @azimuth.multiply(large_positive, large_positive))
  @azimuth.assert_eq(100000000000, @azimuth.multiply(large_negative, large_negative))
  @azimuth.assert_eq(-100000000000, @azimuth.multiply(large_positive, large_negative))
}

test "project_management_workflow" {
  // 项目管理工作流测试
  let team_size = 8
  let story_points_per_person = 5
  let sprint_duration = 2
  let capacity_buffer = 10
  
  // 计算团队总容量
  let base_capacity = @azimuth.multiply(team_size, story_points_per_person)
  let total_capacity = @azimuth.add(base_capacity, capacity_buffer)
  
  // 计算每个冲刺的平均完成量
  let sprint_capacity = @azimuth.divide_with_ceil(total_capacity, sprint_duration)
  
  // 计算完成100个故事点需要的冲刺数
  let total_stories = 100
  let sprints_needed = @azimuth.divide_with_ceil(total_stories, sprint_capacity)
  
  @azimuth.assert_eq(50, base_capacity)
  @azimuth.assert_eq(60, total_capacity)
  @azimuth.assert_eq(30, sprint_capacity)
  @azimuth.assert_eq(4, sprints_needed)
}

test "data_processing_pipeline" {
  // 数据处理管道测试
  let input_records = 1000
  let batch_size = 75
  let processing_rate = 20
  let error_rate = -5
  
  // 计算批次数
  let batches = @azimuth.divide_with_ceil(input_records, batch_size)
  
  // 计算预期处理时间（分钟）
  let total_batches = @azimuth.multiply(batches, batch_size)
  let effective_records = @azimuth.add(total_batches, error_rate)
  let processing_time = @azimuth.divide_with_ceil(effective_records, processing_rate)
  
  // 计算实际处理的记录数
  let actual_processed = @azimuth.multiply(processing_time, processing_rate)
  
  @azimuth.assert_eq(14, batches)  // ceil(1000/75) = 14
  @azimuth.assert_eq(1050, total_batches)  // 14 * 75 = 1050
  @azimuth.assert_eq(1045, effective_records)  // 1050 - 5 = 1045
  @azimuth.assert_eq(53, processing_time)  // ceil(1045/20) = 53
  @azimuth.assert_eq(1060, actual_processed)  // 53 * 20 = 1060
}

test "mathematical_series_and_patterns" {
  // 数学序列和模式测试
  // 斐波那契数列相关计算
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  
  // 计算前5个斐波那契数的和
  let fib_sum = @azimuth.add(fib1, @azimuth.add(fib2, @azimuth.add(fib3, @azimuth.add(fib4, fib5))))
  @azimuth.assert_eq(12, fib_sum)
  
  // 计算几何级数: 2^1 + 2^2 + 2^3 + 2^4
  let geo_sum = @azimuth.add(2, @azimuth.add(4, @azimuth.add(8, 16)))
  @azimuth.assert_eq(30, geo_sum)
  
  // 测试分配律: a * (b + c) = a*b + a*c
  let a = 6
  let b = 7
  let c = 8
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)  // 6*(7+8) = 6*7 + 6*8 = 90
}

test "resource_allocation_scenarios" {
  // 资源分配场景测试
  let total_budget = 50000
  let priority_projects = 3
  let standard_projects = 5
  let reserve_percentage = 10
  
  // 计算预留资金
  let reserve_amount = @azimuth.divide_with_ceil(@azimuth.multiply(total_budget, reserve_percentage), 100)
  let available_budget = @azimuth.subtract(total_budget, reserve_amount)
  
  // 计算优先项目平均预算
  let priority_budget_per_project = @azimuth.divide_with_ceil(@azimuth.multiply(available_budget, 60), 100)
  let total_priority_budget = @azimuth.multiply(priority_budget_per_project, priority_projects)
  
  // 计算标准项目平均预算
  let remaining_budget = @azimuth.subtract(available_budget, total_priority_budget)
  let standard_budget_per_project = @azimuth.divide_with_ceil(remaining_budget, standard_projects)
  
  @azimuth.assert_eq(5000, reserve_amount)  // ceil(50000*10/100) = 5000
  @azimuth.assert_eq(45000, available_budget)  // 50000 - 5000 = 45000
  @azimuth.assert_eq(27000, total_priority_budget)  // ceil(45000*60/100) * 3 = 27000
  @azimuth.assert_eq(9000, priority_budget_per_project)  // ceil(45000*60/100) = 9000
  @azimuth.assert_eq(3600, standard_budget_per_project)  // ceil(18000/5) = 3600
}

test "performance_benchmarking" {
  // 性能基准测试
  let iterations = 1000
  let base_value = 42
  let increment = 17
  
  // 模拟重复运算的性能测试
  let result1 = @azimuth.add(base_value, increment)
  let result2 = @azimuth.multiply(result1, 2)
  let result3 = @azimuth.divide_with_ceil(result2, 3)
  let result4 = @azimuth.subtract(result3, 5)
  
  // 验证计算链的正确性
  @azimuth.assert_eq(59, result1)  // 42 + 17 = 59
  @azimuth.assert_eq(118, result2)  // 59 * 2 = 118
  @azimuth.assert_eq(40, result3)  // ceil(118/3) = 40
  @azimuth.assert_eq(35, result4)  // 40 - 5 = 35
  
  // 测试大规模运算的一致性
  let large_scale = @azimuth.multiply(iterations, @azimuth.add(base_value, increment))
  let batch_processed = @azimuth.divide_with_ceil(large_scale, 100)
  let final_result = @azimuth.multiply(batch_processed, @azimuth.divide_with_ceil(100, iterations))
  
  @azimuth.assert_eq(59000, large_scale)  // 1000 * (42 + 17) = 59000
  @azimuth.assert_eq(590, batch_processed)  // ceil(59000/100) = 590
  @azimuth.assert_eq(590, final_result)  // 590 * ceil(100/1000) = 590
}
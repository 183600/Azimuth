// Azimuth 项目新增高质量 MoonBit 测试用例
// 包含10个精心设计的测试用例，覆盖各种场景和边界情况

test "mathematical_identity_and_commutative_laws" {
  // 数学恒等式和交换律验证
  let x = 42
  let y = 17
  
  // 加法恒等式和交换律
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  assert_eq(add(x, y), add(y, x))
  
  // 乘法恒等式和交换律
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "boundary_values_and_extreme_cases" {
  // 边界值和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(1000, 0))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(100, 0))
  
  // 单位值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
  
  // 大数测试（在合理范围内）
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(1000001, divide_with_ceil(1000001, 1))
}

test "complex_business_workflow_calculation" {
  // 复杂业务工作流计算：项目管理场景
  let team_members = 8
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  
  // 计算需要的冲刺数（每冲刺最多100个任务）
  let sprint_capacity = 100
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  // 验证计算结果
  assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  assert_eq(1920, total_tasks)  // 960 * 2 = 1920
  assert_eq(20, sprints_needed)  // ceil(1920/100) = 20
}

test "internationalization_and_unicode_support" {
  // 国际化和Unicode支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "error_handling_and_edge_case_resilience" {
  // 错误处理和边缘情况恢复能力测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  
  // 极值组合测试
  let max_positive = 1000000
  let max_negative = -1000000
  
  assert_eq(0, add(max_positive, max_negative))
  assert_eq(2000000, add(max_positive, max_positive))
  assert_eq(-2000000, add(max_negative, max_negative))
  assert_eq(-1000000000000, multiply(max_positive, max_negative))
  assert_eq(1000000000000, multiply(max_negative, max_negative))
}

test "resource_optimization_and_allocation" {
  // 资源优化和分配测试：仓库管理场景
  let total_products = 487
  let box_types = [(24, "large"), (12, "medium"), (6, "small")]
  
  // 优先使用大箱子
  let large_boxes_needed = divide_with_ceil(total_products, 24)
  assert_eq(21, large_boxes_needed)
  
  // 如果使用中等箱子
  let medium_boxes_needed = divide_with_ceil(total_products, 12)
  assert_eq(41, medium_boxes_needed)
  
  // 如果使用小箱子
  let small_boxes_needed = divide_with_ceil(total_products, 6)
  assert_eq(82, small_boxes_needed)
  
  // 验证大箱子方案的总容量
  let large_total_capacity = multiply(large_boxes_needed, 24)
  assert_eq(504, large_total_capacity)
  
  // 验证剩余空间
  let large_remaining_space = subtract(large_total_capacity, total_products)
  assert_eq(17, large_remaining_space)
}

test "mathematical_series_and_patterns" {
  // 数学级数和模式测试
  // 斐波那契数列相关计算
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  let fib6 = add(fib4, fib5)  // 8
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  assert_eq(8, fib6)
  
  // 阶乘相关计算
  let factorial_3 = multiply(3, multiply(2, 1))
  let factorial_4 = multiply(4, factorial_3)
  let factorial_5 = multiply(5, factorial_4)
  
  assert_eq(6, factorial_3)
  assert_eq(24, factorial_4)
  assert_eq(120, factorial_5)
  
  // 等差数列求和
  let first_term = 3
  let common_diff = 2
  let num_terms = 5
  
  let last_term = add(first_term, multiply(common_diff, subtract(num_terms, 1)))
  let sum = divide_with_ceil(multiply(num_terms, add(first_term, last_term)), 2)
  
  assert_eq(11, last_term)  // 3 + 2*(5-1) = 11
  assert_eq(35, sum)        // 5*(3+11)/2 = 35
}

test "financial_calculation_and_budgeting" {
  // 金融计算和预算测试
  let project_budget = 50000
  let team_members = 5
  let monthly_salary_per_person = 6000
  let project_duration_months = 8
  
  // 计算总人力成本
  let total_labor_cost = multiply(team_members, multiply(monthly_salary_per_person, project_duration_months))
  
  // 计算剩余预算
  let remaining_budget = subtract(project_budget, total_labor_cost)
  
  // 计算需要的资金分期（每期不超过15000）
  let installment_limit = 15000
  let installments_needed = divide_with_ceil(total_labor_cost, installment_limit)
  
  // 验证计算结果
  assert_eq(240000, total_labor_cost)  // 5 * 6000 * 8 = 240000
  assert_eq(-190000, remaining_budget)  // 50000 - 240000 = -190000 (预算不足)
  assert_eq(16, installments_needed)    // ceil(240000/15000) = 16
}

test "data_processing_and_analysis" {
  // 数据处理和分析测试
  let data_points = [12, 15, 18, 21, 24, 27, 30]
  let num_points = 7
  
  // 计算总和（手动计算）
  let sum = add(add(add(add(add(add(12, 15), 18), 21), 24), 27), 30)
  
  // 计算平均值
  let average = divide_with_ceil(sum, num_points)
  
  // 计算最大值和最小值的差
  let max_value = 30
  let min_value = 12
  let range = subtract(max_value, min_value)
  
  // 计算数据分组（每组5个数据点）
  let groups_needed = divide_with_ceil(num_points, 5)
  
  // 验证计算结果
  assert_eq(147, sum)
  assert_eq(21, average)  // ceil(147/7) = 21
  assert_eq(18, range)    // 30 - 12 = 18
  assert_eq(2, groups_needed)  // ceil(7/5) = 2
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let attendees = 256
  let tables_available = 30
  let chairs_per_table = 8
  let cost_per_chair = 25
  let venue_capacity = 300
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  
  // 检查桌子是否足够
  let table_sufficiency = subtract(tables_available, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  
  // 计算剩余座位
  let available_seats = subtract(total_seats, attendees)
  
  // 检查场地容量
  let capacity_utilization = divide_with_ceil(multiply(attendees, 100), venue_capacity)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  
  // 验证计算结果
  assert_eq(32, tables_needed)      // ceil(256/8) = 32
  assert_eq(-2, table_sufficiency)  // 30 - 32 = -2 (桌子不足)
  assert_eq(256, total_seats)       // 32 * 8 = 256
  assert_eq(0, available_seats)     // 256 - 256 = 0
  assert_eq(85, capacity_utilization)  // ceil(25600/300) = 85
  assert_eq(6400, total_cost)       // 256 * 25 = 6400
}
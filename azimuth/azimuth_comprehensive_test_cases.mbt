// Azimuth 综合测试用例 - 10个高质量标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "arithmetic_identity_properties" {
  // 算术恒等式性质测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "complex_business_shipping_calculation" {
  // 复杂业务运输计算测试
  let total_weight = 1250  // 总重量(kg)
  let max_weight_per_truck = 300  // 每车最大载重
  let trucks_needed = divide_with_ceil(total_weight, max_weight_per_truck)
  
  assert_eq(5, trucks_needed)
  
  // 计算总运输能力
  let total_capacity = multiply(trucks_needed, max_weight_per_truck)
  assert_eq(1500, total_capacity)
  
  // 计算剩余运力
  let remaining_capacity = subtract(total_capacity, total_weight)
  assert_eq(250, remaining_capacity)
  
  // 计算运力利用率
  let utilization_rate = divide_with_ceil(multiply(total_weight, 100), total_capacity)
  assert_eq(84, utilization_rate)  // floor(125000/1500) = 83%
}

test "mathematical_commutative_and_associative_laws" {
  // 数学交换律和结合律测试
  let a = 7
  let b = 11
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(a, c), add(c, a))
  assert_eq(add(b, c), add(c, b))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(a, c), multiply(c, a))
  assert_eq(multiply(b, c), multiply(c, b))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "edge_case_zero_and_boundary_values" {
  // 边界情况：零值和边界值测试
  // 零值运算
  assert_eq(0, add(0, 0))
  assert_eq(5, add(0, 5))
  assert_eq(-5, add(0, -5))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(0, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  
  // 除零处理
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-2, divide_with_ceil(-3, 2))
}

test "internationalization_and_unicode_support" {
  // 国际化和Unicode支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, empty!", greet(""))
}

test "resource_allocation_optimization_scenario" {
  // 资源分配优化场景测试
  let total_tasks = 87
  let team_members = 5
  let working_days = 15
  
  // 计算每人需要完成的任务数
  let tasks_per_person = divide_with_ceil(total_tasks, team_members)
  assert_eq(18, tasks_per_person)
  
  // 计算每人每天需要完成的任务数
  let tasks_per_person_per_day = divide_with_ceil(tasks_per_person, working_days)
  assert_eq(2, tasks_per_person_per_day)
  
  // 验证总任务完成能力
  let total_completion_capacity = multiply(team_members, multiply(tasks_per_person_per_day, working_days))
  assert_eq(150, total_completion_capacity)
  
  // 验证任务完成率
  assert_true(total_completion_capacity >= total_tasks)
  
  // 计算额外处理能力
  let extra_capacity = subtract(total_completion_capacity, total_tasks)
  assert_eq(63, extra_capacity)
}

test "financial_budget_calculation_with_constraints" {
  // 带约束的财务预算计算测试
  let project_budget = 50000
  let team_members = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_duration_days = 20
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_duration_days))
  assert_eq(1280, total_hours)
  
  // 计算总人力成本
  let total_labor_cost = multiply(total_hours, hourly_rate)
  assert_eq(96000, total_labor_cost)
  
  // 计算预算缺口
  let budget_shortage = subtract(total_labor_cost, project_budget)
  assert_eq(46000, budget_shortage)
  
  // 计算需要削减的工时
  let hours_to_reduce = divide_with_ceil(budget_shortage, hourly_rate)
  assert_eq(614, hours_to_reduce)
  
  // 计算调整后的总工时
  let adjusted_total_hours = subtract(total_hours, hours_to_reduce)
  assert_eq(666, adjusted_total_hours)
  
  // 计算调整后的项目天数
  let adjusted_project_days = divide_with_ceil(adjusted_total_hours, multiply(team_members, hours_per_day))
  assert_eq(9, adjusted_project_days)
}

test "mathematical_distributive_property_validation" {
  // 数学分配律验证测试
  let a = 6
  let b = 8
  let c = 3
  
  // 分配律: a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 验证具体数值
  assert_eq(66, left_side)  // 6 * (8 + 3) = 6 * 11 = 66
  assert_eq(66, right_side)  // 6 * 8 + 6 * 3 = 48 + 18 = 66
  
  // 反向分配律: (a + b) * c = a * c + b * c
  let reverse_left = multiply(add(a, b), c)
  let reverse_right = add(multiply(a, c), multiply(b, c))
  assert_eq(reverse_left, reverse_right)
  
  // 验证具体数值
  assert_eq(42, reverse_left)  // (6 + 8) * 3 = 14 * 3 = 42
  assert_eq(42, reverse_right)  // 6 * 3 + 8 * 3 = 18 + 24 = 42
}

test "comprehensive_negative_number_operations" {
  // 综合负数运算测试
  // 负数加法
  assert_eq(-10, add(-5, -5))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
  assert_eq(0, add(-10, 10))
  
  // 负数乘法
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(0, multiply(-5, 0))
  
  // 负数除法
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(2, divide_with_ceil(-10, -5))
  assert_eq(0, divide_with_ceil(-10, 0))
  
  // 负数减法
  assert_eq(-15, subtract(-10, 5))
  assert_eq(-5, subtract(-10, -5))
  assert_eq(15, subtract(5, -10))
  assert_eq(0, subtract(-10, -10))
}

test "real_world_event_planning_comprehensive" {
  // 真实世界活动规划综合测试
  let expected_attendees = 350
  let tables_available = 40
  let chairs_per_table = 10
  let budget_per_chair = 25
  let total_event_budget = 12000
  let staff_members = 15
  let staff_hours_per_event = 6
  let staff_hourly_rate = 30
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(expected_attendees, chairs_per_table)
  assert_eq(35, tables_needed)
  
  // 验证桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(350, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, expected_attendees)
  assert_eq(0, remaining_seats)
  
  // 计算椅子总成本
  let total_chair_cost = multiply(total_seats, budget_per_chair)
  assert_eq(8750, total_chair_cost)
  
  // 计算人员成本
  let total_staff_cost = multiply(staff_members, multiply(staff_hours_per_event, staff_hourly_rate))
  assert_eq(2700, total_staff_cost)
  
  // 计算总成本
  let total_event_cost = add(total_chair_cost, total_staff_cost)
  assert_eq(11450, total_event_cost)
  
  // 验证预算充足
  assert_true(total_event_cost <= total_event_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_event_budget, total_event_cost)
  assert_eq(550, remaining_budget)
  
  // 计算预算利用率
  let budget_utilization = divide_with_ceil(multiply(total_event_cost, 100), total_event_budget)
  assert_eq(96, budget_utilization)  // floor(1145000/12000) = 95%
}
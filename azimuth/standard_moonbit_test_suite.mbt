// 标准 MoonBit 测试套件 - 10个高质量测试用例
// 覆盖 azimuth 库的核心功能和各种使用场景

test "basic_arithmetic_operations_comprehensive" {
  // 综合测试基本算术运算
  assert_eq(15, add(8, 7))
  assert_eq(-1, add(-8, 7))
  assert_eq(56, multiply(7, 8))
  assert_eq(-56, multiply(-7, 8))
  assert_eq(0, multiply(0, 123))
  assert_eq(3, divide_with_ceil(25, 8))
  assert_eq(-3, divide_with_ceil(-25, 8))
  assert_eq(1, subtract(10, 9))
  assert_eq(-1, subtract(9, 10))
}

test "mathematical_properties_and_identities" {
  // 测试数学性质和恒等式
  let x = 42
  let y = 17
  
  // 加法交换律和结合律
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(add(x, y), 5), add(x, add(y, 5)))
  
  // 乘法交换律和结合律
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(multiply(multiply(x, y), 2), multiply(x, multiply(y, 2)))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
  
  // 恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
}

test "edge_cases_and_boundary_conditions" {
  // 测试边缘情况和边界条件
  // 零值处理
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  
  // 除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 单位值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(5, divide_with_ceil(5, 1))
  assert_eq(-5, divide_with_ceil(-5, 1))
}

test "string_and_unicode_comprehensive" {
  // 全面测试字符串和Unicode处理
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  
  // Unicode字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  
  // 特殊字符测试
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
}

test "business_logic_inventory_management" {
  // 业务逻辑：库存管理场景测试
  let total_products = 157
  let box_capacity = 24
  let warehouses = 3
  
  // 计算每个仓库需要的箱子数量
  let boxes_per_warehouse = divide_with_ceil(total_products, warehouses)
  assert_eq(53, boxes_per_warehouse)  // ceil(157/3) = 53
  
  // 计算每个仓库需要的箱子数量（基于容量）
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(7, boxes_needed)  // ceil(157/24) = 7
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 计算空间利用率
  let utilization = multiply(divide_with_ceil(multiply(total_products, 100), total_capacity), 1)
  assert_eq(93, utilization)  // floor(15700/168) = 93%
}

test "financial_calculations_and_budgeting" {
  // 金融计算和预算场景测试
  let project_budget = 50000
  let team_members = 8
  let project_duration_months = 6
  
  // 计算每人每月预算
  let total_person_months = multiply(team_members, project_duration_months)
  let budget_per_person_month = divide_with_ceil(project_budget, total_person_months)
  assert_eq(1042, budget_per_person_month)  // ceil(50000/48)
  
  // 计算总分配预算
  let total_allocated = multiply(budget_per_person_month, total_person_months)
  assert_eq(50016, total_allocated)
  
  // 计算预算溢出
  let budget_overflow = subtract(total_allocated, project_budget)
  assert_eq(16, budget_overflow)
  
  // 计算应急储备金（10%）
  let emergency_fund = divide_with_ceil(multiply(project_budget, 10), 100)
  assert_eq(5000, emergency_fund)
  
  // 计算总需求
  let total_required = add(project_budget, emergency_fund)
  assert_eq(55000, total_required)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial_value = 12
  
  // 计算流水线：((value * 3 + 8) / 5) * 2 - 3
  let step1 = multiply(initial_value, 3)      // 36
  let step2 = add(step1, 8)                   // 44
  let step3 = divide_with_ceil(step2, 5)      // 9 (ceil(44/5))
  let step4 = multiply(step3, 2)              // 18
  let final_result = subtract(step4, 3)       // 15
  
  assert_eq(15, final_result)
  
  // 验证中间步骤
  assert_eq(36, step1)
  assert_eq(44, step2)
  assert_eq(9, step3)
  assert_eq(18, step4)
}

test "resource_optimization_and_allocation" {
  // 资源优化和分配测试
  let total_resources = 1000
  let projects = 4
  let min_resources_per_project = 200
  
  // 计算每个项目的基础资源
  let base_resources = divide_with_ceil(total_resources, projects)
  assert_eq(250, base_resources)  // ceil(1000/4)
  
  // 验证所有项目都能满足最小资源需求
  assert_true(base_resources >= min_resources_per_project)
  
  // 计算总分配资源
  let total_allocated = multiply(base_resources, projects)
  assert_eq(1000, total_allocated)
  
  // 计算资源余缺
  let resource_balance = subtract(total_allocated, total_resources)
  assert_eq(0, resource_balance)
  
  // 如果有额外资源，重新分配
  let additional_resources = 150
  let new_total = add(total_resources, additional_resources)
  let new_allocation = divide_with_ceil(new_total, projects)
  assert_eq(288, new_allocation)  // ceil(1150/4)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试各种异常输入情况
  
  // 除零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  
  // 字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, A!", greet("A"))
}

test "real_world_application_scenarios" {
  // 真实世界应用场景综合测试
  // 场景：活动策划和资源分配
  
  let event_participants = 87
  let tables_available = 10
  let chairs_per_table = 8
  let budget_per_participant = 50
  let venue_cost_per_hour = 200
  let event_duration_hours = 4
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(event_participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8)
  
  // 验证桌子数量是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, event_participants)
  assert_eq(1, remaining_seats)
  
  // 计算参与者总预算
  let total_participant_budget = multiply(event_participants, budget_per_participant)
  assert_eq(4350, total_participant_budget)
  
  // 计算场地总成本
  let total_venue_cost = multiply(venue_cost_per_hour, event_duration_hours)
  assert_eq(800, total_venue_cost)
  
  // 计算活动总成本
  let total_event_cost = add(total_participant_budget, total_venue_cost)
  assert_eq(5150, total_event_cost)
  
  // 计算人均实际成本
  let actual_cost_per_person = divide_with_ceil(total_event_cost, event_participants)
  assert_eq(60, actual_cost_per_person)  // ceil(5150/87)
  
  // 验证预算充足性
  assert_true(actual_cost_per_person <= budget_per_participant + 20)  // 允许20元溢出
}
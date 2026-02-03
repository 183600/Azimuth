// Azimuth 增强标准测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "mathematical_distributive_property" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 4
  let c = 3
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
}

test "complex_resource_allocation" {
  // 复杂资源分配场景测试
  let team_members = 8
  let tasks_per_member = 7
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  let total_capacity = @azimuth.multiply(sprints_needed, sprint_capacity)
  let unused_capacity = @azimuth.subtract(total_capacity, total_tasks)
  
  @azimuth.assert_eq(56, total_tasks)        // 8 * 7 = 56
  @azimuth.assert_eq(3, sprints_needed)      // ceil(56/20) = 3
  @azimuth.assert_eq(60, total_capacity)     // 3 * 20 = 60
  @azimuth.assert_eq(4, unused_capacity)     // 60 - 56 = 4
}

test "negative_number_operations_comprehensive" {
  // 负数运算综合测试
  let a = -12
  let b = -8
  let c = 15
  
  // 负数加法
  @azimuth.assert_eq(-20, @azimuth.add(a, b))
  @azimuth.assert_eq(3, @azimuth.add(a, c))
  
  // 负数乘法
  @azimuth.assert_eq(96, @azimuth.multiply(a, b))
  @azimuth.assert_eq(-180, @azimuth.multiply(a, c))
  
  // 负数除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(a, 5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(c, -20))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, special@#$%!", @azimuth.greet("special@#$%"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, éspanol!", @azimuth.greet("éspanol"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "business_inventory_management" {
  // 库存管理业务场景测试
  let total_products = 147
  let box_capacity = 18
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  
  // 验证箱子数量
  @azimuth.assert_eq(9, boxes_needed)  // ceil(147/18) = 9
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(162, total_capacity)  // 9 * 18 = 162
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(15, remaining_space)  // 162 - 147 = 15
  
  // 验证空间利用率
  let utilization_percent = @azimuth.divide_with_ceil(@azimuth.multiply(total_products, 100), total_capacity)
  @azimuth.assert_eq(91, utilization_percent)  // floor(14700/162) = 90%
}

test "mathematical_boundary_conditions" {
  // 数学边界条件测试
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  
  // 单位值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  
  // 大数测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
}

test "sequential_calculation_pipeline" {
  // 连续计算流水线测试
  let initial_value = 12
  
  // 计算步骤1: 加法
  let step1 = @azimuth.add(initial_value, 8)  // 12 + 8 = 20
  
  // 计算步骤2: 乘法
  let step2 = @azimuth.multiply(step1, 3)     // 20 * 3 = 60
  
  // 计算步骤3: 除法
  let step3 = @azimuth.divide_with_ceil(step2, 17)  // ceil(60/17) = 4
  
  // 计算步骤4: 减法
  let final_result = @azimuth.subtract(step3, 1)    // 4 - 1 = 3
  
  @azimuth.assert_eq(20, step1)
  @azimuth.assert_eq(60, step2)
  @azimuth.assert_eq(4, step3)
  @azimuth.assert_eq(3, final_result)
}

test "event_planning_complex_scenario" {
  // 活动规划复杂场景测试
  let attendees = 256
  let tables_available = 30
  let chairs_per_table = 10
  let cost_per_chair = 25
  let total_budget = 8000
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(26, tables_needed)  // ceil(256/10) = 26
  
  // 验证桌子是否足够
  @azimuth.assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(260, total_seats)  // 26 * 10 = 260
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(total_seats, attendees)
  @azimuth.assert_eq(4, remaining_seats)  // 260 - 256 = 4
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_seats, cost_per_chair)
  @azimuth.assert_eq(6500, total_cost)  // 260 * 25 = 6500
  
  // 验证预算充足
  @azimuth.assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = @azimuth.subtract(total_budget, total_cost)
  @azimuth.assert_eq(1500, remaining_budget)  // 8000 - 6500 = 1500
  
  // 计算预算利用率
  let budget_utilization = @azimuth.divide_with_ceil(@azimuth.multiply(total_cost, 100), total_budget)
  @azimuth.assert_eq(82, budget_utilization)  // floor(650000/8000) = 81%
}
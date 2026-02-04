// 标准 MoonBit 测试用例 - 高质量覆盖测试
// 为 Azimuth 项目添加的10个标准测试用例

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-2, add(-5, 3))
  assert_eq(0, add(5, -5))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法恒等式
  assert_eq(a, add(a, 0))
  assert_eq(a, add(0, a))
  
  // 乘法恒等式
  assert_eq(a, multiply(a, 1))
  assert_eq(a, multiply(1, a))
  assert_eq(0, multiply(a, 0))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, émoji 🚀!", greet("émoji 🚀"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "business_logistics_scenario" {
  // 业务物流场景测试：仓库包装计算
  let total_products = 158
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(10, remaining_space)
  
  // 验证空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(94, utilization_percent)  // floor(15800/168) = 94%
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试：项目资源规划
  let team_members = 9
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_day = 8
  let sprint_capacity = multiply(sprint_duration, tasks_per_day)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(54, total_tasks)  // 9 * 6 = 54
  assert_eq(16, sprint_capacity)  // 2 * 8 = 16
  assert_eq(4, sprints_needed)  // ceil(54/16) = 4
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(64, total_capacity)
  
  // 验证剩余处理能力
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(10, remaining_capacity)
}

test "boundary_values_and_extremes" {
  // 边界值和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(0, -10))
  
  // 单位值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 大数测试（在安全范围内）
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
}

test "negative_number_comprehensive" {
  // 负数综合测试
  // 加法负数测试
  assert_eq(-10, add(-5, -5))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
  assert_eq(0, add(-10, 10))
  
  // 乘法负数测试
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(0, multiply(-5, 0))
  
  // 除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let attendees = 125
  let tables_available = 15
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(16, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(128, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(3, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(3200, total_cost)
  
  // 验证预算充足性（在这个测试中，预算不够，这是一个真实的测试场景）
  assert_true(total_cost > total_budget)
  
  // 计算预算缺口
  let budget_shortage = subtract(total_cost, total_budget)
  assert_eq(200, budget_shortage)
}

test "mathematical_distributive_property" {
  // 数学分配律测试
  let a = 6
  let b = 4
  let c = 3
  
  // 分配律：a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
  
  // 另一个分配律验证
  let x = 8
  let y = 5
  let z = 2
  
  let distributive_result = multiply(x, add(y, z))
  let expanded_result = add(multiply(x, y), multiply(x, z))
  assert_eq(distributive_result, expanded_result)  // 8*(5+2) = 8*5 + 8*2 = 56
}

test "sequential_operations_chain" {
  // 连续操作链测试
  let initial_value = 10
  
  // 复杂计算链：((初始值 + 3) * 2) / 5 + 1
  let step1 = add(initial_value, 3)        // 10 + 3 = 13
  let step2 = multiply(step1, 2)           // 13 * 2 = 26
  let step3 = divide_with_ceil(step2, 5)   // ceil(26/5) = 6
  let step4 = add(step3, 1)                // 6 + 1 = 7
  
  assert_eq(7, step4)
  
  // 另一个计算链验证
  let base = 5
  let chain_result = add(
    divide_with_ceil(
      multiply(add(base, 3), 4),
      7
    ),
    2
  )
  
  // 手动计算：(5+3)*4 = 32, ceil(32/7) = 5, 5+2 = 7
  assert_eq(7, chain_result)
}
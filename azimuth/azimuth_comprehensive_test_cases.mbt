// Azimuth 综合测试用例 - 10个高质量 MoonBit 测试用例
// 覆盖基本功能、边界情况、错误处理和实际应用场景

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  
  assert_eq(21, multiply(3, 7))
  assert_eq(-15, multiply(3, -5))
  assert_eq(12, multiply(-3, -4))
  
  assert_eq(2, subtract(8, 6))
  assert_eq(14, subtract(8, -6))
  assert_eq(-2, subtract(-8, -6))
}

test "boundary_values_and_extremes" {
  // 边界值和极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(100, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1000001, divide_with_ceil(1000001, 1))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(9999, 10000))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-3, 7))
  assert_eq(-4, add(3, -7))
  
  assert_eq(21, multiply(-3, -7))
  assert_eq(-21, multiply(3, -7))
  assert_eq(-21, multiply(-3, 7))
  
  assert_eq(-2, divide_with_ceil(-10, 4))
  assert_eq(-3, divide_with_ceil(10, -4))
  assert_eq(3, divide_with_ceil(-10, -4))
}

test "complex_business_logic" {
  // 复杂业务逻辑测试 - 资源分配场景
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  let buffer_tasks = 5
  let final_tasks = add(total_tasks, buffer_tasks)
  let final_sprints = divide_with_ceil(final_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)
  assert_eq(4, sprints_needed)
  assert_eq(53, final_tasks)
  assert_eq(4, final_sprints)
}

test "string_processing_international" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(
    add(add(a, b), c),
    add(a, add(b, c))
  )
  
  // 乘法结合律
  assert_eq(
    multiply(multiply(a, b), c),
    multiply(a, multiply(b, c))
  )
  
  // 分配律
  assert_eq(
    multiply(a, add(b, c)),
    add(multiply(a, b), multiply(a, c))
  )
}

test "error_handling_and_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零情况的全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(9999, 0))
  
  // 极小值测试
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 单位元测试
  assert_eq(42, add(42, 0))
  assert_eq(42, multiply(42, 1))
  assert_eq(0, subtract(42, 42))
}

test "inventory_management_scenario" {
  // 库存管理场景测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 验证需要的箱子数量
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 验证空间利用率百分比（整数）
  let utilization_percent = divide_with_ceil(
    multiply(total_products, 100),
    total_capacity
  )
  assert_eq(94, utilization_percent)
}

test "financial_calculation_workflow" {
  // 财务计算工作流测试
  let initial_investment = 10000
  let monthly_return = 500
  let months = 12
  
  // 计算总回报
  let total_return = multiply(monthly_return, months)
  
  // 计算最终金额
  let final_amount = add(initial_investment, total_return)
  
  // 计算每个季度的回报
  let quarterly_return = multiply(monthly_return, 3)
  
  // 计算需要的季度报告数量
  let quarterly_reports = divide_with_ceil(months, 3)
  
  assert_eq(6000, total_return)
  assert_eq(16000, final_amount)
  assert_eq(1500, quarterly_return)
  assert_eq(4, quarterly_reports)
}

test "comprehensive_integration_test" {
  // 综合集成测试 - 多步骤复杂计算
  // 场景：活动规划和预算计算
  let attendees = 250
  let tables_per_room = 10
  let chairs_per_table = 6
  let cost_per_chair = 25
  let rooms_available = 5
  let total_budget = 5000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  
  // 计算需要的房间数量
  let rooms_needed = divide_with_ceil(tables_needed, tables_per_room)
  
  // 计算总椅子数量
  let total_chairs = multiply(tables_needed, chairs_per_table)
  
  // 计算总成本
  let total_cost = multiply(total_chairs, cost_per_chair)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  
  // 计算额外椅子数量（由于向上取整）
  let extra_chairs = subtract(total_chairs, attendees)
  
  // 验证所有计算
  assert_eq(42, tables_needed)  // ceil(250/6) = 42
  assert_eq(5, rooms_needed)   // ceil(42/10) = 5
  assert_eq(252, total_chairs) // 42 * 6 = 252
  assert_eq(6300, total_cost)  // 252 * 25 = 6300
  assert_eq(-1300, remaining_budget)  // 5000 - 6300 = -1300 (超预算)
  assert_eq(2, extra_chairs)   // 252 - 250 = 2
  
  // 验证房间是否足够
  assert_true(rooms_needed <= rooms_available)
  
  // 验证座位是否足够
  assert_true(total_chairs >= attendees)
}
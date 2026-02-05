// Azimuth 项目增强测试用例
// 包含 8 个高质量的标准 MoonBit 测试用例

test "arithmetic_precision_tests" {
  // 算术精度测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1001, divide_with_ceil(2001, 2))
  assert_eq(667, divide_with_ceil(2001, 3))
  assert_eq(334, divide_with_ceil(1001, 3))
}

test "mathematical_associative_laws" {
  // 数学结合律测试
  let a = 4
  let b = 6
  let c = 8
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "inventory_management_comprehensive" {
  // 综合库存管理测试
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
  assert_eq(94, utilization_percent)
}

test "complex_financial_calculation" {
  // 复杂金融计算测试
  let principal = 10000
  let interest_rate = 5
  let years = 3
  
  // 计算简单利息
  let total_interest = multiply(principal, multiply(interest_rate, years))
  let final_amount = add(principal, divide_with_ceil(total_interest, 100))
  
  assert_eq(1500, total_interest)
  assert_eq(11500, final_amount)
  
  // 计算月供
  let monthly_payment = divide_with_ceil(final_amount, multiply(years, 12))
  assert_eq(320, monthly_payment)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, CAPS!", greet("CAPS"))
  assert_eq_string("Hello, mix123ED!", greet("mix123ED"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced   "))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, émoji🎉!", greet("émoji🎉"))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_tasks = 95
  let team_members = 8
  let working_days = 5
  
  // 计算每人需要承担的任务
  let tasks_per_person = divide_with_ceil(total_tasks, team_members)
  assert_eq(12, tasks_per_person)
  
  // 计算每天需要完成的任务
  let daily_tasks = divide_with_ceil(total_tasks, multiply(team_members, working_days))
  assert_eq(3, daily_tasks)
  
  // 验证总任务分配
  let allocated_tasks = multiply(tasks_per_person, team_members)
  assert_eq(96, allocated_tasks)
  
  // 验证任务覆盖率
  let coverage = divide_with_ceil(multiply(total_tasks, 100), allocated_tasks)
  assert_eq(99, coverage)
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  assert_eq(0, divide_with_ceil(-999999, 0))
  
  // 极值运算测试
  let large_positive = 100000
  let large_negative = -100000
  
  assert_eq(0, multiply(large_positive, 0))
  assert_eq(0, multiply(large_negative, 0))
  assert_eq(large_positive, add(large_positive, 0))
  assert_eq(large_negative, add(large_negative, 0))
  
  // 边界减法测试
  assert_eq(0, subtract(large_positive, large_positive))
  assert_eq(0, subtract(large_negative, large_negative))
  assert_eq(200000, subtract(large_positive, large_negative))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let attendees = 247
  let tables_available = 30
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 6000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(31, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(248, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(1, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6200, total_cost)
  
  // 验证预算是否充足（在这个测试中预算不足）
  assert_true(total_cost > total_budget)
  
  // 计算预算缺口
  let budget_shortage = subtract(total_cost, total_budget)
  assert_eq(200, budget_shortage)
  
  // 计算需要的额外预算百分比
  let additional_budget_percent = divide_with_ceil(multiply(budget_shortage, 100), total_budget)
  assert_eq(4, additional_budget_percent)
}
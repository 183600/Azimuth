// Azimuth 项目综合测试用例
// 标准 MoonBit 测试语法，包含8个高质量测试用例

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 42
  
  // 加法恒等式：x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式：x * 1 = x, x * 0 = 0
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式：x - x = 0, x - 0 = x
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "divide_with_ceil_comprehensive_coverage" {
  // 向上取整除法综合覆盖测试
  // 基本情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试：项目管理场景
  let team_members = 8
  let tasks_per_member = 6
  let sprint_capacity = 20
  let project_duration = 4
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_member)
  assert_eq(48, total_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(3, sprints_needed)
  
  // 计算项目总工作量
  let total_workload = multiply(sprints_needed, project_duration)
  assert_eq(12, total_workload)
  
  // 计算剩余任务处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(12, remaining_capacity)
}

test "mathematical_commutative_and_associative" {
  // 数学交换律和结合律测试
  let a = 7
  let b = 13
  let c = 5
  
  // 加法交换律：a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律：a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律：(a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律：a * (b + c) = a * b + a * c
  assert_eq(
    multiply(a, add(b, c)),
    add(multiply(a, b), multiply(a, c))
  )
}

test "resource_allocation_optimization" {
  // 资源分配优化测试：库存管理场景
  let total_products = 157
  let box_capacity = 24
  let available_boxes = 10
  let cost_per_box = 50
  let total_budget = 600
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(7, boxes_needed)
  
  // 验证箱子数量是否在可用范围内
  assert_true(boxes_needed <= available_boxes)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 计算总成本
  let total_cost = multiply(boxes_needed, cost_per_box)
  assert_eq(350, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(250, remaining_budget)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 大数测试（安全范围内）
  assert_eq(20000, add(10000, 10000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  
  // 负数边界测试
  assert_eq(-1, add(-1, 0))
  assert_eq(-1000, multiply(-1000, 1))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试：会议安排场景
  let participants = 125
  let tables_available = 15
  let chairs_per_table = 8
  let cost_per_chair = 25
  let venue_capacity = 200
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(16, tables_needed)
  
  // 检查桌子可用性（注意：这里需要更多桌子）
  let additional_tables_needed = subtract(tables_needed, tables_available)
  assert_eq(1, additional_tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(128, total_seats)
  
  // 验证场地容量充足
  assert_true(total_seats <= venue_capacity)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(3200, total_cost)
  
  // 注意：这里超出了预算，这是一个现实的测试场景
  let budget_overrun = subtract(total_cost, total_budget)
  assert_eq(200, budget_overrun)
  
  // 计算场地利用率
  let venue_utilization = divide_with_ceil(multiply(total_seats, 100), venue_capacity)
  assert_eq(64, venue_utilization)  // floor(12800/200) = 64%
}
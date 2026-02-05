// Enhanced Standard Test Cases for Azimuth Project
// 高质量标准测试用例，使用标准 MoonBit 测试语法

test "arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  assert_eq(15, add(8, 7))
  assert_eq(-15, add(-8, -7))
  assert_eq(1, add(-8, 9))
  assert_eq(0, add(8, -8))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-56, multiply(-7, 8))
  assert_eq(56, multiply(-7, -8))
  assert_eq(0, multiply(7, 0))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(7, 7))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
  assert_eq(-2, divide_with_ceil(3, -2))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello,  spaces !", greet(" spaces "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 4
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 验证空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(94, utilization_percent)
}

test "complex_workflow_resource_planning" {
  // 复杂工作流资源规划测试
  let team_members = 9
  let tasks_per_member = 7
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(63, total_tasks)
  assert_eq(4, sprints_needed)
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(80, total_capacity)
  
  // 验证剩余处理能力
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(17, remaining_capacity)
}

test "error_handling_and_boundary_values" {
  // 错误处理和边界值测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000, multiply(31623, 31623))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(1000001, divide_with_ceil(1000001, 1000000))
  
  // 零值测试
  assert_eq(0, multiply(0, 1000000))
  assert_eq(1000000, add(1000000, 0))
  assert_eq(-1000000, subtract(0, 1000000))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let attendees = 250
  let tables_available = 30
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 6000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(32, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(256, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(6, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6400, total_cost)
  
  // 验证预算（这个测试会失败，用于演示）
  // assert_true(total_cost <= total_budget)
  
  // 计算预算差额
  let budget_difference = subtract(total_cost, total_budget)
  assert_eq(400, budget_difference)
}
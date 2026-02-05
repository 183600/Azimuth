// 标准 MoonBit 测试套件 - 为 Azimuth 项目添加
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-3, -7))
  
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(5, 0))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(21, multiply(-3, -7))
  
  assert_eq(5, subtract(12, 7))
  assert_eq(-5, subtract(7, 12))
  assert_eq(0, subtract(10, 10))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法综合场景测试
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
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
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 13
  let b = 17
  
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
  
  // 减法恒等式
  assert_eq(0, subtract(a, a))
  assert_eq(a, subtract(a, 0))
}

test "business_resource_allocation" {
  // 业务资源分配测试
  let total_tasks = 87
  let team_members = 8
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  
  assert_eq(11, tasks_per_member)
  
  // 验证总任务分配
  let allocated_tasks = multiply(tasks_per_member, team_members)
  assert_eq(88, allocated_tasks)
  
  // 验证剩余任务容量
  let remaining_capacity = subtract(allocated_tasks, total_tasks)
  assert_eq(1, remaining_capacity)
  
  // 验证每个成员实际可处理的平均任务数
  let average_tasks = divide_with_ceil(total_tasks, team_members)
  assert_eq(11, average_tasks)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目管理中的资源计算
  let project_duration = 12  // 周
  let team_size = 6
  let hours_per_week = 40
  let total_hours = multiply(project_duration, multiply(team_size, hours_per_week))
  
  assert_eq(2880, total_hours)  // 12 * 6 * 40 = 2880
  
  // 计算冲刺数量（每冲刺80小时）
  let sprint_hours = 80
  let sprints_needed = divide_with_ceil(total_hours, sprint_hours)
  
  assert_eq(36, sprints_needed)  // ceil(2880/80) = 36
  
  // 计算实际总冲刺小时数
  let actual_sprint_hours = multiply(sprints_needed, sprint_hours)
  assert_eq(2880, actual_sprint_hours)
  
  // 验证冲刺利用率
  let utilization = divide_with_ceil(multiply(total_hours, 100), actual_sprint_hours)
  assert_eq(100, utilization)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
}

test "inventory_management_system" {
  // 库存管理系统测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 计算空间利用率
  let utilization_percentage = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(94, utilization_percentage)  // floor(15700/168) = 93%
  
  // 验证空间充足性
  assert_true(total_capacity >= total_products)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let loan_amount = 50000
  let monthly_payment = 2200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(23, months_needed)
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)
  
  // 计算额外支付金额
  let extra_payment = subtract(total_payment, loan_amount)
  assert_eq(600, extra_payment)
  
  // 计算月均实际支付
  let actual_monthly = divide_with_ceil(total_payment, months_needed)
  assert_eq(2200, actual_monthly)
}

test "event_planning_comprehensive" {
  // 活动规划综合测试
  let attendees = 250
  let tables_available = 30
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 7000
  
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
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(600, remaining_budget)
  
  // 计算预算利用率
  let budget_utilization = divide_with_ceil(multiply(total_cost, 100), total_budget)
  assert_eq(92, budget_utilization)  // floor(640000/7000) = 91%
}
// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "standard_arithmetic_operations" {
  // 标准算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-1, @azimuth.add(5, -6))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
}

test "standard_divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "standard_greet_function_comprehensive" {
  // 问候函数综合测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, spaced name!", @azimuth.greet("spaced name"))
}

test "standard_mathematical_properties" {
  // 数学性质标准测试
  let x = 12
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
}

test "standard_business_packaging_scenario" {
  // 标准业务包装场景测试
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "standard_complex_calculation_workflow" {
  // 标准复杂计算工作流测试
  // 场景：项目资源管理
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(3, sprints_needed)  // ceil(48/20) = 3
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  let unused_capacity = subtract(total_capacity, total_tasks)
  assert_eq(12, unused_capacity)  // 60 - 48 = 12
}

test "standard_boundary_values_and_edge_cases" {
  // 标准边界值和边缘情况测试
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  
  // 单位值测试
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  
  // �数边界测试
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
}

test "standard_financial_calculation_scenario" {
  // 标准金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 15000
  let monthly_payment = 1200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(13, months_needed)
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(15600, total_payment)
  
  // 计算额外支付
  let extra_payment = subtract(total_payment, loan_amount)
  assert_eq(600, extra_payment)
}

test "standard_resource_allocation_optimization" {
  // 标准资源分配优化测试
  // 场景：服务器资源分配
  let total_cpu_cores = 32
  let services = 5
  let cores_per_service = divide_with_ceil(total_cpu_cores, services)
  
  assert_eq(7, cores_per_service)
  
  // 验证总分配
  let total_allocated = multiply(cores_per_service, services)
  assert_eq(35, total_allocated)
  
  // 计算资源超额
  let resource_overcommit = subtract(total_allocated, total_cpu_cores)
  assert_eq(3, resource_overcommit)
  
  // 计算资源利用率
  let utilization = multiply(divide_with_ceil(total_cpu_cores, total_allocated), 100)
  assert_eq(91, utilization)  // floor(32/35 * 100) = 91%
}

test "standard_event_planning_logistics" {
  // 标准活动策划后勤测试
  // 场景：会议座位安排
  let attendees = 125
  let tables_available = 15
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 3500
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(16, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(128, total_seats)
  
  // 计算剩余座位
  let empty_seats = subtract(total_seats, attendees)
  assert_eq(3, empty_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(3200, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(300, remaining_budget)
}
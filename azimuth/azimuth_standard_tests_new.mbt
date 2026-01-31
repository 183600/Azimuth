// Azimuth 标准测试用例 - 新增10个高质量测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "basic_arithmetic_comprehensive" {
  // 综合基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(0, 0))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-20, multiply(-5, 4))
  assert_eq(0, multiply(0, 100))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-9, subtract(5, 14))
  assert_eq(0, subtract(100, 100))
}

test "divide_with_ceil_precision_test" {
  // 向上取整除法精度测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, special@#$!", greet("special@#$"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 结合律
  assert_eq(
    add(add(x, y), 5),
    add(x, add(y, 5))
  )
}

test "business_inventory_management" {
  // 库存管理业务逻辑测试
  let total_products = 157
  let box_capacity = 24
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_members = 6
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  assert_eq(720, total_hours)
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  assert_eq(1440, total_tasks)
  
  // 计算需要的冲刺数（每冲刺处理500个任务）
  let sprints_needed = divide_with_ceil(total_tasks, 500)
  assert_eq(3, sprints_needed)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(1000, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 25000
  let departments = 4
  
  // 计算每个部门的最小预算
  let min_budget_per_dept = divide_with_ceil(total_budget, departments)
  assert_eq(6250, min_budget_per_dept)
  
  // 验证总分配
  let total_allocated = multiply(min_budget_per_dept, departments)
  assert_eq(25000, total_allocated)
  
  // 计算额外预算
  let extra_budget = subtract(total_allocated, total_budget)
  assert_eq(0, extra_budget)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 50000
  let monthly_payment = 2200
  
  // 计算需要的月数
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  assert_eq(23, months_needed)
  
  // 验证总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)
  
  // 计算最后一个月的实际支付
  let final_payment = subtract(
    total_payment, 
    multiply(months_needed - 1, monthly_payment)
  )
  assert_eq(200, final_payment)
}

test "event_planning_logistics" {
  // 活动规划后勤测试
  let attendees = 245
  let tables_available = 30
  let chairs_per_table = 8
  let cost_per_chair = 25
  
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
  assert_eq(3, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6200, total_cost)
  
  // 验证成本合理性
  assert_true(total_cost > 0)
  assert_true(total_cost < 10000)
}
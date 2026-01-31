// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含8个高质量测试用例，覆盖核心功能和边界情况

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(0, 0))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-20, multiply(-5, 4))
  assert_eq(0, multiply(0, 100))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-9, subtract(2, 11))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
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

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
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
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
}

test "business_logistics_scenario" {
  // 物流业务场景测试
  let total_packages = 157
  let packages_per_box = 24
  let boxes_needed = divide_with_ceil(total_packages, packages_per_box)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, packages_per_box)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
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
  
  // 计算需要的冲刺次数（每冲刺400个任务）
  let sprints_needed = divide_with_ceil(total_tasks, 400)
  assert_eq(4, sprints_needed)
}

test "error_handling_boundary_cases" {
  // 错误处理和边界情况测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 250
  let tables_available = 30
  let chairs_per_table = 8
  let budget_per_chair = 25
  let total_budget = 6000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(32, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(256, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(6, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, budget_per_chair)
  assert_eq(6400, total_cost)
  
  // 计算预算差额
  let budget_overage = subtract(total_cost, total_budget)
  assert_eq(400, budget_overage)
}
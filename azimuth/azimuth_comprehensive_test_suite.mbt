// Azimuth 综合测试套件
// 包含10个高质量的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  // 测试加法、减法、乘法和除法向上取整的基本功能
  
  // 加法测试
  assert_eq(42, add(20, 22))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-15, 5))
  assert_eq(100000, add(50000, 50000))
  
  // 减法测试
  assert_eq(15, subtract(25, 10))
  assert_eq(-15, subtract(10, 25))
  assert_eq(0, subtract(100, 100))
  
  // 乘法测试
  assert_eq(36, multiply(6, 6))
  assert_eq(0, multiply(100, 0))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(10000, multiply(100, 100))
  
  // 除法向上取整测试
  assert_eq(5, divide_with_ceil(22, 5))
  assert_eq(3, divide_with_ceil(15, 5))
  assert_eq(1, divide_with_ceil(5, 10))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "boundary_conditions_and_edge_cases" {
  // 边界条件和边缘情况测试
  
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(2, divide_with_ceil(101, 100))
  assert_eq(100, divide_with_ceil(100, 1))
  
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
}

test "negative_number_operations" {
  // 负数运算测试
  
  // 负数加法
  assert_eq(-20, add(-10, -10))
  assert_eq(0, add(-10, 10))
  assert_eq(5, add(-15, 20))
  
  // 负数减法
  assert_eq(-20, subtract(-10, 10))
  assert_eq(20, subtract(10, -10))
  assert_eq(-5, subtract(-15, -10))
  
  // 负数乘法
  assert_eq(100, multiply(-10, -10))
  assert_eq(-100, multiply(-10, 10))
  assert_eq(-100, multiply(10, -10))
  
  // 负数除法向上取整
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "complex_business_scenarios" {
  // 复杂业务场景测试
  
  // 场景1：计算项目资源需求
  let team_members = 12
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(96, total_tasks)  // 12 * 8 = 96
  assert_eq(5, sprints_needed)  // ceil(96/20) = 5
  
  // 场景2：库存管理
  let total_items = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(7, boxes_needed)  // ceil(157/24) = 7
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(11, remaining_space)  // 7*24 - 157 = 168 - 157 = 11
  
  // 场景3：预算计算
  let daily_rate = 500
  let project_days = 15
  let team_size = 6
  let total_budget = multiply(daily_rate, multiply(project_days, team_size))
  
  assert_eq(45000, total_budget)  // 500 * 15 * 6 = 45000
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  
  let a = 15
  let b = 25
  let c = 10
  
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
  
  // 减法性质
  assert_eq(subtract(a, b), add(a, multiply(b, -1)))
  
  // 恒等式
  assert_eq(a, add(a, 0))
  assert_eq(a, multiply(a, 1))
  assert_eq(0, multiply(a, 0))
  assert_eq(0, subtract(a, a))
}

test "string_processing_and_greetings" {
  // 字符串处理和问候功能测试
  
  // 基本问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  
  // 特殊字符测试
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, space !", greet("space "))
  assert_eq_string("Hello,  space!", greet(" space"))
  
  // Unicode字符测试
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  
  // 空白字符测试
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
}

test "large_number_precision_tests" {
  // 大数精度测试
  
  // 大数加法
  assert_eq(2000000000, add(1000000000, 1000000000))
  assert_eq(1000001, add(1000000, 1))
  assert_eq(1000000000000, add(500000000000, 500000000000))
  
  // 大数乘法
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(100000000000000, multiply(10000000, 10000000))
  
  // 大数除法向上取整
  assert_eq(1000000, divide_with_ceil(1000000000, 1000))
  assert_eq(1000001, divide_with_ceil(1000001000, 1000))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
  
  // 大数减法
  assert_eq(0, subtract(1000000000, 1000000000))
  assert_eq(999999999, subtract(1000000000, 1))
  assert_eq(1000000000, subtract(2000000000, 1000000000))
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  
  // 除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  
  // 边界值处理
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(-1, divide_with_ceil(1, -1))
  
  // 极小值处理
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(-1, divide_with_ceil(-1, 1000000))
  assert_eq(1, divide_with_ceil(1, -1000000))
  assert_eq(-1, divide_with_ceil(-1, -1000000))
  
  // 字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, !", greet(String::from_ascii_with_code(0)))
}

test "complex_computation_chains" {
  // 复杂计算链测试
  
  // 计算链1：((a + b) * c) / d
  let a = 10
  let b = 15
  let c = 3
  let d = 4
  
  let step1 = add(a, b)          // 25
  let step2 = multiply(step1, c) // 75
  let result1 = divide_with_ceil(step2, d) // 19
  assert_eq(19, result1)
  
  // 计算链2：项目成本分析
  let hourly_rate = 50
  let hours_per_day = 8
  let team_size = 5
  let project_days = 12
  
  let daily_cost = multiply(hourly_rate, hours_per_day)      // 400
  let team_daily_cost = multiply(daily_cost, team_size)     // 2000
  let total_cost = multiply(team_daily_cost, project_days)  // 24000
  
  let budget_per_phase = 10000
  let phases_needed = divide_with_ceil(total_cost, budget_per_phase) // 3
  
  assert_eq(24000, total_cost)
  assert_eq(3, phases_needed)
  
  // 计算链3：资源分配优化
  let total_resources = 100
  let teams = 3
  let base_allocation = divide_with_ceil(total_resources, teams) // 34
  let remaining = subtract(total_resources, multiply(base_allocation, teams)) // -2
  
  assert_eq(34, base_allocation)
  assert_eq(-2, remaining)
}

test "real_world_application_scenarios" {
  // 实际应用场景测试
  
  // 场景1：电商订单处理
  let order_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, items_per_box)
  let unused_capacity = subtract(total_capacity, order_items)
  assert_eq(1, unused_capacity)
  
  // 场景2：活动座位安排
  let attendees = 250
  let seats_per_row = 18
  let rows_needed = divide_with_ceil(attendees, seats_per_row)
  
  assert_eq(14, rows_needed)
  
  let total_seats = multiply(rows_needed, seats_per_row)
  let empty_seats = subtract(total_seats, attendees)
  assert_eq(2, empty_seats)
  
  // 场景3：数据传输分块
  let data_size = 10000
  let chunk_size = 3000
  let chunks_needed = divide_with_ceil(data_size, chunk_size)
  
  assert_eq(4, chunks_needed)
  
  let last_chunk_size = subtract(data_size, multiply(chunks_needed - 1, chunk_size))
  assert_eq(1000, last_chunk_size)
  
  // 场景4：员工排班
  let total_shifts = 35
  let employees = 8
  let shifts_per_employee = divide_with_ceil(total_shifts, employees)
  
  assert_eq(5, shifts_per_employee)
  
  let total_assigned_shifts = multiply(shifts_per_employee, employees)
  let extra_shifts = subtract(total_assigned_shifts, total_shifts)
  assert_eq(5, extra_shifts)
}
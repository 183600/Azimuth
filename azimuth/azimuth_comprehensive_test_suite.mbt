// Azimuth 综合测试套件 - 标准 MoonBit 测试语法
// 包含10个高质量测试用例，覆盖核心功能和边界情况

test "azimuth_basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(56, multiply(7, 8))
  assert_eq(-1, subtract(7, 8))
  assert_eq(2, divide_with_ceil(15, 8))
}

test "azimuth_mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 23
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(40, add(23, 17))
  assert_eq(391, multiply(23, 17))
}

test "azimuth_divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(1, divide_with_ceil(3, 10))
  assert_eq(0, divide_with_ceil(0, 7))
  assert_eq(0, divide_with_ceil(9, 0))
}

test "azimuth_negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(21, multiply(-3, -7))
  assert_eq(4, subtract(-3, -7))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(1, divide_with_ceil(-3, -4))
}

test "azimuth_greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "azimuth_business_packaging_scenario" {
  // 业务包装场景测试
  let total_products = 127
  let package_capacity = 15
  let packages_needed = divide_with_ceil(total_products, package_capacity)
  
  assert_eq(9, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, package_capacity)
  assert_eq(135, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(8, remaining_space)
}

test "azimuth_complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目管理资源计算
  let team_size = 9
  let tasks_per_member = 7
  let total_tasks = multiply(team_size, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_day = 10
  let sprint_capacity = multiply(sprint_duration, tasks_per_day)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(63, total_tasks)  // 9 * 7 = 63
  assert_eq(20, sprint_capacity)  // 2 * 10 = 20
  assert_eq(4, sprints_needed)  // ceil(63/20) = 4
}

test "azimuth_boundary_values_and_extremes" {
  // 边界值和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(200000, add(100000, 100000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(100001, divide_with_ceil(100000, 1))
}

test "azimuth_mathematical_distributive_property" {
  // 数学分配律测试
  let a = 6
  let b = 8
  let c = 4
  
  // 分配律: a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 6 * (8 + 4) = 6 * 8 + 6 * 4 = 72
}

test "azimuth_real_world_event_planning" {
  // 真实世界事件规划测试
  let attendees = 185
  let tables_available = 20
  let seats_per_table = 10
  let cost_per_seat = 25
  let total_budget = 5000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, seats_per_table)
  assert_eq(19, tables_needed)
  
  // 检查桌子是否足够
  let total_seats = multiply(tables_needed, seats_per_table)
  assert_eq(190, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(5, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_seat)
  assert_eq(4750, total_cost)
  
  // 验证预算充足
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(250, remaining_budget)
}
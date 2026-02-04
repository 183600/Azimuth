// 标准 MoonBit 测试用例
// 为 Azimuth 项目创建的高质量测试用例

test "basic_addition_functionality" {
  // 基本加法功能测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "greet_function_basic_tests" {
  // 问候函数基本测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive_numbers" {
  // 向上取整除法正数测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "subtract_function_basic_tests" {
  // 减法函数基本测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let items = 25
  let capacity = 6
  let packages = divide_with_ceil(items, capacity)
  assert_eq(5, packages)
  
  let total_capacity = multiply(packages, capacity)
  let remaining = subtract(total_capacity, items)
  assert_eq(5, remaining)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let team = 4
  let tasks_per_person = 8
  let total_tasks = multiply(team, tasks_per_person)
  
  let sprint_capacity = 15
  let sprints = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(32, total_tasks)  // 4 * 8 = 32
  assert_eq(3, sprints)       // ceil(32/15) = 3
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}

// 新增的高质量测试用例
test "arithmetic_identity_properties" {
  // 算术恒等式测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(5, add(-10, 15))
  
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(0, multiply(-5, 0))
  
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-2, divide_with_ceil(-3, 2))
}

test "greeting_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@#$!", greet("special@#$"))
}

test "resource_allocation_calculation" {
  // 资源分配计算测试
  let total_resources = 100
  let teams = 3
  let min_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(34, min_per_team)
  
  let allocated_total = multiply(min_per_team, teams)
  let remaining = subtract(allocated_total, total_resources)
  assert_eq(2, remaining)
}

test "mathematical_distributive_property" {
  // 数学分配律测试
  let a = 6
  let b = 4
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "event_planning_scenario" {
  // 活动规划场景测试
  let participants = 87
  let chairs_per_table = 10
  
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  let total_seats = multiply(tables_needed, chairs_per_table)
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(3, remaining_seats)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 最小值测试
  assert_eq(1, add(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 最大安全值测试
  assert_eq(20000, add(10000, 10000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 除法边界
  assert_eq(1, divide_with_ceil(9999, 10000))
  assert_eq(10000, divide_with_ceil(10000, 1))
}

test "complex_workflow_integration" {
  // 复杂工作流集成测试
  // 场景：项目管理计算
  let team_size = 8
  let hours_per_day = 8
  let days_per_sprint = 2
  let hourly_rate = 50
  let total_budget = 10000
  
  let daily_capacity = multiply(team_size, hours_per_day)
  let sprint_capacity = multiply(daily_capacity, days_per_sprint)
  let sprint_cost = multiply(sprint_capacity, hourly_rate)
  let max_sprints = divide_with_ceil(total_budget, sprint_cost)
  
  assert_eq(64, daily_capacity)
  assert_eq(128, sprint_capacity)
  assert_eq(6400, sprint_cost)
  assert_eq(2, max_sprints)
}
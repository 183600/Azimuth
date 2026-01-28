// 新增的标准 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(15, add(8, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-8, 13))
  assert_eq(2000, add(1000, 1000))
}

test "multiply_function_divisibility" {
  // 测试乘法函数的可除性
  assert_eq(0, multiply(0, 123))
  assert_eq(123, multiply(1, 123))
  assert_eq(-123, multiply(-1, 123))
  assert_eq(123, multiply(-1, -123))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, español!", greet("español"))
}

test "divide_with_ceil_logistics_calculation" {
  // 测试向上取整除法的物流计算场景
  let packages = 58
  let capacity_per_truck = 12
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(60, total_capacity)
}

test "subtract_function_financial_scenario" {
  // 测试减法函数的财务场景
  let budget = 10000
  let expenses = 3750
  let remaining = subtract(budget, expenses)
  assert_eq(6250, remaining)
  
  let over_budget = subtract(expenses, budget)
  assert_eq(-6250, over_budget)
}

test "complex_project_scheduling" {
  // 复杂项目排期测试
  let tasks = 37
  let team_members = 5
  let tasks_per_person_per_day = 2
  
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  let days_needed = divide_with_ceil(tasks, daily_capacity)
  assert_eq(4, days_needed)
  
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(40, total_capacity)
}

test "mathematical_associative_properties" {
  // 测试数学结合律性质
  let a = 3
  let b = 4
  let c = 5
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "resource_planning_optimization" {
  // 资源规划优化测试
  let total_budget = 15000
  let departments = 4
  let budget_per_department = divide_with_ceil(total_budget, departments)
  assert_eq(3750, budget_per_department)
  
  let total_allocated = multiply(budget_per_department, departments)
  assert_eq(15000, total_allocated)
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let guests = 125
  let tables = 15
  let chairs_per_table = 8
  
  let total_chairs = multiply(tables, chairs_per_table)
  assert_eq(120, total_chairs)
  
  let additional_chairs_needed = subtract(guests, total_chairs)
  assert_eq(5, additional_chairs_needed)
  
  let extra_tables = divide_with_ceil(additional_chairs_needed, chairs_per_table)
  assert_eq(1, extra_tables)
}
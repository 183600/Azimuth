// Azimuth 项目标准测试用例
// 包含8个高质量的 MoonBit 测试用例，覆盖核心功能和实际应用场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, divide_with_ceil(8, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greeting_functionality" {
  // 问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "mathematical_properties" {
  // 数学性质验证
  let x = 12
  let y = 7
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
}

test "packaging_calculation_scenario" {
  // 包装计算场景测试
  let total_items = 47
  let items_per_package = 10
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(50, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(3, remaining_space)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-5, subtract(3, 8))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let teams = 3
  let min_resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(34, min_resources_per_team)
  
  // 验证分配结果
  let total_allocated = multiply(min_resources_per_team, teams)
  assert_eq(102, total_allocated)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), total_allocated)
  assert_eq(98, utilization)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目时间估算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let days_per_sprint = 5
  let tasks_per_day = 10
  let sprint_capacity = multiply(days_per_sprint, tasks_per_day)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)
  assert_eq(50, sprint_capacity)
  assert_eq(1, sprints_needed)
}

test "boundary_and_edge_cases" {
  // 边界和边缘情况测试
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 特殊字符串测试
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}
// 新的标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "mathematical_distributive_property" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 2
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 5 * (3 + 2) = 5 * 3 + 5 * 2 = 25
}

test "complex_resource_allocation" {
  // 测试复杂资源分配场景
  let team_members = 7
  let projects_per_member = 4
  let total_projects = multiply(team_members, projects_per_member)
  
  let sprints_available = 6
  let projects_per_sprint = divide_with_ceil(total_projects, sprints_available)
  
  assert_eq(28, total_projects)  // 7 * 4 = 28
  assert_eq(5, projects_per_sprint)  // ceil(28/6) = 5
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, 1234567890!", greet("1234567890"))
  assert_eq_string("Hello, mixed123!", greet("mixed123"))
  assert_eq_string("Hello, CamelCase!", greet("CamelCase"))
  assert_eq_string("Hello, snake_case!", greet("snake_case"))
}

test "division_precision_scenarios" {
  // 测试除法精度场景
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(34, divide_with_ceil(99, 3))
  assert_eq(34, divide_with_ceil(98, 3))
  assert_eq(34, divide_with_ceil(97, 3))
  assert_eq(33, divide_with_ceil(96, 3))
}

test "negative_zero_interactions" {
  // 测试负数与零的交互
  assert_eq(-5, add(-5, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(-5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "sequential_calculation_pipeline" {
  // 测试连续计算管道
  let initial_value = 10
  
  // 计算管道: ((x + 5) * 2) / 3 + 1
  let step1 = add(initial_value, 5)        // 15
  let step2 = multiply(step1, 2)           // 30
  let step3 = divide_with_ceil(step2, 3)   // 10
  let step4 = add(step3, 1)                // 11
  
  assert_eq(11, step4)
}

test "inventory_optimization_scenario" {
  // 测试库存优化场景
  let demand_a = 45
  let demand_b = 32
  let demand_c = 28
  let total_demand = add(add(demand_a, demand_b), demand_c)
  
  let package_size = 20
  let packages_needed = divide_with_ceil(total_demand, package_size)
  
  assert_eq(105, total_demand)  // 45 + 32 + 28 = 105
  assert_eq(6, packages_needed)  // ceil(105/20) = 6
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, package_size)
  let surplus = subtract(total_capacity, total_demand)
  
  assert_eq(120, total_capacity)  // 6 * 20 = 120
  assert_eq(15, surplus)  // 120 - 105 = 15
}

test "mathematical_boundary_conditions" {
  // 测试数学边界条件
  // 最小正值边界
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(2, divide_with_ceil(101, 100))
  
  // 大数边界
  assert_eq(1000, divide_with_ceil(999999, 1000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  
  // 负数边界
  assert_eq(-1, divide_with_ceil(-1, 100))
  assert_eq(-10, divide_with_ceil(-999, 100))
}
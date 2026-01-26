// Azimuth 新增标准测试用例
// 包含10个高质量的 MoonBit 测试用例，覆盖核心功能模块

test "add_function_edge_cases" {
  // 测试加法函数的边缘情况
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(-1000000, add(-500000, -500000))
}

test "multiply_function_various_cases" {
  // 测试乘法函数的各种情况
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(12345, multiply(-1, -12345))
  assert_eq(1000000, multiply(1000, 1000))
}

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(10, 10))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000, divide_with_ceil(1999, 2))
  assert_eq(666667, divide_with_ceil(2000000, 3))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, empty!", greet("empty"))
}

test "mathematical_commutative_laws" {
  // 验证数学交换律
  let a = 42
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_items = 157
  let containers = 12
  let items_per_container = divide_with_ceil(total_items, containers)
  
  assert_eq(14, items_per_container)  // ceil(157/12) = 14
  
  // 验证总容量
  let total_capacity = multiply(items_per_container, containers)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(11, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial_value = 20
  
  // 计算流程：((initial / 3) * 2) + 5
  let step1 = divide_with_ceil(initial_value, 3)
  let step2 = multiply(step1, 2)
  let result = add(step2, 5)
  
  assert_eq(19, result)  // ceil(20/3) * 2 + 5 = 7 * 2 + 5 = 19
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "real_world_business_calculation" {
  // 真实世界业务计算测试
  // 场景：计算项目成本和资源需求
  let team_members = 8
  let daily_rate = 150
  let project_days = 15
  let budget_per_sprint = 5000
  
  // 计算总成本
  let total_cost = multiply(team_members, multiply(daily_rate, project_days))
  assert_eq(18000, total_cost)  // 8 * 150 * 15 = 18000
  
  // 计算需要的冲刺次数
  let sprints_needed = divide_with_ceil(total_cost, budget_per_sprint)
  assert_eq(4, sprints_needed)  // ceil(18000/5000) = 4
  
  // 计算最终冲刺的预算
  let final_sprint_budget = subtract(
    multiply(sprints_needed, budget_per_sprint),
    multiply(sprints_needed - 1, budget_per_sprint)
  )
  assert_eq(5000, final_sprint_budget)
}
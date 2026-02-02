// 高质量标准 MoonBit 测试用例
// 为 Azimuth 项目新增的测试用例集合

test "arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  assert_eq(10, add(6, 4))
  assert_eq(-10, add(-6, -4))
  assert_eq(2, add(-6, 8))
  assert_eq(0, add(5, -5))
  
  assert_eq(24, multiply(6, 4))
  assert_eq(24, multiply(-6, -4))
  assert_eq(-24, multiply(-6, 4))
  assert_eq(0, multiply(6, 0))
  
  assert_eq(2, subtract(6, 4))
  assert_eq(-2, subtract(4, 6))
  assert_eq(10, subtract(6, -4))
  assert_eq(-10, subtract(-6, 4))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 3))
  assert_eq(0, divide_with_ceil(0, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  
  // 负数情况
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 15
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  let c = 3
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(6, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(3, sprints_needed)  // ceil(48/20) = 3
  
  // 验证最后一轮的工作量
  let full_sprints_work = multiply(sprints_needed - 1, sprint_capacity)
  let final_sprint_work = subtract(total_tasks, full_sprints_work)
  assert_eq(8, final_sprint_work)  // 48 - 2*20 = 8
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let participants = 125
  let tables_available = 15
  let chairs_per_table = 8
  let budget_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(16, tables_needed)
  
  // 检查桌子是否足够
  let chairs_available = multiply(tables_available, chairs_per_table)
  assert_true(chairs_available >= participants)
  
  // 计算总成本
  let total_chairs_needed = multiply(tables_needed, chairs_per_table)
  let total_cost = multiply(total_chairs_needed, budget_per_chair)
  assert_eq(3200, total_cost)
  
  // 验证预算情况
  assert_true(total_cost > total_budget)
  let budget_shortage = subtract(total_cost, total_budget)
  assert_eq(200, budget_shortage)
}

test "assert_functions_functionality" {
  // 断言函数功能测试
  // 测试 assert_true 和 assert_false
  assert_true(5 > 3)
  assert_true(0 == 0)
  assert_true(-5 < -3)
  
  assert_false(5 < 3)
  assert_false(0 != 0)
  assert_false(-5 > -3)
  
  // 测试 assert_eq 和 assert_eq_string
  assert_eq(10, add(3, 7))
  assert_eq_string("Hello, Test!", greet("Test"))
}
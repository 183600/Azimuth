// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，包含数学性质、复杂业务场景和边界条件测试

test "mathematical_associative_law" {
  // 测试数学结合律
  let a = 5
  let b = 7
  let c = 3
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "mathematical_distributive_law" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "complex_resource_allocation" {
  // 复杂资源分配场景测试
  let team_members = 12
  let projects = 3
  let members_per_project = divide_with_ceil(team_members, projects)
  
  assert_eq(4, members_per_project)
  
  // 验证总分配人数
  let total_allocated = multiply(members_per_project, projects)
  let extra_members = subtract(total_allocated, team_members)
  assert_eq(0, extra_members)
}

test "time_calculation_scenario" {
  // 时间计算场景测试
  let days_in_month = 30
  let working_hours_per_day = 8
  let total_working_hours = multiply(days_in_month, working_hours_per_day)
  
  let tasks_per_hour = 2
  let total_tasks = multiply(total_working_hours, tasks_per_hour)
  
  let tasks_per_sprint = 100
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(240, total_working_hours)  // 30 * 8 = 240
  assert_eq(480, total_tasks)         // 240 * 2 = 480
  assert_eq(5, sprints_needed)        // ceil(480/100) = 5
}

test "string_boundary_conditions" {
  // 字符串边界条件测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, x!", greet("x"))
  assert_eq_string("Hello, XYZ!", greet("XYZ"))
  assert_eq_string("Hello, 1234567890!", greet("1234567890"))
}

test "extreme_number_operations" {
  // 极值运算测试
  let large_num = 1000000
  let small_num = 1
  
  assert_eq(1000001, add(large_num, small_num))
  assert_eq(999999, subtract(large_num, small_num))
  assert_eq(1000000, multiply(large_num, 1))
  assert_eq(1000000, divide_with_ceil(large_num, 1))
  assert_eq(1, divide_with_ceil(large_num, large_num))
}

test "negative_zero_interactions" {
  // 负数与零的交互测试
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(-5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(-5, subtract(-5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "fractional_division_precision" {
  // 分数除法精度测试
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
  assert_eq(34, divide_with_ceil(67, 2))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：计算产品制造的资源需求
  let products_needed = 150
  let components_per_product = 8
  let total_components = multiply(products_needed, components_per_product)
  
  let package_size = 25
  let packages_needed = divide_with_ceil(total_components, package_size)
  
  let cost_per_package = 50
  let total_cost = multiply(packages_needed, cost_per_package)
  
  assert_eq(1200, total_components)    // 150 * 8 = 1200
  assert_eq(48, packages_needed)       // ceil(1200/25) = 48
  assert_eq(2400, total_cost)          // 48 * 50 = 2400
}

test "error_handling_boundary" {
  // 错误处理边界测试
  // 除数为零的情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数除法测试
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(500001, divide_with_ceil(1000001, 2))
}
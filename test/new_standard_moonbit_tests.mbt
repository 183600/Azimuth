// 新增的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能，包括基本运算、边界条件和实际应用场景

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
  assert_eq(-10, subtract(-5, 5))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列
  let a = add(10, 5)        // 15
  let b = multiply(a, 2)    // 30
  let c = subtract(b, 8)    // 22
  let d = divide_with_ceil(c, 5)  // 5
  assert_eq(5, d)
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  let project_days = 15
  let team_members = 6
  let tasks_per_member = 4
  let total_tasks = multiply(team_members, tasks_per_member)  // 24
  let tasks_per_day = divide_with_ceil(total_tasks, project_days)  // 2
  assert_eq(2, tasks_per_day)
}

test "mathematical_properties_extended" {
  // 扩展的数学性质测试
  let x = 12
  let y = 8
  
  // 加法结合律
  assert_eq(add(add(x, y), 3), add(x, add(y, 3)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), 2), multiply(x, multiply(y, 2)))
  
  // 减法性质
  assert_eq(subtract(x, y), add(x, multiply(y, -1)))
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况
  assert_eq_string("Hello, space!", greet("space"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
  assert_eq_string("Hello, special@#$!", greet("special@#$"))
  assert_eq_string("Hello, emoji🎉!", greet("emoji🎉"))
}

test "business_calculation_complex" {
  // 复杂业务计算测试
  // 场景：计算项目成本和资源分配
  let base_cost = 5000
  let team_size = 8
  let project_duration = 12
  let daily_cost = divide_with_ceil(base_cost, project_duration)  // 417
  let total_cost = multiply(daily_cost, project_duration)  // 5004
  let cost_per_member = divide_with_ceil(total_cost, team_size)  // 626
  assert_eq(626, cost_per_member)
}

test "performance_optimization_test" {
  // 性能优化测试
  // 模拟算法优化场景
  let data_size = 1000
  let old_algorithm_steps = multiply(data_size, 10)  // O(n) 算法
  let new_algorithm_steps = divide_with_ceil(data_size, 10)  // O(log n) 算法
  let improvement_ratio = divide_with_ceil(old_algorithm_steps, new_algorithm_steps)
  assert_eq(100, improvement_ratio)  // 100倍改进
}

test "boundary_conditions_extensive" {
  // 广泛的边界条件测试
  // 测试极值情况
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
}

test "real_world_application_comprehensive" {
  // 真实世界应用综合测试
  // 场景：活动组织和资源分配
  let participants = 156
  let tables = 12
  let chairs_per_table = 15
  let tables_needed = divide_with_ceil(participants, chairs_per_table)  // 11
  let total_chairs = multiply(tables_needed, chairs_per_table)  // 165
  let empty_chairs = subtract(total_chairs, participants)  // 9
  let cost_per_chair = 25
  let total_cost = multiply(total_chairs, cost_per_chair)  // 4125
  assert_eq(11, tables_needed)
  assert_eq(165, total_chairs)
  assert_eq(9, empty_chairs)
  assert_eq(4125, total_cost)
}
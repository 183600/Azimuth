// 新增标准 MoonBit 测试用例
// 为 azimuth 项目增加的高质量测试覆盖
test "arithmetic_comprehensive_test" {
  // 综合算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(-3, 3))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-40, multiply(-5, 8))
  assert_eq(35, multiply(-5, -7))
  
  assert_eq(3, divide_with_ceil(25, 8))
  assert_eq(4, divide_with_ceil(25, 6))
  assert_eq(0, divide_with_ceil(25, 0))
}

test "boundary_values_test" {
  // 边界值测试
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(999, 0))
  assert_eq(999, multiply(999, 1))
  
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(1, divide_with_ceil(99, 99))
  assert_eq(0, divide_with_ceil(0, 99))
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  assert_eq_string("Hello, Developer!", greet("Developer"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
  assert_eq_string("Hello, special@#$!", greet("special@#$"))
}

test "business_logic_inventory_test" {
  // 库存管理业务逻辑测试
  let total_items = 127
  let box_capacity = 15
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(9, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(135, total_capacity)
  
  let unused_space = subtract(total_capacity, total_items)
  assert_eq(8, unused_space)
}

test "mathematical_properties_test" {
  // 数学性质验证测试
  let x = 23
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 5)),
    add(multiply(x, y), multiply(x, 5))
  )
  
  // 结合律验证
  assert_eq(
    multiply(multiply(x, y), 2),
    multiply(x, multiply(y, 2))
  )
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(0, multiply(large_num, 0))
  assert_eq(large_num, multiply(large_num, 1))
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源分配
  let team_members = 12
  let tasks_per_person = 8
  let total_tasks = multiply(team_members, tasks_per_person)
  
  let sprint_duration = 3
  let tasks_per_sprint = 35
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(96, total_tasks)
  assert_eq(3, sprints_needed)
  
  let total_project_days = multiply(sprints_needed, sprint_duration)
  assert_eq(9, total_project_days)
}

test "unicode_and_internationalization" {
  // Unicode 和国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
}

test "performance_large_numbers" {
  // 大数性能测试
  let large_a = 50000
  let large_b = 30000
  
  assert_eq(80000, add(large_a, large_b))
  assert_eq(1500000000, multiply(large_a, large_b))
  assert_eq(2, divide_with_ceil(large_a, large_b))
  assert_eq(1, divide_with_ceil(large_b, large_a))
  
  // 极大值测试
  let max_test = 1000000
  assert_eq(2000000, add(max_test, max_test))
  assert_eq(1, divide_with_ceil(max_test, max_test))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let attendees = 245
  let tables_available = 30
  let chairs_per_table = 8
  
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(31, tables_needed)
  
  let total_chairs = multiply(tables_needed, chairs_per_table)
  assert_eq(248, total_chairs)
  
  let extra_chairs = subtract(total_chairs, attendees)
  assert_eq(3, extra_chairs)
  
  let tables_sufficient = tables_needed <= tables_available
  assert_true(tables_sufficient)
}
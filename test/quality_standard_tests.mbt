// 质量 MoonBit 测试用例 - 10个标准测试用例
// 为 Azimuth 项目添加的高质量测试覆盖

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(0, 999))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(16, multiply(-4, -4))
  assert_eq(1000000, multiply(1000, 1000))
}

test "subtract_function_various_scenarios" {
  // 减法函数各种场景测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(5, -10))
  assert_eq(-15, subtract(-5, 10))
}

test "divide_with_ceil_positive_fractional" {
  // 向上取整除法正数分数测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(2, divide_with_ceil(7, 4))
  assert_eq(1, divide_with_ceil(3, 5))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "divide_with_ceil_negative_comprehensive" {
  // 向上取整除法负数综合测试
  assert_eq(-3, divide_with_ceil(-11, 3))
  assert_eq(-4, divide_with_ceil(11, -3))
  assert_eq(4, divide_with_ceil(-11, -3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 13
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体计算结果
  assert_eq(30, add(13, 17))
  assert_eq(221, multiply(13, 17))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  let total_packages = 47
  let truck_capacity = 12
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  assert_eq(4, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(1, remaining_space)
}

test "complex_workflow_resource_planning" {
  // 复杂工作流资源规划测试
  let team_members = 9
  let tasks_per_member = 7
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_day = 8
  let sprint_capacity = multiply(sprint_duration, tasks_per_day)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(63, total_tasks)  // 9 * 7 = 63
  assert_eq(16, sprint_capacity)  // 2 * 8 = 16
  assert_eq(4, sprints_needed)  // ceil(63/16) = 4
}

test "boundary_and_extreme_values" {
  // 边界和极值测试
  // 除法边界测试
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 999))
  
  // 大数运算测试
  assert_eq(20000, add(10000, 10000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊字符串处理测试
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}
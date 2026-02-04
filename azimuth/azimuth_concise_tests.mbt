// Azimuth 简洁测试用例 - 8个标准 MoonBit 测试
// 测试核心功能的各种场景

test "add_function_various_cases" {
  // 加法函数多种情况测试
  azimuth::assert_eq(10, azimuth::add(3, 7))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(-5, azimuth::add(-2, -3))
  azimuth::assert_eq(1, azimuth::add(-3, 4))
  azimuth::assert_eq(1000, azimuth::add(500, 500))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  azimuth::assert_eq(12, azimuth::multiply(3, 4))
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(-8, azimuth::multiply(-2, 4))
  azimuth::assert_eq(8, azimuth::multiply(-2, -4))
  azimuth::assert_eq(10000, azimuth::multiply(100, 100))
}

test "divide_with_ceil_mixed_scenarios" {
  // 向上取整除法混合场景测试
  azimuth::assert_eq(3, azimuth::divide_with_ceil(8, 3))
  azimuth::assert_eq(2, azimuth::divide_with_ceil(6, 3))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 5))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(10, 0))
  azimuth::assert_eq(-2, azimuth::divide_with_ceil(-8, 3))
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(8, -3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(-8, -3))
}

test "greet_function_international" {
  // 问候函数国际化测试
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, user123!", azimuth::greet("user123"))
}

test "subtract_function_various_cases" {
  // 减法函数多种情况测试
  azimuth::assert_eq(5, azimuth::subtract(10, 5))
  azimuth::assert_eq(-5, azimuth::subtract(5, 10))
  azimuth::assert_eq(0, azimuth::subtract(15, 15))
  azimuth::assert_eq(20, azimuth::subtract(10, -10))
  azimuth::assert_eq(-20, azimuth::subtract(-10, 10))
  azimuth::assert_eq(15, azimuth::subtract(-5, -20))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 8
  let b = 6
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 加法恒等式
  azimuth::assert_eq(a, azimuth::add(a, 0))
  azimuth::assert_eq(a, azimuth::add(0, a))
  
  // 乘法恒等式
  azimuth::assert_eq(a, azimuth::multiply(a, 1))
  azimuth::assert_eq(a, azimuth::multiply(1, a))
  azimuth::assert_eq(0, azimuth::multiply(a, 0))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：项目资源计算和分配
  let team_members = 7
  let tasks_per_member = 9
  let total_tasks = azimuth::multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = azimuth::divide_with_ceil(total_tasks, sprint_capacity)
  
  let total_capacity = azimuth::multiply(sprints_needed, sprint_capacity)
  let unused_capacity = azimuth::subtract(total_capacity, total_tasks)
  
  azimuth::assert_eq(63, total_tasks)  // 7 * 9 = 63
  azimuth::assert_eq(4, sprints_needed)  // ceil(63/20) = 4
  azimuth::assert_eq(80, total_capacity)  // 4 * 20 = 80
  azimuth::assert_eq(17, unused_capacity)  // 80 - 63 = 17
}

test "edge_cases_and_error_handling" {
  // 边界情况和错误处理测试
  // 除零情况
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(-5, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))
  
  // 极值测试
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(-1, -1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 1))
  
  // 大数测试
  azimuth::assert_eq(100000, azimuth::add(50000, 50000))
  azimuth::assert_eq(100000000, azimuth::multiply(10000, 10000))
  
  // 特殊字符串测试
  azimuth::assert_eq_string("Hello,   !", azimuth::greet("  "))
  azimuth::assert_eq_string("Hello, special@#$!", azimuth::greet("special@#$"))
}
// 标准 MoonBit 测试用例集
// 为 azimuth 库添加的高质量测试用例

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
  assert_eq(0, multiply(0, 100))
  assert_eq(-30, multiply(-6, 5))
  assert_eq(30, multiply(-6, -5))
  assert_eq(1, multiply(1, 1000000))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(4, divide_with_ceil(13, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 50))
  assert_eq(0, divide_with_ceil(25, 0))
}

test "divide_with_ceil_negative_handling" {
  // 向上取整除法负数处理测试
  assert_eq(-3, divide_with_ceil(-13, 4))
  assert_eq(-4, divide_with_ceil(13, -4))
  assert_eq(3, divide_with_ceil(-13, -4))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, Café!", greet("Café"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(8, 12))
  assert_eq(96, multiply(8, 12))
}

test "logistics_packaging_scenario" {
  // 物流包装场景测试
  let total_items = 58
  let package_capacity = 15
  let packages_needed = divide_with_ceil(total_items, package_capacity)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, package_capacity)
  assert_eq(60, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(2, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目管理资源计算
  let team_members = 7
  let tasks_per_member = 9
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(63, total_tasks)  // 7 * 9 = 63
  assert_eq(4, sprints_needed)  // ceil(63/20) = 4
}

test "boundary_and_extreme_values" {
  // 边界和极值测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(10, 10))
  assert_eq(1, divide_with_ceil(-10, -10))
  assert_eq(0, divide_with_ceil(0, 100))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊字符串处理
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}
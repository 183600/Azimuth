// Azimuth 核心标准测试用例
// 包含 10 个高质量的 MoonBit 测试用例，覆盖核心功能

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(8, azimuth.add(3, 5))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(-2, azimuth.add(-5, 3))
  assert_eq(100, azimuth.add(60, 40))
  assert_eq(-100, azimuth.add(-60, -40))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(15, azimuth.multiply(3, 5))
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(-15, azimuth.multiply(-3, 5))
  assert_eq(15, azimuth.multiply(-3, -5))
  assert_eq(10000, azimuth.multiply(100, 100))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(3, azimuth.divide_with_ceil(8, 3))
  assert_eq(2, azimuth.divide_with_ceil(6, 3))
  assert_eq(1, azimuth.divide_with_ceil(2, 5))
  assert_eq(0, azimuth.divide_with_ceil(0, 10))
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-2, azimuth.divide_with_ceil(-8, 3))
  assert_eq(-3, azimuth.divide_with_ceil(8, -3))
  assert_eq(3, azimuth.divide_with_ceil(-8, -3))
  assert_eq(-1, azimuth.divide_with_ceil(-1, 2))
  assert_eq(1, azimuth.divide_with_ceil(-1, -2))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, Azimuth!", azimuth.greet("Azimuth"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 2024!", azimuth.greet("2024"))
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(3, azimuth.subtract(8, 5))
  assert_eq(-3, azimuth.subtract(5, 8))
  assert_eq(0, azimuth.subtract(10, 10))
  assert_eq(15, azimuth.subtract(5, -10))
  assert_eq(-15, azimuth.subtract(-5, 10))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 7
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 验证具体数值
  assert_eq(19, azimuth.add(12, 7))
  assert_eq(84, azimuth.multiply(12, 7))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = azimuth.divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = azimuth.multiply(packages_needed, items_per_package)
  assert_eq(40, total_capacity)
  
  // 验证剩余空间
  let remaining_space = azimuth.subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 除法边界情况
  assert_eq(1, azimuth.divide_with_ceil(5, 5))
  assert_eq(1, azimuth.divide_with_ceil(-5, -5))
  assert_eq(0, azimuth.divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000000, azimuth.add(1000000, 1000000))
  assert_eq(100000000, azimuth.multiply(10000, 10000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", azimuth.greet("  "))
  assert_eq_string("Hello, special@chars!", azimuth.greet("special@chars"))
}
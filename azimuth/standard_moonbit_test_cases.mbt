// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含 8 个高质量测试用例，覆盖各种场景和边界情况

test "arithmetics_addition_comprehensive" {
  // 综合加法测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "arithmetics_multiplication_edge_cases" {
  // 乘法边界情况测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "divide_with_ceil_boundary_conditions" {
  // 向上取整除法边界条件测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  @azimuth.assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(1, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源分配
  let team_members = 6
  let tasks_per_member = 7
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(42, total_tasks)  // 6 * 7 = 42
  @azimuth.assert_eq(3, sprints_needed)  // ceil(42/20) = 3
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试极值运算
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
}
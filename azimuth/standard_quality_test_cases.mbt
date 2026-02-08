// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_addition_comprehensive" {
  // 综合加法测试 - 正数、负数、零值
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(-8, add(-5, -3))
  assert_eq(1000, add(500, 500))
}

test "basic_arithmetic_multiplication_edge_cases" {
  // 乘法边界情况测试
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法综合场景测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_number_handling" {
  // 向上取整除法负数处理测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_internationalization_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_commutative_law" {
  // 数学交换律性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}

test "business_packaging_calculation_scenario" {
  // 业务包装计算场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_workflow_resource_calculation" {
  // 复杂工作流资源计算测试
  // 场景：项目资源规划
  let team_members = 7
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(42, total_tasks)  // 7 * 6 = 42
  assert_eq(3, sprints_needed)  // ceil(42/20) = 3
}
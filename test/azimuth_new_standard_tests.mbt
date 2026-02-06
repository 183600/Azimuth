// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "basic_addition_comprehensive" {
  // 基本加法综合测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(-8, 13))
  assert_eq(1000, add(500, 500))
}

test "multiply_various_scenarios" {
  // 乘法各种场景测试
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(10, 0))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_international" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(4, subtract(10, 6))
  assert_eq(-4, subtract(6, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
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

test "business_packaging_calculation" {
  // 业务包装计算测试
  let total_items = 45
  let items_per_package = 10
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(50, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 5
  let tasks_per_member = 7
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 12
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(35, total_tasks)  // 5 * 7 = 35
  assert_eq(3, sprints_needed)  // ceil(35/12) = 3
}
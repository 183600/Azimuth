// Azimuth 新标准测试用例
// 包含8个高质量测试用例，覆盖核心功能

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_properties" {
  // 乘法函数性质测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(20, multiply(-4, -5))
  assert_eq(1, multiply(1, 100))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-2, divide_with_ceil(-8, 3))
}

test "subtract_function_validation" {
  // 减法函数验证测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "greet_function_international" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证测试
  let a = 8
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体值
  assert_eq(21, add(8, 13))
  assert_eq(104, multiply(8, 13))
}

test "business_packaging_logic" {
  // 业务包装逻辑测试
  let total_items = 47
  let items_per_package = 10
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(50, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(3, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源分配
  let team_members = 7
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(42, total_tasks)  // 7 * 6 = 42
  assert_eq(3, sprints_needed)  // ceil(42/20) = 3
  
  // 验证最后冲刺的任务数
  let last_sprint_tasks = subtract(total_tasks, multiply(2, sprint_capacity))
  assert_eq(2, last_sprint_tasks)  // 42 - 40 = 2
}
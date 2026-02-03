// Azimuth 库的标准 MoonBit 测试用例
// 测试核心功能：add, multiply, subtract, greet, divide_with_ceil

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "divide_with_ceil_positive_numbers" {
  // 测试向上取整除法的正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "subtract_function_basic_cases" {
  // 测试减法函数的基本情况
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_scenario" {
  // 测试业务包装场景
  let total_items = 25
  let items_per_package = 6
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "complex_workflow_calculation" {
  // 测试复杂工作流计算
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和极值
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}
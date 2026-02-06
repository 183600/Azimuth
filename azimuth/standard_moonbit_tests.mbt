// 标准 MoonBit 测试用例 - Azimuth 库
// 包含 8 个高质量测试用例，测试基本数学运算和字符串处理功能

test "basic_addition_test" {
  assert_eq(7, add(3, 4))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "basic_multiplication_test" {
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
}

test "basic_subtraction_test" {
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_basic_test" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_test" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_commutative_test" {
  let a = 8
  let b = 12
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "packaging_scenario_test" {
  let total_items = 25
  let items_per_package = 6
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  assert_eq(5, packages_needed)
  
  let total_capacity = multiply(packages_needed, items_per_package)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "complex_workflow_test" {
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)
  assert_eq(4, sprints_needed)
}
// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含10个高质量测试用例，覆盖核心功能和边界情况

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "zero_element_properties" {
  // 零元素性质测试
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(2, subtract(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "mixed_sign_operations" {
  // 混合符号运算测试
  assert_eq(2, add(-3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-8, subtract(3, -5))
  assert_eq(-4, divide_with_ceil(10, -3))
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(20000, add(10000, 10000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(0, subtract(10000, 10000))
  assert_eq(1000, divide_with_ceil(1000000, 1000))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 25
  let items_per_package = 6
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  assert_eq(5, packages_needed)
  
  let total_capacity = multiply(packages_needed, items_per_package)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  let team_members = 4
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  assert_eq(32, total_tasks)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(3, sprints_needed)
  
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  let unused_capacity = subtract(total_capacity, total_tasks)
  assert_eq(13, unused_capacity)
}
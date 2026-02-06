// Azimuth 标准测试套件
// 包含8个高质量的 MoonBit 测试用例，覆盖所有核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(7, add(3, 4))
  assert_eq(12, multiply(3, 4))
  assert_eq(-1, subtract(3, 4))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
}

test "negative_numbers_handling" {
  // 负数处理测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-8, subtract(3, 11))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(1, divide_with_ceil(-3, -4))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
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

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值测试
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "boolean_assertions_validation" {
  // 布尔断言验证测试
  assert_true(true)
  assert_false(false)
  assert_true(5 > 3)
  assert_false(5 < 3)
  assert_true(add(2, 3) == 5)
  assert_false(multiply(2, 3) == 7)
}
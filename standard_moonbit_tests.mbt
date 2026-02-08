// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "subtract_function_basic" {
  // 测试基本的减法功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
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
  // 复杂工作流计算测试
  let team_members = 4
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(32, total_tasks)  // 4 * 8 = 32
  assert_eq(3, sprints_needed)  // ceil(32/15) = 3
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(5, subtract(-2, -7))
}
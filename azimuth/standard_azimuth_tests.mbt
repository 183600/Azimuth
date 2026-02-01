// 标准 Azimuth 测试用例
// 包含10个高质量的 MoonBit 测试用例

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-3, 4))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
}

test "basic_arithmetic_subtraction" {
  // 基本减法运算测试
  assert_eq(-4, subtract(3, 7))
  assert_eq(5, subtract(5, 0))
  assert_eq(-8, subtract(-3, 5))
  assert_eq(2, subtract(-3, -5))
}

test "divide_with_ceil_basic_functionality" {
  // 向上取整除法基本功能测试
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(1, divide_with_ceil(1, 4))
  assert_eq(0, divide_with_ceil(0, 4))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-2, divide_with_ceil(7, -4))
  assert_eq(2, divide_with_ceil(-7, -4))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "greet_function_standard_cases" {
  // 问候函数标准测试用例
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 6
  let b = 9
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_calculation" {
  // 业务包装计算场景测试
  let total_items = 23
  let items_per_package = 5
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(25, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(2, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源分配
  let team_members = 4
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 10
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(24, total_tasks)  // 4 * 6 = 24
  assert_eq(3, sprints_needed)  // ceil(24/10) = 3
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 特殊字符串
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}
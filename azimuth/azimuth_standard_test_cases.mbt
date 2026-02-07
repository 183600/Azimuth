// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含10个高质量测试用例，覆盖核心功能

test "string_greeting_functionality" {
  // 测试字符串问候功能的各种场景
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "arithmetics_addition_comprehensive" {
  // 加法运算的综合测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-4, add(-2, -2))
  assert_eq(1, add(-5, 6))
  assert_eq(1000, add(999, 1))
}

test "arithmetics_multiplication_properties" {
  // 乘法运算的性质测试
  assert_eq(0, multiply(5, 0))
  assert_eq(25, multiply(5, 5))
  assert_eq(-30, multiply(-6, 5))
  assert_eq(30, multiply(-6, -5))
  assert_eq(100, multiply(10, 10))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(1, divide_with_ceil(1, 4))
  assert_eq(0, divide_with_ceil(0, 4))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-3, divide_with_ceil(7, -3))
  assert_eq(3, divide_with_ceil(-7, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "subtract_function_basic" {
  // 减法函数基本测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 12
  let b = 8
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_calculation" {
  // 业务包装计算测试
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
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(3, sprints_needed)  // ceil(48/20) = 3
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000, add(1000, 1000))
  assert_eq(10000, multiply(100, 100))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}
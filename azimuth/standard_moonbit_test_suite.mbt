// 标准 MoonBit 测试套件 - 为 Azimuth 项目添加
// 包含10个高质量的标准测试用例

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(10, add(6, 4))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(100, add(50, 50))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(-5, -5))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "ceil_division_positive_numbers" {
  // 正数向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "ceil_division_negative_handling" {
  // 负数向上取整除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative_property" {
  // 数学交换律性质测试
  let a = 15
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(40, total_capacity)
}

test "subtraction_functionality" {
  // 减法功能测试
  assert_eq(6, subtract(10, 4))
  assert_eq(-6, subtract(4, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源需求计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let days_per_sprint = 5
  let tasks_per_day = 10
  let sprint_capacity = multiply(days_per_sprint, tasks_per_day)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)
  assert_eq(50, sprint_capacity)
  assert_eq(1, sprints_needed)
}

test "edge_cases_and_boundaries" {
  // 边界情况和极值测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
}
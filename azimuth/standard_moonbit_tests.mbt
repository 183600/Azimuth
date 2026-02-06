// 标准 MoonBit 测试用例
// 为 Azimuth 项目添加的10个高质量测试用例

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(10, add(6, 4))
  assert_eq(24, multiply(6, 4))
  assert_eq(2, divide_with_ceil(10, 6))
  assert_eq(4, subtract(10, 6))
  
  // 边界情况
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "mathematical_commutative_and_associative" {
  // 数学交换律和结合律测试
  let a = 7
  let b = 13
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "negative_numbers_operations" {
  // 负数运算测试
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(-2, subtract(-5, -3))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
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

test "large_numbers_operations" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  assert_eq(0, divide_with_ceil(1000000, 0))
  assert_eq(1000000, subtract(2000000, 1000000))
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  let days_per_sprint = 5
  let total_days = multiply(sprints_needed, days_per_sprint)
  
  assert_eq(48, total_tasks)
  assert_eq(3, sprints_needed)
  assert_eq(15, total_days)
}

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "edge_cases_and_special_values" {
  // 边界情况和特殊值测试
  // 最小值测试
  assert_eq(1, add(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 零值测试
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 1000000))
  assert_eq(0, divide_with_ceil(0, 1000000))
  
  // 特殊字符串测试
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
  assert_eq_string("Hello, line\nbreak!", greet("line\nbreak"))
}
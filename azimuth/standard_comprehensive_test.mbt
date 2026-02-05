// 标准 MoonBit 测试用例 - Azimuth 库综合测试
// 包含 10 个高质量测试用例，覆盖核心功能

test "basic_addition_functionality" {
  // 基本加法功能测试
  add(2, 3) = 5
  add(0, 0) = 0
  add(-3, 2) = -1
  add(50, 50) = 100
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  multiply(2, 3) = 6
  multiply(5, 0) = 0
  multiply(-2, 3) = -6
  multiply(-3, -5) = 15
}

test "divide_with_ceil_positive_numbers" {
  // 向上取整除法正数测试
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(9, 3) = 3
  divide_with_ceil(1, 5) = 1
  divide_with_ceil(5, 0) = 0
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  divide_with_ceil(-10, 3) = -3
  divide_with_ceil(10, -3) = -4
  divide_with_ceil(-10, -3) = 4
  divide_with_ceil(-1, 2) = -1
}

test "greet_function_standard_cases" {
  // 问候函数标准测试
  greet("World") = "Hello, World!"
  greet("MoonBit") = "Hello, MoonBit!"
  greet("") = "Hello, !"
  greet("123") = "Hello, 123!"
}

test "subtract_function_basic_cases" {
  // 减法函数基本测试
  subtract(5, 3) = 2
  subtract(3, 5) = -2
  subtract(5, 5) = 0
  subtract(0, -10) = 10
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 7
  let b = 13
  
  // 加法交换律
  add(a, b) = add(b, a)
  
  // 乘法交换律
  multiply(a, b) = multiply(b, a)
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 25
  let items_per_package = 6
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  packages_needed = 5
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  let remaining_space = subtract(total_capacity, total_items)
  remaining_space = 5
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  total_tasks = 48  // 6 * 8 = 48
  sprints_needed = 4  // ceil(48/15) = 4
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 除法边界情况
  divide_with_ceil(5, 5) = 1
  divide_with_ceil(-5, -5) = 1
  divide_with_ceil(0, 5) = 0
  
  // 大数运算
  add(1000000, 1000000) = 2000000
  multiply(10000, 10000) = 100000000
  
  // 特殊问候情况
  greet("  ") = "Hello,   !"
  greet("special@chars") = "Hello, special@chars!"
}
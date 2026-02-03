// Azimuth 项目额外的标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-20, multiply(-4, 5))
  assert_eq(20, multiply(-4, -5))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_scenarios" {
  // 向上取整除法负数场景测试
  assert_eq(-3, divide_with_ceil(-11, 4))
  assert_eq(-4, divide_with_ceil(11, -4))
  assert_eq(3, divide_with_ceil(-11, -4))
  assert_eq(-1, divide_with_ceil(-3, 5))
  assert_eq(1, divide_with_ceil(-3, -3))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(6, subtract(10, 4))
  assert_eq(-6, subtract(4, 10))
  assert_eq(0, subtract(8, 8))
  assert_eq(15, subtract(5, -10))
  assert_eq(-15, subtract(-5, 10))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 15
  let y = 7
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证具体数值
  assert_eq(22, add(15, 7))
  assert_eq(105, multiply(15, 7))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 43
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(6, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 7
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(42, total_tasks)  // 7 * 6 = 42
  assert_eq(3, sprints_needed)  // ceil(42/20) = 3
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(8, 8))
  assert_eq(1, divide_with_ceil(-8, -8))
  assert_eq(0, divide_with_ceil(0, 8))
  
  // 大数运算
  assert_eq(200000, add(100000, 100000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}
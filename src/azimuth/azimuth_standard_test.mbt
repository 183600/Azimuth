// Azimuth 库的标准 MoonBit 测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "azimuth_add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(5, add(-5, 10))
  assert_eq(-10, add(-3, -7))
}

test "azimuth_multiply_comprehensive" {
  // 测试乘法功能
  assert_eq(20, multiply(4, 5))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(24, multiply(-4, -6))
  assert_eq(0, multiply(8, 0))
}

test "azimuth_subtract_edge_cases" {
  // 测试减法边界情况
  assert_eq(7, subtract(10, 3))
  assert_eq(-7, subtract(5, 12))
  assert_eq(0, subtract(0, 0))
  assert_eq(5, subtract(-5, -10))
}

test "azimuth_divide_with_ceil_positive" {
  // 测试向上取整除法（正数）
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 7))
  assert_eq(0, divide_with_ceil(15, 0))
}

test "azimuth_divide_with_ceil_negative" {
  // 测试向上取整除法（负数）
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "azimuth_greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "azimuth_mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "azimuth_business_packaging_scenario" {
  // 测试业务包装场景
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(packages_needed, 5)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(total_capacity, 40)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(remaining_space, 5)
}

test "azimuth_complex_workflow_calculation" {
  // 测试复杂工作流计算
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(total_tasks, 48)  // 6 * 8 = 48
  assert_eq(sprints_needed, 4)  // ceil(48/15) = 4
}

test "azimuth_boundary_and_special_cases" {
  // 测试边界和特殊情况
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}
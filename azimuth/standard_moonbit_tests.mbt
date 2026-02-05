// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "azimuth_unique_add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  assert_eq(1000, add(500, 500))
}

test "azimuth_unique_multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-20, multiply(-4, 5))
  assert_eq(20, multiply(-4, -5))
  assert_eq(10000, multiply(100, 100))
}

test "azimuth_unique_divide_with_ceil_various_scenarios" {
  // 向上取整除法各种场景测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "azimuth_unique_divide_with_ceil_negative_values" {
  // 向上取整除法负值测试
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "azimuth_unique_greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "azimuth_unique_subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "azimuth_unique_mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}

test "azimuth_unique_business_packaging_calculation" {
  // 业务包装计算测试
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

test "azimuth_unique_complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源分配
  let team_members = 8
  let tasks_per_member = 7
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(56, total_tasks)  // 8 * 7 = 56
  assert_eq(4, sprints_needed)  // ceil(56/15) = 4
}

test "azimuth_unique_boundary_and_precision_tests" {
  // 边界和精度测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊字符串处理
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}
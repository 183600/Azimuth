// 标准 MoonBit 测试用例 - 为 Azimuth 项目新增
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "azimuth_core_addition_comprehensive" {
  // 核心加法功能综合测试
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-8, 13))
  assert_eq(100000, add(50000, 50000))
  assert_eq(1, add(2147483647, -2147483646))
}

test "azimuth_core_multiplication_comprehensive" {
  // 核心乘法功能综合测试
  assert_eq(42, multiply(6, 7))
  assert_eq(0, multiply(15, 0))
  assert_eq(-24, multiply(-3, 8))
  assert_eq(24, multiply(-3, -8))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-2147483647, multiply(-2147483647, 1))
}

test "azimuth_core_divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1001, divide_with_ceil(1000000, 999))
}

test "azimuth_core_divide_with_ceil_negative_cases" {
  // 向上取整除法负数情况测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
  assert_eq(-1000, divide_with_ceil(-1000000, 1000))
}

test "azimuth_core_greet_function_international" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "azimuth_core_mathematical_properties" {
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
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "azimuth_business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(40, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "azimuth_complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
  
  // 验证总工作量分配
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  let unused_capacity = subtract(total_capacity, total_tasks)
  assert_eq(12, unused_capacity)  // 4*15 - 48 = 12
}

test "azimuth_financial_calculation_scenario" {
  // 金融计算场景测试
  let principal = 10000
  let interest_rate = 5
  let years = 3
  
  // 简化的复利计算（每年计算一次）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate), 100))
  
  assert_eq(10500, year1_amount)  // 10000 + ceil(10000*5/100) = 10000 + 500 = 10500
  assert_eq(11025, year2_amount)  // 10500 + ceil(10500*5/100) = 10500 + 525 = 11025
  assert_eq(11576, year3_amount)  // 11025 + ceil(11025*5/100) = 11025 + 551 = 11576
}

test "azimuth_edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
  assert_eq_string("Hello, 新年2024!", greet("新年2024"))
}
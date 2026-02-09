// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

// 导入必要的包
test {
  // 导入 azimuth 包中的函数
  // 导入测试断言函数
} {}

test "standard_arithmetic_addition_comprehensive" {
  // 综合加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  assert_eq(1000, add(500, 500))
}

test "standard_arithmetic_multiplication_edge_cases" {
  // 乘法边界情况测试
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(100, 0))
  assert_eq(-30, multiply(-6, 5))
  assert_eq(30, multiply(-6, -5))
  assert_eq(10000, multiply(100, 100))
}

test "standard_divide_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(1, divide_with_ceil(1, 10))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(8, 0))
}

test "standard_divide_ceil_negative_scenarios" {
  // 向上取整除法负数场景测试
  assert_eq(-2, divide_with_ceil(-8, 4))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "standard_greet_function_international" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "standard_subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "standard_mathematical_properties" {
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

test "standard_business_packaging_scenario" {
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

test "standard_complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "standard_boundary_and_extreme_values" {
  // 边界值和极值测试
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
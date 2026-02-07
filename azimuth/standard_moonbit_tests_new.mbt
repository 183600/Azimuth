// 标准 MoonBit 测试用例 - 10个高质量测试
// 测试 azimuth 库的核心功能

test "add_function_comprehensive" {
  // 加法函数综合测试
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(10000, @azimuth.multiply(100, 100))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(8, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-8, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -2))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "subtract_function_basic" {
  // 减法函数基本测试
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(15, 15))
  @azimuth.assert_eq(20, @azimuth.subtract(10, -10))
  @azimuth.assert_eq(-20, @azimuth.subtract(-10, 10))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 12
  let b = 7
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体数值
  @azimuth.assert_eq(19, @azimuth.add(12, 7))
  @azimuth.assert_eq(84, @azimuth.multiply(12, 7))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  @azimuth.assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  @azimuth.assert_eq(40, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(5, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：计算项目资源需求
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let days_per_sprint = 5
  let tasks_per_day = 10
  let sprint_capacity = @azimuth.multiply(days_per_sprint, tasks_per_day)
  
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)  // 6 * 8 = 48
  @azimuth.assert_eq(50, sprint_capacity)  // 5 * 10 = 50
  @azimuth.assert_eq(1, sprints_needed)  // ceil(48/50) = 1
}

test "boundary_and_special_cases" {
  // 边界和特殊情况测试
  // 除法边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-5, -5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  
  // 特殊问候情况
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, special@chars!", @azimuth.greet("special@chars"))
}
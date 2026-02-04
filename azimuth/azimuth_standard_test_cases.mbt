// Azimuth 标准测试用例 - 10个高质量 MoonBit 测试
// 覆盖核心功能和边界情况

test "add_function_comprehensive" {
  // 加法函数综合测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1, @azimuth.add(-5, 6))
  @azimuth.assert_eq(100000, @azimuth.add(50000, 50000))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
  @azimuth.assert_eq(30, @azimuth.multiply(-5, -6))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1000000))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_scenarios" {
  // 向上取整除法负数场景测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -2))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, éspanol!", @azimuth.greet("éspanol"))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体数值
  @azimuth.assert_eq(20, @azimuth.add(12, 8))
  @azimuth.assert_eq(96, @azimuth.multiply(12, 8))
}

test "business_logistics_calculation" {
  // 物流业务计算测试
  let total_items = 58
  let container_capacity = 15
  let containers_needed = @azimuth.divide_with_ceil(total_items, container_capacity)
  
  @azimuth.assert_eq(4, containers_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(containers_needed, container_capacity)
  @azimuth.assert_eq(60, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(2, remaining_space)
}

test "complex_workflow_operations" {
  // 复杂工作流操作测试
  // 场景：计算项目资源需求
  let team_members = 7
  let tasks_per_member = 9
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(63, total_tasks)  // 7 * 9 = 63
  @azimuth.assert_eq(4, sprints_needed)  // ceil(63/20) = 4
}

test "boundary_value_analysis" {
  // 边界值分析测试
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

test "mathematical_distributive_property" {
  // 数学分配律验证测试
  let a = 5
  let b = 3
  let c = 2
  
  // 分配律: a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)  // 5 * (3 + 2) = 5 * 3 + 5 * 2 = 25
}
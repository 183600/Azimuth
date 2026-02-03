// 标准 MoonBit 综合测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "addition_various_scenarios" {
  // 加法多种场景测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "multiplication_comprehensive" {
  // 乘法综合测试
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(10, 0))
  @azimuth.assert_eq(-24, @azimuth.multiply(-8, 3))
  @azimuth.assert_eq(36, @azimuth.multiply(-6, -6))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(20, 4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(15, 0))
}

test "divide_with_ceil_negative_dividend" {
  // 向上取整除法负被除数测试
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 4))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 5))
}

test "greet_function_international" {
  // 问候函数国际化测试
  @azimuth.assert_eq_string("Hello, Universe!", @azimuth.greet("Universe"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "subtract_function_various_cases" {
  // 减法函数多种情况测试
  @azimuth.assert_eq(7, @azimuth.subtract(10, 3))
  @azimuth.assert_eq(-7, @azimuth.subtract(3, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(15, 15))
  @azimuth.assert_eq(20, @azimuth.subtract(5, -15))
}

test "mathematical_distributive_property" {
  // 数学分配律测试
  let a = 5
  let b = 3
  let c = 2
  
  // a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_resources = 50
  let team_members = 8
  let resources_per_member = @azimuth.divide_with_ceil(total_resources, team_members)
  
  @azimuth.assert_eq(7, resources_per_member)
  
  // 验证总分配资源
  let total_allocated = @azimuth.multiply(resources_per_member, team_members)
  @azimuth.assert_eq(56, total_allocated)
  
  // 验证剩余资源
  let remaining = @azimuth.subtract(total_allocated, total_resources)
  @azimuth.assert_eq(6, remaining)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目管理计算
  let team_size = 5
  let tasks_per_person = 7
  let total_tasks = @azimuth.multiply(team_size, tasks_per_person)
  
  let sprint_duration = 10
  let tasks_per_sprint = @azimuth.divide_with_ceil(total_tasks, sprint_duration)
  
  @azimuth.assert_eq(35, total_tasks)  // 5 * 7 = 35
  @azimuth.assert_eq(4, tasks_per_sprint)  // ceil(35/10) = 4
}

test "boundary_and_precision_tests" {
  // 边界和精度测试
  // 极值测试
  @azimuth.assert_eq(200000, @azimuth.add(100000, 100000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 边界除法测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(100, 1))
  
  // 字符串边界测试
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, single!", @azimuth.greet("single"))
}
// 标准 MoonBit 测试套件
// 为 Azimuth 项目添加 8 个高质量测试用例
// 使用标准 MoonBit 测试语法

test "basic_arithmetic_comprehensive" {
  // 综合基本算术运算测试
  @azimuth.assert_eq(10, @azimuth.add(6, 4))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-20, @azimuth.multiply(-5, 4))
  @azimuth.assert_eq(20, @azimuth.multiply(-5, -4))
  
  @azimuth.assert_eq(2, @azimuth.subtract(6, 4))
  @azimuth.assert_eq(-9, @azimuth.subtract(-5, 4))
  @azimuth.assert_eq(-10, @azimuth.subtract(5, 15))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_international" {
  // 问候函数国际化测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, São Paulo!", @azimuth.greet("São Paulo"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法恒等式
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
  
  // 乘法恒等式
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(a, @azimuth.multiply(1, a))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  @azimuth.assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(1, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源管理
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_day = 5
  let sprint_capacity = @azimuth.multiply(sprint_duration, tasks_per_day)
  
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)  // 8 * 6 = 48
  @azimuth.assert_eq(10, sprint_capacity)  // 2 * 5 = 10
  @azimuth.assert_eq(5, sprints_needed)  // ceil(48/10) = 5
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 极值测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  @azimuth.assert_eq(1000001, @azimuth.divide_with_ceil(1000000, 1))
  
  // 边界除法测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -100))
  
  // 特殊字符串测试
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, special@chars!", @azimuth.greet("special@chars"))
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 大数运算健壮性
  let large_a = 500000
  let large_b = 500000
  @azimuth.assert_eq(1000000, @azimuth.add(large_a, large_b))
  @azimuth.assert_eq(250000000000, @azimuth.multiply(large_a, large_b))
  
  // 连续运算健壮性
  let result1 = @azimuth.add(@azimuth.multiply(3, 4), @azimuth.divide_with_ceil(10, 3))
  let result2 = @azimuth.multiply(@azimuth.add(5, 2), @azimuth.subtract(10, 3))
  @azimuth.assert_eq(16, result1)  // 3*4 + ceil(10/3) = 12 + 4 = 16
  @azimuth.assert_eq(49, result2)  // (5+2) * (10-3) = 7 * 7 = 49
}
// Azimuth 项目新增标准 MoonBit 测试套件
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  
  @azimuth.assert_eq(2, @azimuth.subtract(7, 5))
  @azimuth.assert_eq(-2, @azimuth.subtract(5, 7))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
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
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)  // 6 * 8 = 48
  @azimuth.assert_eq(4, sprints_needed)  // ceil(48/15) = 4
  
  // 验证总处理能力
  let total_capacity = @azimuth.multiply(sprints_needed, sprint_capacity)
  @azimuth.assert_eq(60, total_capacity)  // 4 * 15 = 60
  
  // 验证剩余处理能力
  let remaining_capacity = @azimuth.subtract(total_capacity, total_tasks)
  @azimuth.assert_eq(12, remaining_capacity)  // 60 - 48 = 12
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 除法边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  
  // 特殊问候情况
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, special@chars!", @azimuth.greet("special@chars"))
  @azimuth.assert_eq_string("Hello, newline\n!", @azimuth.greet("newline\n"))
}

test "error_handling_and_resilience" {
  // 错误处理和恢复能力测试
  // 除零错误的全面测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(9999, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-9999, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值运算测试
  let max_value = 1000000
  let min_value = -1000000
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_value, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_value, 0))
  @azimuth.assert_eq(max_value, @azimuth.add(max_value, 0))
  @azimuth.assert_eq(min_value, @azimuth.add(min_value, 0))
  
  // 字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, x!", @azimuth.greet("x"))
  @azimuth.assert_eq_string("Hello, verylongname!", @azimuth.greet("verylongname"))
}
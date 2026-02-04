// 标准 Azimuth 核心测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "basic_addition_functionality" {
  // 基本加法功能测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-3, 2))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(10000, @azimuth.multiply(100, 100))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -2))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "subtract_function_basic" {
  // 减法函数基本测试
  @azimuth.assert_eq(2, @azimuth.subtract(5, 3))
  @azimuth.assert_eq(-2, @azimuth.subtract(3, 5))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(0, -10))
  @azimuth.assert_eq(-10, @azimuth.subtract(-10, 0))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 7
  let b = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体数值
  @azimuth.assert_eq(20, @azimuth.add(7, 13))
  @azimuth.assert_eq(91, @azimuth.multiply(7, 13))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 25
  let items_per_package = 6
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  @azimuth.assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(5, remaining_space)
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
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
  
  // 验证总工作量分配
  let total_capacity = @azimuth.multiply(sprints_needed, sprint_capacity)
  let extra_capacity = @azimuth.subtract(total_capacity, total_tasks)
  @azimuth.assert_eq(12, extra_capacity)  // 4*15 - 48 = 12
}
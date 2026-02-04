// 标准 MoonBit 质量测试用例 - 8个高质量测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(8, @azimuth.add(-3, 11))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(8, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "subtract_function_basic" {
  // 基本减法功能测试
  @azimuth.assert_eq(2, @azimuth.subtract(6, 4))
  @azimuth.assert_eq(-2, @azimuth.subtract(4, 6))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(5, -5))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 8
  let b = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法恒等式
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  
  // 乘法恒等式
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
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

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源分配
  let team_members = 6
  let tasks_per_member = 7
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(42, total_tasks)  // 6 * 7 = 42
  @azimuth.assert_eq(3, sprints_needed)  // ceil(42/20) = 3
  
  // 验证最后冲刺的剩余容量
  let total_capacity = @azimuth.multiply(sprints_needed, sprint_capacity)
  let remaining_capacity = @azimuth.subtract(total_capacity, total_tasks)
  @azimuth.assert_eq(18, remaining_capacity)  // 3*20 - 42 = 18
}
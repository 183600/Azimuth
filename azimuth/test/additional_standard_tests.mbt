// Azimuth 项目额外的标准 MoonBit 测试用例
// 包含10个高质量测试用例，覆盖核心功能

test "additional_add_function_test" {
  // 加法函数综合测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "additional_multiply_function_test" {
  // 乘法函数边界情况测试
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(-30, @azimuth.multiply(-6, 5))
  @azimuth.assert_eq(36, @azimuth.multiply(-6, -6))
}

test "additional_divide_with_ceil_test" {
  // 向上取整除法精度测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 2))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
}

test "additional_divide_negative_test" {
  // 向上取整除法负数处理测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(8, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-8, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "additional_greet_function_test" {
  // 问候函数国际化测试
  @azimuth.assert_eq_string("Hello, Azimuth!", @azimuth.greet("Azimuth"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 2024!", @azimuth.greet("2024"))
}

test "additional_subtract_function_test" {
  // 减法函数验证测试
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(8, 8))
  @azimuth.assert_eq(15, @azimuth.subtract(5, -10))
}

test "additional_math_properties_test" {
  // 数学交换律性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "additional_packaging_workflow_test" {
  // 业务包装工作流测试
  let total_items = 45
  let package_capacity = 10
  let packages_needed = @azimuth.divide_with_ceil(total_items, package_capacity)
  
  @azimuth.assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, package_capacity)
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(5, remaining_space)
}

test "additional_calculation_sequence_test" {
  // 复杂计算序列测试
  // 场景：项目资源规划
  let team_size = 6
  let tasks_per_member = 7
  let total_tasks = @azimuth.multiply(team_size, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(42, total_tasks)  // 6 * 7 = 42
  @azimuth.assert_eq(3, sprints_needed)  // ceil(42/20) = 3
}

test "additional_boundary_values_test" {
  // 边界和极值测试
  // 除法边界
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  
  // 大数运算
  @azimuth.assert_eq(20000, @azimuth.add(10000, 10000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 特殊问候
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}
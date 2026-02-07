// 标准 MoonBit 测试用例 - 为 Azimuth 项目新增
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "azimuth_standard_add_function" {
  // 加法函数标准测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-5, 2))
  @azimuth.assert_eq(100, @azimuth.add(45, 55))
}

test "azimuth_standard_multiply_function" {
  // 乘法函数标准测试
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(8, 0))
  @azimuth.assert_eq(-24, @azimuth.multiply(-3, 8))
  @azimuth.assert_eq(36, @azimuth.multiply(-4, -9))
}

test "azimuth_standard_divide_with_ceil" {
  // 向上取整除法标准测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(15, 4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(12, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(8, 0))
}

test "azimuth_standard_greet_function" {
  // 问候函数标准测试
  @azimuth.assert_eq_string("Hello, Azimuth!", @azimuth.greet("Azimuth"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 2024!", @azimuth.greet("2024"))
}

test "azimuth_standard_subtract_function" {
  // 减法函数标准测试
  @azimuth.assert_eq(6, @azimuth.subtract(10, 4))
  @azimuth.assert_eq(-6, @azimuth.subtract(4, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(8, 8))
  @azimuth.assert_eq(15, @azimuth.subtract(5, -10))
}

test "azimuth_standard_math_properties" {
  // 数学性质验证测试
  let x = 16
  let y = 7
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 验证具体数值
  @azimuth.assert_eq(23, @azimuth.add(16, 7))
  @azimuth.assert_eq(112, @azimuth.multiply(16, 7))
}

test "azimuth_standard_packaging_scenario" {
  // 包装计算场景测试
  let total_items = 43
  let items_per_package = 10
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  @azimuth.assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  @azimuth.assert_eq(50, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(7, remaining_space)
}

test "azimuth_standard_workflow_calculation" {
  // 工作流计算测试
  // 场景：项目任务分配
  let team_members = 5
  let tasks_per_member = 7
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 12
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(35, total_tasks)  // 5 * 7 = 35
  @azimuth.assert_eq(3, sprints_needed)  // ceil(35/12) = 3
}

test "azimuth_standard_negative_numbers" {
  // 负数处理测试
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(4, @azimuth.subtract(-3, -7))
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-5, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-5, -5))
}

test "azimuth_standard_boundary_values" {
  // 边界值测试
  // 除法边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(10, 10))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-10, -10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  
  // 大数运算
  @azimuth.assert_eq(2000, @azimuth.add(1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 特殊问候情况
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}
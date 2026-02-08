// Azimuth 项目标准 MoonBit 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "azimuth_basic_math_operations" {
  // 基本数学运算测试
  assert_eq(10, @azimuth.add(3, 7))
  assert_eq(15, @azimuth.multiply(3, 5))
  assert_eq(2, @azimuth.subtract(8, 6))
  assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "azimuth_string_operations" {
  // 字符串操作测试
  assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  assert_eq_string("Hello, World!", @azimuth.greet("World"))
  assert_eq_string("Hello, !", @azimuth.greet(""))
  assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "azimuth_negative_number_operations" {
  // 负数运算测试
  assert_eq(-4, @azimuth.add(-2, -2))
  assert_eq(6, @azimuth.multiply(-2, -3))
  assert_eq(-6, @azimuth.multiply(2, -3))
  assert_eq(-2, @azimuth.subtract(-5, -3))
  assert_eq(-2, @azimuth.divide_with_ceil(-8, 4))
}

test "azimuth_mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 5
  
  // 加法交换律
  assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体数值
  assert_eq(17, @azimuth.add(12, 5))
  assert_eq(60, @azimuth.multiply(12, 5))
}

test "azimuth_business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "azimuth_complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(3, sprints_needed)  // ceil(48/20) = 3
}

test "azimuth_boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 除法边界情况
  assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  
  // 大数运算
  assert_eq(2000, @azimuth.add(1000, 1000))
  assert_eq(10000, @azimuth.multiply(100, 100))
  
  // 零值运算
  assert_eq(5, @azimuth.add(5, 0))
  assert_eq(0, @azimuth.multiply(5, 0))
}

test "azimuth_real_world_project_planning" {
  // 真实世界项目规划测试
  let project_days = 15
  let team_size = 6
  let hours_per_day = 8
  let total_hours = @azimuth.multiply(project_days, @azimuth.multiply(team_size, hours_per_day))
  
  let tasks_per_hour = 2
  let total_tasks = @azimuth.multiply(total_hours, tasks_per_hour)
  
  let tasks_per_sprint = 50
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(720, total_hours)  // 15 * 6 * 8 = 720
  assert_eq(1440, total_tasks)  // 720 * 2 = 1440
  assert_eq(29, sprints_needed)  // ceil(1440/50) = 29
}
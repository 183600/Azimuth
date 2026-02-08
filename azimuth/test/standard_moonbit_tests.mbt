// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "basic_arithmetic_comprehensive" {
  // 综合基本算术运算测试
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-2, @azimuth.subtract(3, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(7, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), 3), @azimuth.add(a, @azimuth.add(b, 3)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), 2), @azimuth.multiply(a, @azimuth.multiply(b, 2)))
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-7, -4))
}

test "boundary_value_testing" {
  // 边界值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
}

test "string_processing_comprehensive" {
  // 综合字符串处理测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
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
  // 场景：项目管理计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)  // 6 * 8 = 48
  @azimuth.assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "large_number_operations" {
  // 大数运算测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  @azimuth.assert_eq(1001, @azimuth.divide_with_ceil(1000000, 999))
  @azimuth.assert_eq(0, @azimuth.subtract(1000000, 1000000))
}

test "error_handling_and_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  
  // 特殊字符串处理
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let attendees = 250
  let tables_available = 30
  let chairs_per_table = 8
  let cost_per_chair = 15
  let total_budget = 4000
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(32, tables_needed)
  
  // 检查桌子是否足够
  @azimuth.assert_eq(1, if tables_needed <= tables_available { 1 } else { 0 })
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(256, total_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(total_seats, attendees)
  @azimuth.assert_eq(6, remaining_seats)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_seats, cost_per_chair)
  @azimuth.assert_eq(3840, total_cost)
  
// 验证预算充足
  @azimuth.assert_eq(1, if total_cost <= total_budget { 1 } else { 0 })
  
  // 计算剩余预算
  let remaining_budget = @azimuth.subtract(total_budget, total_cost)
  @azimuth.assert_eq(160, remaining_budget)
}
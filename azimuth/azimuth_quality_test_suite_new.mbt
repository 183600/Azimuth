// Azimuth 项目高质量测试用例集
// 包含10个精心设计的标准 MoonBit 测试用例
// 测试覆盖：算术运算、字符串处理、边界条件、业务逻辑场景

test "arithmetics_addition_comprehensive" {
  // 全面测试加法运算
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
}

test "arithmetics_multiplication_edge_cases" {
  // 乘法运算边界情况测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(10000, @azimuth.multiply(100, 100))
  @azimuth.assert_eq(0, @azimuth.multiply(-100, 0))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法综合场景测试
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(8, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-8, -3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 7
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体数值
  @azimuth.assert_eq(19, @azimuth.add(12, 7))
  @azimuth.assert_eq(84, @azimuth.multiply(12, 7))
  
  // 分配律验证
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 3)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3))
  )
}

test "business_packaging_calculation" {
  // 业务包装计算场景测试
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

test "complex_workflow_project_management" {
  // 复杂工作流：项目管理计算测试
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let days_per_sprint = 5
  let tasks_per_day = 10
  let sprint_capacity = @azimuth.multiply(days_per_sprint, tasks_per_day)
  
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)  // 6 * 8 = 48
  @azimuth.assert_eq(50, sprint_capacity)  // 5 * 10 = 50
  @azimuth.assert_eq(1, sprints_needed)  // ceil(48/50) = 1
}

test "boundary_values_and_extremes" {
  // 边界值和极值测试
  // 除法边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-5, -5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  
  // 减法边界测试
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(15, 15))
  @azimuth.assert_eq(20, @azimuth.subtract(10, -10))
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的全面测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(9999, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-9999, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 特殊字符串处理
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, special@chars!", @azimuth.greet("special@chars"))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let attendees = 87
  let tables_available = 25
  let chairs_per_table = 8
  let cost_per_chair = 15
  let total_budget = 4000
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(11, tables_needed)
  
  // 检查桌子是否足够
  @azimuth.assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(total_seats, attendees)
  @azimuth.assert_eq(1, remaining_seats)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_seats, cost_per_chair)
  @azimuth.assert_eq(1320, total_cost)
  
  // 验证预算充足
  @azimuth.assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = @azimuth.subtract(total_budget, total_cost)
  @azimuth.assert_eq(2680, remaining_budget)
}
// Azimuth 项目增强测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "enhanced_arithmetic_operations" {
  // 增强算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  
  @azimuth.assert_eq(56, @azimuth.multiply(7, 8))
  @azimuth.assert_eq(-40, @azimuth.multiply(-5, 8))
  @azimuth.assert_eq(40, @azimuth.multiply(-5, -8))
}

test "enhanced_divide_with_ceil_operations" {
  // 增强向上取整除法测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 2))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(11, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))
}

test "enhanced_string_operations" {
  // 增强字符串操作测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律验证
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 3)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3))
  )
}

test "business_logistics_scenario" {
  // 业务物流场景测试
  let total_packages = 85
  let capacity_per_truck = 20
  let trucks_needed = @azimuth.divide_with_ceil(total_packages, capacity_per_truck)
  
  @azimuth.assert_eq(5, trucks_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(trucks_needed, capacity_per_truck)
  @azimuth.assert_eq(100, total_capacity)
  
  // 验证剩余容量
  let remaining_capacity = @azimuth.subtract(total_capacity, total_packages)
  @azimuth.assert_eq(15, remaining_capacity)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_members = 9
  let tasks_per_member = 7
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_day = 5
  let sprint_capacity = @azimuth.multiply(sprint_duration, tasks_per_day)
  
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(63, total_tasks)  // 9 * 7 = 63
  @azimuth.assert_eq(10, sprint_capacity)  // 2 * 5 = 10
  @azimuth.assert_eq(7, sprints_needed)  // ceil(63/10) = 7
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  
  // 单位值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  
  // 大数测试（安全范围内）
  @azimuth.assert_eq(20000, @azimuth.add(10000, 10000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let attendees = 156
  let tables_available = 20
  let chairs_per_table = 8
  let budget_per_chair = 25
  let total_budget = 5000
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(20, tables_needed)
  
  // 验证桌子是否足够
  @azimuth.assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(160, total_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(total_seats, attendees)
  @azimuth.assert_eq(4, remaining_seats)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_seats, budget_per_chair)
  @azimuth.assert_eq(4000, total_cost)
  
  // 验证预算充足
  @azimuth.assert_true(total_cost <= total_budget)
}
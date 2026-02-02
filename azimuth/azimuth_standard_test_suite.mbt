// Azimuth 项目标准测试套件
// 包含 10 个高质量 MoonBit 测试用例，使用标准测试语法
// 测试覆盖核心功能：加法、乘法、减法、向上取整除法和问候函数

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-2, @azimuth.subtract(3, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(7, 4))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证测试
  let a = 12
  let b = 8
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体数值
  @azimuth.assert_eq(20, @azimuth.add(12, 8))
  @azimuth.assert_eq(96, @azimuth.multiply(12, 8))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "business_packaging_calculation" {
  // 业务包装计算测试
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

test "negative_number_operations" {
  // 负数运算测试
  // 负数加法
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(-2, @azimuth.add(3, -5))
  
  // 负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  
  // 负数减法
  @azimuth.assert_eq(2, @azimuth.subtract(-3, -5))
  @azimuth.assert_eq(-8, @azimuth.subtract(-3, 5))
  @azimuth.assert_eq(8, @azimuth.subtract(3, -5))
}

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 42
  
  // 加法恒等式: x + 0 = x
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式: x * 1 = x, x * 0 = 0
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  
  // 减法恒等式: x - x = 0, x - 0 = x
  @azimuth.assert_eq(0, @azimuth.subtract(x, x))
  @azimuth.assert_eq(x, @azimuth.subtract(x, 0))
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源管理
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_day = 10
  let sprint_capacity = @azimuth.multiply(sprint_duration, tasks_per_day)
  
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)  // 8 * 6 = 48
  @azimuth.assert_eq(20, sprint_capacity)  // 2 * 10 = 20
  @azimuth.assert_eq(3, sprints_needed)  // ceil(48/20) = 3
}

test "boundary_values_and_extremes" {
  // 边界值和极值测试
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.subtract(5, 0))
  
  // 单位运算
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-5, -5))
  
  // 大数运算（在安全范围内）
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let attendees = 125
  let tables_available = 15
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(16, tables_needed)
  
  // 检查桌子是否足够
  let chairs_needed = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(128, chairs_needed)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(chairs_needed, cost_per_chair)
  @azimuth.assert_eq(3200, total_cost)
  
  // 计算预算差额
  let budget_difference = @azimuth.subtract(total_cost, total_budget)
  @azimuth.assert_eq(200, budget_difference)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(chairs_needed, attendees)
  @azimuth.assert_eq(3, remaining_seats)
}
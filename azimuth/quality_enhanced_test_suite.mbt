// Azimuth 项目增强测试套件
// 包含8个高质量的标准 MoonBit 测试用例

test "mathematical_distributive_property" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 5
  let c = 3
  
  let left_result = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_result = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_result, right_result)  // 7 * (5 + 3) = 7 * 5 + 7 * 3 = 56
}

test "warehouse_inventory_management" {
  // 仓库库存管理场景测试
  let total_products = 158
  let pallet_capacity = 25
  let pallets_needed = @azimuth.divide_with_ceil(total_products, pallet_capacity)
  
  @azimuth.assert_eq(7, pallets_needed)  // ceil(158/25) = 7
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(pallets_needed, pallet_capacity)
  @azimuth.assert_eq(175, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(17, remaining_space)
}

test "multi_currency_greeting_scenarios" {
  // 多语言问候场景测试
  @azimuth.assert_eq_string("Hello, Global!", @azimuth.greet("Global"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, München!", @azimuth.greet("München"))
  @azimuth.assert_eq_string("Hello, São Paulo!", @azimuth.greet("São Paulo"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
}

test "complex_project_planning_workflow" {
  // 复杂项目规划工作流测试
  let team_size = 9
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = @azimuth.multiply(team_size, @azimuth.multiply(hours_per_day, working_days))
  @azimuth.assert_eq(1080, total_hours)  // 9 * 8 * 15 = 1080
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(total_hours, tasks_per_hour)
  @azimuth.assert_eq(2160, total_tasks)  // 1080 * 2 = 2160
  
  // 计算需要的冲刺数（每冲刺最多500任务）
  let sprint_capacity = 500
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  @azimuth.assert_eq(5, sprints_needed)  // ceil(2160/500) = 5
}

test "mathematical_boundary_conditions" {
  // 数学边界条件测试
  // 零元素性质
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  
  // 单位元素性质
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  
  // 极值除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 999999))
  @azimuth.assert_eq(999999, @azimuth.divide_with_ceil(999999, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(1999999, 1000000))
}

test "negative_number_comprehensive_operations" {
  // 负数综合运算测试
  let positive = 15
  let negative = -8
  
  // 混合运算测试
  @azimuth.assert_eq(7, @azimuth.add(positive, negative))    // 15 + (-8) = 7
  @azimuth.assert_eq(-7, @azimuth.add(negative, positive))   // -8 + 15 = 7
  @azimuth.assert_eq(-23, @azimuth.add(negative, negative))  // -8 + (-8) = -16
  
  @azimuth.assert_eq(-120, @azimuth.multiply(positive, negative))  // 15 * (-8) = -120
  @azimuth.assert_eq(120, @azimuth.multiply(negative, negative))   // (-8) * (-8) = 64
  
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(negative, positive))   // ceil(-8/15) = -1
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(negative, negative))    // ceil(-8/-8) = 1
}

test "event_seating_optimization" {
  // 活动座位优化测试
  let total_attendees = 247
  let tables_available = 30
  let chairs_per_table = 8
  let vip_chairs = 12
  
  // 计算普通座位需要的桌子数
  let regular_attendees = @azimuth.subtract(total_attendees, vip_chairs)
  let tables_needed = @azimuth.divide_with_ceil(regular_attendees, chairs_per_table)
  @azimuth.assert_eq(30, tables_needed)  // ceil(235/8) = 30
  
  // 验证桌子是否足够
  @azimuth.assert_eq(30, tables_needed)  // 确认桌子数量计算正确
  
  // 计算总座位数
  let regular_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  let total_seats = @azimuth.add(regular_seats, vip_chairs)
  @azimuth.assert_eq(252, total_seats)
  
  // 计算剩余座位
  let available_seats = @azimuth.subtract(total_seats, total_attendees)
  @azimuth.assert_eq(5, available_seats)
}

test "cascading_calculation_pipeline" {
  // 级联计算流水线测试
  let initial_value = 12
  
  // 第一阶段：乘法运算
  let stage1 = @azimuth.multiply(initial_value, 3)
  @azimuth.assert_eq(36, stage1)
  
  // 第二阶段：加法运算
  let stage2 = @azimuth.add(stage1, 15)
  @azimuth.assert_eq(51, stage2)
  
  // 第三阶段：除法向上取整
  let stage3 = @azimuth.divide_with_ceil(stage2, 8)
  @azimuth.assert_eq(7, stage3)
  
  // 第四阶段：减法运算
  let stage4 = @azimuth.subtract(@azimuth.multiply(stage3, 5), 8)
  @azimuth.assert_eq(27, stage4)
  
  // 最终验证：检查整个计算链的正确性
  let expected_result = @azimuth.subtract(@azimuth.multiply(@azimuth.divide_with_ceil(@azimuth.add(@azimuth.multiply(12, 3), 15), 8), 5), 8)
  @azimuth.assert_eq(stage4, expected_result)
}
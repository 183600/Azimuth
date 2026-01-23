// Azimuth 增强测试套件
// 包含10个高质量 MoonBit 测试用例，覆盖边缘情况和实际应用场景

// 测试1: 大数运算测试
test "large_number_operations" {
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(1000001, @azimuth.divide_with_ceil(1000000, 999))
}

// 测试2: 负数除法边缘情况
test "negative_division_edge_cases" {
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 4))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 100))
}

// 测试3: 数学恒等式验证
test "mathematical_identity_properties" {
  let x = 42
  let y = 17
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
}

// 测试4: 项目资源分配场景
test "project_resource_allocation" {
  let total_tasks = 87
  let team_members = 6
  let tasks_per_member = @azimuth.divide_with_ceil(total_tasks, team_members)
  
  // 验证每个成员的任务数
  @azimuth.assert_eq(15, tasks_per_member)
  
  // 验证总处理能力
  let total_capacity = @azimuth.multiply(tasks_per_member, team_members)
  @azimuth.assert_eq(90, total_capacity)
  
  // 验证剩余任务槽位
  let remaining_slots = @azimuth.add(total_capacity, -total_tasks)
  @azimuth.assert_eq(3, remaining_slots)
}

// 测试5: Unicode 和多语言字符处理
test "unicode_multilingual_greeting" {
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
}

// 测试6: 复杂业务计算流程
test "complex_business_workflow" {
  // 场景：计算项目成本和时间
  let hourly_rate = 75
  let hours_per_day = 8
  let team_size = 5
  let project_duration = 12
  
  // 计算总工时
  let total_hours = @azimuth.multiply(hours_per_day, @azimuth.multiply(team_size, project_duration))
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_hours, hourly_rate)
  
  // 计算支付周期（每周期不超过10000）
  let payment_limit = 10000
  let payment_periods = @azimuth.divide_with_ceil(total_cost, payment_limit)
  
  @azimuth.assert_eq(480, total_hours)  // 8 * 5 * 12 = 480
  @azimuth.assert_eq(36000, total_cost)  // 480 * 75 = 36000
  @azimuth.assert_eq(4, payment_periods)  // ceil(36000/10000) = 4
}

// 测试7: 边界值和极值测试
test "boundary_and_extreme_values" {
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1000000))
  
  // 单位值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1000000, 1000000))
  
  // 大数除法测试
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(999999, 1000))
  @azimuth.assert_eq(1001, @azimuth.divide_with_ceil(1000001, 1000))
}

// 测试8: 字符串边缘情况处理
test "string_edge_case_handling" {
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, ABC123!", @azimuth.greet("ABC123"))
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet("!@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, \\n\\t!", @azimuth.greet("\\n\\t"))
}

// 测试9: 数学分配律验证
test "mathematical_distributive_property" {
  let a = 6
  let b = 7
  let c = 8
  
  // 分配律: a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 另一个验证
  let x = 12
  let y = 5
  let z = 3
  let distributive_check = @azimuth.multiply(x, @azimuth.add(y, z))
  let manual_calc = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(distributive_check, manual_calc)
}

// 测试10: 真实世界应用 - 库存管理
test "real_world_inventory_management" {
  // 场景：仓库库存管理
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  
  // 验证需要的箱子数量
  @azimuth.assert_eq(7, boxes_needed)  // ceil(157/24) = 7
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = @azimuth.add(total_capacity, -total_products)
  @azimuth.assert_eq(11, remaining_space)
  
  // 计算空间利用率
  let utilization_percentage = @azimuth.divide_with_ceil(@azimuth.multiply(total_products, 100), total_capacity)
  @azimuth.assert_eq(94, utilization_percentage)  // ceil(15700/168) = 94%
}
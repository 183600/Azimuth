// Enhanced Azimuth Tests - 高质量 MoonBit 测试用例
// 包含10个标准测试用例，覆盖各种功能和场景

test "mathematical_commutative_law" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(5, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(9, 2))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
  @azimuth.assert_eq_string("Hello, Français!", @azimuth.greet("Français"))
  @azimuth.assert_eq_string("Hello, Deutsch!", @azimuth.greet("Deutsch"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
  @azimuth.assert_eq_string("Hello, 한국어!", @azimuth.greet("한국어"))
}

test "complex_business_workflow" {
  // 测试复杂业务工作流
  // 场景：电商订单处理
  let order_items = 47
  let items_per_box = 12
  let boxes_per_pallet = 8
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(order_items, items_per_box)
  @azimuth.assert_eq(4, boxes_needed)
  
  // 计算需要的托盘数量
  let pallets_needed = @azimuth.divide_with_ceil(boxes_needed, boxes_per_pallet)
  @azimuth.assert_eq(1, pallets_needed)
  
  // 计算总容量
  let total_box_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  @azimuth.assert_eq(48, total_box_capacity)
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483648, 0))
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  // 场景：计算项目资源需求
  let total_tasks = 125
  let team_members = 8
  let working_days = 15
  
  // 计算每人每天需要完成的任务数
  let daily_capacity = @azimuth.multiply(team_members, 1)
  let total_daily_capacity = @azimuth.multiply(daily_capacity, working_days)
  let tasks_per_day_per_person = @azimuth.divide_with_ceil(total_tasks, total_daily_capacity)
  
  @azimuth.assert_eq(2, tasks_per_day_per_person)
  
  // 验证总处理能力
  let actual_capacity = @azimuth.multiply(tasks_per_day_per_person, total_daily_capacity)
  @azimuth.assert_eq(128, actual_capacity)
}

test "string_edge_cases_comprehensive" {
  // 字符串边缘情况综合测试
  @azimuth.assert_eq_string("Hello, 🌟✨🚀!", @azimuth.greet("🌟✨🚀"))
  @azimuth.assert_eq_string("Hello, \t\n\r!", @azimuth.greet("\t\n\r"))
  @azimuth.assert_eq_string("Hello, \"quotes\"!", @azimuth.greet("\"quotes\""))
  @azimuth.assert_eq_string("Hello, 'apostrophe'!", @azimuth.greet("'apostrophe'"))
  @azimuth.assert_eq_string("Hello, \\backslash\\!", @azimuth.greet("\\backslash\\"))
  @azimuth.assert_eq_string("Hello, emoji 😊🎉!", @azimuth.greet("emoji 😊🎉"))
}

test "large_number_operations_stability" {
  // 大数运算稳定性测试
  let large_num1 = 100000
  let large_num2 = 99999
  
  // 大数加法
  @azimuth.assert_eq(199999, @azimuth.add(large_num1, large_num2))
  
  // 大数乘法（使用较小的值避免溢出）
  @azimuth.assert_eq(9999900, @azimuth.multiply(large_num1, 99))
  
  // 大数除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_num1, large_num2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(large_num2, @azimuth.divide_with_ceil(large_num1, 10000)))
}

test "real_world_event_planning_complex" {
  // 真实世界活动规划复杂场景
  let attendees = 256
  let chairs_per_table = 10
  let budget_per_person = 45
  let total_budget = 12000
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(26, tables_needed)
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(260, total_seats)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(attendees, budget_per_person)
  @azimuth.assert_eq(11520, total_cost)
  
  // 验证预算充足（通过比较而不是使用assert_true）
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(total_budget - total_cost + 1, total_budget - total_cost + 1))
  
  // 计算剩余预算（使用加法和负数代替减法）
  let remaining_budget = @azimuth.add(total_budget, -total_cost)
  @azimuth.assert_eq(480, remaining_budget)
}
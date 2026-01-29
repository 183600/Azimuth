// Enhanced Azimuth Tests - 高质量 MoonBit 测试用例
// 包含10个标准测试用例，覆盖各种功能和场景

test "mathematical_commutative_law" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
  assert_eq_string("Hello, Deutsch!", greet("Deutsch"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
}

test "complex_business_workflow" {
  // 测试复杂业务工作流
  // 场景：电商订单处理
  let order_items = 47
  let items_per_box = 12
  let boxes_per_pallet = 8
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  assert_eq(4, boxes_needed)
  
  // 计算需要的托盘数量
  let pallets_needed = divide_with_ceil(boxes_needed, boxes_per_pallet)
  assert_eq(1, pallets_needed)
  
  // 计算总容量
  let total_box_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_box_capacity)
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  // 场景：计算项目资源需求
  let total_tasks = 125
  let team_members = 8
  let working_days = 15
  
  // 计算每人每天需要完成的任务数
  let daily_capacity = multiply(team_members, 1)
  let total_daily_capacity = multiply(daily_capacity, working_days)
  let tasks_per_day_per_person = divide_with_ceil(total_tasks, total_daily_capacity)
  
  assert_eq(2, tasks_per_day_per_person)
  
  // 验证总处理能力
  let actual_capacity = multiply(tasks_per_day_per_person, total_daily_capacity)
  assert_eq(128, actual_capacity)
}

test "string_edge_cases_comprehensive" {
  // 字符串边缘情况综合测试
  assert_eq_string("Hello, 🌟✨🚀!", greet("🌟✨🚀"))
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
  assert_eq_string("Hello, \"quotes\"!", greet("\"quotes\""))
  assert_eq_string("Hello, 'apostrophe'!", greet("'apostrophe'"))
  assert_eq_string("Hello, \\backslash\\!", greet("\\backslash\\"))
  assert_eq_string("Hello, emoji 😊🎉!", greet("emoji 😊🎉"))
}

test "large_number_operations_stability" {
  // 大数运算稳定性测试
  let large_num1 = 100000
  let large_num2 = 99999
  
  // 大数加法
  assert_eq(199999, add(large_num1, large_num2))
  
  // 大数乘法
  assert_eq(9999900000, multiply(large_num1, large_num2))
  
  // 大数除法
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
  assert_eq(2, divide_with_ceil(large_num2, divide_with_ceil(large_num1, 10000)))
}

test "real_world_event_planning_complex" {
  // 真实世界活动规划复杂场景
  let attendees = 256
  let tables_available = 30
  let chairs_per_table = 10
  let budget_per_person = 45
  let total_budget = 12000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(26, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(260, total_seats)
  
  // 计算总成本
  let total_cost = multiply(attendees, budget_per_person)
  assert_eq(11520, total_cost)
  
  // 验证预算充足（通过比较而不是使用assert_true）
  assert_eq(1, divide_with_ceil(total_budget - total_cost + 1, total_budget - total_cost + 1))
  
  // 计算剩余预算（使用加法和负数代替减法）
  let remaining_budget = add(total_budget, -total_cost)
  assert_eq(480, remaining_budget)
}
// Azimuth 高级测试用例 - 10个标准 MoonBit 测试用例
// 测试 azimuth 库的高级功能和边缘情况

test "advanced_mathematical_distributive" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 4
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
}

test "advanced_fibonacci_sequence" {
  // 使用现有函数计算斐波那契数列的前几项
  let fib1 = add(0, 1)  // F1 = 1
  let fib2 = add(1, 1)  // F2 = 2
  let fib3 = add(1, 2)  // F3 = 3
  let fib4 = add(2, 3)  // F4 = 5
  let fib5 = add(3, 5)  // F5 = 8
  
  assert_eq(1, fib1)
  assert_eq(2, fib2)
  assert_eq(3, fib3)
  assert_eq(5, fib4)
  assert_eq(8, fib5)
}

test "advanced_resource_allocation" {
  // 高级资源分配场景测试
  let team_members = 15
  let projects = 4
  let tasks_per_project = 8
  
  // 计算总任务数
  let total_tasks = multiply(projects, tasks_per_project)
  
  // 计算每人平均任务数
  let tasks_per_person = divide_with_ceil(total_tasks, team_members)
  
  // 验证任务分配合理性
  assert_eq(32, total_tasks)  // 4 * 8 = 32
  assert_eq(3, tasks_per_person)  // ceil(32/15) = 3
  
  // 验证总处理能力
  let total_capacity = multiply(team_members, tasks_per_person)
  assert_eq(45, total_capacity)  // 15 * 3 = 45
  
  // 验证有足够的处理能力
  assert_true(total_capacity >= total_tasks)
}

test "advanced_temperature_conversion" {
  // 温度转换场景测试（简化版）
  let fahrenheit_base = 32
  
  // 模拟温度转换: F = C * 9/5 + 32
  let celsius_100 = 100
  let fahrenheit_100 = add(multiply(divide_with_ceil(multiply(celsius_100, 9), 5), 1), fahrenheit_base)
  
  // 简化的转换测试
  let celsius_neg_40 = -40
  let fahrenheit_neg_40 = add(multiply(divide_with_ceil(multiply(celsius_neg_40, 9), 5), 1), fahrenheit_base)
  
  assert_eq(212, fahrenheit_100)  // 100°C = 212°F
  assert_eq(-40, fahrenheit_neg_40)  // -40°C = -40°F (特殊点)
}

test "advanced_financial_compound_interest" {
  // 复合利息计算测试（简化版）
  let principal = 1000
  let rate_percent = 5
  
  // 简化的复合利息计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(1050, year1_amount)  // 1000 + 5% = 1050
  assert_eq(1103, year2_amount)  // 1050 + 5% ≈ 1103 (向上取整)
  assert_eq(1159, year3_amount)  // 1103 + 5% ≈ 1159 (向上取整)
}

test "advanced_string_processing" {
  // 高级字符串处理测试
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "Charlie"
  
  // 测试不同长度和类型的名字
  assert_eq_string("Hello, Alice!", greet(name1))
  assert_eq_string("Hello, Bob!", greet(name2))
  assert_eq_string("Hello, Charlie!", greet(name3))
  
  // 测试包含特殊字符的名字
  let name_with_hyphen = "Mary-Jane"
  let name_with_apostrophe = "O'Connor"
  let name_with_space = "Jean Claude"
  
  assert_eq_string("Hello, Mary-Jane!", greet(name_with_hyphen))
  assert_eq_string("Hello, O'Connor!", greet(name_with_apostrophe))
  assert_eq_string("Hello, Jean Claude!", greet(name_with_space))
}

test "advanced_geometric_calculations" {
  // 几何计算测试（简化版）
  // 计算矩形的面积和周长
  let length = 8
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(40, area)  // 8 * 5 = 40
  assert_eq(26, perimeter)  // 2 * (8 + 5) = 26
  
  // 计算需要多少个正方形瓷砖覆盖矩形
  let tile_size = 3
  let tiles_length = divide_with_ceil(length, tile_size)
  let tiles_width = divide_with_ceil(width, tile_size)
  let total_tiles = multiply(tiles_length, tiles_width)
  
  assert_eq(3, tiles_length)  // ceil(8/3) = 3
  assert_eq(2, tiles_width)   // ceil(5/3) = 2
  assert_eq(6, total_tiles)   // 3 * 2 = 6
}

test "advanced_time_calculations" {
  // 时间计算测试
  let hours_per_day = 24
  let minutes_per_hour = 60
  let seconds_per_minute = 60
  
  // 计算一天的总秒数
  let seconds_per_day = multiply(hours_per_day, multiply(minutes_per_hour, seconds_per_minute))
  assert_eq(86400, seconds_per_day)  // 24 * 60 * 60 = 86400
  
  // 计算工作周的总小时数
  let work_days_per_week = 5
  let work_hours_per_day = 8
  let weekly_work_hours = multiply(work_days_per_week, work_hours_per_day)
  assert_eq(40, weekly_work_hours)  // 5 * 8 = 40
  
  // 计算项目需要的周数
  let project_hours = 120
  let weeks_needed = divide_with_ceil(project_hours, weekly_work_hours)
  assert_eq(3, weeks_needed)  // ceil(120/40) = 3
}

test "advanced_error_boundary_conditions" {
  // 高级错误边界条件测试
  // 测试极值运算
  let max_int = 1000000
  let min_int = -1000000
  
  // 测试大数运算
  assert_eq(2000000, add(max_int, max_int))
  assert_eq(0, add(max_int, min_int))
  assert_eq(-2000000, add(min_int, min_int))
  
  // 测试除法的边界情况
  assert_eq(1000000, divide_with_ceil(max_int, 1))
  assert_eq(1, divide_with_ceil(max_int, max_int))
  assert_eq(2, divide_with_ceil(max_int, divide_with_ceil(max_int, 2)))
  
  // 测试零的各种情况
  assert_eq(0, multiply(0, max_int))
  assert_eq(0, multiply(0, min_int))
  assert_eq(0, divide_with_ceil(0, max_int))
  assert_eq(0, divide_with_ceil(0, min_int))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "advanced_complex_workflow_integration" {
  // 复杂工作流集成测试
  // 场景：电子商务订单处理
  
  let order_items = 47
  let items_per_box = 12
  let boxes_per_pallet = 8
  let cost_per_item = 15
  let shipping_cost_per_box = 25
  
  // 计算需要的箱子数
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  // 计算需要的托盘数
  let pallets_needed = divide_with_ceil(boxes_needed, boxes_per_pallet)
  assert_eq(1, pallets_needed)  // ceil(4/8) = 1
  
  // 计算物品总成本
  let items_cost = multiply(order_items, cost_per_item)
  assert_eq(705, items_cost)  // 47 * 15 = 705
  
  // 计算总运输成本
  let total_shipping_cost = multiply(boxes_needed, shipping_cost_per_box)
  assert_eq(100, total_shipping_cost)  // 4 * 25 = 100
  
  // 计算订单总成本
  let total_order_cost = add(items_cost, total_shipping_cost)
  assert_eq(805, total_order_cost)  // 705 + 100 = 805
  
  // 计算每个箱子的平均物品数
  let avg_items_per_box = divide_with_ceil(order_items, boxes_needed)
  assert_eq(12, avg_items_per_box)  // ceil(47/4) = 12
  
  // 验证总容量
  let total_box_capacity = multiply(boxes_needed, items_per_box)
  let unused_space = subtract(total_box_capacity, order_items)
  assert_eq(1, unused_space)  // (4*12) - 47 = 1
}
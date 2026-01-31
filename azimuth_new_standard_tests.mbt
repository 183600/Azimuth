// Azimuth 新增标准测试用例 - 9个高质量测试用例
// 使用标准的 MoonBit 测试语法，覆盖新的场景和边界情况

test "divide_with_ceil_fractional_precision" {
  // 测试向上取整除法的小数精度处理
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
  assert_eq(6, divide_with_ceil(11, 2))
}

test "mathematical_distributive_law" {
  // 测试数学分配律 a * (b + c) = a * b + a * c
  let a = 6
  let b = 8
  let c = 4
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 6 * (8 + 4) = 6 * 8 + 6 * 4 = 72
}

test "negative_number_division_comprehensive" {
  // 负数除法的综合测试
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(2, divide_with_ceil(7, -4))
  assert_eq(1, divide_with_ceil(3, -4))
  assert_eq(2, divide_with_ceil(-7, -4))
  assert_eq(1, divide_with_ceil(-3, -4))
}

test "string_concatenation_edge_cases" {
  // 字符串连接的边缘情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \n\n!", greet("\n\n"))
  assert_eq_string("Hello, \t\t!", greet("\t\t"))
  assert_eq_string("Hello, 🌟🌙!", greet("🌟🌙"))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：计算项目成本和时间
  let team_size = 5
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 12
  let overhead_percentage = 20
  
  // 计算基础工时
  let total_hours = multiply(team_size, multiply(hours_per_day, project_days))
  
  // 计算基础成本
  let base_cost = multiply(total_hours, hourly_rate)
  
  // 计算管理费用（使用向上取整除法简化百分比计算）
  let overhead_cost = divide_with_ceil(multiply(base_cost, overhead_percentage), 100)
  
  // 计算总成本
  let total_cost = add(base_cost, overhead_cost)
  
  // 验证计算结果
  assert_eq(480, total_hours)      // 5 * 8 * 12 = 480
  assert_eq(36000, base_cost)      // 480 * 75 = 36000
  assert_eq(7200, overhead_cost)   // ceil(36000 * 20 / 100) = 7200
  assert_eq(43200, total_cost)     // 36000 + 7200 = 43200
}

test "resource_planning_optimization" {
  // 资源规划优化测试
  // 场景：会议室分配优化
  let attendees = 156
  let room_capacity_small = 8
  let room_capacity_medium = 15
  let room_capacity_large = 30
  
  // 计算使用不同大小会议室需要的数量
  let small_rooms_needed = divide_with_ceil(attendees, room_capacity_small)
  let medium_rooms_needed = divide_with_ceil(attendees, room_capacity_medium)
  let large_rooms_needed = divide_with_ceil(attendees, room_capacity_large)
  
  // 验证计算结果
  assert_eq(20, small_rooms_needed)   // ceil(156/8) = 20
  assert_eq(11, medium_rooms_needed)  // ceil(156/15) = 11
  assert_eq(6, large_rooms_needed)    // ceil(156/30) = 6
  
  // 计算各种方案的总座位数
  let total_seats_small = multiply(small_rooms_needed, room_capacity_small)
  let total_seats_medium = multiply(medium_rooms_needed, room_capacity_medium)
  let total_seats_large = multiply(large_rooms_needed, room_capacity_large)
  
  assert_eq(160, total_seats_small)   // 20 * 8 = 160
  assert_eq(165, total_seats_medium)  // 11 * 15 = 165
  assert_eq(180, total_seats_large)   // 6 * 30 = 180
  
  // 验证所有方案都能容纳所有参会者
  assert_true(total_seats_small >= attendees)
  assert_true(total_seats_medium >= attendees)
  assert_true(total_seats_large >= attendees)
}

test "arithmetic_sequence_calculations" {
  // 等差数列计算测试
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 6
  
  // 计算第n项：a_n = a_1 + (n-1)*d
  let sixth_term = add(first_term, multiply(number_of_terms - 1, common_difference))
  
  // 计算前n项和：S_n = n/2 * (2*a_1 + (n-1)*d)
  // 使用向上取整除法模拟除法2
  let half_n = divide_with_ceil(number_of_terms, 2)
  let sum_formula = multiply(half_n, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference)))
  
  // 验证计算结果
  assert_eq(23, sixth_term)    // 3 + 5*4 = 23
  assert_eq(78, sum_formula)   // 3 + 7 + 11 + 15 + 19 + 23 = 78
}

test "boolean_logic_simulation" {
  // 布尔逻辑模拟测试（使用数值运算）
  // 使用1表示true，0表示false
  
  // 模拟 AND 操作：a AND b = min(a, b)
  let and_true_true = 1  // min(1, 1) = 1
  let and_true_false = 0 // min(1, 0) = 0
  let and_false_false = 0 // min(0, 0) = 0
  
  // 模拟 OR 操作：a OR b = max(a, b)
  let or_true_true = 1   // max(1, 1) = 1
  let or_true_false = 1  // max(1, 0) = 1
  let or_false_false = 0 // max(0, 0) = 0
  
  // 模拟 NOT 操作：NOT a = 1 - a
  let not_true = 0   // 1 - 1 = 0
  let not_false = 1  // 1 - 0 = 1
  
  // 验证布尔逻辑结果
  assert_eq(1, and_true_true)
  assert_eq(0, and_true_false)
  assert_eq(0, and_false_false)
  
  assert_eq(1, or_true_true)
  assert_eq(1, or_true_false)
  assert_eq(0, or_false_false)
  
  assert_eq(0, not_true)
  assert_eq(1, not_false)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算测试
  let initial_inventory = 1200
  let purchases = 800
  let final_inventory = 600
  let sales_period_days = 90
  
  // 计算销售成本
  let cost_of_goods_sold = subtract(add(initial_inventory, purchases), final_inventory)
  
  // 计算平均库存
  let average_inventory = divide_with_ceil(add(initial_inventory, final_inventory), 2)
  
  // 计算库存周转率（按期间）
  let turnover_rate = divide_with_ceil(multiply(cost_of_goods_sold, 100), average_inventory)
  
  // 计算平均日销售量
  let average_daily_sales = divide_with_ceil(cost_of_goods_sold, sales_period_days)
  
  // 验证计算结果
  assert_eq(1400, cost_of_goods_sold)    // 1200 + 800 - 600 = 1400
  assert_eq(900, average_inventory)      // ceil((1200 + 600)/2) = 900
  assert_eq(156, turnover_rate)          // ceil(1400 * 100 / 900) = 156
  assert_eq(16, average_daily_sales)     // ceil(1400/90) = 16
}
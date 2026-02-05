// Azimuth 增强测试用例 - 标准MoonBit测试语法
// 包含8个高质量测试用例，覆盖各种场景

test "algebraic_manipulation_test" {
  // 代数运算测试：验证 (a+b)*(c-d) 的计算
  let a = 8
  let b = 7
  let c = 15
  let d = 3
  
  let sum_ab = add(a, b)
  let diff_cd = subtract(c, d)
  let result = multiply(sum_ab, diff_cd)
  
  assert_eq(15, sum_ab)      // 8 + 7 = 15
  assert_eq(12, diff_cd)     // 15 - 3 = 12
  assert_eq(180, result)     // 15 * 12 = 180
}

test "resource_planning_optimization" {
  // 资源规划优化测试：项目团队分配
  let total_tasks = 95
  let team_capacity = 6
  let weeks_available = 4
  
  // 计算需要的团队数量
  let teams_needed = divide_with_ceil(total_tasks, multiply(team_capacity, weeks_available))
  assert_eq(4, teams_needed)  // ceil(95/24) = 4
  
  // 验证总处理能力
  let total_capacity = multiply(teams_needed, multiply(team_capacity, weeks_available))
  assert_eq(144, total_capacity)
  
  // 计算剩余处理能力
  let surplus_capacity = subtract(total_capacity, total_tasks)
  assert_eq(49, surplus_capacity)
}

test "multi_step_financial_calculation" {
  // 多步骤财务计算测试：投资回报计算
  let initial_investment = 10000
  let monthly_return = 850
  let months = 12
  
  // 计算总回报
  let total_return = multiply(monthly_return, months)
  assert_eq(10200, total_return)
  
  // 计算净利润
  let net_profit = subtract(total_return, initial_investment)
  assert_eq(200, net_profit)
  
  // 计算回报率（向下取整到百分比）
  let roi_percentage = divide_with_ceil(multiply(net_profit, 100), initial_investment)
  assert_eq(2, roi_percentage)
}

test "inventory_batch_processing" {
  // 库存批量处理测试：订单分批
  let total_orders = 127
  let batch_size = 15
  let processing_days = 3
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_orders, batch_size)
  assert_eq(9, batches_needed)  // ceil(127/15) = 9
  
  // 计算每天处理的批次
  let batches_per_day = divide_with_ceil(batches_needed, processing_days)
  assert_eq(3, batches_per_day)  // ceil(9/3) = 3
  
  // 验证处理能力
  let daily_capacity = multiply(batches_per_day, batch_size)
  let total_processed = multiply(daily_capacity, processing_days)
  assert_eq(135, total_processed)
}

test "string_concatenation_validation" {
  // 字符串连接验证测试：复杂问候场景
  let formal_greeting = greet("Dr. Smith")
  let casual_greeting = greet("Alice")
  let empty_greeting = greet("")
  let numeric_greeting = greet("12345")
  
  assert_eq_string("Hello, Dr. Smith!", formal_greeting)
  assert_eq_string("Hello, Alice!", casual_greeting)
  assert_eq_string("Hello, !", empty_greeting)
  assert_eq_string("Hello, 12345!", numeric_greeting)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算测试：等差数列求和
  let first_term = 3
  let common_diff = 4
  let num_terms = 6
  
  // 计算最后一项
  let last_term = add(first_term, multiply(common_diff, subtract(num_terms, 1)))
  assert_eq(23, last_term)  // 3 + 4*(6-1) = 23
  
  // 计算序列和：(首项 + 末项) * 项数 / 2
  let sequence_sum = divide_with_ceil(multiply(add(first_term, last_term), num_terms), 2)
  assert_eq(78, sequence_sum)  // ceil((3+23)*6/2) = 78
}

test "capacity_planning_scenario" {
  // 容量规划场景测试：服务器资源分配
  let expected_users = 2500
  let users_per_server = 300
  let backup_servers = 2
  
  // 计算主服务器数量
  let main_servers = divide_with_ceil(expected_users, users_per_server)
  assert_eq(9, main_servers)  // ceil(2500/300) = 9
  
  // 计算总服务器数量
  let total_servers = add(main_servers, backup_servers)
  assert_eq(11, total_servers)
  
  // 计算总容量
  let total_capacity = multiply(total_servers, users_per_server)
  assert_eq(3300, total_capacity)
  
  // 计算冗余容量
  let redundant_capacity = subtract(total_capacity, expected_users)
  assert_eq(800, redundant_capacity)
}

test "complex_workflow_integration" {
  // 复杂工作流集成测试：生产流程优化
  let raw_materials = 500
  let defect_rate = 15  // 15% 次品率
  let packaging_capacity = 40
  
  // 计算合格产品数量
  let quality_products = subtract(raw_materials, divide_with_ceil(multiply(raw_materials, defect_rate), 100))
  assert_eq(425, quality_products)  // 500 - ceil(500*15/100) = 425
  
  // 计算需要的包装批次
  let packaging_batches = divide_with_ceil(quality_products, packaging_capacity)
  assert_eq(11, packaging_batches)  // ceil(425/40) = 11
  
  // 计算总包装容量
  let total_packaging_capacity = multiply(packaging_batches, packaging_capacity)
  assert_eq(440, total_packaging_capacity)
  
  // 计算剩余包装空间
  let leftover_space = subtract(total_packaging_capacity, quality_products)
  assert_eq(15, leftover_space)
}
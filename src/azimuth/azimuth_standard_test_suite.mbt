// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的全面功能测试

test "arithmetics_advanced_operations" {
  // 高级算术运算测试
  let x = 15
  let y = 7
  
  // 复合运算
  let result1 = add(multiply(x, 2), divide_with_ceil(y, 3))
  assert_eq(32, result1)  // 15*2 + ceil(7/3) = 30 + 3 = 33
  
  // 嵌套运算
  let result2 = multiply(add(x, y), divide_with_ceil(x, y))
  assert_eq(44, result2)  // (15+7) * ceil(15/7) = 22 * 3 = 66
}

test "string_greeting_multilingual_support" {
  // 多语言问候支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Мир!", greet("Мир"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, José!", greet("José"))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法的全面场景测试
  // 正数场景
  assert_eq(7, divide_with_ceil(40, 6))    // 40/6 = 6.666... -> 7
  assert_eq(10, divide_with_ceil(100, 10)) // 100/10 = 10 -> 10
  assert_eq(1, divide_with_ceil(3, 25))     // 3/25 = 0.12 -> 1
  
  // 负数场景
  assert_eq(-6, divide_with_ceil(-35, 6))   // -35/6 = -5.833... -> -5
  assert_eq(-10, divide_with_ceil(-100, 10)) // -100/10 = -10 -> -10
  
  // 混合符号场景
  assert_eq(-5, divide_with_ceil(35, -6))   // 35/-6 = -5.833... -> -5
  assert_eq(7, divide_with_ceil(-35, -6))   // -35/-6 = 5.833... -> 6
}

test "business_inventory_calculation" {
  // 库存业务计算测试
  let product_a_qty = 45
  let product_b_qty = 32
  let product_c_qty = 18
  let box_capacity = 12
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(4, boxes_a)   // ceil(45/12) = 4
  assert_eq(3, boxes_b)   // ceil(32/12) = 3
  assert_eq(2, boxes_c)   // ceil(18/12) = 2
  assert_eq(9, total_boxes) // 4 + 3 + 2 = 9
}

test "mathematical_sequence_calculation" {
  // 数学序列计算测试
  // 计算等差数列的和：1 + 3 + 5 + ... + 19 (前10个奇数)
  let first_term = 1
  let common_diff = 2
  let num_terms = 10
  
  // 使用求和公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(100, sum)  // 10/2 * (2 + 18) = 5 * 20 = 100
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_budget = 5000
  let team_members = 8
  let project_duration_days = 15
  let daily_expenses = 75
  
  // 计算人员成本
  let personnel_cost = multiply(team_members, multiply(daily_expenses, project_duration_days))
  // 计算剩余预算
  let remaining_budget = add(total_budget, -personnel_cost)
  // 计算可以支持的额外设备数量
  let equipment_cost = 250
  let equipment_count = divide_with_ceil(remaining_budget, equipment_cost)
  
  assert_eq(9000, personnel_cost)  // 8 * 75 * 15 = 9000
  assert_eq(-4000, remaining_budget)  // 5000 - 9000 = -4000
  assert_eq(0, equipment_count)  // ceil(-4000/250) = 0 (负数处理)
}

test "edge_cases_boundary_values" {
  // 边界值和极端情况测试
  // 零值运算
  assert_eq(0, add(0, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, divide_with_ceil(0, 100))
  
  // 单位运算
  assert_eq(1, add(0, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(100, divide_with_ceil(100, 1))
  
  // 负数边界
  assert_eq(0, add(5, -5))
  assert_eq(-1, multiply(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "complex_financial_calculation" {
  // 复杂金融计算测试
  let principal = 10000
  let interest_rate_percent = 8
  let tax_rate_percent = 20
  let years = 3
  
  // 计算年利息
  let annual_interest = divide_with_ceil(multiply(principal, interest_rate_percent), 100)
  // 计算税后利息
  let after_tax_interest = add(annual_interest, -divide_with_ceil(multiply(annual_interest, tax_rate_percent), 100))
  // 计算三年后的总金额
  let final_amount = add(principal, multiply(after_tax_interest, years))
  
  assert_eq(800, annual_interest)     // ceil(10000 * 8 / 100) = 800
  assert_eq(640, after_tax_interest)  // 800 - ceil(800 * 20 / 100) = 800 - 160 = 640
  assert_eq(11920, final_amount)      // 10000 + 640 * 3 = 10000 + 1920 = 11920
}

test "time_management_scheduling" {
  // 时间管理调度测试
  let total_tasks = 67
  let tasks_per_day = 8
  let working_days_per_week = 5
  let meetings_per_day = 2
  
  // 计算需要的工作天数
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(days_needed, working_days_per_week)
  // 计算总会议数
  let total_meetings = multiply(days_needed, meetings_per_day)
  
  assert_eq(9, days_needed)    // ceil(67/8) = 9
  assert_eq(2, weeks_needed)   // ceil(9/5) = 2
  assert_eq(18, total_meetings) // 9 * 2 = 18
}

test "data_processing_batch_analysis" {
  // 数据处理批处理分析测试
  let data_records = 1250
  let batch_size = 100
  let processing_time_per_batch = 15  // 分钟
  let setup_time = 30  // 分钟
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(data_records, batch_size)
  // 计算总处理时间
  let processing_time = multiply(batches_needed, processing_time_per_batch)
  // 计算总时间（包括设置时间）
  let total_time = add(processing_time, setup_time)
  
  assert_eq(13, batches_needed)    // ceil(1250/100) = 13
  assert_eq(195, processing_time)  // 13 * 15 = 195
  assert_eq(225, total_time)       // 195 + 30 = 225
}
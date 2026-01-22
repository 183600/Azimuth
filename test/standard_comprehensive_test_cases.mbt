// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算
  let first_term = 2
  let common_difference = 3
  let n_terms = 5
  
  // 计算等差数列的第n项: a_n = a_1 + (n-1)d
  let nth_term = add(first_term, multiply(n_terms - 1, common_difference))
  
  // 计算前n项和: S_n = n/2 * (2a_1 + (n-1)d)
  let sum_n_terms = divide_with_ceil(
    multiply(n_terms, add(multiply(2, first_term), multiply(n_terms - 1, common_difference))),
    2
  )
  
  assert_eq(14, nth_term)  // 2 + 4*3 = 14
  assert_eq(40, sum_n_terms)  // 5/2 * (4 + 12) = 40
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let project_c_cost = 2200
  
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以启动的小项目数量
  let small_project_cost = 800
  let additional_projects = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(8500, allocated_budget)
  assert_eq(1500, remaining_budget)
  assert_eq(2, additional_projects)
}

test "financial_compound_interest" {
  // 测试复利计算
  let principal = 5000
  let annual_rate = 6  // 6%
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let year1_balance = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_balance, annual_rate), 100)
  let year2_balance = add(year1_balance, year2_interest)
  
  // 第三年
  let year3_interest = divide_with_ceil(multiply(year2_balance, annual_rate), 100)
  let year3_balance = add(year2_balance, year3_interest)
  
  assert_eq(300, year1_interest)
  assert_eq(5300, year1_balance)
  assert_eq(318, year2_interest)
  assert_eq(5618, year2_balance)
  assert_eq(337, year3_interest)
  assert_eq(5955, year3_balance)
}

test "inventory_reorder_calculation" {
  // 测试库存重新订购计算
  let daily_sales = 12
  let lead_time_days = 7
  let safety_stock = 30
  
  // 计算在交货期内的销售量
  let lead_time_sales = multiply(daily_sales, lead_time_days)
  
  // 计算重新订购点
  let reorder_point = add(lead_time_sales, safety_stock)
  
  // 计算订购数量（假设订购一个月的量）
  let monthly_demand = multiply(daily_sales, 30)
  let order_quantity = monthly_demand
  
  assert_eq(84, lead_time_sales)
  assert_eq(114, reorder_point)
  assert_eq(360, order_quantity)
}

test "data_processing_batch_size" {
  // 测试数据处理批大小计算
  let total_records = 1250
  let max_batch_size = 100
  
  // 计算需要的批次数
  let number_of_batches = divide_with_ceil(total_records, max_batch_size)
  
  // 计算总处理时间（假设每批5分钟）
  let processing_time_per_batch = 5
  let total_processing_time = multiply(number_of_batches, processing_time_per_batch)
  
  // 计算最后一批的记录数
  let last_batch_size = total_records - multiply(number_of_batches - 1, max_batch_size)
  
  assert_eq(13, number_of_batches)
  assert_eq(65, total_processing_time)
  assert_eq(50, last_batch_size)
}

test "network_bandwidth_planning" {
  // 测试网络带宽规划
  let daily_data_usage_gb = 25
  let peak_hours_multiplier = 3
  let bandwidth_buffer_percentage = 20
  
  // 计算峰值使用量
  let peak_usage = multiply(daily_data_usage_gb, peak_hours_multiplier)
  
  // 计算缓冲带宽
  let buffer_bandwidth = divide_with_ceil(multiply(peak_usage, bandwidth_buffer_percentage), 100)
  
  // 计算所需的总带宽
  let required_bandwidth = add(peak_usage, buffer_bandwidth)
  
  assert_eq(75, peak_usage)
  assert_eq(15, buffer_bandwidth)
  assert_eq(90, required_bandwidth)
}

test "educational_grading_system" {
  // 测试教育评分系统
  let exam_score = 78
  let assignment_score = 85
  let project_score = 92
  let participation_score = 88
  
  // 计算加权平均分（考试40%，作业30%，项目20%，参与10%）
  let exam_weighted = divide_with_ceil(multiply(exam_score, 40), 100)
  let assignment_weighted = divide_with_ceil(multiply(assignment_score, 30), 100)
  let project_weighted = divide_with_ceil(multiply(project_score, 20), 100)
  let participation_weighted = divide_with_ceil(multiply(participation_score, 10), 100)
  
  let final_grade = add(add(add(exam_weighted, assignment_weighted), project_weighted), participation_weighted)
  
  assert_eq(31, exam_weighted)
  assert_eq(26, assignment_weighted)
  assert_eq(18, project_weighted)
  assert_eq(9, participation_weighted)
  assert_eq(84, final_grade)
}

test "agricultural_planning" {
  // 测试农业规划计算
  let field_area_hectares = 50
  let crop_yield_per_hectare = 1200  // 公斤
  let market_price_per_kg = 3  // 元
  let labor_cost_per_hectare = 800
  let equipment_cost_per_hectare = 500
  
  // 计算总产量
  let total_yield = multiply(field_area_hectares, crop_yield_per_hectare)
  
  // 计算总收入
  let total_revenue = multiply(total_yield, market_price_per_kg)
  
  // 计算总成本
  let total_labor_cost = multiply(field_area_hectares, labor_cost_per_hectare)
  let total_equipment_cost = multiply(field_area_hectares, equipment_cost_per_hectare)
  let total_cost = add(total_labor_cost, total_equipment_cost)
  
  // 计算利润
  let profit = add(total_revenue, -total_cost)
  
  assert_eq(60000, total_yield)
  assert_eq(180000, total_revenue)
  assert_eq(40000, total_labor_cost)
  assert_eq(25000, total_equipment_cost)
  assert_eq(65000, total_cost)
  assert_eq(115000, profit)
}
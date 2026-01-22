// 增强的 Azimuth 测试用例
// 包含10个高质量的新测试用例，使用标准 MoonBit 测试语法

test "temperature_conversion" {
  // 温度转换测试：摄氏度到华氏度
  // F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9) / 5, 32)
  assert_eq(77, fahrenheit)
  
  // 零度测试
  let freezing_point = add(multiply(0, 9) / 5, 32)
  assert_eq(32, freezing_point)
  
  // 负温度测试
  let negative_celsius = -10
  let negative_fahrenheit = add(multiply(negative_celsius, 9) / 5, 32)
  assert_eq(14, negative_fahrenheit)
}

test "loan_interest_calculation" {
  // 贷款利息计算测试
  let principal = 10000
  let annual_rate_percent = 6
  let years = 3
  
  // 简单利息计算：A = P * (1 + r * t)
  let total_interest = multiply(principal, multiply(annual_rate_percent, years)) / 100
  let total_amount = add(principal, total_interest)
  
  assert_eq(1800, total_interest)
  assert_eq(11800, total_amount)
}

test "batch_processing_simulation" {
  // 批处理模拟测试
  let total_records = 157
  let batch_size = 25
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_records, batch_size)
  
  // 计算最后一批的记录数
  let last_batch_size = total_records - multiply(batches_needed - 1, batch_size)
  
  assert_eq(7, batches_needed)
  assert_eq(7, last_batch_size)
}

test "game_score_calculation" {
  // 游戏得分计算测试
  let base_score = 1000
  let level_multiplier = 3
  let bonus_points = 250
  let penalty_points = 50
  
  // 计算最终得分
  let level_score = multiply(base_score, level_multiplier)
  let final_score = add(add(level_score, bonus_points), -penalty_points)
  
  assert_eq(3000, level_score)
  assert_eq(3200, final_score)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 1500
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以启动的额外小型项目数量
  let small_project_cost = 300
  let additional_projects = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(4500, allocated_budget)
  assert_eq(500, remaining_budget)
  assert_eq(2, additional_projects)
}

test "cryptographic_hash_simulation" {
  // 加密哈希模拟测试
  let input_number = 12345
  let prime_multiplier = 31
  let modulo_value = 100000
  
  // 简单的哈希函数模拟
  let hash_step1 = multiply(input_number, prime_multiplier)
  let hash_step2 = add(hash_step1, input_number)
  let final_hash = hash_step2 % modulo_value
  
  assert_eq(382760, hash_step1)
  assert_eq(395105, hash_step2)
  assert_eq(95105, final_hash)
}

test "parallel_processing_efficiency" {
  // 并行处理效率测试
  let single_core_time = 120  // 分钟
  let core_count = 4
  let parallel_efficiency_percent = 75  // 并行效率75%
  
  // 计算理论并行时间
  let theoretical_parallel_time = divide_with_ceil(single_core_time, core_count)
  
  // 考虑并行效率的实际时间
  let efficiency_factor = divide_with_ceil(multiply(theoretical_parallel_time, 100), parallel_efficiency_percent)
  
  assert_eq(30, theoretical_parallel_time)
  assert_eq(40, efficiency_factor)
}

test "data_structure_operations" {
  // 数据结构操作测试
  let array_size = 10
  let page_size = 3
  
  // 计算分页数量
  let total_pages = divide_with_ceil(array_size, page_size)
  
  // 计算完整页面的元素数量
  let full_pages = array_size / page_size
  let full_page_elements = multiply(full_pages, page_size)
  
  // 计算最后一页的元素数量
  let last_page_elements = array_size - full_page_elements
  
  assert_eq(4, total_pages)
  assert_eq(9, full_page_elements)
  assert_eq(1, last_page_elements)
}

test "financial_compound_interest" {
  // 复利计算测试
  let principal = 5000
  let annual_rate_percent = 8
  let years = 4
  
  // 逐年计算复利
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, annual_rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, annual_rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, annual_rate_percent), 100))
  let year4_amount = add(year3_amount, divide_with_ceil(multiply(year3_amount, annual_rate_percent), 100))
  
  assert_eq(5400, year1_amount)
  assert_eq(5832, year2_amount)
  assert_eq(6299, year3_amount)
  assert_eq(6803, year4_amount)
}

test "queue_simulation" {
  // 队列模拟测试
  let arrival_rate = 15  // 每小时到达人数
  let service_rate = 20   // 每小时服务人数
  let time_hours = 8      // 营业时间
  
  // 计算总到达人数和总服务人数
  let total_arrivals = multiply(arrival_rate, time_hours)
  let total_served = multiply(service_rate, time_hours)
  
  // 计算队列中剩余人数
  let remaining_in_queue = add(total_arrivals, -total_served)
  if remaining_in_queue < 0 {
    remaining_in_queue = 0
  }
  
  // 计算需要的额外服务时间（分钟）
  let additional_service_time = divide_with_ceil(multiply(remaining_in_queue, 60), service_rate)
  
  assert_eq(120, total_arrivals)
  assert_eq(160, total_served)
  assert_eq(0, remaining_in_queue)
  assert_eq(0, additional_service_time)
}
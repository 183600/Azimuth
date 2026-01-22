// 新增的 MoonBit 测试用例 - 针对 azimuth 库
// 包含实际应用场景和边界情况测试

test "financial_compound_interest" {
  // 金融复利计算场景
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate_percent), 100))
  
  @azimuth.assert_eq(1158, year3)  // 1000 -> 1050 -> 1103 -> 1158
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let team_a_cost = @azimuth.multiply(120, 8)  // 8人团队，每人120成本
  let team_b_cost = @azimuth.multiply(150, 6)  // 6人团队，每人150成本
  let remaining = @azimuth.add(@azimuth.add(total_budget, -team_a_cost), -team_b_cost)
  
  // 计算可以支持的小项目数量
  let small_projects = @azimuth.divide_with_ceil(remaining, 200)
  
  @azimuth.assert_eq(960, team_a_cost)
  @azimuth.assert_eq(900, team_b_cost) 
  @azimuth.assert_eq(3200, remaining)
  @azimuth.assert_eq(16, small_projects)
}

test "time_management_workflow" {
  // 时间管理工作流测试
  let daily_hours = 8
  let task_complexity = 3  // 复杂度倍数
  let total_tasks = 25
  
  // 计算实际需要的时间
  let adjusted_tasks = @azimuth.multiply(total_tasks, task_complexity)
  let days_needed = @azimuth.divide_with_ceil(adjusted_tasks, daily_hours)
  let total_hours = @azimuth.multiply(days_needed, daily_hours)
  
  @azimuth.assert_eq(75, adjusted_tasks)
  @azimuth.assert_eq(10, days_needed)
  @azimuth.assert_eq(80, total_hours)
}

test "inventory_restock_calculation" {
  // 库存补货计算测试
  let current_stock = 45
  let daily_sales = 8
  let lead_time_days = 7
  let safety_buffer = 20
  
  // 计算补货期间的销售量
  let sales_during_lead = @azimuth.multiply(daily_sales, lead_time_days)
  // 计算再订货点
  let reorder_point = @azimuth.add(sales_during_lead, safety_buffer)
  // 计算需要补货的数量
  let restock_quantity = @azimuth.add(reorder_point, -current_stock)
  
  @azimuth.assert_eq(56, sales_during_lead)
  @azimuth.assert_eq(76, reorder_point)
  @azimuth.assert_eq(31, restock_quantity)
}

test "data_transmission_batching" {
  // 数据传输批处理测试
  let total_records = 1000
  let batch_size = 75
  let processing_time_per_batch = 5
  
  // 计算需要的批次数
  let batches_needed = @azimuth.divide_with_ceil(total_records, batch_size)
  // 计算总处理时间
  let total_time = @azimuth.multiply(batches_needed, processing_time_per_batch)
  
  @azimuth.assert_eq(14, batches_needed)
  @azimuth.assert_eq(70, total_time)
}

test "geometric_progression_sum" {
  // 等比数列求和测试
  let first_term = 2
  let ratio = 3
  let terms = 5
  
  // 手动计算等比数列和：2 + 6 + 18 + 54 + 162
  let term1 = first_term
  let term2 = @azimuth.multiply(term1, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  let term5 = @azimuth.multiply(term4, ratio)
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  
  @azimuth.assert_eq(2, term1)
  @azimuth.assert_eq(6, term2)
  @azimuth.assert_eq(18, term3)
  @azimuth.assert_eq(54, term4)
  @azimuth.assert_eq(162, term5)
  @azimuth.assert_eq(242, sum)
}

test "string_template_greeting" {
  // 字符串模板问候测试
  let user_role = "Developer"
  let company = "TechCorp"
  let project = "Azimuth"
  
  // 构建完整的身份标识
  let full_identity = user_role + " at " + company + " working on " + project
  let formal_greeting = @azimuth.greet(full_identity)
  
  @azimuth.assert_eq_string("Hello, Developer at TechCorp working on Azimuth!", formal_greeting)
}

test "load_balancing_calculation" {
  // 负载均衡计算测试
  let total_requests = 1500
  let server_count = 4
  let overflow_threshold = 100
  
  // 计算每台服务器的基准负载
  let base_load_per_server = @azimuth.divide_with_ceil(total_requests, server_count)
  // 计算是否需要负载均衡
  let needs_balancing = base_load_per_server > overflow_threshold
  // 如果需要，计算额外的服务器需求
  let additional_servers = if needs_balancing {
    @azimuth.divide_with_ceil(base_load_per_server - overflow_threshold, overflow_threshold)
  } else {
    0
  }
  
  @azimuth.assert_eq(375, base_load_per_server)
  @azimuth.assert_eq(3, additional_servers)
}

test "energy_efficiency_metrics" {
  // 能效指标计算测试
  let old_power_consumption = 500
  let new_power_consumption = 350
  let performance_improvement = 25  // 百分比
  
  // 计算功率节省
  let power_savings = @azimuth.add(old_power_consumption, -new_power_consumption)
  // 计算节省百分比
  let savings_percentage = @azimuth.divide_with_ceil(@azimuth.multiply(power_savings, 100), old_power_consumption)
  // 计算综合效率提升
  let total_efficiency_gain = @azimuth.add(savings_percentage, performance_improvement)
  
  @azimuth.assert_eq(150, power_savings)
  @azimuth.assert_eq(30, savings_percentage)
  @azimuth.assert_eq(55, total_efficiency_gain)
}

test "network_packet_optimization" {
  // 网络数据包优化测试
  let original_packet_size = 1500
  let compression_ratio = 60  // 压缩到60%
  let overhead_per_packet = 20
  
  // 计算压缩后的包大小
  let compressed_size = @azimuth.multiply(original_packet_size, compression_ratio) / 100
  // 计算添加开销后的最终大小
  let final_size = @azimuth.add(compressed_size, overhead_per_packet)
  // 计算可以并行传输的包数量
  let parallel_packets = @azimuth.divide_with_ceil(10000, final_size)
  
  @azimuth.assert_eq(900, compressed_size)
  @azimuth.assert_eq(920, final_size)
  @azimuth.assert_eq(11, parallel_packets)
}
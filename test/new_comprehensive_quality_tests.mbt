// 新增的高质量测试用例 - 覆盖 azimuth 库的核心功能
// 使用标准 MoonBit 测试语法，包含实际应用场景

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 1500
  
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 300
  let additional_projects = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(4500, allocated_budget)
  assert_eq(500, remaining_budget)
  assert_eq(2, additional_projects)
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = 8  // 与目标时区相差8小时
  
  let target_time = add(local_time, time_difference)
  let adjusted_time = if target_time > 24 {
    add(target_time, -24)
  } else {
    target_time
  }
  
  assert_eq(22, adjusted_time)  // 14 + 8 = 22
}

test "inventory_reorder_calculation" {
  // 库存重新订购计算
  let current_stock = 45
  let daily_sales = 8
  let lead_time_days = 7
  let safety_stock = 20
  
  let expected_sales = multiply(daily_sales, lead_time_days)
  let reorder_point = add(expected_sales, safety_stock)
  let should_reorder = current_stock < reorder_point
  
  assert_eq(56, expected_sales)
  assert_eq(76, reorder_point)
  // 注意：should_reorder 是布尔值，这里我们只验证数值计算
}

test "data_processing_batch_size" {
  // 数据处理批次大小计算
  let total_records = 10000
  let max_memory_per_batch = 2500
  let overhead_factor = 10  // 10% 开销
  
  // 考虑开销后的实际批次大小
  let effective_batch_size = divide_with_ceil(
    multiply(max_memory_per_batch, 100 - overhead_factor),
    100
  )
  
  let num_batches = divide_with_ceil(total_records, effective_batch_size)
  
  assert_eq(2250, effective_batch_size)
  assert_eq(5, num_batches)
}

test "network_packet_calculation" {
  // 网络数据包计算
  let data_size = 8500
  let max_packet_size = 1500
  let header_size = 20
  
  let effective_payload = add(max_packet_size, -header_size)
  let packets_needed = divide_with_ceil(data_size, effective_payload)
  
  assert_eq(1480, effective_payload)
  assert_eq(6, packets_needed)
}

test "temperature_conversion" {
  // 温度转换计算（简化版）
  let celsius = 25
  let freezing_point = 0
  let boiling_point = 100
  
  // 转换为华氏度（简化计算）
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 1), 32)
  
  // 验证温度范围
  let is_above_freezing = celsius > freezing_point
  let is_below_boiling = celsius < boiling_point
  
  assert_eq(77, fahrenheit)  // 25 * 9/5 + 32 = 45 + 32 = 77
}

test "financial_compound_interest" {
  // 复利计算（年度复利）
  let principal = 5000
  let rate_percent = 6
  let years = 3
  
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(5300, year1_amount)
  assert_eq(5618, year2_amount)
  assert_eq(5955, year3_amount)
}

test "digital_storage_conversion" {
  // 数字存储转换计算
  let file_size_mb = 850
  let usb_drive_size_gb = 16
  let usb_drive_size_mb = multiply(usb_drive_size_gb, 1024)
  
  let available_space_mb = add(usb_drive_size_mb, -file_size_mb)
  let additional_files_mb = divide_with_ceil(available_space_mb, 50)
  
  assert_eq(16384, usb_drive_size_mb)
  assert_eq(15534, available_space_mb)
  assert_eq(311, additional_files_mb)
}

test "project_efficiency_metrics" {
  // 项目效率指标计算
  let planned_hours = 160
  let actual_hours = 142
  let tasks_completed = 28
  let total_tasks = 32
  
  let efficiency_percentage = divide_with_ceil(multiply(actual_hours, 100), planned_hours)
  let completion_percentage = divide_with_ceil(multiply(tasks_completed, 100), total_tasks)
  
  // 计算效率得分（完成率 * 效率率）
  let efficiency_score = divide_with_ceil(multiply(completion_percentage, efficiency_percentage), 100)
  
  assert_eq(89, efficiency_percentage)
  assert_eq(88, completion_percentage)
  assert_eq(79, efficiency_score)
}

test "multilingual_greeting_system" {
  // 多语言问候系统测试
  let english_name = "Alice"
  let chinese_name = "张三"
  let japanese_name = "田中"
  
  let english_greeting = greet(english_name)
  let chinese_greeting = greet(chinese_name)
  let japanese_greeting = greet(japanese_name)
  
  assert_eq_string("Hello, Alice!", english_greeting)
  assert_eq_string("Hello, 张三!", chinese_greeting)
  assert_eq_string("Hello, 田中!", japanese_greeting)
  
  // 测试特殊字符组合
  let complex_name = "User_2024@Global"
  let complex_greeting = greet(complex_name)
  assert_eq_string("Hello, User_2024@Global!", complex_greeting)
}
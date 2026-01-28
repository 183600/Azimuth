// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行补充测试

test "mathematical_identity_verification" {
  // 验证数学恒等式
  let x = 42
  let y = 17
  
  // 验证加法恒等式: (x + y) - y = x
  let sum = add(x, y)
  let difference = add(sum, -y)
  assert_eq(x, difference)
  
  // 验证乘法恒等式: (x * y) / y = x (当 y ≠ 0 时)
  let product = multiply(x, y)
  let quotient = divide_with_ceil(product, y)
  // 由于使用向上取整，只有在能整除时才相等
  if product % y == 0 {
    assert_eq(x, quotient)
  }
}

test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  let base_value = 10
  let multiplier = 3
  let offset = 7
  
  // 计算 ((base * multiplier) + offset) * multiplier
  let step1 = multiply(base_value, multiplier)
  let step2 = add(step1, offset)
  let final_result = multiply(step2, multiplier)
  
  assert_eq(111, final_result)  // ((10 * 3) + 7) * 3 = (30 + 7) * 3 = 37 * 3 = 111
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let project_a_needs = 23
  let project_b_needs = 31
  let project_c_needs = 19
  
  // 计算剩余资源
  let allocated = add(add(project_a_needs, project_b_needs), project_c_needs)
  let remaining = add(total_resources, -allocated)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 12
  let additional_projects = divide_with_ceil(remaining, small_project_cost)
  
  assert_eq(73, allocated)
  assert_eq(27, remaining)
  assert_eq(3, additional_projects)
}

test "time_based_calculations" {
  // 基于时间的计算测试
  let hours_per_day = 8
  let work_days_per_week = 5
  let tasks_per_hour = 4
  
  // 计算每周完成的总任务数
  let daily_tasks = multiply(hours_per_day, tasks_per_hour)
  let weekly_tasks = multiply(daily_tasks, work_days_per_week)
  
  // 计算完成100个任务需要的周数
  let total_tasks_needed = 100
  let weeks_needed = divide_with_ceil(total_tasks_needed, weekly_tasks)
  
  assert_eq(160, weekly_tasks)
  assert_eq(1, weeks_needed)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let interest_rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算（每年向上取整）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate), 100))
  
  // 验证结果（由于向上取整，会比标准复利略高）
  assert_eq(1158, year3_amount)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算测试
  let initial_inventory = 500
  let monthly_sales = 85
  let months_period = 6
  
  // 计算期间总销售量
  let total_sales = multiply(monthly_sales, months_period)
  
  // 计算期末库存
  let ending_inventory = add(initial_inventory, -total_sales)
  
  // 计算库存周转率
  let turnover_rate = divide_with_ceil(total_sales, initial_inventory)
  
  assert_eq(510, total_sales)
  assert_eq(-10, ending_inventory)
  assert_eq(2, turnover_rate)
}

test "data_batch_processing" {
  // 数据批处理测试
  let total_records = 1250
  let batch_size = 100
  
  // 计算需要的批次数
  let total_batches = divide_with_ceil(total_records, batch_size)
  
  // 计算最后一批的记录数
  let full_batches = total_records / batch_size
  let last_batch_size = add(total_records, -multiply(full_batches, batch_size))
  
  assert_eq(13, total_batches)
  assert_eq(50, last_batch_size)
}

test "network_packet_routing" {
  // 网络数据包路由测试
  let total_packets = 1024
  let route_capacity = 256
  let backup_route_capacity = 128
  
  // 计算主路由需要的批次
  let primary_batches = divide_with_ceil(total_packets, route_capacity)
  
  // 计算如果主路由故障，备用路由需要的批次
  let backup_batches = divide_with_ceil(total_packets, backup_route_capacity)
  
  assert_eq(4, primary_batches)
  assert_eq(8, backup_batches)
}

test "energy_efficiency_metrics" {
  // 能效指标计算测试
  let power_consumption_watts = 250
  let operating_hours = 24
  let days_in_month = 30
  
  // 计算月度能耗（千瓦时）
  let daily_kwh = divide_with_ceil(multiply(power_consumption_watts, operating_hours), 1000)
  let monthly_kwh = multiply(daily_kwh, days_in_month)
  
  // 计算能效评级（基于简化标准）
  let efficiency_rating = divide_with_ceil(monthly_kwh, 100)
  
  assert_eq(6, daily_kwh)  // 向上取整
  assert_eq(180, monthly_kwh)
  assert_eq(2, efficiency_rating)
}

test "string_pattern_validation" {
  // 字符串模式验证测试
  let valid_inputs = ["user123", "test_001", "admin42"]
  let invalid_inputs = ["", "a", "very_long_username_exceeding_limit"]
  
  // 测试有效输入的问候功能
  assert_eq_string("Hello, user123!", greet(valid_inputs[0]))
  assert_eq_string("Hello, test_001!", greet(valid_inputs[1]))
  assert_eq_string("Hello, admin42!", greet(valid_inputs[2]))
  
  // 测试边界情况
  assert_eq_string("Hello, !", greet(invalid_inputs[0]))
  assert_eq_string("Hello, a!", greet(invalid_inputs[1]))
  assert_eq_string("Hello, very_long_username_exceeding_limit!", greet(invalid_inputs[2]))
}
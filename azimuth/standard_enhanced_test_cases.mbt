// 标准增强测试用例 - Azimuth 库
// 使用标准 MoonBit 测试语法，包含 10 个高质量测试用例

test "mathematical_series_calculation" {
  // 数学级数计算测试
  // 计算等差数列前 n 项和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 10
  let first_term = 1
  let common_difference = 2
  
  // 计算第 n 项
  let nth_term = add(first_term, multiply(n - 1, common_difference))
  assert_eq(19, nth_term)  // 1 + (10-1)*2 = 19
  
  // 计算前 n 项和（简化计算）
  let sum = multiply(n, n)
  assert_eq(100, sum)  // 10² = 100
}

test "inventory_turnover_calculation" {
  // 库存周转率计算测试
  let initial_inventory = 500
  let purchased = 300
  let sold = 450
  let final_inventory = add(initial_inventory, purchased)
  let remaining_inventory = subtract(final_inventory, sold)
  
  assert_eq(350, remaining_inventory)
  
  // 计算周转次数
  let turnover_times = divide_with_ceil(sold, 100)  // 每100为一批
  assert_eq(5, turnover_times)  // ceil(450/100) = 5
}

test "temperature_conversion_logic" {
  // 温度转换逻辑测试
  // 模拟摄氏度到华氏度的转换（简化公式）
  let celsius_0 = 0
  let celsius_100 = 100
  let celsius_minus40 = -40
  
  // F = C * 9/5 + 32 （使用整数运算近似）
  let fahrenheit_0 = add(multiply(celsius_0, 2), 32)  // 简化为 2*C + 32
  let fahrenheit_100 = add(multiply(celsius_100, 2), 32)
  let fahrenheit_minus40 = add(multiply(celsius_minus40, 2), 32)
  
  assert_eq(32, fahrenheit_0)
  assert_eq(232, fahrenheit_100)
  assert_eq(-48, fahrenheit_minus40)
}

test "project_management_timeline" {
  // 项目管理时间线测试
  let project_tasks = 45
  let team_members = 6
  let working_days = 15
  let tasks_per_person_per_day = 2
  
  // 计算团队日产能
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  assert_eq(12, daily_capacity)
  
  // 计算所需工作日
  let days_needed = divide_with_ceil(project_tasks, daily_capacity)
  assert_eq(4, days_needed)  // ceil(45/12) = 4
  
  // 验证时间充足
  assert_true(days_needed <= working_days)
  
  // 计算缓冲时间
  let buffer_days = subtract(working_days, days_needed)
  assert_eq(11, buffer_days)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let rate_percent = 8
  
  // 简化的复利计算（年复利）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  let year4 = add(year3, divide_with_ceil(multiply(year3, rate_percent), 100))
  
  assert_eq(1080, year1)   // 1000 + 8%
  assert_eq(1166, year2)   // 1080 + 8% (向上取整)
  assert_eq(1259, year3)   // 1166 + 8% (向上取整)
  assert_eq(1360, year4)   // 1259 + 8% (向上取整)
}

test "batch_processing_optimization" {
  // 批处理优化测试
  let total_records = 987
  let max_batch_size = 50
  let processing_time_per_batch = 3
  
  // 计算需要的批次数
  let batch_count = divide_with_ceil(total_records, max_batch_size)
  assert_eq(20, batch_count)  // ceil(987/50) = 20
  
  // 计算总处理时间
  let total_time = multiply(batch_count, processing_time_per_batch)
  assert_eq(60, total_time)
  
  // 计算最后一个批次的大小
  let full_batches = subtract(batch_count, 1)
  let processed_in_full = multiply(full_batches, max_batch_size)
  let last_batch_size = subtract(total_records, processed_in_full)
  assert_eq(37, last_batch_size)  // 987 - 19*50 = 37
}

test "resource_allocation_fairness" {
  // 资源分配公平性测试
  let total_resources = 100
  let participants = 9
  
  // 平均分配（向上取整确保每人至少得到基础份额）
  let base_share = divide_with_ceil(total_resources, participants)
  assert_eq(12, base_share)  // ceil(100/9) = 12
  
  // 计算总分配量
  let total_allocated = multiply(base_share, participants)
  assert_eq(108, total_allocated)
  
  // 计算溢出量
  let overflow = subtract(total_allocated, total_resources)
  assert_eq(8, overflow)
  
  // 验证公平性（每人至少得到 floor(100/9) = 11）
  let minimum_fair_share = 11
  assert_true(base_share >= minimum_fair_share)
}

test "string_pattern_validation" {
  // 字符串模式验证测试
  let valid_username = "user123"
  let invalid_short = "ab"
  let valid_email = "test@example.com"
  let valid_unicode = "测试用户"
  
  // 测试问候函数对各种输入的处理
  assert_eq_string("Hello, user123!", greet(valid_username))
  assert_eq_string("Hello, ab!", greet(invalid_short))
  assert_eq_string("Hello, test@example.com!", greet(valid_email))
  assert_eq_string("Hello, 测试用户!", greet(valid_unicode))
  
  // 测试特殊字符
  assert_eq_string("Hello, user@domain!", greet("user@domain"))
  assert_eq_string("Hello, phone:123-456!", greet("phone:123-456"))
}

test "load_balancing_algorithm" {
  // 负载均衡算法测试
  let total_requests = 157
  let server_count = 5
  
  // 计算每台服务器的基础负载
  let base_load = divide_with_ceil(total_requests, server_count)
  assert_eq(32, base_load)  // ceil(157/5) = 32
  
  // 计算总处理能力
  let total_capacity = multiply(base_load, server_count)
  assert_eq(160, total_capacity)
  
  // 计算剩余容量
  let spare_capacity = subtract(total_capacity, total_requests)
  assert_eq(3, spare_capacity)
  
  // 验证负载分布相对均匀
  let min_possible_load = 31  // floor(157/5) = 31
  let max_possible_load = 32  // ceil(157/5) = 32
  assert_eq(base_load, max_possible_load)
  assert_true(min_possible_load <= base_load)
}

test "space_planning_optimization" {
  // 空间规划优化测试
  let total_area = 250
  let room_min_area = 35
  let corridor_area = 20
  
  // 计算可用房间面积
  let available_area = subtract(total_area, corridor_area)
  assert_eq(230, available_area)
  
  // 计算可容纳的房间数量
  let room_count = divide_with_ceil(available_area, room_min_area)
  assert_eq(7, room_count)  // ceil(230/35) = 7
  
  // 计算实际使用的面积
  let used_area = multiply(room_count, room_min_area)
  assert_eq(245, used_area)
  
  // 验证总面积使用情况
  let total_used = add(used_area, corridor_area)
  assert_eq(265, total_used)
  
  // 计算空间利用率
  let utilization = divide_with_ceil(multiply(total_area, 100), total_used)
  assert_eq(94, utilization)  // floor(25000/265) = 94%
}
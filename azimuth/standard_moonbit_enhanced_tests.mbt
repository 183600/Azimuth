// 标准 MoonBit 增强测试用例
// 为 azimuth 库添加额外的高质量测试覆盖

test "arithmetiC_sequence_operations" {
  // 等差数列运算测试
  let first_term = 3
  let common_difference = 5
  let num_terms = 4
  
  // 计算第n项：a_n = a_1 + (n-1)d
  let nth_term = add(first_term, multiply(num_terms - 1, common_difference))
  assert_eq(18, nth_term)  // 3 + 3*5 = 18
  
  // 计算前n项和：S_n = n/2 * (a_1 + a_n)
  let sum = divide_with_ceil(multiply(num_terms, add(first_term, nth_term)), 2)
  assert_eq(42, sum)  // 4/2 * (3+18) = 42
}

test "geometric_progression_calculations" {
  // 等比数列计算测试
  let first_term = 2
  let common_ratio = 3
  let num_terms = 4
  
  // 计算第n项：a_n = a_1 * r^(n-1)
  let nth_term = multiply(first_term, multiply(common_ratio, multiply(common_ratio, common_ratio)))
  assert_eq(54, nth_term)  // 2 * 3^3 = 54
  
  // 计算需要的容器数量
  let items_per_container = 10
  let containers_needed = divide_with_ceil(nth_term, items_per_container)
  assert_eq(6, containers_needed)  // ceil(54/10) = 6
}

test "temperature_conversion_logic" {
  // 温度转换逻辑测试
  // 摄氏度到华氏度：F = C * 9/5 + 32
  let celsius_temp = 25
  let fahrenheit_temp = add(multiply(celsius_temp, divide_with_ceil(9, 5)), 32)
  assert_eq(77, fahrenheit_temp)  // 25*9/5 + 32 = 77
  
  // 华氏度到摄氏度：C = (F - 32) * 5/9
  let fahrenheit_temp_2 = 86
  let celsius_temp_2 = divide_with_ceil(multiply(fahrenheit_temp_2 - 32, 5), 9)
  assert_eq(30, celsius_temp_2)  // (86-32)*5/9 ≈ 30
}

test "inventory_turnover_calculation" {
  // 库存周转率计算测试
  let annual_sales = 12000
  let average_inventory = 3000
  
  // 库存周转率 = 年销售额 / 平均库存
  let turnover_rate = divide_with_ceil(annual_sales, average_inventory)
  assert_eq(4, turnover_rate)  // ceil(12000/3000) = 4
  
  // 计算平均销售周期（天）
  let days_per_year = 365
  let average_days = divide_with_ceil(days_per_year, turnover_rate)
  assert_eq(92, average_days)  // ceil(365/4) = 92
}

test "project_management_metrics" {
  // 项目管理指标测试
  let total_tasks = 150
  let completed_tasks = 87
  let team_members = 8
  let days_elapsed = 15
  
  // 完成率
  let completion_rate = divide_with_ceil(multiply(completed_tasks, 100), total_tasks)
  assert_eq(58, completion_rate)  // ceil(87*100/150) = 58%
  
  // 每人每天完成任务数
  let daily_productivity = divide_with_ceil(completed_tasks, multiply(team_members, days_elapsed))
  assert_eq(1, daily_productivity)  // ceil(87/120) = 1
  
  // 预计完成天数
  let remaining_tasks = subtract(total_tasks, completed_tasks)
  let daily_capacity = multiply(team_members, daily_productivity)
  let days_needed = divide_with_ceil(remaining_tasks, daily_capacity)
  assert_eq(63, days_needed)  // ceil(63/8) = 8
}

test "financial_compound_interest" {
  // 复利计算测试
  let principal = 10000
  let annual_rate = 6  // 6%
  let years = 3
  
  // 简化的年复利计算
  let year1_end = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_end = add(year1_end, divide_with_ceil(multiply(year1_end, annual_rate), 100))
  let year3_end = add(year2_end, divide_with_ceil(multiply(year2_end, annual_rate), 100))
  
  assert_eq(10600, year1_end)  // 10000 + 6%
  assert_eq(11236, year2_end)  // 10600 + 6%
  assert_eq(11911, year3_end)  // 11236 + 6%
  
  // 计算月供
  let total_months = multiply(years, 12)
  let monthly_payment = divide_with_ceil(year3_end, total_months)
  assert_eq(331, monthly_payment)  // ceil(11911/36)
}

test "logistics_optimization" {
  // 物流优化测试
  let total_weight = 1250  // 总重量(kg)
  let truck_capacity = 300  // 卡车容量(kg)
  let cost_per_truck = 150  // 每辆卡车成本
  let handling_cost_per_kg = 2  // 每公斤处理成本
  
  // 计算需要的卡车数量
  let trucks_needed = divide_with_ceil(total_weight, truck_capacity)
  assert_eq(5, trucks_needed)  // ceil(1250/300) = 5
  
  // 计算运输成本
  let transport_cost = multiply(trucks_needed, cost_per_truck)
  assert_eq(750, transport_cost)  // 5 * 150
  
  // 计算处理成本
  let handling_cost = multiply(total_weight, handling_cost_per_kg)
  assert_eq(2500, handling_cost)  // 1250 * 2
  
  // 计算总成本
  let total_cost = add(transport_cost, handling_cost)
  assert_eq(3250, total_cost)
}

test "time_zone_conversion" {
  // 时区转换测试
  let local_time = 14  // 本地时间 14:00
  let time_difference = -8  // 与目标时区相差-8小时
  
  // 转换到目标时区
  let target_time = local_time + time_difference
  let normalized_time = 
    if target_time < 0 { target_time + 24 }
    else if target_time >= 24 { target_time - 24 }
    else { target_time }
  assert_eq(6, normalized_time)  // 14 - 8 = 6
  
  // 计算最佳会议时间（9-17点之间）
  let meeting_time = 10  // 建议会议时间
  let local_meeting_time = meeting_time - time_difference
  let normalized_meeting = 
    if local_meeting_time >= 24 { local_meeting_time - 24 }
    else { local_meeting_time }
  assert_eq(18, normalized_meeting)  // 10 + 8 = 18
}

test "data_compression_metrics" {
  // 数据压缩指标测试
  let original_size = 10000  // 原始大小(KB)
  let compressed_size = 2500  // 压缩后大小(KB)
  let transfer_speed = 500  // 传输速度(KB/s)
  
  // 计算压缩率
  let compression_ratio = divide_with_ceil(multiply(compressed_size, 100), original_size)
  assert_eq(25, compression_ratio)  // ceil(2500*100/10000) = 25%
  
  // 计算原始传输时间
  let original_transfer_time = divide_with_ceil(original_size, transfer_speed)
  assert_eq(20, original_transfer_time)  // ceil(10000/500) = 20秒
  
  // 计算压缩后传输时间
  let compressed_transfer_time = divide_with_ceil(compressed_size, transfer_speed)
  assert_eq(5, compressed_transfer_time)  // ceil(2500/500) = 5秒
  
  // 计算节省的时间
  let time_saved = subtract(original_transfer_time, compressed_transfer_time)
  assert_eq(15, time_saved)
}

test "resource_allocation_fairness" {
  // 资源分配公平性测试
  let total_resources = 100
  let team_a_members = 7
  let team_b_members = 5
  let team_c_members = 8
  let total_members = add(team_a_members, add(team_b_members, team_c_members))
  
  // 按比例分配资源
  let team_a_share = divide_with_ceil(multiply(total_resources, team_a_members), total_members)
  let team_b_share = divide_with_ceil(multiply(total_resources, team_b_members), total_members)
  let team_c_share = divide_with_ceil(multiply(total_resources, team_c_members), total_members)
  
  assert_eq(35, team_a_share)  // ceil(100*7/20) = 35
  assert_eq(25, team_b_share)  // ceil(100*5/20) = 25
  assert_eq(40, team_c_share)  // ceil(100*8/20) = 40
  
  // 验证总分配
  let total_allocated = add(team_a_share, add(team_b_share, team_c_share))
  assert_eq(100, total_allocated)
  
  // 计算分配公平性（最大份额与最小份额的比率）
  let max_share = team_c_share
  let min_share = team_b_share
  let fairness_ratio = divide_with_ceil(multiply(max_share, 100), min_share)
  assert_eq(160, fairness_ratio)  // ceil(40*100/25) = 160%
}
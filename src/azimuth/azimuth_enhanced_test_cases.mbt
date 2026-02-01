// Azimuth 增强测试用例
// 使用标准 MoonBit 测试语法，包含 8 个高质量测试用例

test "mathematical_series_calculation" {
  // 数学级数计算测试
  // 计算等差数列前 n 项和：S = n/2 * (2a + (n-1)d)
  let n = 10
  let a = 3
  let d = 2
  
  // 手动计算各项和
  let term1 = a
  let term2 = add(a, d)
  let term3 = add(term2, d)
  let term4 = add(term3, d)
  let term5 = add(term4, d)
  
  let partial_sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(35, partial_sum)  // 3 + 5 + 7 + 9 + 11 = 35
  
  // 验证级数性质
  let average = divide_with_ceil(add(term1, term5), 2)
  let sum_formula = multiply(5, average)
  assert_eq(35, sum_formula)
}

test "resource_allocation_matrix" {
  // 资源分配矩阵测试
  // 场景：多项目多团队资源分配
  let project_a_tasks = 45
  let project_b_tasks = 38
  let project_c_tasks = 27
  
  let team_x_capacity = 15
  let team_y_capacity = 12
  let team_z_capacity = 10
  
  // 为每个项目分配团队
  let teams_for_a = divide_with_ceil(project_a_tasks, team_x_capacity)
  let teams_for_b = divide_with_ceil(project_b_tasks, team_y_capacity)
  let teams_for_c = divide_with_ceil(project_c_tasks, team_z_capacity)
  
  assert_eq(3, teams_for_a)  // ceil(45/15) = 3
  assert_eq(4, teams_for_b)  // ceil(38/12) = 4
  assert_eq(3, teams_for_c)  // ceil(27/10) = 3
  
  // 计算总团队需求
  let total_teams = add(add(teams_for_a, teams_for_b), teams_for_c)
  assert_eq(10, total_teams)
  
  // 计算总处理能力
  let total_capacity = add(
    multiply(teams_for_a, team_x_capacity),
    add(multiply(teams_for_b, team_y_capacity), multiply(teams_for_c, team_z_capacity))
  )
  assert_eq(141, total_capacity)
}

test "time_zone_conversion_logic" {
  // 时区转换逻辑测试
  // 场景：计算跨时区会议时间
  let base_hour = 14  // 下午2点
  let timezone_diff = 8  // 时差8小时
  
  // 向前转换时间
  let forward_time = add(base_hour, timezone_diff)
  let adjusted_forward = divide_with_ceil(forward_time, 24)
  let final_forward = subtract(forward_time, multiply(adjusted_forward, 24))
  assert_eq(22, final_forward)  // (14 + 8) % 24 = 22
  
  // 向后转换时间
  let backward_time = subtract(base_hour, timezone_diff)
  let adjusted_backward = divide_with_ceil(subtract(0, backward_time), 24)
  let final_backward = add(backward_time, multiply(adjusted_backward, 24))
  assert_eq(6, final_backward)  // (14 - 8 + 24) % 24 = 6
}

test "inventory_reorder_calculation" {
  // 库存重新订购计算测试
  // 场景：基于当前库存和消耗率计算重新订购点
  let current_stock = 125
  let daily_consumption = 8
  let lead_time_days = 7
  let safety_stock = 20
  
  // 计算 lead time 期间的总消耗
  let lead_time_consumption = multiply(daily_consumption, lead_time_days)
  assert_eq(56, lead_time_consumption)  // 8 * 7 = 56
  
  // 计算重新订购点
  let reorder_point = add(lead_time_consumption, safety_stock)
  assert_eq(76, reorder_point)  // 56 + 20 = 76
  
  // 计算剩余可使用天数
  let days_until_reorder = divide_with_ceil(subtract(current_stock, reorder_point), daily_consumption)
  assert_eq(7, days_until_reorder)  // ceil((125-76)/8) = 7
}

test "batch_processing_optimization" {
  // 批处理优化测试
  // 场景：优化批处理大小以提高效率
  let total_items = 987
  let min_batch_size = 50
  let max_batch_size = 100
  let processing_cost_per_batch = 10
  
  // 计算最小批次数
  let min_batches = divide_with_ceil(total_items, max_batch_size)
  assert_eq(10, min_batches)  // ceil(987/100) = 10
  
  // 计算最大批次数
  let max_batches = divide_with_ceil(total_items, min_batch_size)
  assert_eq(20, max_batches)  // ceil(987/50) = 20
  
  // 计算最优批次大小（接近平均值）
  let optimal_batch_size = divide_with_ceil(add(min_batch_size, max_batch_size), 2)
  let optimal_batches = divide_with_ceil(total_items, optimal_batch_size)
  assert_eq(13, optimal_batches)  // ceil(987/75) = 13
  
  // 计算总处理成本
  let total_cost = multiply(optimal_batches, processing_cost_per_batch)
  assert_eq(130, total_cost)  // 13 * 10 = 130
}

test "financial_compound_interest" {
  // 金融复利计算测试
  // 场景：计算复利投资增长
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简化的年复利计算（整数运算）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, annual_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, annual_rate), 100))
  
  assert_eq(10500, year1_amount)  // 10000 + 500
  assert_eq(11025, year2_amount)  // 10500 + 525
  assert_eq(11576, year3_amount)  // 11025 + 551
  
  // 计算总收益
  let total_return = subtract(year3_amount, principal)
  assert_eq(1576, total_return)  // 11576 - 10000 = 1576
  
  // 计算年化收益率
  let annual_return = divide_with_ceil(total_return, years)
  assert_eq(525, annual_return)  // ceil(1576/3) = 525
}

test "load_balancing_algorithm" {
  // 负载均衡算法测试
  // 场景：将请求分配到多个服务器
  let total_requests = 2547
  let server_count = 8
  let max_requests_per_server = 400
  
  // 平均分配请求
  let base_requests_per_server = divide_with_ceil(total_requests, server_count)
  assert_eq(319, base_requests_per_server)  // ceil(2547/8) = 319
  
  // 验证是否超过单服务器最大限制
  let needs_scaling = base_requests_per_server > max_requests_per_server
  assert_true(needs_scaling)
  
  // 计算需要的服务器数量
  let required_servers = divide_with_ceil(total_requests, max_requests_per_server)
  assert_eq(7, required_servers)  // ceil(2547/400) = 7
  
  // 计算实际每服务器负载
  let actual_load_per_server = divide_with_ceil(total_requests, required_servers)
  assert_eq(364, actual_load_per_server)  // ceil(2547/7) = 364
  
  // 验证负载均衡
  let load_variance = subtract(actual_load_per_server, max_requests_per_server)
  assert_eq(-36, load_variance)  // 实际负载在限制内
}

test "digital_signal_processing" {
  // 数字信号处理测试
  // 场景：采样率和缓冲区计算
  let sample_rate = 44100  // Hz
  let buffer_duration_ms = 100  // 毫秒
  let channels = 2  // 立体声
  
  // 计算缓冲区大小（样本数）
  let buffer_duration_samples = divide_with_ceil(
    multiply(sample_rate, buffer_duration_ms), 
    1000
  )
  assert_eq(4410, buffer_duration_samples)  // ceil(44100*100/1000) = 4410
  
  // 计算总样本数（包括所有通道）
  let total_samples = multiply(buffer_duration_samples, channels)
  assert_eq(8820, total_samples)  // 4410 * 2 = 8820
  
  // 计算数据吞吐量（字节/秒，假设每样本2字节）
  let bytes_per_sample = 2
  let throughput_per_second = multiply(sample_rate, multiply(channels, bytes_per_sample))
  assert_eq(176400, throughput_per_second)  // 44100 * 2 * 2 = 176400
  
  // 计算缓冲区间隔
  let buffers_per_second = divide_with_ceil(1000, buffer_duration_ms)
  assert_eq(10, buffers_per_second)  // ceil(1000/100) = 10
}
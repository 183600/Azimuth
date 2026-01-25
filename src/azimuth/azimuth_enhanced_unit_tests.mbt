// Azimuth 增强单元测试 - 10个标准 MoonBit 测试用例
// 测试覆盖：数学运算、边界条件、实际应用场景

test "bitwise_operations_simulation" {
  // 模拟位运算：通过乘法和除法实现左移和右移
  let value = 5
  let shift_left_by_2 = multiply(value, 4)  // 5 * 2^2 = 20
  let shift_right_by_1 = divide_with_ceil(value, 2)  // ceil(5/2) = 3
  
  assert_eq(20, shift_left_by_2)
  assert_eq(3, shift_right_by_1)
}

test "compound_interest_calculation" {
  // 复利计算：本金1000，年利率5%，3年
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1158, year3)  // 1000 -> 1050 -> 1103 -> 1158
}

test "array_index_calculation" {
  // 数组索引计算：模拟二维数组转一维数组的索引计算
  let row = 3
  let col = 4
  let cols_per_row = 5
  
  let linear_index = add(multiply(row, cols_per_row), col)
  assert_eq(19, linear_index)  // 3*5 + 4 = 19
}

test "circular_buffer_index" {
  // 循环缓冲区索引计算
  let buffer_size = 8
  let current_position = 6
  let steps_forward = 5
  
  let new_position = add(current_position, steps_forward) % buffer_size
  assert_eq(3, new_position)  // (6 + 5) % 8 = 3
}

test "temperature_range_analysis" {
  // 温度范围分析：计算一周的平均温度（向上取整）
  let temps = [15, 18, 22, 20, 17, 19, 21]
  let sum = add(add(add(add(add(add(temps[0], temps[1]), temps[2]), temps[3]), temps[4]), temps[5]), temps[6])
  let average = divide_with_ceil(sum, 7)
  
  assert_eq(19, average)  // ceil(132/7) = 19
}

test "resource_allocation_optimization" {
  // 资源分配优化：将资源均匀分配给项目
  let total_resources = 47
  let num_projects = 5
  let resources_per_project = divide_with_ceil(total_resources, num_projects)
  let total_allocated = multiply(resources_per_project, num_projects)
  let unused_resources = add(total_allocated, -total_resources)
  
  assert_eq(10, resources_per_project)  // ceil(47/5) = 10
  assert_eq(50, total_allocated)  // 10 * 5 = 50
  assert_eq(3, unused_resources)  // 50 - 47 = 3
}

test "pagination_calculation" {
  // 分页计算：计算总页数
  let total_items = 123
  let items_per_page = 10
  let total_pages = divide_with_ceil(total_items, items_per_page)
  
  assert_eq(13, total_pages)  // ceil(123/10) = 13
}

test "load_balancing_simulation" {
  // 负载均衡模拟：将任务分配给服务器
  let total_requests = 75
  let num_servers = 4
  let base_requests_per_server = divide_with_ceil(total_requests, num_servers)
  let total_handled = multiply(base_requests_per_server, num_servers)
  
  assert_eq(19, base_requests_per_server)  // ceil(75/4) = 19
  assert_eq(76, total_handled)  // 19 * 4 = 76
}

test "game_score_calculation" {
  // 游戏得分计算：基础分 + 连击奖励
  let base_score = 100
  let combo_multiplier = 3
  let combo_bonus = multiply(base_score, combo_multiplier)
  let final_score = add(base_score, combo_bonus)
  
  assert_eq(400, final_score)  // 100 + 100*3 = 400
}

test "network_packet_fragmentation" {
  // 网络数据包分片计算
  let data_size = 1500
  let mtu = 512  // 最大传输单元
  let num_fragments = divide_with_ceil(data_size, mtu)
  let last_fragment_size = data_size % mtu
  
  assert_eq(3, num_fragments)  // ceil(1500/512) = 3
  assert_eq(476, last_fragment_size)  // 1500 % 512 = 476
}
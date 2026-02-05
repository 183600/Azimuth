// 额外的标准 MoonBit 测试用例
// 为 azimuth 项目添加的高质量测试

test "arithmetic_sequence_calculation" {
  // 等差数列计算测试
  let first_term = 3
  let common_difference = 4
  let n_terms = 6
  
  // 计算第n项：a_n = a_1 + (n-1)*d
  let nth_term = add(first_term, multiply(common_difference, n_terms - 1))
  
  // 计算前n项和：S_n = n/2 * (2*a_1 + (n-1)*d)
  let sum_n = divide_with_ceil(
    multiply(n_terms, add(multiply(2, first_term), multiply(common_difference, n_terms - 1))),
    2
  )
  
  assert_eq(23, nth_term)  // 3 + 5*4 = 23
  assert_eq(78, sum_n)     // 6/2 * (6 + 20) = 3 * 26 = 78
}

test "temperature_conversion" {
  // 温度转换测试
  let celsius = 25
  
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let back_to_celsius = divide_with_ceil(multiply(subtract(fahrenheit, 32), 5), 9)
  
  assert_eq(77, fahrenheit)      // 25 * 9/5 + 32 = 45 + 32 = 77
  assert_eq(25, back_to_celsius) // (77 - 32) * 5/9 = 45 * 5/9 = 25
}

test "batch_processing_simulation" {
  // 批处理模拟测试
  let total_records = 1047
  let batch_size = 100
  
  // 计算需要的批次数
  let batch_count = divide_with_ceil(total_records, batch_size)
  
  // 计算最后一批的记录数
  let last_batch_size = subtract(total_records, multiply(batch_count - 1, batch_size))
  
  assert_eq(11, batch_count)      // ceil(1047/100) = 11
  assert_eq(47, last_batch_size)  // 1047 - 10*100 = 47
}

test "geometric_progression" {
  // 等比数列测试
  let first_term = 2
  let ratio = 3
  let n_terms = 5
  
  // 计算第n项：a_n = a_1 * r^(n-1)
  let nth_term = multiply(first_term, multiply(ratio, multiply(ratio, multiply(ratio, ratio))))
  
  // 计算前n项和：S_n = a_1 * (r^n - 1) / (r - 1)
  let sum_n = divide_with_ceil(
    multiply(first_term, subtract(multiply(ratio, multiply(ratio, multiply(ratio, multiply(ratio, ratio)))), 1)),
    subtract(ratio, 1)
  )
  
  assert_eq(162, nth_term)  // 2 * 3^4 = 2 * 81 = 162
  assert_eq(242, sum_n)     // 2 * (3^5 - 1) / 2 = 2 * (243 - 1) / 2 = 242
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 500
  let project_a_needs = 120
  let project_b_needs = 180
  let project_c_needs = 90
  
  // 分配资源给各项目
  let allocated_a = multiply(project_a_needs, 2)
  let allocated_b = multiply(project_b_needs, 2)
  let allocated_c = multiply(project_c_needs, 2)
  
  // 计算总分配量
  let total_allocated = add(add(allocated_a, allocated_b), allocated_c)
  
  // 计算剩余资源
  let remaining = subtract(total_resources, total_allocated)
  
  // 计算可以支持的新项目数量
  let new_projects = divide_with_ceil(remaining, 50)
  
  assert_eq(240, allocated_a)
  assert_eq(360, allocated_b)
  assert_eq(180, allocated_c)
  assert_eq(-280, total_allocated)  // 超出预算
  assert_eq(780, remaining)          // 实际需要更多资源
  assert_eq(16, new_projects)        // 需要额外资源的项目数
}

test "data_structure_operations" {
  // 数据结构操作测试
  let array_size = 10
  let page_size = 3
  
  // 计算需要的页数
  let page_count = divide_with_ceil(array_size, page_size)
  
  // 计算总容量
  let total_capacity = multiply(page_count, page_size)
  
  // 计算浪费的空间
  let wasted_space = subtract(total_capacity, array_size)
  
  assert_eq(4, page_count)      // ceil(10/3) = 4
  assert_eq(12, total_capacity) // 4 * 3 = 12
  assert_eq(2, wasted_space)    // 12 - 10 = 2
}

test "financial_compound_interest" {
  // 复利计算测试
  let principal = 10000
  let annual_rate = 8
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, annual_rate), 100))
  
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, annual_rate), 100))
  
  // 计算总收益
  let total_interest = subtract(year3, principal)
  
  assert_eq(10800, year1)
  assert_eq(11664, year2)
  assert_eq(12597, year3)
  assert_eq(2597, total_interest)
}

test "load_balancing_simulation" {
  // 负载均衡模拟测试
  let total_requests = 157
  let server_count = 8
  
  // 计算每台服务器的基本负载
  let base_load = divide_with_ceil(total_requests, server_count)
  
  // 计算总处理能力
  let total_capacity = multiply(base_load, server_count)
  
  // 计算剩余处理能力
  let spare_capacity = subtract(total_capacity, total_requests)
  
  assert_eq(20, base_load)        // ceil(157/8) = 20
  assert_eq(160, total_capacity)  // 20 * 8 = 160
  assert_eq(3, spare_capacity)    // 160 - 157 = 3
}

test "caching_strategy_evaluation" {
  // 缓存策略评估测试
  let total_data_items = 250
  let cache_size = 60
  let hit_rate = 75  // 百分比
  
  // 计算缓存命中数
  let cache_hits = divide_with_ceil(multiply(total_data_items, hit_rate), 100)
  
  // 计算缓存未命中数
  let cache_misses = subtract(total_data_items, cache_hits)
  
  // 计算缓存利用率
  let utilization = divide_with_ceil(multiply(cache_size, 100), total_data_items)
  
  assert_eq(188, cache_hits)    // ceil(250 * 75 / 100) = 188
  assert_eq(62, cache_misses)   // 250 - 188 = 62
  assert_eq(24, utilization)    // ceil(60 * 100 / 250) = 24
}

test "network_packet_routing" {
  // 网络数据包路由测试
  let total_packets = 1024
  let route_capacity = 150
  let backup_routes = 2
  
  // 计算主路由需要的批次
  let primary_batches = divide_with_ceil(total_packets, route_capacity)
  
  // 计算主路由处理的数据包
  let primary_processed = multiply(primary_batches, route_capacity)
  
  // 计算需要备用路由处理的数据包
  let overflow = subtract(primary_processed, total_packets)
  
  // 计算备用路由的负载
  let backup_load = divide_with_ceil(overflow, backup_routes)
  
  assert_eq(7, primary_batches)   // ceil(1024/150) = 7
  assert_eq(1050, primary_processed) // 7 * 150 = 1050
  assert_eq(26, overflow)         // 1050 - 1024 = 26
  assert_eq(13, backup_load)      // ceil(26/2) = 13
}
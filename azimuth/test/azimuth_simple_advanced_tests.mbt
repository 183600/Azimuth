// Azimuth 简化高级测试用例 - 10个标准 MoonBit 测试用例
// 只使用 assert_eq 和 assert_eq_string 函数以确保兼容性

test "azimuth_bearing_calculations" {
  // 方位角计算测试
  let north = 0
  let east = 90
  let south = 180
  let west = 270
  
  // 计算方位角差值
  let ne_diff = subtract(east, north)      // 90 - 0 = 90
  let se_diff = subtract(south, east)      // 180 - 90 = 90
  let sw_diff = subtract(west, south)      // 270 - 180 = 90
  
  assert_eq(90, ne_diff)
  assert_eq(90, se_diff)
  assert_eq(90, sw_diff)
  
  // 计算象限
  let quadrant1 = divide_with_ceil(east, 90)      // ceil(90/90) = 1
  let quadrant2 = divide_with_ceil(south, 90)     // ceil(180/90) = 2
  let quadrant3 = divide_with_ceil(west, 90)      // ceil(270/90) = 3
  
  assert_eq(1, quadrant1)
  assert_eq(2, quadrant2)
  assert_eq(3, quadrant3)
}

test "time_zone_calculations" {
  // 时区转换计算
  let local_time = 15  // 下午3点
  let utc_offset = -5  // UTC-5
  
  // 转换为UTC时间
  let utc_time = add(local_time, utc_offset)  // 15 + (-5) = 10
  assert_eq(10, utc_time)
  
  // 转换为另一个时区（UTC+8）
  let target_offset = 8
  let target_time = add(utc_time, target_offset)  // 10 + 8 = 18
  assert_eq(18, target_time)
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_resources = 100
  let team_members = 8
  let resources_per_member = divide_with_ceil(total_resources, team_members)
  
  assert_eq(13, resources_per_member)  // ceil(100/8) = 13
  
  // 计算总分配
  let total_allocated = multiply(resources_per_member, team_members)
  assert_eq(104, total_allocated)
  
  // 计算剩余资源
  let remaining = subtract(total_allocated, total_resources)
  assert_eq(4, remaining)
}

test "network_packet_distribution" {
  // 网络数据包分布
  let total_packets = 1000
  let router_count = 4
  let packets_per_router = divide_with_ceil(total_packets, router_count)
  
  assert_eq(250, packets_per_router)  // ceil(1000/4) = 250
  
  // 计算总容量
  let total_capacity = multiply(packets_per_router, router_count)
  assert_eq(1000, total_capacity)
  
  // 计算利用率
  let utilization = divide_with_ceil(multiply(total_packets, 100), total_capacity)
  assert_eq(100, utilization)  // ceil(100000/1000) = 100%
}

test "circular_buffer_simulation" {
  // 循环缓冲区模拟
  let buffer_size = 16
  let current_pos = 12
  let items_to_add = 7
  
  // 计算新位置
  let raw_pos = add(current_pos, items_to_add)  // 12 + 7 = 19
  let wrapped_pos = subtract(raw_pos, buffer_size)  // 19 - 16 = 3
  
  assert_eq(3, wrapped_pos)
  
  // 计算需要的循环次数
  let cycles = divide_with_ceil(raw_pos, buffer_size)
  assert_eq(2, cycles)
}

test "financial_interest_compounding" {
  // 复利计算
  let principal = 5000
  let rate_percent = 6
  let years = 2
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let year1_total = add(principal, year1_interest)
  assert_eq(5300, year1_total)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_total, rate_percent), 100)
  let year2_total = add(year1_total, year2_interest)
  assert_eq(5618, year2_total)
  
  // 计算总收益
  let total_profit = subtract(year2_total, principal)
  assert_eq(618, total_profit)
}

test "memory_management_simulation" {
  // 内存管理模拟
  let total_memory = 4096  // 4GB
  let process1 = 512       // 512MB
  let process2 = 256       // 256MB
  let process3 = 128       // 128MB
  
  // 计算总使用量
  let total_used = add(add(process1, process2), process3)
  assert_eq(896, total_used)
  
  // 计算剩余内存
  let remaining = subtract(total_memory, total_used)
  assert_eq(3200, remaining)
  
  // 计算内存块数量（每块1GB）
  let memory_blocks = divide_with_ceil(total_used, 1024)
  assert_eq(1, memory_blocks)
}

test "load_balancing_algorithm" {
  // 负载均衡算法
  let total_requests = 500
  let server_count = 3
  let base_requests = divide_with_ceil(total_requests, server_count)
  
  assert_eq(167, base_requests)  // ceil(500/3) = 167
  
  // 分配请求
  let server1 = base_requests
  let server2 = base_requests
  let server3 = subtract(total_requests, add(server1, server2))
  
  assert_eq(167, server1)
  assert_eq(167, server2)
  assert_eq(166, server3)
  
  // 验证总分配
  let total_allocated = add(add(server1, server2), server3)
  assert_eq(500, total_allocated)
}

test "satellite_communication_range" {
  // 卫星通信范围计算
  let earth_radius = 6371
  let satellite_altitude = 350
  let orbital_radius = add(earth_radius, satellite_altitude)
  
  assert_eq(6721, orbital_radius)
  
  // 计算覆盖半径
  let coverage_angle = 15
  let coverage_radius = divide_with_ceil(multiply(orbital_radius, coverage_angle), 90)
  assert_eq(1120, coverage_radius)
  
  // 计算地面站数量
  let earth_circumference = multiply(earth_radius, 2)
  let stations_needed = divide_with_ceil(earth_circumference, multiply(coverage_radius, 2))
  assert_eq(6, stations_needed)
}

test "data_compression_metrics" {
  // 数据压缩指标
  let original_size = 10000
  let compression_ratio = 70  // 70%压缩率
  let compressed_size = divide_with_ceil(multiply(original_size, compression_ratio), 100)
  
  assert_eq(7000, compressed_size)
  
  // 计算节省空间
  let space_saved = subtract(original_size, compressed_size)
  assert_eq(3000, space_saved)
  
  // 计算压缩效率
  let efficiency = divide_with_ceil(multiply(space_saved, 100), original_size)
  assert_eq(30, efficiency)
  
  // 计算传输时间（假设每秒1000字节）
  let original_time = divide_with_ceil(original_size, 1000)
  let compressed_time = divide_with_ceil(compressed_size, 1000)
  let time_saved = subtract(original_time, compressed_time)
  
  assert_eq(10, original_time)
  assert_eq(7, compressed_time)
  assert_eq(3, time_saved)
}
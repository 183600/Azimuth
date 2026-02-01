// Azimuth 增强标准测试用例
// 包含10个高质量的标准 MoonBit 测试用例

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let common_difference = 4
  let terms_count = 6
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(terms_count, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(terms_count - 1, common_difference))),
    2
  )
  
  @azimuth.assert_eq(78, sum)  // 6/2 * (6 + 20) = 3 * 26 = 78
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 1500
  
  let allocated_budget = @azimuth.add(@azimuth.add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = @azimuth.subtract(total_budget, allocated_budget)
  
  // 计算可以启动的小项目数量
  let small_project_cost = 300
  let additional_projects = @azimuth.divide_with_ceil(remaining_budget, small_project_cost)
  
  @azimuth.assert_eq(4500, allocated_budget)
  @azimuth.assert_eq(500, remaining_budget)
  @azimuth.assert_eq(2, additional_projects)
}

test "temperature_conversion_simulation" {
  // 温度转换模拟（简化版）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(@azimuth.divide_with_ceil(@azimuth.multiply(celsius, 9), 5), 1), 32)
  
  // 反向转换
  let back_to_celsius = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.subtract(fahrenheit, 32), 5), 9)
  
  @azimuth.assert_eq(77, fahrenheit)  // 25°C = 77°F
  @azimuth.assert_eq(25, back_to_celsius)  // 77°F = 25°C
}

test "inventory_turnover_calculation" {
  // 库存周转率计算
  let initial_inventory = 500
  let purchases = 1200
  let final_inventory = 300
  let cost_of_goods_sold = @azimuth.subtract(@azimuth.add(initial_inventory, purchases), final_inventory)
  let average_inventory = @azimuth.divide_with_ceil(@azimuth.add(initial_inventory, final_inventory), 2)
  let turnover_ratio = @azimuth.divide_with_ceil(@azimuth.multiply(cost_of_goods_sold, 100), average_inventory)
  
  @azimuth.assert_eq(1400, cost_of_goods_sold)
  @azimuth.assert_eq(400, average_inventory)
  @azimuth.assert_eq(350, turnover_ratio)
}

test "compound_interest_simplified" {
  // 简化的复利计算
  let principal = 1000
  let rate_percent = 8
  let _years = 3  // 未使用的变量
  
  let year1_amount = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  let year2_amount = @azimuth.add(year1_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year1_amount, rate_percent), 100))
  let year3_amount = @azimuth.add(year2_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year2_amount, rate_percent), 100))
  
  @azimuth.assert_eq(1080, year1_amount)  // 1000 + 80
  @azimuth.assert_eq(1166, year2_amount)  // 1080 + 87
  @azimuth.assert_eq(1259, year3_amount)  // 1166 + 93
}

test "batch_processing_efficiency" {
  // 批处理效率计算
  let total_items = 158
  let batch_size = 25
  let processing_time_per_batch = 4
  
  let batches_needed = @azimuth.divide_with_ceil(total_items, batch_size)
  let total_processing_time = @azimuth.multiply(batches_needed, processing_time_per_batch)
  
  // 计算最后一个批次的实际处理量
  let full_batches = @azimuth.subtract(batches_needed, 1)
  let items_processed_before_last = @azimuth.multiply(full_batches, batch_size)
  let last_batch_size = @azimuth.subtract(total_items, items_processed_before_last)
  
  @azimuth.assert_eq(7, batches_needed)  // ceil(158/25) = 7
  @azimuth.assert_eq(28, total_processing_time)  // 7 * 4 = 28
  @azimuth.assert_eq(8, last_batch_size)  // 158 - 6*25 = 8
}

test "network_packet_routing" {
  // 网络数据包路由模拟
  let total_packets = 95
  let route_capacity_a = 30
  let route_capacity_b = 25
  let route_capacity_c = 20
  
  let packets_route_a = @azimuth.divide_with_ceil(@azimuth.multiply(total_packets, 40), 100)  // 40% via route A
  let packets_route_b = @azimuth.divide_with_ceil(@azimuth.multiply(total_packets, 35), 100)  // 35% via route B
  let packets_route_c = @azimuth.subtract(total_packets, @azimuth.add(packets_route_a, packets_route_b))  // remainder via route C
  
  // 验证路由容量是否足够
  let route_a_sufficient = packets_route_a <= route_capacity_a
  let route_b_sufficient = packets_route_b <= route_capacity_b
  let route_c_sufficient = packets_route_c <= route_capacity_c
  
  @azimuth.assert_eq(38, packets_route_a)  // ceil(95*40/100) = 38
  @azimuth.assert_eq(34, packets_route_b)  // ceil(95*35/100) = 34
  @azimuth.assert_eq(23, packets_route_c)  // 95 - 38 - 34 = 23
  @azimuth.assert_true(route_a_sufficient)
  @azimuth.assert_true(route_b_sufficient)
  @azimuth.assert_true(route_c_sufficient)
}

test "digital_storage_calculation" {
  // 数字存储计算
  let file_size_mb = 850
  let compression_ratio = 60  // 压缩到60%
  let backup_drives = 3
  let drive_capacity_gb = 2
  
  let compressed_size_mb = @azimuth.divide_with_ceil(@azimuth.multiply(file_size_mb, compression_ratio), 100)
  let compressed_size_gb = @azimuth.divide_with_ceil(compressed_size_mb, 1024)
  let total_storage_needed = @azimuth.multiply(compressed_size_gb, backup_drives)
  let storage_sufficient = total_storage_needed <= drive_capacity_gb
  
  @azimuth.assert_eq(510, compressed_size_mb)  // ceil(850*60/100) = 510
  @azimuth.assert_eq(1, compressed_size_gb)   // ceil(510/1024) = 1
  @azimuth.assert_eq(3, total_storage_needed)  // 1 * 3 = 3
  @azimuth.assert_false(storage_sufficient)   // 3GB > 2GB，存储不足
}

test "production_line_optimization" {
  // 生产线优化计算
  let _stations = 5  // 未使用的变量
  let cycle_time_seconds = 45
  let operating_hours_per_day = 8
  let efficiency_factor = 85  // 85%效率
  
  let daily_production_time = @azimuth.multiply(operating_hours_per_day, 3600)
  let effective_production_time = @azimuth.divide_with_ceil(@azimuth.multiply(daily_production_time, efficiency_factor), 100)
  let cycles_per_day = @azimuth.divide_with_ceil(effective_production_time, cycle_time_seconds)
  let units_produced_per_day = cycles_per_day
  
  @azimuth.assert_eq(28800, daily_production_time)  // 8 * 3600
  @azimuth.assert_eq(24480, effective_production_time)  // ceil(28800*85/100) = 24480
  @azimuth.assert_eq(544, cycles_per_day)  // ceil(24480/45) = 544
  @azimuth.assert_eq(544, units_produced_per_day)
}

test "multilingual_greeting_system" {
  // 多语言问候系统测试
  let english_name = "Alice"
  let chinese_name = "小明"
  let spanish_name = "Carlos"
  let russian_name = "Анна"
  let emoji_name = "🌟"
  
  let english_greeting = @azimuth.greet(english_name)
  let chinese_greeting = @azimuth.greet(chinese_name)
  let spanish_greeting = @azimuth.greet(spanish_name)
  let russian_greeting = @azimuth.greet(russian_name)
  let emoji_greeting = @azimuth.greet(emoji_name)
  
  @azimuth.assert_eq_string("Hello, Alice!", english_greeting)
  @azimuth.assert_eq_string("Hello, 小明!", chinese_greeting)
  @azimuth.assert_eq_string("Hello, Carlos!", spanish_greeting)
  @azimuth.assert_eq_string("Hello, Анна!", russian_greeting)
  @azimuth.assert_eq_string("Hello, 🌟!", emoji_greeting)
}
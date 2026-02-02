// Azimuth 库的标准增强测试用例
// 使用标准的 MoonBit 测试语法

// 导入 azimuth 库
use azimuth.*

test "enhanced_mathematical_series" {
  // 数学级数计算：等差数列求和
  let first_term = 5
  let common_diff = 3
  let num_terms = 8
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(104, series_sum)  // 8/2 * (10 + 21) = 4 * 31 = 124
}

test "enhanced_financial_compound_interest" {
  // 复利计算：更精确的模拟
  let principal = 5000
  let rate_percent = 7
  let years = 4
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  // 第四年
  let year4 = add(year3, divide_with_ceil(multiply(year3, rate_percent), 100))
  
  assert_eq(5350, year1)
  assert_eq(5725, year2)
  assert_eq(6126, year3)
  assert_eq(6555, year4)
}

test "enhanced_inventory_optimization" {
  // 库存优化：多产品仓储计算
  let product_a_qty = 45
  let product_b_qty = 32
  let product_c_qty = 67
  let box_capacity = 20
  
  // 计算每种产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  // 计算总箱子数和剩余容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  let total_items = add(add(product_a_qty, product_b_qty), product_c_qty)
  let unused_space = subtract(total_capacity, total_items)
  
  assert_eq(3, boxes_a)   // ceil(45/20) = 3
  assert_eq(2, boxes_b)   // ceil(32/20) = 2
  assert_eq(4, boxes_c)   // ceil(67/20) = 4
  assert_eq(9, total_boxes)
  assert_eq(180, total_capacity)
  assert_eq(144, total_items)
  assert_eq(36, unused_space)
}

test "enhanced_project_resource_allocation" {
  // 项目资源分配：多任务资源计算
  let total_budget = 10000
  let team_members = 8
  let project_duration_days = 30
  
  // 计算每人每日预算
  let daily_budget_per_person = divide_with_ceil(total_budget, multiply(team_members, project_duration_days))
  
  // 计算总人日
  let total_person_days = multiply(team_members, project_duration_days)
  
  // 计算实际分配（考虑向上取整）
  let actual_allocated = multiply(daily_budget_per_person, total_person_days)
  
  assert_eq(42, daily_budget_per_person)  // ceil(10000/240) = 42
  assert_eq(240, total_person_days)
  assert_eq(10080, actual_allocated)
}

test "enhanced_data_processing_pipeline" {
  // 数据处理管道：批量处理计算
  let data_batches = 15
  let records_per_batch = 125
  let processing_time_per_record = 3  // 分钟
  let parallel_processors = 4
  
  // 计算总记录数
  let total_records = multiply(data_batches, records_per_batch)
  
  // 计算单个处理器处理时间
  let single_processor_time = multiply(total_records, processing_time_per_record)
  
  // 计算并行处理时间
  let parallel_processing_time = divide_with_ceil(single_processor_time, parallel_processors)
  
  // 转换为小时
  let processing_hours = divide_with_ceil(parallel_processing_time, 60)
  
  assert_eq(1875, total_records)
  assert_eq(5625, single_processor_time)
  assert_eq(1407, parallel_processing_time)
  assert_eq(24, processing_hours)
}

test "enhanced_energy_consumption_forecast" {
  // 能源消耗预测：季度用电量计算
  let daily_consumption_base = 120  // kWh
  let seasonal_variation = 25  // 百分比
  let days_in_quarter = 90
  
  // 计算考虑季节变化的日消耗量
  let adjusted_daily_consumption = add(
    daily_consumption_base,
    divide_with_ceil(multiply(daily_consumption_base, seasonal_variation), 100)
  )
  
  // 计算季度总消耗量
  let quarterly_consumption = multiply(adjusted_daily_consumption, days_in_quarter)
  
  // 计算月均消耗量
  let monthly_average = divide_with_ceil(quarterly_consumption, 3)
  
  assert_eq(150, adjusted_daily_consumption)  // 120 + ceil(120*25/100) = 120 + 30 = 150
  assert_eq(13500, quarterly_consumption)
  assert_eq(4500, monthly_average)
}

test "enhanced_network_bandwidth_planning" {
  // 网络带宽规划：多用户并发计算
  let users = 50
  let avg_data_per_user_mb = 850  // MB
  let peak_hours_multiplier = 3  // 高峰期倍数
  let network_capacity_gbps = 10
  
  // 计算基础数据量
  let base_data_mb = multiply(users, avg_data_per_user_mb)
  
  // 计算高峰期数据量
  let peak_data_mb = multiply(base_data_mb, peak_hours_multiplier)
  
  // 转换为GB
  let peak_data_gb = divide_with_ceil(peak_data_mb, 1024)
  
  // 计算网络利用率
  let network_utilization = divide_with_ceil(multiply(peak_data_gb, 100), network_capacity_gbps * 1024)
  
  assert_eq(42500, base_data_mb)
  assert_eq(127500, peak_data_mb)
  assert_eq(125, peak_data_gb)
  assert_eq(2, network_utilization)  // ceil(125*100/10240) = 2%
}

test "enhanced_logistics_routing_optimization" {
  // 物流路径优化：多站点配送计算
  let distribution_centers = 6
  let retail_stores_per_center = 12
  let avg_delivery_distance_km = 25
  let fuel_efficiency_km_per_liter = 8
  
  // 计算总配送点数
  let total_stores = multiply(distribution_centers, retail_stores_per_center)
  
  // 计算总配送距离
  let total_distance = multiply(total_stores, avg_delivery_distance_km)
  
  // 计算总燃料消耗
  let total_fuel_liters = divide_with_ceil(total_distance, fuel_efficiency_km_per_liter)
  
  // 计算每个配送中心的平均燃料消耗
  let fuel_per_center = divide_with_ceil(total_fuel_liters, distribution_centers)
  
  assert_eq(72, total_stores)
  assert_eq(1800, total_distance)
  assert_eq(225, total_fuel_liters)
  assert_eq(38, fuel_per_center)
}

test "enhanced_software_performance_metrics" {
  // 软件性能指标：响应时间计算
  let base_response_time_ms = 150
  let concurrent_users = 200
  let server_capacity = 50
  let degradation_factor = 20  // 百分比
  
  // 计算并发用户对性能的影响
  let load_factor = divide_with_ceil(concurrent_users, server_capacity)
  
  // 计算性能退化后的响应时间
  let degraded_response_time = add(
    base_response_time_ms,
    divide_with_ceil(multiply(base_response_time_ms, degradation_factor), 100)
  )
  
  // 计算最终响应时间
  let final_response_time = multiply(degraded_response_time, load_factor)
  
  // 转换为秒
  let response_time_seconds = divide_with_ceil(final_response_time, 1000)
  
  assert_eq(4, load_factor)  // ceil(200/50) = 4
  assert_eq(180, degraded_response_time)  // 150 + ceil(150*20/100) = 150 + 30 = 180
  assert_eq(720, final_response_time)  // 180 * 4 = 720
  assert_eq(1, response_time_seconds)  // ceil(720/1000) = 1
}

test "enhanced_cryptographic_hash_simulation" {
  // 密码学哈希模拟：简化版哈希计算
  let input_value = 12345
  let prime_modulus = 9973
  let base_multiplier = 31
  
  // 第一轮哈希
  let hash_round1 = multiply(input_value, base_multiplier) % prime_modulus
  
  // 第二轮哈希（加入混淆）
  let hash_round2 = multiply(add(hash_round1, 1234), base_multiplier) % prime_modulus
  
  // 第三轮哈希（最终处理）
  let final_hash = multiply(add(hash_round2, 5678), base_multiplier) % prime_modulus
  
  assert_eq(8262, hash_round1)
  assert_eq(6879, hash_round2)
  assert_eq(5536, final_hash)
}
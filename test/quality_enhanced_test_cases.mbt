// 质量增强测试用例 - 包含8个新的高质量测试用例
// 覆盖实际应用场景和边界情况

test "financial_compound_interest" {
  // 金融复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 计算复利（使用向上取整）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1158, year3)
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 1500
  
  let allocated = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -allocated)
  
  // 计算可以支持的小项目数量
  let small_projects = divide_with_ceil(remaining, 300)
  
  assert_eq(4500, allocated)
  assert_eq(500, remaining)
  assert_eq(2, small_projects)
}

test "batch_processing_efficiency" {
  // 批处理效率计算
  let total_records = 987
  let batch_size = 50
  
  let full_batches = divide_with_ceil(total_records, batch_size)
  let last_batch_size = total_records % batch_size
  let adjusted_last_batch = if last_batch_size == 0 { batch_size } else { last_batch_size }
  
  assert_eq(20, full_batches)
  assert_eq(37, last_batch_size)
  assert_eq(37, adjusted_last_batch)
}

test "temperature_conversion_scale" {
  // 温度转换缩放计算
  let celsius = 25
  let fahrenheit_offset = 32
  let scale_factor = 18  // 10/5 = 2, 但使用整数运算
  
  // 简化的温度转换（使用整数运算）
  let scaled_fahrenheit = add(multiply(celsius, scale_factor), fahrenheit_offset)
  let normalized = divide_with_ceil(scaled_fahrenheit, 10)
  
  assert_eq(77, normalized)
}

test "inventory_reorder_calculation" {
  // 库存重新订购计算
  let current_stock = 45
  let daily_sales = 7
  let lead_time_days = 5
  let safety_stock = 20
  
  let projected_sales = multiply(daily_sales, lead_time_days)
  let reorder_point = add(projected_sales, safety_stock)
  let days_until_reorder = divide_with_ceil(add(current_stock, -safety_stock), daily_sales)
  
  assert_eq(35, projected_sales)
  assert_eq(55, reorder_point)
  assert_eq(4, days_until_reorder)
}

test "digital_storage_conversion" {
  // 数字存储转换计算
  let megabytes = 2048
  let kilobytes_per_mb = 1024
  
  let total_kilobytes = multiply(megabytes, kilobytes_per_mb)
  let gigabytes = divide_with_ceil(megabytes, 1024)
  let remaining_mb = megabytes % 1024
  
  assert_eq(2097152, total_kilobytes)
  assert_eq(2, gigabytes)
  assert_eq(0, remaining_mb)
}

test "network_packet_optimization" {
  // 网络数据包优化
  let data_size = 1500
  let header_size = 20
  let max_payload = 1400
  
  let total_size = add(data_size, header_size)
  let packets_needed = divide_with_ceil(data_size, max_payload)
  let total_transmission = multiply(packets_needed, add(max_payload, header_size))
  
  assert_eq(1520, total_size)
  assert_eq(2, packets_needed)
  assert_eq(2840, total_transmission)
}

test "agricultural_planning" {
  // 农业规划计算
  let field_area_hectares = 15
  let seeds_per_hectare = 25000
  let expected_yield_per_hectare = 3000
  let labor_cost_per_hectare = 150
  
  let total_seeds = multiply(field_area_hectares, seeds_per_hectare)
  let expected_harvest = multiply(field_area_hectares, expected_yield_per_hectare)
  let total_labor_cost = multiply(field_area_hectares, labor_cost_per_hectare)
  let cost_per_kg = divide_with_ceil(total_labor_cost, divide_with_ceil(expected_harvest, 1000))
  
  assert_eq(375000, total_seeds)
  assert_eq(45000, expected_harvest)
  assert_eq(2250, total_labor_cost)
  assert_eq(50, cost_per_kg)
}
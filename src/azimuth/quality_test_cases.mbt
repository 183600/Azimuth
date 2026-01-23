// 新增的高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "string_length_calculation" {
  // 测试字符串长度计算相关的功能
  let short_name = "Bob"
  let long_name = "Elizabeth Alexandra Mary"
  
  let short_greeting = greet(short_name)
  let long_greeting = greet(long_name)
  
  assert_eq_string("Hello, Bob!", short_greeting)
  assert_eq_string("Hello, Elizabeth Alexandra Mary!", long_greeting)
}

test "temperature_conversion" {
  // 模拟温度转换计算
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  
  assert_eq(77, fahrenheit)
}

test "array_index_calculation" {
  // 模拟数组索引计算
  let page_size = 20
  let total_items = 85
  let current_page = 4
  
  let start_index = multiply(current_page - 1, page_size)
  let total_pages = divide_with_ceil(total_items, page_size)
  
  assert_eq(60, start_index)
  assert_eq(5, total_pages)
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 250
  let percentage = 15
  
  let percentage_value = multiply(total, percentage) / 100
  let remaining = add(total, -percentage_value)
  
  assert_eq(37, percentage_value)
  assert_eq(213, remaining)
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_time = 14  // 2 PM
  let time_difference = 8
  
  let utc_time = add(local_time, -time_difference)
  let adjusted_utc_time = if utc_time < 0 { add(utc_time, 24) } else { utc_time }
  
  assert_eq(6, adjusted_utc_time)
}

test "geometric_area_calculation" {
  // 几何面积计算
  let length = 12
  let width = 8
  let height = 5
  
  let floor_area = multiply(length, width)
  let wall_area_total = multiply(2, multiply(add(length, width), height))
  let total_surface_area = add(floor_area, wall_area_total)
  
  assert_eq(96, floor_area)
  assert_eq(200, wall_area_total)
  assert_eq(296, total_surface_area)
}

test "compound_interest_simple" {
  // 简单复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(1158, year3_amount)
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let reserve_percentage = 10
  
  let allocated_projects = add(project_a_cost, project_b_cost)
  let reserve_amount = divide_with_ceil(multiply(total_budget, reserve_percentage), 100)
  let remaining_budget = add(add(total_budget, -allocated_projects), -reserve_amount)
  
  assert_eq(6300, allocated_projects)
  assert_eq(1000, reserve_amount)
  assert_eq(2700, remaining_budget)
}

test "batch_processing_calculation" {
  // 批处理计算
  let total_records = 987
  let batch_size = 50
  
  let full_batches = divide_with_ceil(total_records, batch_size)
  let last_batch_size = if total_records % batch_size == 0 { 
    batch_size 
  } else { 
    total_records % batch_size 
  }
  
  assert_eq(20, full_batches)
  assert_eq(37, last_batch_size)
}

test "load_balancing_simulation" {
  // 负载均衡模拟
  let total_requests = 150
  let server_count = 4
  
  let base_requests_per_server = divide_with_ceil(total_requests, server_count)
  let remainder_requests = total_requests - multiply(base_requests_per_server - 1, server_count)
  
  assert_eq(38, base_requests_per_server)
  assert_eq(2, remainder_requests)
}
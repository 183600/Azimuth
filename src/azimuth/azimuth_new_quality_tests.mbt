// 新的高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "mathematical_series_summation" {
  // 测试数学级数求和：计算等差数列 1+3+5+7+9
  let term1 = 1
  let term2 = add(term1, 2)
  let term3 = add(term2, 2)
  let term4 = add(term3, 2)
  let term5 = add(term4, 2)
  
  let series_sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(25, series_sum)
}

test "geometric_progression_calculation" {
  // 测试等比数列计算：计算 2×2×2×2×2
  let base = 2
  let power2 = multiply(base, base)
  let power3 = multiply(power2, base)
  let power4 = multiply(power3, base)
  let power5 = multiply(power4, base)
  
  assert_eq(32, power5)
}

test "inventory_optimization" {
  // 测试库存优化算法
  let warehouse_capacity = 1000
  let avg_item_size = 15
  let safety_margin = 50
  
  let optimal_items = divide_with_ceil(warehouse_capacity - safety_margin, avg_item_size)
  let total_space_used = multiply(optimal_items, avg_item_size)
  
  assert_eq(63, optimal_items)
  assert_eq(945, total_space_used)
}

test "multi_language_greeting_system" {
  // 测试多语言问候系统
  let english_name = "Alice"
  let chinese_name = "小明"
  let emoji_name = "🚀"
  let mixed_name = "User_2024"
  
  let greeting1 = greet(english_name)
  let greeting2 = greet(chinese_name)
  let greeting3 = greet(emoji_name)
  let greeting4 = greet(mixed_name)
  
  assert_eq_string("Hello, Alice!", greeting1)
  assert_eq_string("Hello, 小明!", greeting2)
  assert_eq_string("Hello, 🚀!", greeting3)
  assert_eq_string("Hello, User_2024!", greeting4)
}

test "financial_compound_interest" {
  // 测试复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(1158, year3_amount)
}

test "resource_allocation_algorithm" {
  // 测试资源分配算法
  let total_resources = 100
  let project1_needs = 25
  let project2_needs = 30
  let project3_needs = 20
  
  let allocated1 = multiply(project1_needs, 1)
  let allocated2 = multiply(project2_needs, 1)
  let allocated3 = multiply(project3_needs, 1)
  
  let total_allocated = add(add(allocated1, allocated2), allocated3)
  let remaining = add(total_resources, -total_allocated)
  
  assert_eq(75, total_allocated)
  assert_eq(25, remaining)
}

test "batch_processing_optimization" {
  // 测试批处理优化
  let total_items = 157
  let batch_size = 25
  let processing_time_per_batch = 2
  
  let number_of_batches = divide_with_ceil(total_items, batch_size)
  let total_processing_time = multiply(number_of_batches, processing_time_per_batch)
  
  assert_eq(7, number_of_batches)
  assert_eq(14, total_processing_time)
}

test "temperature_conversion_simulation" {
  // 测试温度转换模拟（简化的线性转换）
  let celsius_temp = 25
  let conversion_factor = 9
  let divisor = 5
  let offset = 32
  
  let fahrenheit_temp = add(divide_with_ceil(multiply(celsius_temp, conversion_factor), divisor), offset)
  
  assert_eq(77, fahrenheit_temp)
}

test "circular_buffer_calculation" {
  // 测试循环缓冲区计算
  let buffer_size = 10
  let current_position = 7
  let steps_forward = 5
  
  let raw_new_position = add(current_position, steps_forward)
  let wrapped_position = raw_new_position % buffer_size
  
  assert_eq(2, wrapped_position)
}

test "load_balancing_algorithm" {
  // 测试负载均衡算法
  let total_requests = 100
  let number_of_servers = 8
  let capacity_per_server = 15
  
  let requests_per_server = divide_with_ceil(total_requests, number_of_servers)
  let total_capacity = multiply(capacity_per_server, number_of_servers)
  let overload = add(requests_per_server, -capacity_per_server)
  
  assert_eq(13, requests_per_server)
  assert_eq(120, total_capacity)
  assert_eq(-2, overload)
}
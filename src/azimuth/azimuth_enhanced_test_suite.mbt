// Azimuth 增强测试套件
// 高质量的 MoonBit 测试用例，针对 azimuth 库的核心功能

test "mathematical_series_summation" {
  // 数学级数求和测试
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 8
  
  // 使用等差数列求和公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(124, sum)  // 8/2 * (10 + 21) = 4 * 31 = 124
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let project_c_cost = 2200
  
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = subtract(total_budget, allocated_budget)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 600
  let additional_projects = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(8500, allocated_budget)
  assert_eq(1500, remaining_budget)
  assert_eq(3, additional_projects)
}

test "temperature_conversion_system" {
  // 温度转换系统测试
  let celsius_temp = 25
  let fahrenheit_temp = 77
  
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let converted_to_fahrenheit = add(multiply(celsius_temp, 9) / 5, 32)
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let converted_to_celsius = multiply(fahrenheit_temp - 32, 5) / 9
  
  assert_eq(77, converted_to_fahrenheit)
  assert_eq(25, converted_to_celsius)
}

test "batch_processing_efficiency" {
  // 批处理效率测试
  let total_records = 1547
  let batch_size = 100
  let processing_time_per_batch = 5  // 分钟
  
  let number_of_batches = divide_with_ceil(total_records, batch_size)
  let total_processing_time = multiply(number_of_batches, processing_time_per_batch)
  
  // 计算每分钟处理的记录数
  let records_per_minute = divide_with_ceil(total_records, total_processing_time)
  
  assert_eq(16, number_of_batches)
  assert_eq(80, total_processing_time)
  assert_eq(20, records_per_minute)
}

test "geometry_area_calculations" {
  // 几何面积计算测试
  let rectangle_length = 12
  let rectangle_width = 8
  let circle_radius = 5
  
  // 矩形面积
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  
  // 圆形面积（简化为 π*r²，使用 π≈3）
  let circle_area = multiply(3, multiply(circle_radius, circle_radius))
  
  // 总面积
  let total_area = add(rectangle_area, circle_area)
  
  assert_eq(96, rectangle_area)
  assert_eq(75, circle_area)
  assert_eq(171, total_area)
}

test "sales_performance_metrics" {
  // 销售绩效指标测试
  let quarterly_targets = [10000, 12000, 15000, 18000]
  let quarterly_actuals = [9500, 13500, 14200, 19500]
  
  // 计算年度目标
  let annual_target = add(add(add(quarterly_targets[0], quarterly_targets[1]), quarterly_targets[2]), quarterly_targets[3])
  
  // 计算年度实际销售
  let annual_actual = add(add(add(quarterly_actuals[0], quarterly_actuals[1]), quarterly_actuals[2]), quarterly_actuals[3])
  
  // 计算达成率（百分比）
  let achievement_rate = divide_with_ceil(multiply(annual_actual, 100), annual_target)
  
  assert_eq(55000, annual_target)
  assert_eq(56700, annual_actual)
  assert_eq(104, achievement_rate)
}

test "cryptography_hash_simulation" {
  // 密码学哈希模拟测试
  let input_value = 12345
  let prime_multiplier = 31
  let modulus_value = 100000
  
  // 简单的哈希函数模拟
  let hash_step1 = multiply(input_value, prime_multiplier)
  let hash_step2 = add(hash_step1, input_value)
  let final_hash = hash_step2 % modulus_value
  
  assert_eq(382760, hash_step1)
  assert_eq(395105, hash_step2)
  assert_eq(95105, final_hash)
}

test "network_latency_calculation" {
  // 网络延迟计算测试
  let data_size_mb = 250
  let network_speed_mbps = 100
  let latency_per_hop_ms = 5
  let number_of_hops = 3
  
  // 传输时间
  let transfer_time_ms = divide_with_ceil(multiply(data_size_mb, 1000), network_speed_mbps)
  
  // 总延迟时间
  let total_latency_ms = multiply(latency_per_hop_ms, number_of_hops)
  
  // 总时间
  let total_time_ms = add(transfer_time_ms, total_latency_ms)
  
  assert_eq(2500, transfer_time_ms)
  assert_eq(15, total_latency_ms)
  assert_eq(2515, total_time_ms)
}

test "string_processing_pipeline" {
  // 字符串处理流水线测试
  let first_name = "John"
  let last_name = "Doe"
  let department = "Engineering"
  let employee_id = "EMP001"
  
  // 构建完整的员工标识
  let full_name = first_name + " " + last_name
  let employee_info = full_name + " - " + department + " (" + employee_id + ")"
  
  // 使用 greet 函数处理
  let greeting_result = greet(employee_info)
  
  assert_eq_string("Hello, John Doe - Engineering (EMP001)!", greeting_result)
  assert_eq_string("Hello, 测试工程师!", greet("测试工程师"))
  assert_eq_string("Hello, Data Analyst 2024!", greet("Data Analyst 2024"))
}

test "comprehensive_error_handling" {
  // 综合错误处理测试
  // 测试除数为零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  let very_large_number = 1000000
  let very_small_number = 1
  
  assert_eq(1000001, add(very_large_number, very_small_number))
  assert_eq(999999, subtract(very_large_number, very_small_number))
  assert_eq(1000000, multiply(very_large_number, very_small_number))
  assert_eq(1000000, divide_with_ceil(very_large_number, very_small_number))
  
  // 测试负数运算
  assert_eq(-1000001, add(-very_large_number, -very_small_number))
  assert_eq(-999999, subtract(-very_large_number, -very_small_number))
  assert_eq(1000000, multiply(-very_large_number, -very_small_number))
  assert_eq(-1000000, divide_with_ceil(-very_large_number, very_small_number))
}
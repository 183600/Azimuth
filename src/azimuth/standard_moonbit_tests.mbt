// 标准 MoonBit 测试用例
// 针对 azimuth 库的全面测试，包含边界情况和实际应用场景

test "addition_with_various_ranges" {
  // 测试不同范围的加法运算
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(50, -50))
  assert_eq(999999, add(500000, 499999))
}

test "multiplication_identity_and_zero" {
  // 测试乘法的单位元和零元素性质
  assert_eq(15, multiply(15, 1))
  assert_eq(-15, multiply(-15, 1))
  assert_eq(0, multiply(15, 0))
  assert_eq(0, multiply(0, 15))
  assert_eq(0, multiply(0, 0))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(5, divide_with_ceil(20, 4))  // 整除
  assert_eq(5, divide_with_ceil(19, 4))  // 4.75 -> 5
  assert_eq(1, divide_with_ceil(3, 5))   // 0.6 -> 1
  assert_eq(-4, divide_with_ceil(-15, 4)) // -3.75 -> -3
  assert_eq(4, divide_with_ceil(-15, -4)) // 3.75 -> 4
  assert_eq(0, divide_with_ceil(10, 0))   // 除数为零
}

test "greet_function_special_cases" {
  // 测试问候函数的特殊情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123456!", greet("123456"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, a+b=c!", greet("a+b=c"))
  assert_eq_string("Hello,   !", greet("   "))
}

test "complex_financial_calculation" {
  // 复杂财务计算场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算
  let year1_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let year1_total = add(principal, year1_interest)
  
  let year2_interest = divide_with_ceil(multiply(year1_total, rate_percent), 100)
  let year2_total = add(year1_total, year2_interest)
  
  let year3_interest = divide_with_ceil(multiply(year2_total, rate_percent), 100)
  let final_amount = add(year2_total, year3_interest)
  
  assert_eq(500, year1_interest)
  assert_eq(10500, year1_total)
  assert_eq(525, year2_interest)
  assert_eq(11025, year2_total)
  assert_eq(552, year3_interest)
  assert_eq(11577, final_amount)
}

test "inventory_optimization" {
  // 库存优化算法
  let demand_forecast = 157
  let safety_stock = 20
  let reorder_point = add(demand_forecast, safety_stock)
  let economic_order_qty = 50
  let orders_needed = divide_with_ceil(reorder_point, economic_order_qty)
  
  assert_eq(177, reorder_point)
  assert_eq(4, orders_needed)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算
  let first_term = 3
  let common_diff = 4
  let n_terms = 6
  
  // 等差数列第n项: a + (n-1)d
  let nth_term = add(first_term, multiply(n_terms - 1, common_diff))
  
  // 等差数列前n项和: n/2 * (2a + (n-1)d)
  let sum_n_terms = divide_with_ceil(
    multiply(n_terms, add(multiply(2, first_term), multiply(n_terms - 1, common_diff))),
    2
  )
  
  assert_eq(23, nth_term)  // 3 + 5*4 = 23
  assert_eq(78, sum_n_terms)  // 6/2 * (6 + 20) = 3 * 26 = 78
}

test "resource_allocation_algorithm" {
  // 资源分配算法
  let total_resources = 100
  let project1_priority = 3
  let project2_priority = 2
  let project3_priority = 1
  let total_priority = add(add(project1_priority, project2_priority), project3_priority)
  
  let project1_allocation = divide_with_ceil(multiply(total_resources, project1_priority), total_priority)
  let project2_allocation = divide_with_ceil(multiply(total_resources, project2_priority), total_priority)
  let project3_allocation = divide_with_ceil(multiply(total_resources, project3_priority), total_priority)
  
  assert_eq(50, project1_allocation)  // ceil(100*3/6) = 50
  assert_eq(34, project2_allocation)  // ceil(100*2/6) = 34
  assert_eq(17, project3_allocation)  // ceil(100*1/6) = 17
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_hour = 14  // 下午2点
  let timezone_offset = -8  // UTC-8
  let meeting_duration = 3  // 3小时会议
  
  // 转换为UTC时间
  let utc_hour = add(local_hour, -timezone_offset)
  let normalized_utc = if utc_hour >= 24 { add(utc_hour, -24) } else if utc_hour < 0 { add(utc_hour, 24) } else { utc_hour }
  
  // 计算会议结束时间
  let meeting_end_utc = add(normalized_utc, meeting_duration)
  let normalized_end = if meeting_end_utc >= 24 { add(meeting_end_utc, -24) } else { meeting_end_utc }
  
  assert_eq(22, normalized_utc)  // 14 + 8 = 22
  assert_eq(1, normalized_end)   // 22 + 3 = 25 -> 1
}

test "data_compression_ratio" {
  // 数据压缩比计算
  let original_size = 1024
  let compressed_size = 256
  let compression_ratio = multiply(divide_with_ceil(compressed_size * 100, original_size), 1)
  
  // 计算节省的空间
  let space_saved = add(original_size, -compressed_size)
  let saving_percentage = multiply(divide_with_ceil(space_saved * 100, original_size), 1)
  
  assert_eq(25, compression_ratio)  // ceil(256*100/1024) = 25%
  assert_eq(75, saving_percentage)  // ceil(768*100/1024) = 75%
}
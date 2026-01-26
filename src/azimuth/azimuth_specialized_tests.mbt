// Azimuth 专门测试用例 - 10个高质量测试
// 测试 azimuth 库的核心功能和边界情况

test "factorial_calculation_simulation" {
  // 模拟阶乘计算：5! = 5 × 4 × 3 × 2 × 1 = 120
  let factorial_5 = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial_5)
  
  // 模拟 4! = 24
  let factorial_4 = multiply(4, multiply(3, multiply(2, 1)))
  assert_eq(24, factorial_4)
}

test "geometric_progression_sum" {
  // 等比数列求和：2 + 4 + 8 + 16 = 30
  let term1 = 2
  let term2 = multiply(term1, 2)
  let term3 = multiply(term2, 2)
  let term4 = multiply(term3, 2)
  let sum = add(add(add(term1, term2), term3), term4)
  assert_eq(30, sum)
}

test "temperature_conversion" {
  // 模拟温度转换：摄氏度到华氏度 F = C × 9/5 + 32
  let celsius = 25
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  assert_eq(77, fahrenheit)  // 25 × 9/5 + 32 = 45 + 32 = 77
}

test "compound_interest_simplified" {
  // 简化的复利计算：1000元，年利率10%，2年
  let principal = 1000
  let rate_percent = 10
  let years = 2
  
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  
  assert_eq(1210, year2_amount)  // 1000 -> 1100 -> 1210
}

test "batch_processing_calculation" {
  // 批处理计算：每批处理25个项目，总共180个项目需要多少批
  let total_items = 180
  let batch_size = 25
  let batches_needed = divide_with_ceil(total_items, batch_size)
  
  // 最后一批的项目数
  let last_batch_size = total_items - multiply(batches_needed - 1, batch_size)
  
  assert_eq(8, batches_needed)  // ceil(180/25) = 8
  assert_eq(5, last_batch_size)  // 180 - 7×25 = 5
}

test "resource_utilization_optimization" {
  // 资源利用率优化计算
  let total_capacity = 1000
  let current_usage = 750
  let projected_growth = 200
  
  let new_total_usage = add(current_usage, projected_growth)
  let utilization_rate = divide_with_ceil(multiply(new_total_usage, 100), total_capacity)
  
  // 计算是否需要扩容
  let needs_expansion = if new_total_usage > total_capacity { 1 } else { 0 }
  
  assert_eq(950, new_total_usage)
  assert_eq(95, utilization_rate)
  assert_eq(0, needs_expansion)  // 950 <= 1000，不需要扩容
}

test "network_packet_round_trip" {
  // 网络数据包往返计算
  let data_size = 1500  // bytes
  let upload_speed = 100  // bytes/sec
  let download_speed = 150  // bytes/sec
  let processing_delay = 2  // seconds
  
  let upload_time = divide_with_ceil(data_size, upload_speed)
  let download_time = divide_with_ceil(data_size, download_speed)
  let total_time = add(add(upload_time, download_time), processing_delay)
  
  assert_eq(15, upload_time)   // ceil(1500/100) = 15
  assert_eq(10, download_time) // ceil(1500/150) = 10
  assert_eq(27, total_time)    // 15 + 10 + 2 = 27
}

test "array_index_calculation" {
  // 模拟数组索引计算：二维数组转一维数组索引
  let row = 3
  let col = 4
  let cols_per_row = 6
  
  let one_d_index = add(multiply(row, cols_per_row), col)
  
  assert_eq(22, one_d_index)  // 3×6 + 4 = 22
}

test "circular_buffer_operations" {
  // 循环缓冲区操作模拟
  let buffer_size = 10
  let current_position = 8
  let steps_forward = 5
  
  let new_position = add(current_position, steps_forward) % buffer_size
  
  assert_eq(3, new_position)  // (8 + 5) % 10 = 3
}

test "binary_search_simulation" {
  // 模拟二分查找的中间值计算
  let low = 0
  let high = 15
  let mid1 = divide_with_ceil(add(low, high), 2)
  
  // 第二次查找（左半部分）
  let high2 = mid1 - 1
  let mid2 = divide_with_ceil(add(low, high2), 2)
  
  // 第三次查找（右半部分）
  let low3 = mid2 + 1
  let mid3 = divide_with_ceil(add(low3, high2), 2)
  
  assert_eq(8, mid1)   // ceil(15/2) = 8
  assert_eq(4, mid2)   // ceil(7/2) = 4
  assert_eq(6, mid3)   // ceil(5/2) = 3
}
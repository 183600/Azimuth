// 新的专门测试用例 - 针对 azimuth 库的特殊场景测试

test "binary_conversion_simulation" {
  // 模拟二进制转换计算
  let decimal_value = 13
  let binary_place_values = multiply(8, 1)  // 2^3
  let remaining = add(decimal_value, 0 - binary_place_values)
  let next_place = multiply(4, 1)  // 2^2
  let final_remaining = add(remaining, 0 - next_place)
  
  assert_eq(5, remaining)
  assert_eq(1, final_remaining)
}

test "temperature_conversion" {
  // 模拟温度转换计算
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  
  assert_eq(77, fahrenheit)
}

test "geometric_progression" {
  // 测试几何级数计算
  let first_term = 3
  let ratio = 2
  let _num_terms = 5
  
  let term2 = multiply(first_term, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  let sum = add(add(add(add(first_term, term2), term3), term4), term5)
  
  assert_eq(93, sum)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let team1_priority = 3
  let team2_priority = 2
  let team3_priority = 1
  
  let total_priority = add(add(team1_priority, team2_priority), team3_priority)
  let resource_per_unit = divide_with_ceil(total_resources, total_priority)
  
  let team1_resources = multiply(team1_priority, resource_per_unit)
  let team2_resources = multiply(team2_priority, resource_per_unit)
  let team3_resources = multiply(team3_priority, resource_per_unit)
  
  assert_eq(17, resource_per_unit)
  assert_eq(51, team1_resources)
  assert_eq(34, team2_resources)
  assert_eq(17, team3_resources)
}

test "string_pattern_greeting" {
  // 测试字符串模式问候
  let user_role = "管理员"
  let department = "技术部"
  let full_title = user_role + "-" + department
  let formal_greeting = greet(full_title)
  
  assert_eq_string("Hello, 管理员-技术部!", formal_greeting)
}

test "circular_reference_calculation" {
  // 循环引用计算测试
  let base_value = 10
  let increment = 3
  let iterations = 4
  
  let iter1 = add(base_value, increment)
  let iter2 = add(iter1, increment)
  let iter3 = add(iter2, increment)
  let iter4 = add(iter3, increment)
  
  let total_change = multiply(increment, iterations)
  let final_value = add(base_value, total_change)
  
  assert_eq(13, iter1)
  assert_eq(16, iter2)
  assert_eq(19, iter3)
  assert_eq(22, iter4)
  assert_eq(22, final_value)
}

test "batch_processing_simulation" {
  // 批处理模拟测试
  let total_records = 95
  let batch_size = 20
  let processing_time_per_batch = 5  // 分钟
  
  let total_batches = divide_with_ceil(total_records, batch_size)
  let total_processing_time = multiply(total_batches, processing_time_per_batch)
  
  let last_batch_size = add(total_records, 0 - multiply(batch_size, add(total_batches, 0 - 1)))
  
  assert_eq(5, total_batches)
  assert_eq(25, total_processing_time)
  assert_eq(15, last_batch_size)
}
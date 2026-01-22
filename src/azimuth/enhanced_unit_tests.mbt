// 增强的单元测试用例 - 针对 azimuth 库的核心功能
// 包含实际应用场景和边界情况测试

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let rate = 5
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  assert_eq(1158, year3)
}

test "resource_allocation" {
  // 资源分配优化测试
  let total_budget = 10000
  let project_a = 3500
  let project_b = 2800
  let project_c = 2200
  
  let allocated = add(add(project_a, project_b), project_c)
  let remaining = add(total_budget, add(0, -allocated))
  let small_projects = divide_with_ceil(remaining, 500)
  
  assert_eq(8500, allocated)
  assert_eq(1500, remaining)
  assert_eq(3, small_projects)
}

test "time_zone_conversion" {
  // 时区转换计算测试
  let local_time = 14
  let time_diff = 8
  
  let forward_time = add(local_time, time_diff)
  let normalized_forward = if forward_time >= 24 {
    add(forward_time, add(0, -24))
  } else {
    forward_time
  }
  
  let backward_time = add(local_time, add(0, -time_diff))
  let normalized_backward = if backward_time < 0 {
    add(backward_time, 24)
  } else {
    backward_time
  }
  
  assert_eq(22, normalized_forward)
  assert_eq(6, normalized_backward)
}

test "inventory_management" {
  // 库存管理测试
  let daily_demand = 25
  let lead_time = 7
  let safety_stock = 50
  
  let lead_time_demand = multiply(daily_demand, lead_time)
  let reorder_point = add(lead_time_demand, safety_stock)
  let containers_needed = divide_with_ceil(reorder_point, 100)
  
  assert_eq(175, lead_time_demand)
  assert_eq(225, reorder_point)
  assert_eq(3, containers_needed)
}

test "batch_processing" {
  // 批处理优化测试
  let total_items = 1000
  let batch_size = 75
  let time_per_batch = 5
  
  let number_of_batches = divide_with_ceil(total_items, batch_size)
  let total_processing_time = multiply(number_of_batches, time_per_batch)
  
  assert_eq(14, number_of_batches)
  assert_eq(70, total_processing_time)
}

test "unicode_greetings" {
  // Unicode字符串处理测试
  let latin_name = "María García"
  let asian_name = "张伟"
  let emoji_name = "🚀🌟💫"
  
  let latin_greeting = greet(latin_name)
  let asian_greeting = greet(asian_name)
  let emoji_greeting = greet(emoji_name)
  
  assert_eq_string("Hello, María García!", latin_greeting)
  assert_eq_string("Hello, 张伟!", asian_greeting)
  assert_eq_string("Hello, 🚀🌟💫!", emoji_greeting)
}

test "mathematical_series" {
  // 数学级数求和测试
  let n = 10
  let first_term = 3
  let common_diff = 2
  
  let sum_formula = divide_with_ceil(
    multiply(n, add(multiply(2, first_term), multiply(n - 1, common_diff))),
    2
  )
  
  let manual_sum = add(add(add(add(3, 5), 7), 9), 11)
  
  assert_eq(120, sum_formula)
  assert_eq(35, manual_sum)
}

test "extreme_values" {
  // 极端值测试
  let large_num = 1000000
  let small_num = 1
  
  let large_addition = add(large_num, large_num)
  let large_multiplication = multiply(1000, 1000)
  let small_division = divide_with_ceil(small_num, large_num)
  let zero_division = divide_with_ceil(large_num, 0)
  
  assert_eq(2000000, large_addition)
  assert_eq(1000000, large_multiplication)
  assert_eq(1, small_division)
  assert_eq(0, zero_division)
}

test "shopping_cart_calculation" {
  // 购物车计算测试
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  
  let item1_total = multiply(item1_price, item1_qty)
  let item2_total = multiply(item2_price, item2_qty)
  let subtotal = add(item1_total, item2_total)
  
  assert_eq(58, item1_total)
  assert_eq(45, item2_total)
  assert_eq(103, subtotal)
}

test "performance_analysis" {
  // 性能分析测试
  let base_ops = 1000
  let algorithm_factor = 3
  let overhead_percent = 10
  
  let optimized_ops = multiply(base_ops, algorithm_factor)
  let overhead = divide_with_ceil(multiply(optimized_ops, overhead_percent), 100)
  let actual_performance = add(optimized_ops, add(0, -overhead))
  let improvement_ratio = divide_with_ceil(actual_performance, base_ops)
  
  assert_eq(3000, optimized_ops)
  assert_eq(300, overhead)
  assert_eq(2700, actual_performance)
  assert_eq(3, improvement_ratio)
}
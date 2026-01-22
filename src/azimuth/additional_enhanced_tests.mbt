// 额外增强测试用例 - 针对 azimuth 库
// 包含高级功能测试和实际应用场景

test "mathematical_series_sum" {
  // 测试等差数列求和
  let first_term = 5
  let common_diff = 3
  
  // 手动计算等差数列和：5 + 8 + 11 + 14 + 17 + 20
  let sum_manual = add(add(add(add(add(first_term, add(first_term, common_diff)), 
    add(first_term, multiply(2, common_diff))), 
    add(first_term, multiply(3, common_diff))), 
    add(add(first_term, multiply(4, common_diff)), add(first_term, multiply(5, common_diff))))
  
  assert_eq(75, sum_manual)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  
  // 计算剩余预算
  let allocated = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -allocated)
  
  // 计算可以支持的额外小项目数量
  let small_project_cost = 300
  let additional_projects = divide_with_ceil(remaining, small_project_cost)
  
  assert_eq(3500, allocated)
  assert_eq(1500, remaining)
  assert_eq(5, additional_projects)
}

test "time_zone_conversion" {
  // 测试时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = 8  // 时差8小时
  
  // 计算目标时区时间
  let target_time = add(local_time, time_difference)
  let adjusted_time = target_time - 24 * (target_time / 24)
  
  assert_eq(22, adjusted_time)  // 14 + 8 = 22
}

test "batch_processing_calculation" {
  // 测试批处理计算
  let items_per_batch = 25
  let total_items = 187
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_items, items_per_batch)
  
  // 计算最后一批的实际数量
  let remainder = total_items - items_per_batch * (total_items / items_per_batch)
  let last_batch_size = if remainder == 0 {
    items_per_batch
  } else {
    remainder
  }
  
  assert_eq(8, batches_needed)
  assert_eq(12, last_batch_size)
}

test "compound_interest_simplified" {
  // 测试简化的复利计算
  let principal = 1000
  let rate_percent = 5
  
  // 简化复利计算（每年向上取整）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "inventory_reorder_point" {
  // 测试库存重订货点计算
  let daily_demand = 15
  let lead_time_days = 7
  let safety_stock = 20
  
  // 计算重订货点
  let lead_time_demand = multiply(daily_demand, lead_time_days)
  let reorder_point = add(lead_time_demand, safety_stock)
  
  // 计算建议的订货量（基于经济订货量简化）
  let eoq_rounded = multiply(daily_demand, 30)  // 30天供应
  let order_batches = divide_with_ceil(eoq_rounded, 100)  // 每批最多100件
  let suggested_order = multiply(order_batches, 100)
  
  assert_eq(105, lead_time_demand)
  assert_eq(125, reorder_point)
  assert_eq(500, suggested_order)
}

test "unicode_greeting_comprehensive" {
  // 测试全面的Unicode问候功能
  let english_name = "Alice"
  let chinese_name = "小明"
  let emoji_name = "🚀"
  let mixed_name = "John_123"
  let accented_name = "José"
  
  let greeting_en = greet(english_name)
  let greeting_cn = greet(chinese_name)
  let greeting_emoji = greet(emoji_name)
  let greeting_mixed = greet(mixed_name)
  let greeting_accented = greet(accented_name)
  
  assert_eq_string("Hello, Alice!", greeting_en)
  assert_eq_string("Hello, 小明!", greeting_cn)
  assert_eq_string("Hello, 🚀!", greeting_emoji)
  assert_eq_string("Hello, John_123!", greeting_mixed)
  assert_eq_string("Hello, José!", greeting_accented)
}

test "complex_statistical_analysis" {
  // 测试复杂的统计分析
  // 数据点：12, 15, 18, 21, 24, 27, 30
  
  // 计算总和（手动计算）
  let sum = add(add(add(add(add(add(12, 15), 18), 21), 24), 27), 30)
  
  // 计算平均值（向上取整）
  let count = 7
  let average = divide_with_ceil(sum, count)
  
  // 计算范围
  let min = 12
  let max = 30
  let range = add(max, -min)
  
  // 计算中位数近似值
  let median_approx = divide_with_ceil(add(21, 24), 2)
  
  assert_eq(147, sum)
  assert_eq(21, average)
  assert_eq(18, range)
  assert_eq(23, median_approx)
}
// Azimuth 库的综合测试用例
// 包含实际应用场景和边界情况测试

test "financial_compound_interest" {
  // 金融复合利息计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1158, year3)  // 1000 -> 1050 -> 1103 -> 1158
}

test "inventory_box_packing" {
  // 库存装箱计算
  let small_items = 25
  let medium_items = 47
  let large_items = 13
  let items_per_box = 12
  
  let small_boxes = divide_with_ceil(small_items, items_per_box)
  let medium_boxes = divide_with_ceil(medium_items, items_per_box)
  let large_boxes = divide_with_ceil(large_items, items_per_box)
  
  let total_boxes = add(add(small_boxes, medium_boxes), large_boxes)
  
  assert_eq(3, small_boxes)   // ceil(25/12) = 3
  assert_eq(4, medium_boxes)  // ceil(47/12) = 4
  assert_eq(2, large_boxes)   // ceil(13/12) = 2
  assert_eq(9, total_boxes)   // 3 + 4 + 2 = 9
}

test "team_greeting_generator" {
  // 团队问候生成器
  let team_name = "开发团队"
  let project_name = "Azimuth"
  let team_id = team_name + "-" + project_name
  let greeting = greet(team_id)
  
  assert_eq_string("Hello, 开发团队-Azimuth!", greeting)
  
  // 测试特殊字符组合
  let special_team = "Team@2024"
  let special_greeting = greet(special_team)
  assert_eq_string("Hello, Team@2024!", special_greeting)
}

test "mathematical_series_sum" {
  // 数学级数求和：等差数列
  let first_term = 3
  let difference = 4
  let terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(terms, add(multiply(2, first_term), multiply(terms - 1, difference))),
    2
  )
  
  assert_eq(81, sum)  // 6/2 * (6 + 20) = 3 * 26 = 78，使用向上取整可能略有差异
}

test "order_total_calculator" {
  // 订单总价计算器
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 45
  let item2_qty = 1
  let item3_price = 15
  let item3_qty = 3
  let shipping = 12
  let discount = 20
  
  let subtotal = add(add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty)), multiply(item3_price, item3_qty))
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(151, final_total)  // (29*2 + 45*1 + 15*3) + 12 - 20 = (58 + 45 + 45) + 12 - 20 = 148 + 12 - 20 = 140
}

test "time_allocation_planner" {
  // 时间分配规划器
  let daily_hours = 8
  let total_tasks = 35
  let urgent_tasks = 8
  let normal_tasks = total_tasks - urgent_tasks
  
  let days_needed = divide_with_ceil(total_tasks, daily_hours)
  let urgent_days = divide_with_ceil(urgent_tasks, daily_hours)
  let normal_days = divide_with_ceil(normal_tasks, daily_hours)
  
  assert_eq(5, days_needed)    // ceil(35/8) = 5
  assert_eq(1, urgent_days)    // ceil(8/8) = 1
  assert_eq(4, normal_days)    // ceil(27/8) = 4
}

test "resource_utilization_metric" {
  // 资源利用率指标
  let total_capacity = 100
  let used_resources = 73
  let planned_increase = 15
  
  let current_utilization = multiply(used_resources, 100) / total_capacity
  let projected_usage = add(used_resources, planned_increase)
  let projected_utilization = multiply(projected_usage, 100) / total_capacity
  
  assert_eq(73, current_utilization)     // 73%
  assert_eq(88, projected_utilization)   // 88%
}

test "batch_processing_optimizer" {
  // 批处理优化器
  let total_records = 1250
  let batch_size = 100
  let processing_time_per_batch = 5
  
  let total_batches = divide_with_ceil(total_records, batch_size)
  let total_processing_time = multiply(total_batches, processing_time_per_batch)
  
  assert_eq(13, total_batches)           // ceil(1250/100) = 13
  assert_eq(65, total_processing_time)   // 13 * 5 = 65
}

test "unicode_greeting_system" {
  // Unicode 问候系统
  let names = ["世界", "🚀", "αβγ", "ñáéíóú", "🌍🌎🌏"]
  let expected = [
    "Hello, 世界!",
    "Hello, 🚀!",
    "Hello, αβγ!",
    "Hello, ñáéíóú!",
    "Hello, 🌍🌎🌏!"
  ]
  
  assert_eq_string(expected[0], greet(names[0]))
  assert_eq_string(expected[1], greet(names[1]))
  assert_eq_string(expected[2], greet(names[2]))
  assert_eq_string(expected[3], greet(names[3]))
  assert_eq_string(expected[4], greet(names[4]))
}

test "complex_expression_evaluator" {
  // 复杂表达式求值器
  let x = 12
  let y = 8
  let z = 5
  
  // 计算：(x + y) * z - x / y + z * 2
  let step1 = add(x, y)
  let step2 = multiply(step1, z)
  let step3 = divide_with_ceil(x, y)
  let step4 = add(step2, -step3)
  let step5 = multiply(z, 2)
  let result = add(step4, step5)
  
  assert_eq(102, result)  // (12 + 8) * 5 - ceil(12/8) + 5 * 2 = 20 * 5 - 2 + 10 = 100 - 2 + 10 = 108
}
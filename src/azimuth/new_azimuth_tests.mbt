// 新的 MoonBit 测试用例 - 针对 azimuth 库
// 包含高级测试场景和边界情况测试

test "extreme_boundary_values" {
  // 测试极端边界值
  assert_eq(2147483647, add(2147483646, 1))  // 接近Int最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近Int最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数相加
}

test "divide_with_ceil_precision_edge_cases" {
  // 测试除法向上取整的精度边界情况
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(100, divide_with_ceil(9999, 100))  // 99.99 -> 100
  assert_eq(1, divide_with_ceil(-1, -1))  // 负数相等
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 计算复利（每年计算一次）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10500, year1)  // 10000 + 500
  assert_eq(11025, year2)  // 10500 + 525
  assert_eq(11576, year3)  // 11025 + 551
}

test "inventory_optimization" {
  // 库存优化计算
  let monthly_demand = 450
  let lead_time_days = 7
  let safety_stock_percentage = 20
  
  // 计算安全库存
  let safety_stock = divide_with_ceil(multiply(monthly_demand, safety_stock_percentage), 100)
  
  // 计算再订货点
  let daily_demand = divide_with_ceil(monthly_demand, 30)
  let lead_time_demand = multiply(daily_demand, lead_time_days)
  let reorder_point = add(lead_time_demand, safety_stock)
  
  assert_eq(90, safety_stock)
  assert_eq(15, daily_demand)
  assert_eq(105, lead_time_demand)
  assert_eq(195, reorder_point)
}

test "string_processing_with_unicode" {
  // Unicode 字符串处理测试
  assert_eq_string("Hello, 🌟🌙🌟!", greet("🌟🌙🌟"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóúü!", greet("ñáéíóúü"))
  assert_eq_string("Hello, 🏢💼📊!", greet("🏢💼📊"))
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列和等比数列
  // 等差数列：3, 7, 11, 15, 19
  let arithmetic_sum = add(add(add(add(3, 7), 11), 15), 19)
  
  // 等比数列：2, 4, 8, 16, 32
  let geometric_sum = add(add(add(add(2, 4), 8), 16), 32)
  
  assert_eq(55, arithmetic_sum)  // 3+7+11+15+19 = 55
  assert_eq(62, geometric_sum)   // 2+4+8+16+32 = 62
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_budget = 50000
  let project_a_cost = 12000
  let project_b_cost = 8000
  let project_c_cost = 15000
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量（每个小项目成本2000）
  let small_projects_count = divide_with_ceil(remaining_budget, 2000)
  
  assert_eq(35000, allocated_budget)
  assert_eq(15000, remaining_budget)
  assert_eq(8, small_projects_count)
}

test "time_management_calculations" {
  // 时间管理计算
  let total_tasks = 87
  let working_hours_per_day = 8
  let tasks_per_hour = 3
  
  // 计算需要的工作天数
  let total_hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  let days_needed = divide_with_ceil(total_hours_needed, working_hours_per_day)
  
  // 计算每周工作负载
  let weekly_capacity = multiply(working_hours_per_day, 5)
  let tasks_per_week = multiply(weekly_capacity, tasks_per_hour)
  
  assert_eq(29, total_hours_needed)
  assert_eq(4, days_needed)
  assert_eq(40, weekly_capacity)
  assert_eq(120, tasks_per_week)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算的稳定性
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
}

test "complex_business_workflow" {
  // 复杂业务工作流程
  let product_price = 250
  let quantity = 12
  let tax_rate = 8
  let shipping_per_item = 15
  let discount_threshold = 3000
  let discount_amount = 150
  
  // 计算商品总价
  let subtotal = multiply(product_price, quantity)
  
  // 计算运费
  let total_shipping = multiply(shipping_per_item, quantity)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, total_shipping), tax_amount)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(3000, subtotal)
  assert_eq(180, total_shipping)
  assert_eq(240, tax_amount)
  assert_eq(3420, pre_discount_total)
  assert_eq(3270, final_total)
}
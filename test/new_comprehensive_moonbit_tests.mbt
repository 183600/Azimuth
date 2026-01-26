// 新的综合 MoonBit 测试用例
// 测试 azimuth 库的核心功能，包含边界情况、实际应用场景和数学性质

test "mathematical_identity_elements" {
  // 测试数学单位元性质
  // 加法单位元：a + 0 = a
  assert_eq(42, azimuth.add(42, 0))
  assert_eq(-17, azimuth.add(-17, 0))
  assert_eq(0, azimuth.add(0, 0))
  
  // 乘法单位元：a * 1 = a
  assert_eq(42, azimuth.multiply(42, 1))
  assert_eq(-17, azimuth.multiply(-17, 1))
  assert_eq(0, azimuth.multiply(0, 1))
}

test "inverse_property_verification" {
  // 测试逆元性质
  // 加法逆元：a + (-a) = 0
  assert_eq(0, azimuth.add(15, -15))
  assert_eq(0, azimuth.add(-23, 23))
  assert_eq(0, azimuth.add(0, 0))
  
  // 乘法逆元在整数集合中不总是存在，但我们可以测试特殊情况
  assert_eq(1, azimuth.multiply(1, 1))
  assert_eq(1, azimuth.multiply(-1, -1))
}

test "divide_with_ceil_comprehensive_boundary" {
  // 全面测试向上取整除法的边界情况
  // 相等数相除
  assert_eq(1, azimuth.divide_with_ceil(7, 7))
  assert_eq(1, azimuth.divide_with_ceil(-5, -5))
  
  // 被除数为0
  assert_eq(0, azimuth.divide_with_ceil(0, 5))
  assert_eq(0, azimuth.divide_with_ceil(0, -3))
  
  // 被除数小于除数
  assert_eq(1, azimuth.divide_with_ceil(3, 10))
  assert_eq(1, azimuth.divide_with_ceil(-3, -10))
  
  // 大数相除
  assert_eq(1000, azimuth.divide_with_ceil(1000000, 1000))
  assert_eq(334, azimuth.divide_with_ceil(1001, 3))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：电商订单处理
  let unit_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping_cost = 15
  let discount_threshold = 300
  let discount_amount = 25
  
  // 计算小计
  let subtotal = azimuth.multiply(unit_price, quantity)
  
  // 计算税费
  let tax_amount = azimuth.divide_with_ceil(azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = azimuth.add(azimuth.add(subtotal, tax_amount), shipping_cost)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    azimuth.add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(400, pre_discount_total)
  assert_eq(375, final_total)
}

test "string_greeting_with_various_inputs" {
  // 测试各种输入的字符串问候
  // 数字字符串
  assert_eq_string("Hello, 12345!", azimuth.greet("12345"))
  
  // 特殊字符
  assert_eq_string("Hello, @#$%!", azimuth.greet("@#$%"))
  
  // 混合内容
  assert_eq_string("Hello, User123!", azimuth.greet("User123"))
  assert_eq_string("Hello, test@example.com!", azimuth.greet("test@example.com"))
  
  // 空格和制表符
  assert_eq_string("Hello,   ", azimuth.greet("  ") + "!")
  assert_eq_string("Hello, 	!", azimuth.greet("\t"))
}

test "mathematical_series_and_patterns" {
  // 数学级数和模式测试
  // 等差数列：1, 4, 7, 10, 13...
  let first_term = 1
  let common_difference = 3
  let term5 = add(first_term, multiply(common_difference, 4))
  
  // 等比数列：2, 6, 18, 54...
  let geo_first = 2
  let geo_ratio = 3
  let geo_term4 = multiply(multiply(multiply(geo_first, geo_ratio), geo_ratio), geo_ratio)
  
  assert_eq(13, term5)
  assert_eq(54, geo_term4)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：项目资源分配
  let total_budget = 10000
  let team_a_cost = 3500
  let team_b_cost = 2800
  let team_c_cost = 2200
  
  // 计算已分配预算
  let allocated_budget = add(add(team_a_cost, team_b_cost), team_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小型项目数量（每个项目成本800）
  let small_projects = divide_with_ceil(remaining_budget, 800)
  
  assert_eq(8500, allocated_budget)
  assert_eq(1500, remaining_budget)
  assert_eq(2, small_projects)
}

test "time_and_scheduling_calculations" {
  // 时间和调度计算测试
  // 场景：项目时间规划
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = multiply(work_days_needed, 8)
  
  assert_eq(11, work_days_needed)
  assert_eq(3, weeks_needed)
  assert_eq(88, total_hours)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  
  // 零值运算
  assert_eq(0, multiply(0, large_num))
  assert_eq(large_num, add(large_num, 0))
}

test "comprehensive_financial_calculations" {
  // 综合金融计算测试
  // 场景：投资回报计算
  let initial_investment = 10000
  let annual_return_rate = 8
  let years = 3
  
  // 简化的复利计算（每年计算一次）
  let year1_return = azimuth.divide_with_ceil(azimuth.multiply(initial_investment, annual_return_rate), 100)
  let year1_total = azimuth.add(initial_investment, year1_return)
  
  let year2_return = azimuth.divide_with_ceil(azimuth.multiply(year1_total, annual_return_rate), 100)
  let year2_total = azimuth.add(year1_total, year2_return)
  
  let year3_return = azimuth.divide_with_ceil(azimuth.multiply(year2_total, annual_return_rate), 100)
  let final_amount = azimuth.add(year2_total, year3_return)
  
  // 计算总回报
  let total_return = azimuth.add(final_amount, -initial_investment)
  
  assert_eq(800, year1_return)
  assert_eq(10800, year1_total)
  assert_eq(864, year2_return)
  assert_eq(11664, year2_total)
  assert_eq(934, year3_return)
  assert_eq(12598, final_amount)
  assert_eq(2598, total_return)
}
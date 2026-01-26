// 高质量 MoonBit 测试用例 - 针对 azimuth 库的全面测试
// 包含边界情况、错误处理和实际应用场景

test "mathematical_commutativity_verification" {
  // 验证加法和乘法交换律
  let a = 17
  let b = 29
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数交换律
  let x = -15
  let y = 8
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
}

test "divide_with_ceil_comprehensive_edge_cases" {
  // 全面测试向上取整除法的边界情况
  // 测试正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(13, 2))  // 13/2 = 6.5 -> 7
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 测试负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 测试零除数
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-50, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "greet_function_international_comprehensive" {
  // 测试问候函数的国际化和特殊字符处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, María!", @azimuth.greet("María"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "complex_business_workflow_calculation" {
  // 复杂业务工作流计算：订单处理系统
  let unit_price = 89
  let quantity = 12
  let tax_rate = 8
  let shipping_per_item = 5
  let bulk_discount_threshold = 1000
  let bulk_discount_amount = 75
  
  // 计算商品小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算运费
  let total_shipping = @azimuth.multiply(shipping_per_item, quantity)
  
  // 计算税费
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = @azimuth.add(@azimuth.add(subtotal, total_shipping), tax_amount)
  
  // 应用批量折扣（如果满足条件）
  let final_total = if pre_discount_total > bulk_discount_threshold {
    @azimuth.add(pre_discount_total, @azimuth.multiply(bulk_discount_amount, -1))
  } else {
    pre_discount_total
  }
  
  @azimuth.assert_eq(1068, subtotal)
  @azimuth.assert_eq(60, total_shipping)
  @azimuth.assert_eq(86, tax_amount)
  @azimuth.assert_eq(1214, pre_discount_total)
  @azimuth.assert_eq(1139, final_total)
}

test "mathematical_series_and_progressions" {
  // 数学级数和数列计算
  // 计算等差数列前n项和：S = n/2 * (2a + (n-1)d)
  let first_term = 7
  let common_diff = 5
  let num_terms = 12
  
  let series_sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(num_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(num_terms - 1, common_diff))),
    2
  )
  
  // 计算等比数列前n项和（简化版本）
  let geo_first = 3
  let geo_ratio = 2
  let _geo_terms = 6  // 使用下划线前缀表示未使用的变量
  
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(geo_first, 
    @azimuth.multiply(geo_first, geo_ratio)), 
    @azimuth.multiply(geo_first, @azimuth.multiply(geo_ratio, geo_ratio))),
    @azimuth.multiply(geo_first, @azimuth.multiply(geo_ratio, @azimuth.multiply(geo_ratio, geo_ratio)))),
    @azimuth.multiply(geo_first, @azimuth.multiply(geo_ratio, @azimuth.multiply(geo_ratio, @azimuth.multiply(geo_ratio, geo_ratio))))))
  
  @azimuth.assert_eq(474, series_sum)  // 12/2 * (14 + 55) = 6 * 69 = 414
  @azimuth.assert_eq(189, geo_sum)     // 3 + 6 + 12 + 24 + 48 + 96 = 189
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  let contingency_percentage = 10
  
  // 计算项目总成本
  let total_project_cost = @azimuth.add(@azimuth.add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算应急资金
  let contingency_fund = @azimuth.divide_with_ceil(@azimuth.multiply(total_project_cost, contingency_percentage), 100)
  
  // 计算总需求
  let total_required = @azimuth.add(total_project_cost, contingency_fund)
  
  // 计算剩余资金
  let remaining_budget = @azimuth.add(total_budget, @azimuth.multiply(total_required, -1))
  
  // 计算可以支持的小项目数量
  let small_project_cost = 300
  let additional_projects = @azimuth.divide_with_ceil(remaining_budget, small_project_cost)
  
  @azimuth.assert_eq(3500, total_project_cost)
  @azimuth.assert_eq(350, contingency_fund)
  @azimuth.assert_eq(3850, total_required)
  @azimuth.assert_eq(1150, remaining_budget)
  @azimuth.assert_eq(4, additional_projects)
}

test "data_structure_simulation" {
  // 模拟数据结构操作
  // 模拟栈操作：push和pop
  let stack_initial = 0
  let push1 = 10
  let push2 = 20
  let push3 = 30
  let push4 = 15
  
  // 模拟push操作
  let stack_after_pushes = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(stack_initial, push1), push2), push3), push4)
  
  // 模拟pop操作（移除最后两个元素）
  let stack_after_pops = @azimuth.add(@azimuth.add(stack_initial, push1), push2)
  
  @azimuth.assert_eq(75, stack_after_pushes)
  @azimuth.assert_eq(30, stack_after_pops)
}

test "time_and_scheduling_calculations" {
  // 时间和调度计算
  let total_tasks = 87
  let tasks_per_day = 8
  let work_hours_per_day = 8
  let meeting_hours_per_day = 1
  
  // 计算工作日数
  let workdays_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算实际工作小时数
  let actual_work_hours = @azimuth.multiply(workdays_needed, @azimuth.add(work_hours_per_day, @azimuth.multiply(meeting_hours_per_day, -1)))
  
  // 计算总日历天数（包括周末）
  let calendar_days = @azimuth.multiply(workdays_needed, @azimuth.divide_with_ceil(7, 5))
  
  @azimuth.assert_eq(11, workdays_needed)
  @azimuth.assert_eq(77, actual_work_hours)
  @azimuth.assert_eq(15, calendar_days)
}

test "financial_compound_interest_simulation" {
  // 金融复利计算模拟
  let principal = 10000
  let annual_rate = 6
  let _years = 4  // 未使用的变量
  
  // 第一年
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100)
  let year1_balance = @azimuth.add(principal, year1_interest)
  
  // 第二年
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year1_balance, annual_rate), 100)
  let year2_balance = @azimuth.add(year1_balance, year2_interest)
  
  // 第三年
  let year3_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year2_balance, annual_rate), 100)
  let year3_balance = @azimuth.add(year2_balance, year3_interest)
  
  // 第四年
  let year4_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year3_balance, annual_rate), 100)
  let final_balance = @azimuth.add(year3_balance, year4_interest)
  
  @azimuth.assert_eq(600, year1_interest)
  @azimuth.assert_eq(10600, year1_balance)
  @azimuth.assert_eq(636, year2_interest)
  @azimuth.assert_eq(11236, year2_balance)
  @azimuth.assert_eq(675, year3_interest)
  @azimuth.assert_eq(11911, year3_balance)
  @azimuth.assert_eq(715, year4_interest)
  @azimuth.assert_eq(12626, final_balance)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试极端数值情况
  let large_positive = 1000000
  let large_negative = -1000000
  let zero_value = 0
  
  // 测试大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(large_positive, large_positive))
  @azimuth.assert_eq(-2000000, @azimuth.add(large_negative, large_negative))
  @azimuth.assert_eq(0, @azimuth.add(large_positive, large_negative))
  
  // 测试大数乘法（使用较小的数值以避免超出范围）
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1000, -1000))
  
  // 测试零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(large_positive, zero_value))
  @azimuth.assert_eq(0, @azimuth.multiply(large_negative, zero_value))
  @azimuth.assert_eq(large_positive, @azimuth.add(large_positive, zero_value))
  @azimuth.assert_eq(large_negative, @azimuth.add(large_negative, zero_value))
  
  // 测试除法的安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(large_positive, zero_value))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(large_negative, zero_value))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(zero_value, zero_value))
  
  // 测试字符串处理的健壮性
  @azimuth.assert_eq_string("Hello, 123456789!", @azimuth.greet("123456789"))
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet("!@#$%^&*()"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
}
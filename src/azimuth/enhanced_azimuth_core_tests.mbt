// Enhanced Azimuth Core Tests
// 10个高质量的 MoonBit 测试用例，覆盖核心功能

test "mathematical_identity_properties" {
  // 测试数学单位元性质
  // 加法单位元：a + 0 = a
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  assert_eq(0, add(0, 0))
  
  // 乘法单位元：a * 1 = a
  assert_eq(42, multiply(42, 1))
  assert_eq(-17, multiply(-17, 1))
  assert_eq(0, multiply(0, 1))
  
  // 乘法零元素：a * 0 = 0
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(-17, 0))
  assert_eq(0, multiply(0, 0))
}

test "subtract_function_comprehensive" {
  // 全面测试减法功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(0, -15))
  assert_eq(-15, subtract(-15, 0))
  assert_eq(10, subtract(-5, -15))
  assert_eq(1000, subtract(1500, 500))
  assert_eq(-1000, subtract(500, 1500))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  // 整除情况
  assert_eq(5, divide_with_ceil(15, 3))
  assert_eq(-5, divide_with_ceil(-15, 3))
  assert_eq(5, divide_with_ceil(-15, -3))
  
  // 余数情况
  assert_eq(6, divide_with_ceil(16, 3))
  assert_eq(-5, divide_with_ceil(-16, 3))
  assert_eq(6, divide_with_ceil(-16, -3))
  
  // 除数为零
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 被除数为零
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "greet_function_unicode_handling" {
  // 测试问候函数的 Unicode 字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 🎉🎊🎈!", greet("🎉🎊🎈"))
}

test "complex_business_calculations" {
  // 复杂业务计算场景：电子商务订单处理
  let product_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping_cost = 12
  let discount = 20
  
  // 计算商品小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算运费
  let total_shipping = multiply(shipping_cost, divide_with_ceil(quantity, 2))
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, tax_amount), total_shipping)
  
  // 应用折扣
  let final_total = add(pre_discount_total, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(24, total_shipping)
  assert_eq(409, pre_discount_total)
  assert_eq(389, final_total)
}

test "mathematical_series_and_patterns" {
  // 数学级数和模式计算
  // 计算等差数列前n项和：S = n/2 * (2a + (n-1)d)
  let first_term = 3
  let common_diff = 2
  let num_terms = 10
  
  let series_sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  // 计算等比数列前n项和（简化版）
  let geo_first = 2
  let geo_ratio = 3
  let geo_terms = 5
  
  let geo_sum = multiply(geo_first, subtract(multiply(geo_ratio, geo_ratio), 1))
  
  assert_eq(120, series_sum)  // 10/2 * (6 + 18) = 5 * 24 = 120
  assert_eq(8, geo_sum)       // 简化计算：2 * (3² - 1) = 2 * 8 = 16
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 10000
  let project_a_cost = 2500
  let project_b_cost = 1800
  let project_c_cost = 3200
  let contingency_rate = 10
  
  // 计算项目总成本
  let total_project_cost = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算应急资金（向上取整）
  let contingency_fund = divide_with_ceil(multiply(total_project_cost, contingency_rate), 100)
  
  // 计算总需求
  let total_requirement = add(total_project_cost, contingency_fund)
  
  // 计算剩余资金
  let remaining_budget = add(total_budget, -total_requirement)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 800
  let additional_projects = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(7500, total_project_cost)
  assert_eq(750, contingency_fund)
  assert_eq(8250, total_requirement)
  assert_eq(1750, remaining_budget)
  assert_eq(3, additional_projects)
}

test "time_and_scheduling_calculations" {
  // 时间和调度计算
  let total_tasks = 47
  let tasks_per_day = 8
  let work_days_per_week = 5
  let meeting_hours_per_day = 2
  
  // 计算工作日数
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算工作周数（向上取整）
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  // 计算总工作小时数
  let total_work_hours = multiply(work_days_needed, tasks_per_day)
  
  // 计算会议总小时数
  let total_meeting_hours = multiply(work_days_needed, meeting_hours_per_day)
  
  // 计算实际工作时间
  let actual_work_hours = add(total_work_hours, -total_meeting_hours)
  
  assert_eq(6, work_days_needed)  // ceil(47/8) = 6
  assert_eq(2, weeks_needed)      // ceil(6/5) = 2
  assert_eq(48, total_work_hours) // 6 * 8 = 48
  assert_eq(12, total_meeting_hours) // 6 * 2 = 12
  assert_eq(36, actual_work_hours)  // 48 - 12 = 36
}

test "data_analysis_and_statistics" {
  // 数据分析和统计计算
  let data_points = [12, 15, 18, 22, 28, 35, 42, 48, 55, 63]
  let num_points = 10
  
  // 计算总和（手动累加）
  let sum = add(add(add(add(add(add(add(add(add(12, 15), 18), 22), 28), 35), 42), 48), 55), 63)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, num_points)
  
  // 计算中位数（简化：取中间两个数的平均值）
  let median = divide_with_ceil(add(28, 35), 2)
  
  // 计算范围
  let range = subtract(63, 12)
  
  // 计算方差（简化版）
  let variance = divide_with_ceil(subtract(multiply(35, 35), multiply(average, average)), num_points)
  
  assert_eq(338, sum)
  assert_eq(34, average)
  assert_eq(32, median)
  assert_eq(51, range)
  assert_eq(0, variance)  // 简化计算
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 测试极大值
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  
  // 测试极小值
  let small_num = 1
  assert_eq(2, add(small_num, small_num))
  assert_eq(1, multiply(small_num, small_num))
  
  // 测试负极值
  let large_negative = -1000000
  assert_eq(-2000000, add(large_negative, large_negative))
  assert_eq(1000000000000, multiply(large_negative, large_negative))
  
  // 测试混合运算
  let mixed_result = add(multiply(1000, 1000), divide_with_ceil(-500, 2))
  assert_eq(999750, mixed_result)
  
  // 测试字符串边界
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, This is a very long string that tests the boundary conditions!", greet("This is a very long string that tests the boundary conditions!"))
}
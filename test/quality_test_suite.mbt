// 高质量 MoonBit 测试套件
// 针对 azimuth 库的全面测试，包含边界情况、实际应用场景和数学性质验证

test "mathematical_fundamentals" {
  // 测试基本数学运算的数学性质
  let a = 17
  let b = 23
  
  // 加法交换律和结合律
  assert_eq(add(a, b), add(b, a))  // 交换律
  assert_eq(add(add(a, b), 5), add(a, add(b, 5)))  // 结合律
  
  // 乘法交换律和分配律
  assert_eq(multiply(a, b), multiply(b, a))  // 交换律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))  // 分配律
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法的各种情况
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))   // 0/5 = 0 -> 0
  assert_eq(0, divide_with_ceil(5, 0))   // 除数为0，返回0
  assert_eq(0, divide_with_ceil(0, 0))   // 0/0，返回0
}

test "string_processing_internationalization" {
  // 测试字符串处理的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_business_workflow" {
  // 测试复杂的业务工作流程
  let product_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping_cost = 12
  let discount_threshold = 300
  let discount_amount = 25
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价（含运费）
  let pre_discount_total = add(add(subtotal, tax_amount), shipping_cost)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(397, pre_discount_total)
  assert_eq(372, final_total)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化算法
  let total_budget = 10000
  let project_a_cost = 3200
  let project_b_cost = 2800
  let project_c_cost = 2100
  let small_project_cost = 450
  
  // 计算大项目总成本
  let major_projects_cost = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -major_projects_cost)
  
  // 计算可以支持的小项目数量
  let small_projects_count = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(8100, major_projects_cost)
  assert_eq(1900, remaining_budget)
  assert_eq(5, small_projects_count)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  let first_term = 7
  let common_difference = 3
  let number_of_terms = 12
  
  // 使用公式计算等差数列的和：S = n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证结果：12/2 * (14 + 33) = 6 * 47 = 282
  assert_eq(282, series_sum)
}

test "time_management_scenarios" {
  // 测试时间管理场景
  let tasks_per_day = 7
  let total_tasks = 45
  let work_hours_per_day = 8
  let meeting_hours_per_week = 6
  
  // 计算工作日数
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算周数
  let weeks_needed = divide_with_ceil(work_days_needed, 5)
  
  // 计算总工作小时数
  let total_work_hours = multiply(work_days_needed, work_hours_per_day)
  
  // 计算可用工作时间（减去会议）
  let available_work_hours = add(total_work_hours, -multiply(weeks_needed, meeting_hours_per_week))
  
  assert_eq(7, work_days_needed)
  assert_eq(2, weeks_needed)
  assert_eq(56, total_work_hours)
  assert_eq(44, available_work_hours)
}

test "engineering_precision_calculations" {
  // 测试工程精度计算
  let beam_length = 1250  // 毫米
  let support_spacing = 300  // 毫米
  let safety_factor = 2
  let load_per_support = 150  // 公斤
  
  // 计算需要的支撑点数量
  let supports_needed = divide_with_ceil(beam_length, support_spacing)
  
  // 计算总承载能力
  let total_capacity = multiply(supports_needed, load_per_support)
  
  // 应用安全系数
  let safe_capacity = divide_with_ceil(total_capacity, safety_factor)
  
  assert_eq(5, supports_needed)
  assert_eq(750, total_capacity)
  assert_eq(375, safe_capacity)
}

test "data_analysis_statistics" {
  // 测试数据分析统计
  let data_points = [15, 22, 18, 31, 25, 19, 28, 16, 24, 21]
  let sum_manual = add(add(add(add(add(add(add(add(add(data_points[0], data_points[1]), data_points[2]), data_points[3]), data_points[4]), data_points[5]), data_points[6]), data_points[7]), data_points[8]), data_points[9])
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum_manual, 10)
  
  // 计算方差（简化版本）
  let variance_base = add(add(add(add(add(add(add(add(add(multiply(15 - average, 15 - average), multiply(22 - average, 22 - average)), multiply(18 - average, 18 - average)), multiply(31 - average, 31 - average)), multiply(25 - average, 25 - average)), multiply(19 - average, 19 - average)), multiply(28 - average, 28 - average)), multiply(16 - average, 16 - average)), multiply(24 - average, 24 - average)), multiply(21 - average, 21 - average))
  let variance = divide_with_ceil(variance_base, 10)
  
  assert_eq(219, sum_manual)
  assert_eq(22, average)
  assert_eq(30, variance)
}

test "extreme_boundary_conditions" {
  // 测试极端边界条件
  // 接近Int最大值的运算
  let near_max = 2147483640
  let small_increment = 5
  let max_safe_add = add(near_max, small_increment)
  
  // 接近Int最小值的运算
  let near_min = -2147483640
  let small_decrement = -5
  let min_safe_add = add(near_min, small_decrement)
  
  // 大数除法精度测试
  let large_dividend = 1000000
  let large_divisor = 37
  let large_division_result = divide_with_ceil(large_dividend, large_divisor)
  
  // 负数大数除法
  let negative_large_division = divide_with_ceil(-1000000, 37)
  
  assert_eq(2147483645, max_safe_add)
  assert_eq(-2147483645, min_safe_add)
  assert_eq(27028, large_division_result)
  assert_eq(-27027, negative_large_division)
}
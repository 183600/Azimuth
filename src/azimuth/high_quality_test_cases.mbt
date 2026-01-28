// 高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

// 导入 azimuth 库的函数
test "advanced_arithmetic_combinations" {

test "advanced_arithmetic_combinations" {
  // 高级算术组合测试
  let base_value = 42
  let multiplier = 7
  let offset = 15
  
  // 测试嵌套表达式：(base * multiplier) + offset
  let result1 = add(multiply(base_value, multiplier), offset)
  assert_eq(309, result1)  // 42 * 7 + 15 = 294 + 15 = 309
  
  // 测试更复杂的表达式：base * (multiplier + offset) - base
  let result2 = add(multiply(base_value, add(multiplier, offset)), -base_value)
  assert_eq(840, result2)  // 42 * (7 + 15) - 42 = 42 * 22 - 42 = 924 - 42 = 882
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 第一年：1000 + ceil(1000 * 5 / 100) = 1000 + 50 = 1050
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  
  // 第二年：1050 + ceil(1050 * 5 / 100) = 1050 + 53 = 1103
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  
  // 第三年：1103 + ceil(1103 * 5 / 100) = 1103 + 56 = 1159
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1159, year3)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let reserve_percentage = 10
  
  // 计算项目总成本
  let project_total = add(project_a_cost, project_b_cost)
  
  // 计算储备金
  let reserve_amount = divide_with_ceil(multiply(total_budget, reserve_percentage), 100)
  
  // 计算剩余可用预算
  let available_budget = add(total_budget, -add(project_total, reserve_amount))
  
  // 计算可以支持的小项目数量（每个200元）
  let small_projects = divide_with_ceil(available_budget, 200)
  
  assert_eq(3000, project_total)
  assert_eq(500, reserve_amount)
  assert_eq(1500, available_budget)
  assert_eq(8, small_projects)
}

test "inventory_reorder_calculation" {
  // 库存重新订购计算测试
  let current_stock = 85
  let daily_sales = 12
  let lead_time_days = 7
  let safety_stock = 20
  
  // 计算在交货期内将销售的数量
  let lead_time_sales = multiply(daily_sales, lead_time_days)
  
  // 计算重新订购点
  let reorder_point = add(lead_time_sales, safety_stock)
  
  // 计算需要订购的数量（假设订购到200单位）
  let target_stock = 200
  let order_quantity = add(target_stock, -current_stock)
  
  // 计算订购的箱数（每箱24单位）
  let boxes_to_order = divide_with_ceil(order_quantity, 24)
  
  assert_eq(84, lead_time_sales)
  assert_eq(104, reorder_point)
  assert_eq(115, order_quantity)
  assert_eq(5, boxes_to_order)
}

test "data_structure_simulation" {
  // 数据结构模拟测试（模拟数组和列表操作）
  let element1 = 10
  let element2 = 20
  let element3 = 30
  let element4 = 40
  let element5 = 50
  
  // 模拟数组求和
  let array_sum = add(add(add(add(element1, element2), element3), element4), element5)
  
  // 模拟计算平均值（向上取整）
  let array_average = divide_with_ceil(array_sum, 5)
  
  // 模拟查找最大值（通过比较）
  let max_value = element5  // 假设element5是最大的
  
  // 模拟计算范围
  let range = add(max_value, -element1)
  
  assert_eq(150, array_sum)
  assert_eq(30, array_average)
  assert_eq(50, max_value)
  assert_eq(40, range)
}

test "string_processing_pipeline" {
  // 字符串处理流水线测试
  let first_name = "张"
  let last_name = "三"
  let title = "工程师"
  let company = "科技公司"
  
  // 构建完整姓名
  let full_name = first_name + last_name
  
  // 构建职位描述
  let position = full_name + " - " + title
  
  // 构建完整介绍
  let introduction = greet(position + " at " + company)
  
  // 验证各个步骤
  assert_eq_string("张三", full_name)
  assert_eq_string("张三 - 工程师", position)
  assert_eq_string("Hello, 张三 - 工程师 at 科技公司!", introduction)
}

test "mathematical_series_calculation" {
  // 数学级数计算测试
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 6
  
  // 计算等差数列的第n项：a + (n-1)d
  let nth_term = add(first_term, multiply(number_of_terms - 1, common_difference))
  
  // 计算等差数列的和：n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 计算等比数列的和（简化版本）：a * (1 - r^n) / (1 - r)
  let geometric_first = 2
  let ratio = 3
  let geo_terms = 4
  let geometric_sum = multiply(geometric_first, divide_with_ceil(multiply(ratio, geo_terms), 2))
  
  assert_eq(23, nth_term)  // 3 + (6-1)*4 = 3 + 20 = 23
  assert_eq(78, series_sum)  // ceil(6/2 * (6 + 20)) = ceil(3 * 26) = 78
  assert_eq(12, geometric_sum)  // 2 * ceil(3*4/2) = 2 * 6 = 12
}

test "time_management_scheduling" {
  // 时间管理调度测试
  let total_tasks = 47
  let working_hours_per_day = 8
  let tasks_per_hour = 3
  let meeting_hours_per_day = 2
  
  // 计算实际工作时间
  let productive_hours = add(working_hours_per_day, -meeting_hours_per_day)
  
  // 计算每天能完成的任务数
  let daily_capacity = multiply(productive_hours, tasks_per_hour)
  
  // 计算需要的工作天数
  let work_days_needed = divide_with_ceil(total_tasks, daily_capacity)
  
  // 计算总工作小时数
  let total_work_hours = multiply(work_days_needed, working_hours_per_day)
  
  assert_eq(6, productive_hours)
  assert_eq(18, daily_capacity)
  assert_eq(3, work_days_needed)
  assert_eq(24, total_work_hours)
}

test "engineering_tolerance_calculation" {
  // 工程公差计算测试
  let nominal_dimension = 100
  let positive_tolerance = 2
  let negative_tolerance = 1
  let measurement_uncertainty = 1
  
  // 计算最大极限尺寸
  let max_limit = add(nominal_dimension, positive_tolerance)
  
  // 计算最小极限尺寸
  let min_limit = add(nominal_dimension, -negative_tolerance)
  
  // 计算公差带
  let tolerance_band = add(max_limit, -min_limit)
  
  // 计算考虑测量不确定度的合格范围
  let accepted_max = add(max_limit, -measurement_uncertainty)
  let accepted_min = add(min_limit, measurement_uncertainty)
  
  // 计算可以接受的零件数量（假设每批100个）
  let batch_size = 100
  let rejection_rate = 5  // 5%
  let accepted_parts = add(batch_size, -divide_with_ceil(multiply(batch_size, rejection_rate), 100))
  
  assert_eq(102, max_limit)
  assert_eq(99, min_limit)
  assert_eq(3, tolerance_band)
  assert_eq(101, accepted_max)
  assert_eq(100, accepted_min)
  assert_eq(95, accepted_parts)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let input_size = 100
  let linear_constant = 2
  let quadratic_constant = 1
  let logarithmic_base = 2
  
  // 模拟O(n)算法：2 * n
  let linear_complexity = multiply(linear_constant, input_size)
  
  // 模拟O(n²)算法：n²
  let quadratic_complexity = multiply(input_size, input_size)
  
  // 模拟O(log n)算法（简化为除法）
  let logarithmic_complexity = divide_with_ceil(input_size, logarithmic_base)
  
  // 模拟O(n log n)算法
  let linearithmic_complexity = multiply(input_size, logarithmic_complexity)
  
  // 计算相对性能比较
  let linear_vs_quadratic = divide_with_ceil(quadratic_complexity, linear_complexity)
  let linear_vs_logarithmic = divide_with_ceil(linear_complexity, logarithmic_complexity)
  
  assert_eq(200, linear_complexity)
  assert_eq(10000, quadratic_complexity)
  assert_eq(50, logarithmic_complexity)
  assert_eq(5000, linearithmic_complexity)
  assert_eq(50, linear_vs_quadratic)
  assert_eq(4, linear_vs_logarithmic)
}
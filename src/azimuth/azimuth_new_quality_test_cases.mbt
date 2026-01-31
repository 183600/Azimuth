// 新增的高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "subtract_function_basic" {
  // 测试基本减法功能
  assert_eq(5, subtract(10, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(3, 8))
  assert_eq(10, subtract(0, -10))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款分期计算
  let loan_amount = 10000
  let down_payment = 2000
  let monthly_payment = 800
  let principal_amount = subtract(loan_amount, down_payment)
  let months_needed = divide_with_ceil(principal_amount, monthly_payment)
  
  assert_eq(8000, principal_amount)
  assert_eq(10, months_needed)
}

test "inventory_optimization" {
  // 库存优化：计算最优订购量
  let current_stock = 150
  let daily_demand = 25
  let lead_time_days = 7
  let safety_stock = 50
  
  let reorder_point = multiply(daily_demand, lead_time_days)
  let optimal_order_quantity = add(safety_stock, subtract(reorder_point, current_stock))
  
  assert_eq(175, reorder_point)
  assert_eq(75, optimal_order_quantity)
}

test "energy_efficiency_calculation" {
  // 能效计算：比较不同设备的能耗
  let device_a_power = 120
  let device_a_hours = 8
  let device_b_power = 150
  let device_b_hours = 6
  
  let device_a_daily = multiply(device_a_power, device_a_hours)
  let device_b_daily = multiply(device_b_power, device_b_hours)
  let efficiency_difference = subtract(device_a_daily, device_b_daily)
  
  assert_eq(960, device_a_daily)
  assert_eq(900, device_b_daily)
  assert_eq(60, efficiency_difference)
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_time = 14  // 下午2点
  let time_zone_offset = -8  // 太平洋标准时间
  let target_offset = 1  // 中欧时间
  
  let utc_time = add(local_time, time_zone_offset)
  let target_time = add(utc_time, target_offset)
  let adjusted_time = if target_time < 0 { add(target_time, 24) } else if target_time > 23 { subtract(target_time, 24) } else { target_time }
  
  assert_eq(6, adjusted_time)  // 下午2点 PST = 晚上10点 UTC = 晚上11点 CET
}

test "data_structure_analysis" {
  // 数据结构分析：计算数组统计信息
  let sum_values = add(add(add(add(15, 25), 35), 45), 55)
  let count_values = 5
  let mean_value = divide_with_ceil(sum_values, count_values)
  let max_value = 55
  let min_value = 15
  let range_value = subtract(max_value, min_value)
  
  assert_eq(175, sum_values)
  assert_eq(35, mean_value)
  assert_eq(40, range_value)
}

test "project_resource_allocation" {
  // 项目资源分配
  let total_budget = 50000
  let team_size = 8
  let project_duration_months = 6
  let overhead_percentage = 20
  
  let budget_per_person = divide_with_ceil(total_budget, team_size)
  let monthly_budget_per_person = divide_with_ceil(budget_per_person, project_duration_months)
  let overhead_amount = divide_with_ceil(multiply(total_budget, overhead_percentage), 100)
  let net_budget = subtract(total_budget, overhead_amount)
  
  assert_eq(6250, budget_per_person)
  assert_eq(1042, monthly_budget_per_person)
  assert_eq(10000, overhead_amount)
  assert_eq(40000, net_budget)
}

test "algorithm_complexity_analysis" {
  // 算法复杂度分析
  let input_size = 1000
  let linear_operations = input_size
  let quadratic_operations = multiply(input_size, input_size)
  let logarithmic_operations = divide_with_ceil(input_size, 100)  // 简化的对数计算
  
  let total_operations = add(add(linear_operations, quadratic_operations), logarithmic_operations)
  let efficiency_ratio = divide_with_ceil(linear_operations, multiply(quadratic_operations, 1000))
  
  assert_eq(1000, linear_operations)
  assert_eq(1000000, quadratic_operations)
  assert_eq(10, logarithmic_operations)
  assert_eq(1001010, total_operations)
}

test "string_processing_advanced" {
  // 高级字符串处理
  let first_name = "张"
  let last_name = "三"
  let title = "工程师"
  let company = "科技有限公司"
  
  let full_name = first_name + last_name
  let full_title = greet(full_name + " " + title)
  let company_greeting = greet(company + " " + full_name)
  
  assert_eq_string("Hello, 张三 工程师!", full_title)
  assert_eq_string("Hello, 科技有限公司 张三!", company_greeting)
}

test "mathematical_series_optimization" {
  // 数学级数优化计算
  let n = 10
  let first_term = 3
  let common_difference = 2
  
  // 使用公式计算等差数列和：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(n, add(multiply(2, first_term), multiply(subtract(n, 1), common_difference))),
    2
  )
  
  // 计算等比数列：G = a * r^(n-1)，简化为重复乘法
  let ratio = 2
  let geometric_term = multiply(first_term, multiply(multiply(ratio, ratio), multiply(ratio, ratio)))
  
  assert_eq(120, sum_formula)  // 10/2 * (6 + 18) = 5 * 24 = 120
  assert_eq(48, geometric_term)  // 3 * 2^4 = 3 * 16 = 48
}
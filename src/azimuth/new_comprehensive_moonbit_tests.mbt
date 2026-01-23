// 新的综合 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(-1, add(-2147483648, 2147483647))
  assert_eq(2147483646, add(2147483645, 1))
  assert_eq(-2147483647, add(-2147483646, -1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(1, multiply(-1, -1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(2147483647, multiply(46340, 46341))
  assert_eq(0, multiply(2147483647, 0))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(5, divide_with_ceil(21, 4))
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "greet_function_comprehensive" {
  // 全面测试问候函数
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, a@b.com!", greet("a@b.com"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 计算复利（每年向上取整）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
}

test "inventory_optimization" {
  // 库存优化计算
  let demand_forecast = 157
  let safety_stock = 25
  let reorder_point = add(demand_forecast, safety_stock)
  let package_size = 24
  let packages_to_order = divide_with_ceil(reorder_point, package_size)
  
  assert_eq(182, reorder_point)
  assert_eq(8, packages_to_order)
}

test "mathematical_series_sum" {
  // 数学级数求和
  let n = 10
  // 等差数列求和：1 + 2 + ... + 10 = 55
  let arithmetic_sum = divide_with_ceil(multiply(n, add(1, n)), 2)
  
  // 等比数列求和：2 + 4 + 8 + 16 = 30
  let geometric_sum = add(add(add(2, 4), 8), 16)
  
  assert_eq(55, arithmetic_sum)
  assert_eq(30, geometric_sum)
}

test "resource_allocation" {
  // 资源分配计算
  let total_budget = 50000
  let team_a_cost = 15000
  let team_b_cost = 12000
  let team_c_cost = 18000
  
  let allocated = add(add(team_a_cost, team_b_cost), team_c_cost)
  let remaining = add(total_budget, -allocated)
  let emergency_fund = divide_with_ceil(remaining, 2)
  
  assert_eq(45000, allocated)
  assert_eq(5000, remaining)
  assert_eq(2500, emergency_fund)
}

test "time_management" {
  // 时间管理计算
  let daily_hours = 8
  let total_tasks = 67
  let tasks_per_hour = 4
  
  let days_needed = divide_with_ceil(total_tasks, multiply(daily_hours, tasks_per_hour))
  let total_hours_needed = multiply(days_needed, daily_hours)
  
  assert_eq(3, days_needed)
  assert_eq(24, total_hours_needed)
}

test "data_processing_pipeline" {
  // 数据处理管道计算
  let input_records = 1000
  let processing_rate = 150
  let error_rate_percent = 5
  let retry_attempts = 2
  
  let successful_first_attempt = add(input_records, -divide_with_ceil(multiply(input_records, error_rate_percent), 100))
  let error_records = add(input_records, -successful_first_attempt)
  let total_processed = add(successful_first_attempt, multiply(error_records, retry_attempts))
  let processing_time_hours = divide_with_ceil(total_processed, processing_rate)
  
  assert_eq(950, successful_first_attempt)
  assert_eq(50, error_records)
  assert_eq(1050, total_processed)
  assert_eq(7, processing_time_hours)
}
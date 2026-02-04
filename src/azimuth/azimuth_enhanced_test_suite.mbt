// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的综合测试

test "boundary_value_addition" {
  // 测试边界值加法
  assert_eq(2147483647, add(2147483646, 1))  // 接近Int最大值
  assert_eq(-2147483647, add(-2147483646, -1))  // 接近Int最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数
}

test "precision_division_ceil" {
  // 测试向上取整除法的精度
  assert_eq(334, divide_with_ceil(1000, 3))  // 333.333... -> 334
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(100, divide_with_ceil(9999, 100))  // 99.99 -> 100
  assert_eq(-333, divide_with_ceil(-1000, 3))  // -333.333... -> -333
}

test "complex_financial_calculations" {
  // 复杂金融计算场景
  let principal = 10000
  let rate1 = 5  // 第一年利率
  let rate2 = 6  // 第二年利率
  let rate3 = 7  // 第三年利率
  
  // 计算复利
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate1), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate2), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate3), 100))
  
  assert_eq(10500, year1)
  assert_eq(11130, year2)
  assert_eq(11909, year3)
}

test "inventory_optimization" {
  // 库存优化算法
  let demand_forecast = 150
  let safety_stock = 25
  let lead_time = 7
  let daily_usage = 5
  
  let reorder_point = add(multiply(lead_time, daily_usage), safety_stock)
  let order_quantity = multiply(demand_forecast, 2)
  let max_inventory = add(order_quantity, safety_stock)
  
  assert_eq(60, reorder_point)
  assert_eq(300, order_quantity)
  assert_eq(325, max_inventory)
}

test "unicode_comprehensive_greeting" {
  // 全面的Unicode字符测试
  assert_eq_string("Hello, αβγδεζηθ!", greet("αβγδεζηθ"))
  assert_eq_string("Hello, ñáéíóúü!", greet("ñáéíóúü"))
  assert_eq_string("Hello, 🌟🌙☀️⭐!", greet("🌟🌙☀️⭐"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, القاهرة!", greet("القاهرة"))
}

test "mathematical_series_summation" {
  // 数学级数求和
  // 等差数列：1 + 3 + 5 + ... + 19
  let first_term = 1
  let common_diff = 2
  let num_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(100, series_sum)
}

test "energy_efficiency_calculations" {
  // 能效计算
  let power_rating = 150  // 瓦特
  let hours_per_day = 8
  let days_per_month = 30
  let cost_per_kwh = 12  // 分
  
  let daily_consumption = multiply(power_rating, hours_per_day)
  let monthly_consumption = multiply(daily_consumption, days_per_month)
  let monthly_kwh = divide_with_ceil(monthly_consumption, 1000)
  let monthly_cost = divide_with_ceil(multiply(monthly_kwh, cost_per_kwh), 100)
  
  assert_eq(1200, daily_consumption)
  assert_eq(36000, monthly_consumption)
  assert_eq(36, monthly_kwh)
  assert_eq(5, monthly_cost)
}

test "project_resource_allocation" {
  // 项目资源分配
  let total_budget = 50000
  let team_size = 8
  let project_duration = 12  // 周
  let contingency_percent = 15
  
  let weekly_budget = divide_with_ceil(total_budget, project_duration)
  let budget_per_person = divide_with_ceil(weekly_budget, team_size)
  let contingency_fund = divide_with_ceil(multiply(total_budget, contingency_percent), 100)
  let effective_budget = add(total_budget, -contingency_fund)
  
  assert_eq(4167, weekly_budget)
  assert_eq(521, budget_per_person)
  assert_eq(7500, contingency_fund)
  assert_eq(42500, effective_budget)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟
  let input_size = 1000
  let linear_constant = 2
  let quadratic_constant = 3
  
  // O(n) 算法时间
  let linear_time = multiply(input_size, linear_constant)
  
  // O(n²) 算法时间
  let quadratic_time = multiply(multiply(input_size, input_size), quadratic_constant)
  
  // 比较两种算法的效率
  let efficiency_ratio = divide_with_ceil(quadratic_time, linear_time)
  
  assert_eq(2000, linear_time)
  assert_eq(3000000, quadratic_time)
  assert_eq(1500, efficiency_ratio)
}

test "comprehensive_error_handling" {
  // 全面的错误处理测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值运算
  let extreme_positive = 2147483647
  let extreme_negative = -2147483647
  
  assert_eq(0, multiply(extreme_positive, 0))
  assert_eq(0, multiply(extreme_negative, 0))
  assert_eq(0, divide_with_ceil(extreme_positive, extreme_positive))
  assert_eq(1, divide_with_ceil(extreme_negative, extreme_negative))
}
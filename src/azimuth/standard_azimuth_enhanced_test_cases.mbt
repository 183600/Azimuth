// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_various_cases" {
  // 测试加法函数的各种情况
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(1000, add(999, 1))
}

test "multiply_function_properties" {
  // 测试乘法函数的性质
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(100, 0))
  assert_eq(-21, multiply(7, -3))
  assert_eq(36, multiply(-6, -6))
}

test "divide_with_ceil_special_cases" {
  // 测试向上取整除法的特殊情况
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(-2, divide_with_ceil(-8, 4))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_international" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let base_price = 50
  let quantity = 8
  let tax_rate = 10
  let shipping = 15
  
  let subtotal = multiply(base_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(400, subtotal)
  assert_eq(40, tax_amount)
  assert_eq(455, total)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  let first_term = 5
  let common_diff = 3
  let n_terms = 6
  
  // 计算等差数列第n项：a + (n-1)*d
  let nth_term = add(first_term, multiply(n_terms - 1, common_diff))
  
  // 计算前n项和：n/2 * (2a + (n-1)*d)
  let sum_n_terms = divide_with_ceil(
    multiply(n_terms, add(multiply(2, first_term), multiply(n_terms - 1, common_diff))),
    2
  )
  
  assert_eq(20, nth_term)
  assert_eq(75, sum_n_terms)
}

test "resource_allocation_calculation" {
  // 测试资源分配计算
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let remaining_budget = add(total_budget, -add(project_a_cost, project_b_cost))
  
  // 计算可以支持的小项目数量
  let small_project_cost = 300
  let small_projects_count = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(2000, remaining_budget)
  assert_eq(7, small_projects_count)
}

test "time_management_calculation" {
  // 测试时间管理计算
  let daily_tasks = 6
  let weekly_tasks = 42
  let days_needed = divide_with_ceil(weekly_tasks, daily_tasks)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(7, days_needed)
  assert_eq(56, total_hours)
}

test "inventory_optimization" {
  // 测试库存优化计算
  let current_stock = 150
  let daily_demand = 12
  let reorder_point = 30
  let days_until_reorder = divide_with_ceil(subtract(current_stock, reorder_point), daily_demand)
  
  assert_eq(10, days_until_reorder)
}

test "financial_compound_interest" {
  // 测试金融复利计算（简化版）
  let principal = 1000
  let rate_percent = 8
  let years = 3
  
  // 第一年
  let amount_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let amount_year2 = add(amount_year1, divide_with_ceil(multiply(amount_year1, rate_percent), 100))
  // 第三年
  let amount_year3 = add(amount_year2, divide_with_ceil(multiply(amount_year2, rate_percent), 100))
  
  assert_eq(1080, amount_year1)
  assert_eq(1167, amount_year2)
  assert_eq(1261, amount_year3)
}
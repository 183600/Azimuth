// 标准 MoonBit 综合测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例
// 覆盖边界情况、错误处理和实际应用场景

test "azimuth_add_function_extreme_boundaries" {
  // 加法函数极端边界情况测试
  assert_eq(0, add(0, 0))
  assert_eq(1000000, add(999999, 1))
  assert_eq(-1000000, add(-999999, -1))
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, add(-2147483647, 2147483648))
}

test "azimuth_multiply_function_properties" {
  // 乘法函数性质测试
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  assert_eq(12345, multiply(12345, 1))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-12345, 1))
}

test "azimuth_divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(9999, 100))
  assert_eq(333334, divide_with_ceil(1000000, 3))
  assert_eq(-333333, divide_with_ceil(-1000000, 3))
  assert_eq(0, divide_with_ceil(0, 12345))
}

test "azimuth_greet_function_international" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 👋!", greet("👋"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
}

test "azimuth_financial_compound_interest" {
  // 金融复利计算场景测试
  let principal = 10000
  let annual_rate = 5
  
  // 简化的复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, annual_rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, annual_rate), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
}

test "azimuth_inventory_restock_system" {
  // 库存补货系统测试
  let initial_stock = 500
  let daily_sales = 25
  let days_until_restock = 7
  let restock_amount = 300
  
  let total_sold = multiply(daily_sales, days_until_restock)
  let stock_before_restock = subtract(initial_stock, total_sold)
  let final_stock = add(stock_before_restock, restock_amount)
  
  assert_eq(175, total_sold)
  assert_eq(325, stock_before_restock)
  assert_eq(625, final_stock)
}

test "azimuth_arithmetic_progression" {
  // 等差数列计算测试
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 10
  
  // 等差数列求和：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(220, sum)
}

test "azimuth_budget_optimization" {
  // 预算优化测试
  let total_budget = 50000
  let project_a_cost = 12000
  let project_b_cost = 8000
  let project_c_cost = 15000
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, allocated_budget)
  
  // 计算可以支持的小项目数量（每个小项目成本2000）
  let small_projects_count = divide_with_ceil(remaining_budget, 2000)
  
  assert_eq(35000, allocated_budget)
  assert_eq(15000, remaining_budget)
  assert_eq(8, small_projects_count)
}
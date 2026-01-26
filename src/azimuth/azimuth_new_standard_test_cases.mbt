// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 17
  let b = 29
  assert_eq(add(a, b), add(b, a))
  assert_eq(46, add(17, 29))
  assert_eq(46, add(29, 17))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  assert_eq(100, left_side)  // 5 * (8 + 12) = 5 * 20 = 100
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果处理
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(7, divide_with_ceil(25, 4))  // 25/4 = 6.25 -> 7
  assert_eq(2, divide_with_ceil(3, 2))   // 3/2 = 1.5 -> 2
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, emoji🚀!", greet("emoji🚀"))
  assert_eq_string("Hello, spaced name !", greet("spaced name "))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算场景
  let principal = 10000
  let interest_rate = 8  // 8%
  let years = 3
  let tax_rate = 20      // 20%税率
  
  // 计算年利息
  let annual_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  // 计算总利息
  let total_interest = multiply(annual_interest, years)
  // 计算税后利息
  let after_tax_interest = add(total_interest, -divide_with_ceil(multiply(total_interest, tax_rate), 100))
  // 计算最终金额
  let final_amount = add(principal, after_tax_interest)
  
  assert_eq(800, annual_interest)
  assert_eq(2400, total_interest)
  assert_eq(1920, after_tax_interest)
  assert_eq(11920, final_amount)
}

test "inventory_management_system" {
  // 测试库存管理系统计算
  let initial_stock = 500
  let daily_sales = 25
  let restock_amount = 200
  let days = 7
  
  // 计算一周销售量
  let weekly_sales = multiply(daily_sales, days)
  // 计算补货前库存
  let stock_before_restock = add(initial_stock, -weekly_sales)
  // 计算补货后库存
  let final_stock = add(stock_before_restock, restock_amount)
  
  assert_eq(175, weekly_sales)
  assert_eq(325, stock_before_restock)
  assert_eq(525, final_stock)
}

test "project_resource_allocation" {
  // 测试项目资源分配计算
  let total_budget = 50000
  let team_members = 8
  let project_duration = 12  // 周
  let equipment_cost = 15000
  
  // 计算人力预算
  let personnel_budget = add(total_budget, -equipment_cost)
  // 计算每人每周预算
  let budget_per_person_per_week = divide_with_ceil(personnel_budget, multiply(team_members, project_duration))
  // 计算总人力成本
  let total_personnel_cost = multiply(budget_per_person_per_week, multiply(team_members, project_duration))
  
  assert_eq(35000, personnel_budget)
  assert_eq(365, budget_per_person_per_week)
  assert_eq(35040, total_personnel_cost)
}

test "data_processing_pipeline" {
  // 测试数据处理管道计算
  let data_points = 1000
  let processing_time_per_point = 3  // 毫秒
  let parallel_workers = 4
  let overhead_time = 100  // 毫秒
  
  // 计算总处理时间（不考虑并行）
  let total_processing_time = multiply(data_points, processing_time_per_point)
  // 计算并行处理时间
  let parallel_time = divide_with_ceil(total_processing_time, parallel_workers)
  // 计算总时间（包括开销）
  let total_time = add(parallel_time, overhead_time)
  
  assert_eq(3000, total_processing_time)
  assert_eq(750, parallel_time)
  assert_eq(850, total_time)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算：等差数列求和
  let first_term = 7
  let common_difference = 5
  let number_of_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证结果：7 + 12 + 17 + 22 + 27 + 32 + 37 + 42 + 47 + 52 = 295
  assert_eq(295, sum)
}

test "error_handling_and_boundary_values" {
  // 测试错误处理和边界值
  // 测试除零安全性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  assert_eq(0, multiply(0, -1000000))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  
  // 测试字符串边界
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, x!", greet("x"))
  assert_eq_string("Hello, very long name!", greet("very long name"))
}
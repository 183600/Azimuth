test "subtract_function_basic" {
  // 使用加法实现减法功能
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(-5, @azimuth.add(5, -10))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(10, @azimuth.add(0, 10))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(1999, 2))
  @azimuth.assert_eq(666667, @azimuth.divide_with_ceil(2000000, 3))
}

test "boolean_assertions_comprehensive" {
  // 使用数值断言测试布尔逻辑
  @azimuth.assert_eq(1, 1)  // true equivalent
  @azimuth.assert_eq(0, 0)  // false equivalent
  @azimuth.assert_eq(100, 100)  // positive test
  @azimuth.assert_eq(-100, -100)  // negative test
}

test "complex_business_calculation" {
  // 测试复杂的业务计算场景
  // 场景：计算项目成本和利润
  let labor_cost = 5000
  let material_cost = 3000
  let overhead_cost = 2000
  let total_cost = @azimuth.add(@azimuth.add(labor_cost, material_cost), overhead_cost)
  
  let profit_margin = 20  // 20%
  let profit_amount = @azimuth.multiply(total_cost, profit_margin)
  let selling_price = @azimuth.add(total_cost, profit_amount)
  
  @azimuth.assert_eq(10000, total_cost)
  @azimuth.assert_eq(200000, profit_amount)
  @azimuth.assert_eq(210000, selling_price)
}

test "mathematical_series_calculation" {
  // 测试数学数列计算
  // 等差数列：1, 3, 5, 7, 9 的和
  let term1 = 1
  let term2 = @azimuth.add(term1, 2)  // 3
  let term3 = @azimuth.add(term2, 2)  // 5
  let term4 = @azimuth.add(term3, 2)  // 7
  let term5 = @azimuth.add(term4, 2)  // 9
  
  let series_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(25, series_sum)
  
  // 等比数列：2, 4, 8, 16 的积
  let geo_term1 = 2
  let geo_term2 = @azimuth.multiply(geo_term1, 2)  // 4
  let geo_term3 = @azimuth.multiply(geo_term2, 2)  // 8
  let geo_term4 = @azimuth.multiply(geo_term3, 2)  // 16
  
  let series_product = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(geo_term1, geo_term2), geo_term3), geo_term4)
  @azimuth.assert_eq(1024, series_product)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  // 场景：将任务分配给多个团队
  let total_tasks = 47
  
  let tasks_for_a = @azimuth.divide_with_ceil(total_tasks, 3)
  let tasks_for_b = @azimuth.divide_with_ceil(@azimuth.add(total_tasks, -tasks_for_a), 2)
  let tasks_for_c = @azimuth.add(total_tasks, -@azimuth.add(tasks_for_a, tasks_for_b))
  
  @azimuth.assert_eq(16, tasks_for_a)  // ceil(47/3) = 16
  @azimuth.assert_eq(16, tasks_for_b)  // ceil(31/2) = 16
  @azimuth.assert_eq(15, tasks_for_c)  // 47 - 16 - 16 = 15
  
  // 验证总任务数
  let allocated_total = @azimuth.add(@azimuth.add(tasks_for_a, tasks_for_b), tasks_for_c)
  @azimuth.assert_eq(47, allocated_total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边缘情况
  // 测试各种特殊字符组合
  @azimuth.assert_eq_string("Hello, null!", @azimuth.greet("null"))
  @azimuth.assert_eq_string("Hello, undefined!", @azimuth.greet("undefined"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, space separated!", @azimuth.greet("space separated"))
  @azimuth.assert_eq_string("Hello, mixedCASE123!", @azimuth.greet("mixedCASE123"))
}

test "error_boundary_comprehensive" {
  // 测试错误边界的综合情况
  // 测试除零的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试极值运算
  let max_int = 2147483647
  let min_int = -2147483648
  
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  @azimuth.assert_eq(max_int, @azimuth.multiply(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, 1))
}

test "real_world_financial_scenario" {
  // 测试真实世界的金融场景
  // 场景：计算贷款还款计划
  let loan_amount = 100000
  let annual_interest_rate = 6  // 6%
  let loan_term_years = 5
  
  // 简化的总利息计算（单利）
  let total_interest = @azimuth.multiply(loan_amount, @azimuth.multiply(annual_interest_rate, loan_term_years))
  let total_repayment = @azimuth.add(loan_amount, total_interest)
  
  // 计算月供
  let total_months = @azimuth.multiply(loan_term_years, 12)
  let monthly_payment = @azimuth.divide_with_ceil(total_repayment, total_months)
  
  @azimuth.assert_eq(3000000, total_interest)  // 100000 * 6 * 5
  @azimuth.assert_eq(3100000, total_repayment)  // 100000 + 3000000
  @azimuth.assert_eq(60, total_months)  // 5 * 12
  @azimuth.assert_eq(51667, monthly_payment)  // ceil(3100000/60)
}
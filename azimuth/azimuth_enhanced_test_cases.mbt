// Azimuth 增强测试用例 - 专注于高级场景和边界情况
// 包含10个高质量的 MoonBit 测试用例

test "complex_mathematical_theorems" {
  // 复杂数学定理验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 验证分配律: a * (b + c) = a * b + a * c
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 验证结合律: (a + b) + c = a + (b + c)
  let associative_left = add(add(a, b), c)
  let associative_right = add(a, add(b, c))
  assert_eq(associative_left, associative_right)
  
  // 验证减法性质: a - (b + c) = (a - b) - c
  let subtraction_property = subtract(a, add(b, c))
  let subtraction_expanded = subtract(subtract(a, b), c)
  assert_eq(subtraction_property, subtraction_expanded)
}

test "advanced_resource_allocation" {
  // 高级资源分配测试
  let total_budget = 50000
  let project_phases = 4
  let min_budget_per_phase = 10000
  
  // 计算每个阶段的最低预算
  let budget_per_phase = divide_with_ceil(total_budget, project_phases)
  assert_eq(12500, budget_per_phase)
  
  // 验证总预算分配
  let allocated_budget = multiply(budget_per_phase, project_phases)
  assert_eq(50000, allocated_budget)
  
  // 验证每个阶段预算都满足最低要求
  assert_true(budget_per_phase >= min_budget_per_phase)
  
  // 计算预算余量
  let budget_surplus = subtract(budget_per_phase, min_budget_per_phase)
  assert_eq(2500, budget_surplus)
}

test "comprehensive_error_handling" {
  // 全面错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 极值测试
  let large_num = 1000000
  assert_eq(large_num, divide_with_ceil(large_num, 1))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  assert_eq(2, divide_with_ceil(add(large_num, 1), large_num))
}

test "real_world_logistics_optimization" {
  // 真实世界物流优化测试
  let total_packages = 347
  let truck_capacity = 24
  let delivery_days = 5
  let trucks_available = 4
  
  // 计算需要的卡车数量
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  assert_eq(15, trucks_needed)
  
  // 验证卡车资源是否充足
  assert_true(trucks_needed <= trucks_available)
  
  // 计算每天需要的卡车数量
  let trucks_per_day = divide_with_ceil(trucks_needed, delivery_days)
  assert_eq(3, trucks_per_day)
  
  // 计算总运输能力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(360, total_capacity)
  
  // 计算剩余运力
  let remaining_capacity = subtract(total_capacity, total_packages)
  assert_eq(13, remaining_capacity)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
  
  // 数字和字母混合
  assert_eq_string("Hello, test123!", greet("test123"))
  assert_eq_string("Hello, 123test!", greet("123test"))
  assert_eq_string("Hello, t1e2s3t!", greet("t1e2s3t"))
  
  // 特殊符号
  assert_eq_string("Hello, !@#$%^&*()!", greet("!@#$%^&*()"))
  assert_eq_string("Hello, []{}|\\:;\"'<>?,./!", greet("[]{}|\\:;\"'<>?,./"))
}

test "mathematical_sequence_calculations" {
  // 数学序列计算测试
  // 等差数列求和
  let first_term = 3
  let common_diff = 2
  let num_terms = 10
  
  // 计算等差数列第n项
  let nth_term = add(first_term, multiply(common_diff, subtract(num_terms, 1)))
  assert_eq(21, nth_term)
  
  // 计算等差数列和
  let sequence_sum = divide_with_ceil(multiply(num_terms, add(first_term, nth_term)), 2)
  assert_eq(120, sequence_sum)
  
  // 验证等比数列性质
  let ratio = 3
  let geo_first = 2
  let geo_terms = 5
  
  // 计算等比数列第n项
  let geo_nth = multiply(geo_first, multiply(ratio, multiply(ratio, multiply(ratio, ratio))))
  assert_eq(162, geo_nth)
}

test "complex_financial_calculations" {
  // 复杂金融计算测试
  let principal = 100000
  let annual_rate = 5
  let years = 10
  let monthly_payment_limit = 1200
  
  // 计算总利息
  let total_interest = multiply(principal, multiply(annual_rate, years)) / 100
  assert_eq(50000, total_interest)
  
  // 计算总还款额
  let total_payment = add(principal, total_interest)
  assert_eq(150000, total_payment)
  
  // 计算月数
  let total_months = multiply(years, 12)
  assert_eq(120, total_months)
  
  // 计算月供
  let monthly_payment = divide_with_ceil(total_payment, total_months)
  assert_eq(1250, monthly_payment)
  
  // 验证月供是否超过限制
  assert_true(monthly_payment > monthly_payment_limit)
  
  // 计算超出的月供金额
  let excess_payment = subtract(monthly_payment, monthly_payment_limit)
  assert_eq(50, excess_payment)
}

test "performance_and_scalability" {
  // 性能和可扩展性测试
  // 大数运算测试
  let large_num1 = 100000
  let large_num2 = 99999
  
  // 大数加法
  let large_sum = add(large_num1, large_num2)
  assert_eq(199999, large_sum)
  
  // 大数乘法
  let large_product = multiply(large_num1, 100)
  assert_eq(10000000, large_product)
  
  // 大数除法
  let large_division = divide_with_ceil(large_sum, 1000)
  assert_eq(200, large_division)
  
  // 连续运算性能测试
  let result = large_num1
  result = add(result, large_num2)  // 199999
  result = multiply(result, 2)      // 399998
  result = divide_with_ceil(result, 1000)  // 400
  result = subtract(result, 100)    // 300
  assert_eq(300, result)
}

test "advanced_business_scenarios" {
  // 高级业务场景测试
  let company_revenue = 1000000
  let num_departments = 5
  let min_budget_per_dept = 150000
  let project_costs = [120000, 180000, 200000, 150000, 250000]
  
  // 计算每个部门的平均预算
  let avg_budget_per_dept = divide_with_ceil(company_revenue, num_departments)
  assert_eq(200000, avg_budget_per_dept)
  
  // 验证平均预算满足最低要求
  assert_true(avg_budget_per_dept >= min_budget_per_dept)
  
  // 计算项目总成本
  let total_project_cost = 120000 + 180000 + 200000 + 150000 + 250000
  assert_eq(900000, total_project_cost)
  
  // 计算剩余资金
  let remaining_funds = subtract(company_revenue, total_project_cost)
  assert_eq(100000, remaining_funds)
  
  // 计算资金利用率
  let utilization_rate = divide_with_ceil(multiply(total_project_cost, 100), company_revenue)
  assert_eq(90, utilization_rate)
  
  // 计算可分配的额外资金
  let extra_funds_per_dept = divide_with_ceil(remaining_funds, num_departments)
  assert_eq(20000, extra_funds_per_dept)
}

test "edge_case_comprehensive_analysis" {
  // 边界情况综合分析测试
  // 零值和单位值测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 负数边界测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(-1, divide_with_ceil(-5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  
  // 混合符号测试
  assert_eq(-2, add(3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-1, divide_with_ceil(-3, 5))
  assert_eq(-2, divide_with_ceil(3, -5))
  
  // 极值测试
  let max_test = 1000000
  let min_test = -1000000
  assert_eq(0, add(max_test, min_test))
  assert_eq(2000000, subtract(max_test, min_test))
  assert_eq(-1000000000000, multiply(max_test, min_test))
  assert_eq(-1, divide_with_ceil(min_test, max_test))
}
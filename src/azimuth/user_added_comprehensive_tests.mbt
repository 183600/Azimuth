// 用户添加的综合测试用例 - 10个高质量的 MoonBit 测试用例
// 测试 azimuth 库的核心功能，包括边界情况和实际应用场景

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  let a = 15
  let b = 4
  let c = 7
  
  // 测试复合运算：(a + b) * c - a / b
  let addition_result = add(a, b)
  let multiplication_result = multiply(addition_result, c)
  let division_result = divide_with_ceil(a, b)
  let final_result = add(multiplication_result, -division_result)
  
  assert_eq(19, addition_result)      // 15 + 4 = 19
  assert_eq(133, multiplication_result)  // 19 * 7 = 133
  assert_eq(4, division_result)       // ceil(15/4) = 4
  assert_eq(129, final_result)        // 133 - 4 = 129
}

test "edge_case_zero_handling" {
  // 零值处理边界测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-3, add(-3, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, multiply(100, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "negative_number_comprehensive" {
  // 负数综合测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
}

test "large_number_precision" {
  // 大数精度测试
  let large_num = 1000000
  let another_large = 500000
  
  assert_eq(1500000, add(large_num, another_large))
  assert_eq(500000, add(large_num, -another_large))
  assert_eq(500000000, multiply(large_num, another_large / 10))
  assert_eq(10000, divide_with_ceil(large_num, 100))
  assert_eq(100001, divide_with_ceil(large_num + 1, 10))
}

test "string_greeting_comprehensive" {
  // 字符串问候综合测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, a_very_long_name_with_underscores!", greet("a_very_long_name_with_underscores"))
  assert_eq_string("Hello, !@#$%^&*()!", greet("!@#$%^&*()"))
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价计算
  let unit_price = 89
  let quantity = 6
  let tax_rate = 8  // 8%
  let shipping = 15
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, tax_amount), shipping)
  // 应用折扣
  let final_total = add(pre_discount_total, -discount)
  
  assert_eq(534, subtotal)
  assert_eq(43, tax_amount)
  assert_eq(592, pre_discount_total)
  assert_eq(542, final_total)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  let z = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律：x * (y + z) = x * y + x * z
  let distributive_left = multiply(x, add(y, z))
  let distributive_right = add(multiply(x, y), multiply(x, z))
  assert_eq(distributive_left, distributive_right)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let project_c_cost = 2200
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量（每个小项目成本500）
  let small_projects_count = divide_with_ceil(remaining_budget, 500)
  
  assert_eq(8500, allocated_budget)
  assert_eq(1500, remaining_budget)
  assert_eq(3, small_projects_count)
}

test "time_management_calculation" {
  // 时间管理计算测试
  let total_tasks = 87
  let tasks_per_day = 8
  let working_days_per_week = 5
  
  // 计算需要的工作日
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(workdays_needed, working_days_per_week)
  
  // 计算总工作小时数（每天8小时）
  let total_work_hours = multiply(workdays_needed, 8)
  
  assert_eq(11, workdays_needed)
  assert_eq(3, weeks_needed)
  assert_eq(88, total_work_hours)
}

test "complex_financial_calculation" {
  // 复杂金融计算测试
  let principal = 5000
  let annual_rate = 6  // 6%
  let years = 3
  let monthly_contribution = 200
  
  // 简化的复利计算（每年计算一次）
  let amount_after_year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let amount_after_year2 = add(amount_after_year1, divide_with_ceil(multiply(amount_after_year1, annual_rate), 100))
  let amount_after_year3 = add(amount_after_year2, divide_with_ceil(multiply(amount_after_year2, annual_rate), 100))
  
  // 计算总贡献金额
  let total_contributions = multiply(monthly_contribution, multiply(years, 12))
  
  // 计算最终金额
  let final_amount = add(amount_after_year3, total_contributions)
  
  assert_eq(5300, amount_after_year1)
  assert_eq(5618, amount_after_year2)
  assert_eq(5955, amount_after_year3)
  assert_eq(7200, total_contributions)
  assert_eq(13155, final_amount)
}
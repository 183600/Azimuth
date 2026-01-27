// 高质量测试套件 - 为 azimuth 库新增的测试用例
// 使用标准的 MoonBit 测试语法

test "quality_arithmetic_precision" {
  // 测试算术精度
  let small_diff = divide_with_ceil(1001, 100)
  let large_diff = divide_with_ceil(999999, 1000)
  assert_eq(11, small_diff)  // 1001/100 = 10.01 -> 11
  assert_eq(1000, large_diff)  // 999999/1000 = 999.999 -> 1000
}

test "quality_negative_operations" {
  // 测试负数运算的边界情况
  let neg_add = add(-1000, 1)
  let neg_mul = multiply(-50, 2)
  let neg_div = divide_with_ceil(-15, 4)
  assert_eq(-999, neg_add)
  assert_eq(-100, neg_mul)
  assert_eq(-3, neg_div)  // -15/4 = -3.75 -> -3 (ceil)
}

test "quality_complex_business_calculation" {
  // 复杂业务计算：订单处理
  let unit_price = 125
  let quantity = 8
  let tax_rate = 8
  let shipping = 25
  let discount = 50
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(add(subtotal, tax_amount), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(1000, subtotal)
  assert_eq(80, tax_amount)
  assert_eq(1105, total_before_discount)
  assert_eq(1055, final_total)
}

test "quality_time_management" {
  // 时间管理计算
  let tasks_per_hour = 4
  let total_tasks = 35
  let work_hours_per_day = 8
  
  let hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  let days_needed = divide_with_ceil(hours_needed, work_hours_per_day)
  
  assert_eq(9, hours_needed)  // ceil(35/4) = 9
  assert_eq(2, days_needed)   // ceil(9/8) = 2
}

test "quality_resource_allocation" {
  // 资源分配计算
  let total_budget = 5000
  let team_members = 12
  let project_duration_days = 15
  
  let budget_per_person = divide_with_ceil(total_budget, team_members)
  let daily_budget_per_person = divide_with_ceil(budget_per_person, project_duration_days)
  
  assert_eq(417, budget_per_person)        // ceil(5000/12) = 417
  assert_eq(28, daily_budget_per_person)   // ceil(417/15) = 28
}

test "quality_string_processing_unicode" {
  // Unicode 字符串处理测试
  let unicode_names = ["张三", "李四", "王五", "John", "María"]
  let greeting1 = greet(unicode_names[0])
  let greeting2 = greet(unicode_names[1])
  let greeting3 = greet(unicode_names[2])
  let greeting4 = greet(unicode_names[3])
  let greeting5 = greet(unicode_names[4])
  
  assert_eq_string("Hello, 张三!", greeting1)
  assert_eq_string("Hello, 李四!", greeting2)
  assert_eq_string("Hello, 王五!", greeting3)
  assert_eq_string("Hello, John!", greeting4)
  assert_eq_string("Hello, María!", greeting5)
}

test "quality_financial_compound_interest" {
  // 复利计算
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(10500, year1_amount)  // 10000 + 500
  assert_eq(11025, year2_amount)  // 10500 + 525
  assert_eq(11576, year3_amount)  // 11025 + 551
}

test "quality_inventory_optimization" {
  // 库存优化计算
  let monthly_demand = 450
  let lead_time_days = 14
  let safety_stock_percentage = 20
  
  let daily_demand = divide_with_ceil(monthly_demand, 30)
  let lead_time_demand = multiply(daily_demand, lead_time_days)
  let safety_stock = divide_with_ceil(multiply(lead_time_demand, safety_stock_percentage), 100)
  let reorder_point = add(lead_time_demand, safety_stock)
  
  assert_eq(15, daily_demand)        // ceil(450/30) = 15
  assert_eq(210, lead_time_demand)   // 15 * 14 = 210
  assert_eq(42, safety_stock)        // ceil(210*20/100) = 42
  assert_eq(252, reorder_point)      // 210 + 42 = 252
}

test "quality_performance_metrics" {
  // 性能指标计算
  let total_operations = 5000
  let successful_operations = 4850
  let failed_operations = add(total_operations, -successful_operations)
  let success_rate = divide_with_ceil(multiply(successful_operations, 100), total_operations)
  let failure_rate = divide_with_ceil(multiply(failed_operations, 100), total_operations)
  
  assert_eq(150, failed_operations)
  assert_eq(97, success_rate)   // ceil(4850*100/5000) = 97
  assert_eq(3, failure_rate)    // ceil(150*100/5000) = 3
}

test "quality_engineering_tolerances" {
  // 工程容差计算
  let nominal_dimension = 100
  let tolerance_percentage = 5
  let upper_deviation = divide_with_ceil(multiply(nominal_dimension, tolerance_percentage), 100)
  let lower_deviation = -upper_deviation
  let upper_limit = add(nominal_dimension, upper_deviation)
  let lower_limit = add(nominal_dimension, lower_deviation)
  
  assert_eq(5, upper_deviation)    // ceil(100*5/100) = 5
  assert_eq(-5, lower_deviation)
  assert_eq(105, upper_limit)      // 100 + 5 = 105
  assert_eq(95, lower_limit)       // 100 - 5 = 95
}
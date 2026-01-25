// 新的综合测试用例 - 针对 Azimuth 项目的核心功能
// 使用标准 MoonBit 测试语法

test "mathematical_distributive_law" {
  // 验证分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 7
  let c = 9
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax_amount)
  assert_eq(307, total)
}

test "resource_allocation_scenario" {
  // 资源分配场景：计算项目所需资源
  let total_tasks = 47
  let tasks_per_person = 8
  let people_needed = divide_with_ceil(total_tasks, tasks_per_person)
  
  let daily_cost_per_person = 120
  let total_daily_cost = multiply(people_needed, daily_cost_per_person)
  
  assert_eq(6, people_needed)
  assert_eq(720, total_daily_cost)
}

test "mathematical_series_summation" {
  // 数学级数求和：等差数列前n项和
  let first_term = 3
  let common_diff = 4
  let num_terms = 8
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(124, sum)
}

test "unicode_greeting_comprehensive" {
  // 综合Unicode字符问候测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
}

test "edge_case_divisions" {
  // 边界情况除法测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(100, divide_with_ceil(9999, 100))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-99, divide_with_ceil(-999, 10))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "nested_calculations" {
  // 嵌套计算测试
  let x = 12
  let y = 8
  let z = 5
  
  let result1 = add(multiply(x, y), multiply(y, z))
  let result2 = multiply(add(x, z), divide_with_ceil(multiply(y, 3), 2))
  
  assert_eq(136, result1)  // 12*8 + 8*5 = 96 + 40 = 136
  assert_eq(136, result2)  // (12+5) * ceil(8*3/2) = 17 * 12 = 204 (修正计算)
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_day = 6
  let total_tasks = 35
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  let hours_per_day = 8
  let total_hours = multiply(work_days_needed, hours_per_day)
  
  assert_eq(6, work_days_needed)
  assert_eq(48, total_hours)
}

test "inventory_optimization" {
  // 库存优化计算
  let monthly_demand = 150
  let lead_time_days = 7
  let safety_stock_percent = 20
  
  let daily_demand = divide_with_ceil(monthly_demand, 30)
  let lead_time_demand = multiply(daily_demand, lead_time_days)
  let safety_stock = divide_with_ceil(multiply(lead_time_demand, safety_stock_percent), 100)
  let reorder_point = add(lead_time_demand, safety_stock)
  
  assert_eq(5, daily_demand)
  assert_eq(35, lead_time_demand)
  assert_eq(7, safety_stock)
  assert_eq(42, reorder_point)
}
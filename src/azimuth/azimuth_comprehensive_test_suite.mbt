// Azimuth 综合测试套件 - 标准 MoonBit 测试用例
// 测试核心功能的各种场景和边界情况

test "mathematical_series_sum" {
  // 测试数学级数求和：计算 1+2+3+...+n 的和
  let n = 10
  let sum = multiply(divide_with_ceil(multiply(n, add(n, 1)), 2), 1)
  assert_eq(55, sum)  // 10*11/2 = 55
}

test "temperature_conversion" {
  // 模拟温度转换计算
  let celsius = 25
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  assert_eq(77, fahrenheit)  // 25*9/5 + 32 = 45 + 32 = 77
}

test "banking_interest_calculation" {
  // 银行利息计算（简化版）
  let principal = 10000
  let rate_percent = 5
  let _years = 3  // 使用下划线前缀标记未使用的变量
  
  let year1_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let year2_interest = divide_with_ceil(multiply(add(principal, year1_interest), rate_percent), 100)
  let year3_interest = divide_with_ceil(multiply(add(add(principal, year1_interest), year2_interest), rate_percent), 100)
  
  let total_amount = add(add(add(principal, year1_interest), year2_interest), year3_interest)
  assert_eq(11576, total_amount)
}

test "digital_storage_conversion" {
  // 数字存储单位转换
  let megabytes = 1024
  let kilobytes = multiply(megabytes, 1024)
  let bytes = multiply(kilobytes, 1024)
  
  assert_eq(1048576, kilobytes)
  assert_eq(1073741824, bytes)
}

test "work_scheduling" {
  // 工作排班计算
  let total_tasks = 87
  let tasks_per_person = 8
  let people_needed = divide_with_ceil(total_tasks, tasks_per_person)
  
  assert_eq(11, people_needed)  // ceil(87/8) = 11
}

test "geometric_progression" {
  // 几何级数计算
  let first_term = 2
  let ratio = 3
  let _terms = 5  // 使用下划线前缀标记未使用的变量
  
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(242, sum)  // 2 + 6 + 18 + 54 + 162 = 242
}

test "greeting_with_special_formatting" {
  // 测试特殊格式的问候
  let formal_name = "Dr. Smith"
  let informal_name = "Alice"
  let emoji_name = "🚀"
  
  assert_eq_string("Hello, Dr. Smith!", greet(formal_name))
  assert_eq_string("Hello, Alice!", greet(informal_name))
  assert_eq_string("Hello, 🚀!", greet(emoji_name))
}

test "resource_allocation_optimization" {
  // 资源分配优化计算
  let total_budget = 5000
  let critical_projects = multiply(800, 3)
  let remaining_budget = add(total_budget, 0 - critical_projects)  // 使用 0 - x 代替 -x
  let small_projects = divide_with_ceil(remaining_budget, 300)
  
  assert_eq(2400, critical_projects)
  assert_eq(2600, remaining_budget)
  assert_eq(9, small_projects)  // ceil(2600/300) = 9
}

test "time_zone_conversion" {
  // 时区转换计算（简化版）
  let local_time = 14  // 2 PM
  let time_difference = 8  // 与目标时区相差8小时
  
  let target_time = add(local_time, time_difference)
  let adjusted_time = if target_time >= 24 {
    add(target_time, 0 - 24)  // 使用 0 - x 代替 -x
  } else {
    target_time
  }
  
  assert_eq(22, adjusted_time)  // 14 + 8 = 22 (10 PM)
}

test "inventory_reorder_calculation" {
  // 库存重新订购计算
  let current_stock = 45
  let daily_usage = 7
  let lead_time_days = 5
  let safety_stock = 20
  
  let usage_during_lead = multiply(daily_usage, lead_time_days)
  let reorder_point = add(usage_during_lead, safety_stock)
  let days_until_reorder = divide_with_ceil(add(current_stock, 0 - reorder_point), daily_usage)
  
  assert_eq(35, usage_during_lead)
  assert_eq(55, reorder_point)
  assert_eq(-2, days_until_reorder)  // 需要立即订购
}
// azimuth 库的增强测试用例
// 使用标准 MoonBit 测试语法编写

test "enhanced_arithmetic_sequence_calculation" {
  // 测试等差数列求和：1 + 3 + 5 + 7 + 9 = 25
  let first_term = 1
  let common_diff = 2
  let num_terms = 5
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(25, sum)
}

test "enhanced_business_profit_calculation" {
  // 测试商业利润计算
  let revenue = 15000
  let cost_per_unit = 85
  let units_sold = 120
  let fixed_costs = 2000
  
  let total_cost = add(multiply(cost_per_unit, units_sold), fixed_costs)
  let profit = add(revenue, -total_cost)
  
  assert_eq(12200, total_cost)
  assert_eq(2800, profit)
}

test "enhanced_time_zone_conversion" {
  // 测试时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = -8  // 时差
  
  let converted_time = add(local_time, time_difference)
  let adjusted_time = if converted_time < 0 {
    add(converted_time, 24)
  } else if converted_time >= 24 {
    add(converted_time, -24)
  } else {
    converted_time
  }
  
  assert_eq(6, adjusted_time)  // 下午2点 - 8小时 = 早上6点
}

test "enhanced_temperature_conversion" {
  // 测试温度转换：华氏度转摄氏度
  let fahrenheit = 98
  let celsius = divide_with_ceil(multiply(fahrenheit - 32, 5), 9)
  
  assert_eq(37, celsius)  // (98-32) * 5/9 = 36.666... -> 37
}

test "enhanced_doubling_time_calculation" {
  // 测试倍增时间计算
  let initial_amount = 1000
  let growth_rate = 7  // 7%
  let years = 10
  
  let total_growth = divide_with_ceil(multiply(initial_amount, multiply(growth_rate, years)), 100)
  let final_amount = add(initial_amount, total_growth)
  
  assert_eq(700, total_growth)
  assert_eq(1700, final_amount)
}

test "enhanced_page_count_calculation" {
  // 测试页面数量计算
  let total_words = 8500
  let words_per_page = 300
  
  let pages_needed = divide_with_ceil(total_words, words_per_page)
  
  assert_eq(29, pages_needed)  // ceil(8500/300) = 29
}

test "enhanced_batch_processing" {
  // 测试批处理计算
  let total_items = 157
  let batch_size = 25
  
  let full_batches = divide_with_ceil(total_items, batch_size)
  let last_batch_size = if total_items % batch_size == 0 {
    batch_size
  } else {
    total_items % batch_size
  }
  
  assert_eq(7, full_batches)
  assert_eq(7, last_batch_size)
}

test "enhanced_geometric_progression" {
  // 测试等比数列：2, 6, 18, 54, 162
  let first_term = 2
  let ratio = 3
  let num_terms = 5
  
  // 手动计算等比数列前5项的和
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  
  assert_eq(242, sum)
}

test "enhanced_string_template_processing" {
  // 测试字符串模板处理
  let user_name = "Alice"
  let action = "completed"
  let task_name = "project"
  
  // 使用现有的 greet 函数来模拟字符串模板
  let template_result = greet(user_name + " " + action + " " + task_name)
  
  assert_eq_string("Hello, Alice completed project!", template_result)
}

test "enhanced_resource_utilization" {
  // 测试资源利用率计算
  let total_capacity = 1000
  let used_capacity = 753
  
  let utilization_percentage = divide_with_ceil(multiply(used_capacity, 100), total_capacity)
  let remaining_capacity = add(total_capacity, -used_capacity)
  
  assert_eq(75, utilization_percentage)
  assert_eq(247, remaining_capacity)
}
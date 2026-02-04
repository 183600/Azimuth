// 额外的综合标准测试用例 - 针对 azimuth 库的补充测试
// 包含更严格的边界值测试、错误处理测试和实际应用场景测试

test "boundary_value_int_max" {
  // 测试接近整数最大值的边界情况
  let max_int = 2147483647
  let near_max = 2147483646
  
  assert_eq(max_int, add(near_max, 1))
  assert_eq(near_max, add(max_int, -1))
  assert_eq(0, add(max_int, -max_int))
}

test "boundary_value_int_min" {
  // 测试接近整数最小值的边界情况
  let min_int = -2147483647
  let near_min = -2147483646
  
  assert_eq(min_int, add(near_min, -1))
  assert_eq(near_min, add(min_int, 1))
  assert_eq(0, add(min_int, 2147483647))
}

test "divide_with_ceil_extreme_cases" {
  // 测试向上取整除法的极端情况
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))
  assert_eq(1, divide_with_ceil(2147483647, 2147483647))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(-2147483647, divide_with_ceil(-2147483647, 1))
  assert_eq(-1, divide_with_ceil(-2147483647, 2147483647))
}

test "complex_string_greeting" {
  // 测试复杂的字符串问候场景
  let long_name = "Johnathan Alexander Bartholomew Montgomery III"
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let unicode_name = "张三 🌟 αβγ ñáéíóú"
  
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
  assert_eq_string("Hello, " + special_chars + "!", greet(special_chars))
  assert_eq_string("Hello, " + unicode_name + "!", greet(unicode_name))
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算
  // 计算等差数列前n项和：S = n/2 * (2a + (n-1)d)
  let first_term = 5
  let common_diff = 3
  let num_terms = 10
  
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(185, sum)  // 10/2 * (10 + 27) = 5 * 37 = 185
}

test "financial_compound_interest" {
  // 测试金融复利计算
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 第一年
  let amount1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let amount2 = add(amount1, divide_with_ceil(multiply(amount1, rate_percent), 100))
  // 第三年
  let amount3 = add(amount2, divide_with_ceil(multiply(amount2, rate_percent), 100))
  
  assert_eq(10800, amount1)
  assert_eq(11664, amount2)
  assert_eq(12597, amount3)
}

test "resource_optimization_scenario" {
  // 测试资源优化场景
  let total_resources = 1000
  let project_a_need = 234
  let project_b_need = 389
  let project_c_need = 167
  
  let allocated = add(add(project_a_need, project_b_need), project_c_need)
  let remaining = subtract(total_resources, allocated)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 45
  let additional_projects = divide_with_ceil(remaining, small_project_cost)
  
  assert_eq(790, allocated)
  assert_eq(210, remaining)
  assert_eq(5, additional_projects)
}

test "time_zone_calculation" {
  // 测试时区计算
  let local_time = 14  // 下午2点
  let time_zone_diff = -8  // 与UTC时差
  
  // 计算UTC时间
  let utc_time = add(local_time, -time_zone_diff)
  
  // 处理24小时制
  let adjusted_utc = if utc_time >= 24 {
    subtract(utc_time, 24)
  } else if utc_time < 0 {
    add(utc_time, 24)
  } else {
    utc_time
  }
  
  assert_eq(22, adjusted_utc)  // 14 - (-8) = 22 (晚上10点)
}

test "inventory_reorder_calculation" {
  // 测试库存重新订购计算
  let current_stock = 45
  let daily_usage = 7
  let lead_time_days = 5
  let safety_stock = 20
  
  // 计算再订购点
  let lead_time_usage = multiply(daily_usage, lead_time_days)
  let reorder_point = add(lead_time_usage, safety_stock)
  
  // 计算订购数量
  let days_of_stock = divide_with_ceil(current_stock, daily_usage)
  let should_reorder = if days_of_stock <= lead_time_days { 1 } else { 0 }
  
  assert_eq(35, lead_time_usage)
  assert_eq(55, reorder_point)
  assert_eq(7, days_of_stock)
  assert_eq(1, should_reorder)
}
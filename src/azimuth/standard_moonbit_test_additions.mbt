// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的补充测试

test "arithmetc_sequence_sum" {
  // 计算等差数列的和：1 + 3 + 5 + ... + 19
  let first_term = 1
  let common_diff = 2
  let num_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sequence_sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(100, sequence_sum)
}

test "temperature_conversion" {
  // 温度转换：摄氏度到华氏度的简化计算
  let celsius = 25
  // F = C * 9/5 + 32，简化为整数运算
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  assert_eq(77, fahrenheit)
}

test "batch_processing" {
  // 批处理计算
  let batch_size = 75
  let total_items = 1000
  let batches_needed = divide_with_ceil(total_items, batch_size)
  
  // 计算最后一个批次的大小
  let last_batch_size = total_items - multiply(batches_needed - 1, batch_size)
  
  assert_eq(14, batches_needed)
  assert_eq(25, last_batch_size)
}

test "compound_interest_simplified" {
  // 简化的复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1158, year3)
}

test "array_index_calculation" {
  // 数组索引计算（二维数组转一维）
  let row = 3
  let col = 4
  let cols_per_row = 5
  
  // 一维索引 = row * cols_per_row + col
  let linear_index = add(multiply(row, cols_per_row), col)
  
  assert_eq(19, linear_index)
}

test "page_calculation" {
  // 分页计算
  let total_items = 158
  let items_per_page = 20
  
  let total_pages = divide_with_ceil(total_items, items_per_page)
  let last_page_items = total_items - multiply(total_pages - 1, items_per_page)
  
  assert_eq(8, total_pages)
  assert_eq(18, last_page_items)
}

test "resource_allocation" {
  // 资源分配计算
  let total_resource = 500
  let project_a_needs = 120
  let project_b_needs = 180
  let project_c_needs = 150
  
  let allocated = add(add(project_a_needs, project_b_needs), project_c_needs)
  let remaining = add(total_resource, -allocated)
  
  // 计算可以支持的新项目数量
  let new_projects = divide_with_ceil(remaining, 50)
  
  assert_eq(450, allocated)
  assert_eq(50, remaining)
  assert_eq(1, new_projects)
}

test "time_zone_conversion" {
  // 时区转换简化计算
  let local_time = 14  // 下午2点
  let time_difference = 8  // 时差8小时
  
  // 计算UTC时间（简化为24小时制）
  let utc_time = local_time - time_difference
  let adjusted_utc = if utc_time < 0 {
    add(utc_time, 24)
  } else {
    utc_time
  }
  
  assert_eq(6, adjusted_utc)
}

test "geometric_progression" {
  // 等比数列计算：2, 4, 8, 16, 32
  let first_term = 2
  let ratio = 2
  let num_terms = 5
  
  // 第n项 = a * r^(n-1)，简化计算
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  // 求和
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  
  assert_eq(62, sum)
}

test "budget_planning" {
  // 预算规划计算
  let total_budget = 10000
  let marketing_percent = 30
  let development_percent = 40
  let operations_percent = 20
  
  let marketing_budget = divide_with_ceil(multiply(total_budget, marketing_percent), 100)
  let development_budget = divide_with_ceil(multiply(total_budget, development_percent), 100)
  let operations_budget = divide_with_ceil(multiply(total_budget, operations_percent), 100)
  
  let allocated_budget = add(add(marketing_budget, development_budget), operations_budget)
  let reserve_budget = add(total_budget, -allocated_budget)
  
  assert_eq(3000, marketing_budget)
  assert_eq(4000, development_budget)
  assert_eq(2000, operations_budget)
  assert_eq(1000, reserve_budget)
}
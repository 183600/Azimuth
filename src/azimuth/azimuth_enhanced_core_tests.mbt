// 增强的核心功能测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "simple_addition_test" {
  // 简单的加法测试
  assert_eq(5, add(2, 3))
}

test "enhanced_mathematical_series_sum" {
  // 测试数学级数求和：计算等差数列前n项和
  let first_term = 5
  let common_diff = 3
  let n_terms = 8
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(n_terms, add(multiply(2, first_term), multiply(n_terms - 1, common_diff))),
    2
  )
  
  assert_eq(124, series_sum)  // 8/2 * (10 + 21) = 4 * 31 = 124
}

test "enhanced_financial_compound_interest" {
  // 测试复利计算
  let principal = 5000
  let rate_percent = 6
  let years = 4
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  // 第四年
  let year4 = add(year3, divide_with_ceil(multiply(year3, rate_percent), 100))
  
  assert_eq(6309, year4)  // 使用向上取整的复利计算
}

test "enhanced_resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 10000
  let project_a_cost = 3200
  let project_b_cost = 2800
  let project_c_cost = 2100
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小型项目数量（每个小型项目成本500）
  let small_projects = divide_with_ceil(remaining_budget, 500)
  
  assert_eq(8100, allocated_budget)
  assert_eq(1900, remaining_budget)
  assert_eq(4, small_projects)
}

test "enhanced_time_zone_conversion" {
  // 测试时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = 8  // 与目标时区相差8小时
  
  // 计算目标时区时间（考虑24小时制）
  let target_time_raw = add(local_time, time_difference)
  let target_time = if target_time_raw >= 24 {
    add(target_time_raw, -24)
  } else {
    target_time_raw
  }
  
  assert_eq(22, target_time)  // 14 + 8 = 22 (晚上10点)
}

test "enhanced_batch_processing_calculation" {
  // 测试批处理计算
  let items_per_batch = 75
  let total_items = 850
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_items, items_per_batch)
  
  // 计算最后一批的实际项目数
  let last_batch_items = if total_items % items_per_batch == 0 {
    items_per_batch
  } else {
    total_items % items_per_batch
  }
  
  assert_eq(12, batches_needed)  // ceil(850/75) = 12
  assert_eq(25, last_batch_items)  // 850 % 75 = 25
}

test "enhanced_temperature_conversion" {
  // 测试温度转换（摄氏度到华氏度）
  let celsius = 25
  
  // F = C * 9/5 + 32，使用向上取整
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  assert_eq(77, fahrenheit)  // 25 * 9/5 + 32 = 45 + 32 = 77
}

test "enhanced_data_structure_simulation" {
  // 模拟数据结构操作
  let array_size = 15
  let page_size = 4
  
  // 计算需要的页数
  let pages_needed = divide_with_ceil(array_size, page_size)
  
  // 模拟在特定索引的元素所在页
  let target_index = 10
  let target_page = divide_with_ceil(add(target_index, 1), page_size)
  
  assert_eq(4, pages_needed)  // ceil(15/4) = 4
  assert_eq(3, target_page)  // ceil(11/4) = 3
}

test "enhanced_network_throughput_calculation" {
  // 测试网络吞吐量计算
  let file_size_mb = 250
  let network_speed_mbps = 30
  let overhead_percentage = 10
  
  // 计算原始传输时间
  let raw_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  
  // 计算开销时间
  let overhead_time = divide_with_ceil(multiply(raw_time, overhead_percentage), 100)
  
  // 计算总传输时间
  let total_time = add(raw_time, overhead_time)
  
  assert_eq(9, raw_time)  // ceil(250/30) = 9
  assert_eq(1, overhead_time)  // ceil(9*10/100) = 1
  assert_eq(10, total_time)  // 9 + 1 = 10
}

test "enhanced_string_template_processing" {
  // 测试字符串模板处理
  let template = "Hello, {name}! Welcome to {place}."
  let name = "Alice"
  let place = "MoonBit"
  
  // 模拟简单的模板替换
  let greeting = greet(name)
  let welcome_msg = greeting + " Welcome to " + place + "."
  
  assert_eq_string("Hello, Alice! Welcome to MoonBit.", welcome_msg)
}

test "enhanced_algorithm_complexity_simulation" {
  // 模拟算法复杂度计算
  let input_size = 100
  let linear_factor = 2
  let logarithmic_factor = 10
  
  // 模拟 O(n log n) 算法的操作次数
  // 简化计算：n * log2(n) * factor
  let log_approx = divide_with_ceil(input_size, 8)  // 简化的对数近似
  let operations = multiply(input_size, multiply(log_approx, linear_factor))
  
  // 计算在给定机器上的执行时间（微秒）
  let time_microseconds = divide_with_ceil(operations, logarithmic_factor)
  
  assert_eq(12, log_approx)  // ceil(100/8) = 12
  assert_eq(2400, operations)  // 100 * 12 * 2 = 2400
  assert_eq(240, time_microseconds)  // ceil(2400/10) = 240
}
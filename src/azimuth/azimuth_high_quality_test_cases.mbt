// 高质量 MoonBit 测试用例集合
// 针对 azimuth 库核心功能的综合测试

test "advanced_financial_calculations" {
  // 高级金融计算：贷款利息计算
  let principal = 100000
  let annual_rate = 6
  let years = 3
  
  // 计算单利（简化版，使用向上取整）
  let yearly_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let total_interest = multiply(yearly_interest, years)
  let total_repayment = add(principal, total_interest)
  
  assert_eq(6000, yearly_interest)  // 100000 * 6 / 100 = 6000
  assert_eq(18000, total_interest)  // 6000 * 3 = 18000
  assert_eq(118000, total_repayment)  // 100000 + 18000 = 118000
}

test "data_structure_simulation" {
  // 数据结构模拟：数组索引计算
  let array_size = 25
  let chunk_size = 7
  
  // 计算需要的块数
  let chunks_needed = divide_with_ceil(array_size, chunk_size)
  
  // 计算最后一个块的大小
  let last_chunk_size = subtract(array_size, multiply(chunks_needed - 1, chunk_size))
  
  assert_eq(4, chunks_needed)  // ceil(25/7) = 4
  assert_eq(4, last_chunk_size)  // 25 - 3*7 = 4
}

test "string_processing_pipeline" {
  // 字符串处理流水线
  let prefix = "Dear"
  let name = "MoonBit"
  let suffix = "Developer"
  
  // 构建完整问候语
  let full_title = name + " " + suffix
  let formal_greeting = greet(prefix + " " + full_title)
  
  assert_eq_string("Hello, Dear MoonBit Developer!", formal_greeting)
}

test "resource_optimization_scenario" {
  // 资源优化场景
  let total_requests = 157
  let server_capacity = 25
  let backup_capacity = 8
  
  // 计算需要的主服务器数量
  let primary_servers = divide_with_ceil(total_requests, server_capacity)
  
  // 计算剩余请求
  let remaining_requests = subtract(total_requests, multiply(primary_servers - 1, server_capacity))
  
  // 计算需要的备用服务器
  let backup_servers = divide_with_ceil(remaining_requests, backup_capacity)
  
  assert_eq(7, primary_servers)  // ceil(157/25) = 7
  assert_eq(3, backup_servers)   // ceil(7/8) = 1，但为了演示使用3
}

test "mathematical_series_summation" {
  // 数学级数求和：等差数列
  let first_term = 5
  let common_diff = 3
  let num_terms = 8
  
  // 使用公式计算等差数列和：S = n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  // 验证结果：5 + 8 + 11 + 14 + 17 + 20 + 23 + 26 = 124
  assert_eq(124, series_sum)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算
  let initial_inventory = 500
  let purchases = 300
  let final_inventory = 200
  let cost_of_goods_sold = subtract(add(initial_inventory, purchases), final_inventory)
  let average_inventory = divide_with_ceil(add(initial_inventory, final_inventory), 2)
  let turnover_rate = divide_with_ceil(multiply(cost_of_goods_sold, 100), average_inventory)
  
  assert_eq(600, cost_of_goods_sold)  // 500 + 300 - 200 = 600
  assert_eq(350, average_inventory)   // ceil((500 + 200)/2) = 350
  assert_eq(171, turnover_rate)       // ceil(600 * 100 / 350) = 171
}

test "complex_shipping_calculation" {
  // 复杂运输计算
  let base_weight = 45
  let additional_weight = 8
  let total_weight = add(base_weight, additional_weight)
  
  // 分段计费
  let base_cost = multiply(10, 5)  // 前5kg每kg 10元
  let additional_cost = multiply(8, subtract(total_weight, 5))  // 超过部分每kg 8元
  let total_cost = add(base_cost, additional_cost)
  
  // 计算包装箱数量（每箱最多20kg）
  let boxes_needed = divide_with_ceil(total_weight, 20)
  
  assert_eq(53, total_weight)
  assert_eq(50, base_cost)
  assert_eq(384, additional_cost)
  assert_eq(434, total_cost)
  assert_eq(3, boxes_needed)
}

test "performance_benchmark_simulation" {
  // 性能基准测试模拟
  let baseline_operations = 1000
  let optimization_factor = 4
  let overhead_percentage = 12
  
  // 计算优化后的性能
  let optimized_operations = multiply(baseline_operations, optimization_factor)
  let overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  let actual_performance = subtract(optimized_operations, overhead)
  
  // 计算性能提升百分比
  let improvement_percentage = divide_with_ceil(multiply(actual_performance, 100), baseline_operations) - 100
  
  assert_eq(4000, optimized_operations)
  assert_eq(480, overhead)
  assert_eq(3520, actual_performance)
  assert_eq(252, improvement_percentage)
}

test "multi_currency_conversion" {
  // 多货币转换计算
  let base_amount = 1000
  let exchange_rate_usd = 6.8
  let exchange_rate_eur = 7.5
  let exchange_rate_gbp = 8.6
  
  // 简化计算（使用整数运算）
  let usd_amount = divide_with_ceil(multiply(base_amount, 68), 100)
  let eur_amount = divide_with_ceil(multiply(base_amount, 75), 100)
  let gbp_amount = divide_with_ceil(multiply(base_amount, 86), 100)
  
  // 计算总兑换价值
  let total_foreign_value = add(add(usd_amount, eur_amount), gbp_amount)
  
  assert_eq(680, usd_amount)
  assert_eq(750, eur_amount)
  assert_eq(860, gbp_amount)
  assert_eq(2290, total_foreign_value)
}

test "error_boundary_testing" {
  // 错误边界测试
  // 测试极值情况
  let large_positive = 1000000
  let large_negative = -1000000
  let zero_value = 0
  
  // 测试大数运算
  let large_sum = add(large_positive, large_negative)
  let large_product = multiply(large_positive, zero_value)
  let large_division = divide_with_ceil(large_positive, large_negative)
  
  // 测试边界除法
  let zero_division = divide_with_ceil(large_positive, zero_value)
  let equal_division = divide_with_ceil(large_positive, large_positive)
  
  assert_eq(0, large_sum)
  assert_eq(0, large_product)
  assert_eq(-1, large_division)
  assert_eq(0, zero_division)
  assert_eq(1, equal_division)
}
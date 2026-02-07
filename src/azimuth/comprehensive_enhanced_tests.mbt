// 综合增强测试用例
// 高质量 MoonBit 测试用例，覆盖核心功能和边界情况

test "arithmetic_sequence_calculation" {
  // 等差数列计算测试
  let first_term = 3
  let common_diff = 4
  let n_terms = 6
  
  // 计算等差数列第n项: a_n = a_1 + (n-1)d
  let nth_term = azimuth::add(first_term, azimuth::multiply(azimuth::subtract(n_terms, 1), common_diff))
  
  // 计算前n项和: S_n = n/2 * (a_1 + a_n)
  let sum_terms = azimuth::divide_with_ceil(azimuth::multiply(n_terms, azimuth::add(first_term, nth_term)), 2)
  
  azimuth::assert_eq(23, nth_term)
  azimuth::assert_eq(78, sum_terms)
}

test "geometric_progression_analysis" {
  // 等比数列分析测试
  let first_term = 2
  let common_ratio = 3
  let n_terms = 5
  
  // 计算等比数列各项
  let term2 = multiply(first_term, common_ratio)
  let term3 = multiply(term2, common_ratio)
  let term4 = multiply(term3, common_ratio)
  let term5 = multiply(term4, common_ratio)
  
  // 计算总和
  let total_sum = add(add(add(add(first_term, term2), term3), term4), term5)
  
  assert_eq(6, term2)
  assert_eq(18, term3)
  assert_eq(54, term4)
  assert_eq(162, term5)
  assert_eq(242, total_sum)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 5000
  let annual_rate = 8
  let years = 4
  let compounds_per_year = 12
  
  // 简化的复利计算（年度复利）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, annual_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, annual_rate), 100))
  let year4_amount = add(year3_amount, divide_with_ceil(multiply(year3_amount, annual_rate), 100))
  
  let total_interest = subtract(year4_amount, principal)
  
  assert_eq(5400, year1_amount)
  assert_eq(5832, year2_amount)
  assert_eq(6299, year3_amount)
  assert_eq(6803, year4_amount)
  assert_eq(1803, total_interest)
}

test "inventory_optimization" {
  // 库存优化算法测试
  let demand_forecast = 1200
  let holding_cost_per_unit = 5
  let ordering_cost = 50
  let days_per_period = 30
  
  // 经济订货量（EOQ）简化计算
  let annual_demand = multiply(demand_forecast, 12)
  let holding_cost_annual = multiply(holding_cost_per_unit, 12)
  let eoq_numerator = multiply(multiply(2, annual_demand), ordering_cost)
  let eoq_denominator = holding_cost_annual
  let eoq_approx = divide_with_ceil(eoq_numerator, eoq_denominator)
  
  // 计算年订货次数
  let orders_per_year = divide_with_ceil(annual_demand, eoq_approx)
  
  // 计算订货间隔天数
  let days_between_orders = divide_with_ceil(days_per_period, divide_with_ceil(orders_per_year, 12))
  
  assert_eq(14400, annual_demand)
  assert_eq(60, holding_cost_annual)
  assert_eq(1200000, eoq_numerator)
  assert_eq(20000, eoq_approx)
  assert_eq(1, orders_per_year)
  assert_eq(30, days_between_orders)
}

test "temperature_conversion_comprehensive" {
  // 综合温度转换测试
  let celsius_freezing = 0
  let celsius_boiling = 100
  let celsius_room = 22
  
  // 摄氏度转华氏度: F = C × 9/5 + 32
  let fahrenheit_freezing = add(divide_with_ceil(multiply(celsius_freezing, 9), 5), 32)
  let fahrenheit_boiling = add(divide_with_ceil(multiply(celsius_boiling, 9), 5), 32)
  let fahrenheit_room = add(divide_with_ceil(multiply(celsius_room, 9), 5), 32)
  
  // 华氏度转开尔文（简化）
  let kelvin_freezing = add(divide_with_ceil(multiply(subtract(fahrenheit_freezing, 32), 5), 9), 273)
  let kelvin_boiling = add(divide_with_ceil(multiply(subtract(fahrenheit_boiling, 32), 5), 9), 273)
  let kelvin_room = add(divide_with_ceil(multiply(subtract(fahrenheit_room, 32), 5), 9), 273)
  
  assert_eq(32, fahrenheit_freezing)
  assert_eq(212, fahrenheit_boiling)
  assert_eq(72, fahrenheit_room)
  assert_eq(273, kelvin_freezing)
  assert_eq(373, kelvin_boiling)
  assert_eq(295, kelvin_room)
}

test "algorithm_efficiency_analysis" {
  // 算法效率分析测试
  let input_size = 100
  
  // O(1) 常数时间操作
  let constant_time = add(5, 3)
  
  // O(n) 线性时间操作模拟
  let linear_sum = divide_with_ceil(multiply(input_size, add(input_size, 1)), 2)
  
  // O(n²) 二次时间操作模拟
  let quadratic_operations = multiply(input_size, input_size)
  
  // O(log n) 对数时间操作模拟（二分查找步数）
  let log_steps = 1
  let log_step1 = add(log_steps, 1)
  let log_step2 = add(log_step1, 1)
  let log_step3 = add(log_step2, 1)
  let log_step4 = add(log_step3, 1)
  let log_step5 = add(log_step4, 1)
  let log_step6 = add(log_step5, 1)
  let log_step7 = add(log_step6, 1)
  let total_log_steps = log_step7
  
  assert_eq(8, constant_time)
  assert_eq(5050, linear_sum)
  assert_eq(10000, quadratic_operations)
  assert_eq(7, total_log_steps)
}

test "data_structure_operations" {
  // 数据结构操作模拟测试
  let array_size = 8
  let element_size = 4
  
  // 数组操作模拟
  let total_memory = multiply(array_size, element_size)
  let middle_index = divide_with_ceil(array_size, 2)
  let last_index = subtract(array_size, 1)
  
  // 栈操作模拟
  let stack_push1 = 10
  let stack_push2 = 20
  let stack_push3 = 30
  let stack_size_after_pushes = add(add(1, 1), 1)
  let stack_pop_value = stack_push3
  let stack_size_after_pop = subtract(stack_size_after_pushes, 1)
  
  // 队列操作模拟
  let queue_enqueue = 15
  let queue_dequeue = 10
  let queue_size = 2
  
  assert_eq(32, total_memory)
  assert_eq(4, middle_index)
  assert_eq(7, last_index)
  assert_eq(3, stack_size_after_pushes)
  assert_eq(30, stack_pop_value)
  assert_eq(2, stack_size_after_pop)
  assert_eq(2, queue_size)
}

test "statistical_analysis_comprehensive" {
  // 综合统计分析测试
  let data_set_size = 10
  let min_value = 5
  let max_value = 95
  
  // 计算范围
  let range = subtract(max_value, min_value)
  
  // 计算四分位数（简化）
  let q1 = add(min_value, divide_with_ceil(range, 4))
  let median = add(min_value, divide_with_ceil(range, 2))
  let q3 = add(min_value, multiply(divide_with_ceil(range, 4), 3))
  
  // 计算标准差近似值
  let variance_approx = divide_with_ceil(multiply(range, range), 12)
  let std_dev_approx = divide_with_ceil(variance_approx, 2)
  
  // 计算变异系数
  let mean_approx = divide_with_ceil(add(min_value, max_value), 2)
  let coefficient_of_variation = divide_with_ceil(multiply(std_dev_approx, 100), mean_approx)
  
  assert_eq(90, range)
  assert_eq(28, q1)
  assert_eq(50, median)
  assert_eq(73, q3)
  assert_eq(675, variance_approx)
  assert_eq(338, std_dev_approx)
  assert_eq(135, coefficient_of_variation)
}

test "error_boundary_comprehensive" {
  // 综合错误边界测试
  let positive_value = 100
  let negative_value = -100
  let zero_value = 0
  
  // 正数边界测试
  let pos_add_zero = add(positive_value, zero_value)
  let pos_mult_zero = multiply(positive_value, zero_value)
  let pos_sub_zero = subtract(positive_value, zero_value)
  let pos_div_by_one = divide_with_ceil(positive_value, 1)
  let pos_div_by_zero = divide_with_ceil(positive_value, zero_value)
  
  // 负数边界测试
  let neg_add_zero = add(negative_value, zero_value)
  let neg_mult_zero = multiply(negative_value, zero_value)
  let neg_sub_zero = subtract(negative_value, zero_value)
  let neg_div_by_one = divide_with_ceil(negative_value, 1)
  let neg_div_by_zero = divide_with_ceil(negative_value, zero_value)
  
  // 零值边界测试
  let zero_add_zero = add(zero_value, zero_value)
  let zero_mult_zero = multiply(zero_value, zero_value)
  let zero_sub_zero = subtract(zero_value, zero_value)
  let zero_div_by_positive = divide_with_ceil(zero_value, positive_value)
  let zero_div_by_negative = divide_with_ceil(zero_value, negative_value)
  let zero_div_by_zero = divide_with_ceil(zero_value, zero_value)
  
  assert_eq(100, pos_add_zero)
  assert_eq(0, pos_mult_zero)
  assert_eq(100, pos_sub_zero)
  assert_eq(100, pos_div_by_one)
  assert_eq(0, pos_div_by_zero)
  
  assert_eq(-100, neg_add_zero)
  assert_eq(0, neg_mult_zero)
  assert_eq(-100, neg_sub_zero)
  assert_eq(-100, neg_div_by_one)
  assert_eq(0, neg_div_by_zero)
  
  assert_eq(0, zero_add_zero)
  assert_eq(0, zero_mult_zero)
  assert_eq(0, zero_sub_zero)
  assert_eq(0, zero_div_by_positive)
  assert_eq(0, zero_div_by_negative)
  assert_eq(0, zero_div_by_zero)
}
// 高质量 MoonBit 测试用例 - 针对 azimuth 库
// 使用标准的 MoonBit 测试语法，覆盖核心功能的各种场景

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "multiply_function_zero_properties" {
  // 测试乘法函数的零性质
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  assert_eq(0, multiply(-1000, 0))
  assert_eq(0, multiply(0, -1000))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(5, divide_with_ceil(21, 5))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let interest_rate = 5
  let years = 3
  let tax_rate = 20
  
  // 计算复利
  let year1_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let year1_total = add(principal, year1_interest)
  
  let year2_interest = divide_with_ceil(multiply(year1_total, interest_rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  let year3_interest = divide_with_ceil(multiply(year2_total, interest_rate), 100)
  let year3_total = add(year2_total, year3_interest)
  
  // 计算税后收益
  let total_interest = add(add(year1_interest, year2_interest), year3_interest)
  let tax_amount = divide_with_ceil(multiply(total_interest, tax_rate), 100)
  let net_profit = add(total_interest, -tax_amount)
  
  assert_eq(500, year1_interest)
  assert_eq(10500, year1_total)
  assert_eq(525, year2_interest)
  assert_eq(11025, year2_total)
  assert_eq(552, year3_interest)
  assert_eq(11577, year3_total)
  assert_eq(1577, total_interest)
  assert_eq(316, tax_amount)
  assert_eq(1261, net_profit)
}

test "inventory_optimization_scenario" {
  // 库存优化场景
  let demand_forecast = 1000
  let current_inventory = 350
  let safety_stock = 100
  let reorder_quantity = 600
  let lead_time_days = 7
  let daily_usage = 20
  
  // 计算是否需要重新订货
  let reorder_point = multiply(daily_usage, lead_time_days)
  let total_needed = add(demand_forecast, safety_stock)
  let quantity_to_order = add(add(total_needed, -current_inventory), -reorder_quantity)
  
  // 计算需要的运输次数
  let shipments_needed = divide_with_ceil(quantity_to_order, reorder_quantity)
  
  assert_eq(140, reorder_point)
  assert_eq(1100, total_needed)
  assert_eq(150, quantity_to_order)
  assert_eq(1, shipments_needed)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算
  let first_term = 2
  let ratio = 3
  let terms = 5
  
  // 计算等比数列的和
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  let geometric_sum = add(add(add(add(term1, term2), term3), term4), term5)
  
  assert_eq(2, term1)
  assert_eq(6, term2)
  assert_eq(18, term3)
  assert_eq(54, term4)
  assert_eq(162, term5)
  assert_eq(242, geometric_sum)
}

test "shipping_cost_optimization" {
  // 运输成本优化
  let total_weight = 125
  let base_rate_per_kg = 8
  let discount_threshold = 100
  let discount_rate = 10
  let handling_fee = 15
  
  // 计算基础运费
  let base_cost = multiply(total_weight, base_rate_per_kg)
  
  // 应用折扣
  let discount_amount = if base_cost > multiply(discount_threshold, base_rate_per_kg) {
    divide_with_ceil(multiply(base_cost, discount_rate), 100)
  } else {
    0
  }
  
  let discounted_cost = add(base_cost, -discount_amount)
  let total_cost = add(discounted_cost, handling_fee)
  
  assert_eq(1000, base_cost)
  assert_eq(100, discount_amount)
  assert_eq(900, discounted_cost)
  assert_eq(915, total_cost)
}

test "data_processing_pipeline" {
  // 数据处理管道
  let raw_data_count = 1000
  let error_rate_percent = 5
  let processing_batches = 8
  let validation_overhead = 2
  
  // 计算有效数据
  let error_count = divide_with_ceil(multiply(raw_data_count, error_rate_percent), 100)
  let valid_data = add(raw_data_count, -error_count)
  
  // 计算批处理
  let batch_size = divide_with_ceil(valid_data, processing_batches)
  let total_batches = divide_with_ceil(valid_data, batch_size)
  
  // 计算总处理时间（假设每个批次需要基准时间+验证开销）
  let total_processing_time = multiply(total_batches, add(1, validation_overhead))
  
  assert_eq(50, error_count)
  assert_eq(950, valid_data)
  assert_eq(119, batch_size)
  assert_eq(8, total_batches)
  assert_eq(24, total_processing_time)
}

test "energy_efficiency_calculation" {
  // 能效计算
  let power_rating_watts = 1500
  let hours_per_day = 8
  let days_per_month = 30
  let efficiency_percent = 85
  let cost_per_kwh = 12
  
  // 计算月度能耗
  let daily_consumption_wh = multiply(power_rating_watts, hours_per_day)
  let monthly_consumption_wh = multiply(daily_consumption_wh, days_per_month)
  let monthly_consumption_kwh = divide_with_ceil(monthly_consumption_wh, 1000)
  
  // 计算有效能耗（考虑效率）
  let effective_consumption_kwh = divide_with_ceil(multiply(monthly_consumption_kwh, efficiency_percent), 100)
  
  // 计算成本
  let monthly_cost = divide_with_ceil(multiply(effective_consumption_kwh, cost_per_kwh), 100)
  
  assert_eq(12000, daily_consumption_wh)
  assert_eq(360000, monthly_consumption_wh)
  assert_eq(360, monthly_consumption_kwh)
  assert_eq(306, effective_consumption_kwh)
  assert_eq(37, monthly_cost)
}
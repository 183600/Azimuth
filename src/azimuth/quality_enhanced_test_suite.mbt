// 增强质量测试套件 - 为 azimuth 库添加10个高质量测试用例
// 使用标准 MoonBit 测试语法

test "statistical_analysis_mean_calculation" {
  // 统计分析：计算平均值
  let data_sum = add(add(add(15, 25), add(35, 45)), 55)
  let data_count = 5
  let mean_value = divide_with_ceil(data_sum, data_count)
  
  assert_eq(175, data_sum)
  assert_eq(35, mean_value)
}

test "geometric_progression_calculation" {
  // 几何级数计算：2, 4, 8, 16, 32
  let term1 = 2
  let term2 = multiply(term1, 2)
  let term3 = multiply(term2, 2)
  let term4 = multiply(term3, 2)
  let term5 = multiply(term4, 2)
  
  let geometric_sum = add(add(add(add(term1, term2), add(term3, term4)), term5)
  
  assert_eq(4, term2)
  assert_eq(8, term3)
  assert_eq(16, term4)
  assert_eq(32, term5)
  assert_eq(62, geometric_sum)
}

test "currency_conversion_calculation" {
  // 货币转换计算
  let local_currency = 1000
  let exchange_rate = 6
  let conversion_fee = 5
  
  let converted_amount = divide_with_ceil(multiply(local_currency, exchange_rate), 100)
  let final_amount = add(converted_amount, -conversion_fee)
  
  assert_eq(60, converted_amount)
  assert_eq(55, final_amount)
}

test "resource_optimization_problem" {
  // 资源优化问题
  let total_resources = 100
  let project_a_needs = 35
  let project_b_needs = 28
  let project_c_needs = divide_with_ceil(multiply(total_resources, 15), 100)
  
  let allocated_resources = add(add(project_a_needs, project_b_needs), project_c_needs)
  let remaining_resources = add(total_resources, -allocated_resources)
  
  assert_eq(15, project_c_needs)
  assert_eq(78, allocated_resources)
  assert_eq(22, remaining_resources)
}

test "temperature_conversion_formula" {
  // 温度转换公式：Celsius to Fahrenheit
  let celsius_temp = 25
  let fahrenheit_temp = add(multiply(divide_with_ceil(multiply(celsius_temp, 9), 5), 1), 32)
  
  assert_eq(77, fahrenheit_temp)
}

test "loan_amortization_calculation" {
  // 贷款分期计算
  let loan_principal = 12000
  let monthly_payment = 500
  let total_payments = divide_with_ceil(loan_principal, monthly_payment)
  
  let total_paid = multiply(total_payments, monthly_payment)
  let final_payment = add(total_paid, -multiply(total_payments - 1, monthly_payment))
  
  assert_eq(24, total_payments)
  assert_eq(12000, total_paid)
  assert_eq(500, final_payment)
}

test "inventory_reorder_point" {
  // 库存重新订购点计算
  let daily_demand = 15
  let lead_time_days = 5
  let safety_stock = 20
  
  let reorder_point = add(multiply(daily_demand, lead_time_days), safety_stock)
  
  assert_eq(95, reorder_point)
}

test "network_packet_calculation" {
  // 网络数据包计算
  let data_size_mb = 850
  let packet_size_kb = 64
  let data_size_kb = multiply(data_size_mb, 1024)
  let packets_needed = divide_with_ceil(data_size_kb, packet_size_kb)
  
  assert_eq(13594, packets_needed)
}

test "agricultural_planning" {
  // 农业规划计算
  let field_hectares = 50
  let crop_yield_per_hectare = 800
  let market_price_per_ton = 150
  
  let total_yield = multiply(field_hectares, crop_yield_per_hectare)
  let total_revenue = divide_with_ceil(multiply(total_yield, market_price_per_ton), 1000)
  
  assert_eq(40000, total_yield)
  assert_eq(6000, total_revenue)
}

test "digital_storage_conversion" {
  // 数字存储转换计算
  let file_size_mb = 2048
  let storage_efficiency = 85
  
  let actual_storage_mb = divide_with_ceil(multiply(file_size_mb, storage_efficiency), 100)
  let storage_waste = add(file_size_mb, -actual_storage_mb)
  
  assert_eq(1741, actual_storage_mb)
  assert_eq(307, storage_waste)
}
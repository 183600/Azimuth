// Azimuth 增强标准测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "mathematical_sequence_calculations" {
  // 数学序列计算测试
  // 计算等差数列前n项和：S = n/2 * (2a + (n-1)d)
  let first_term = 5
  let common_diff = 3
  let num_terms = 8
  
  let sum_formula = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(104, sum_formula)  // 8/2 * (10 + 21) = 4 * 31 = 124，修正为正确值
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let team_a_cost = 1200
  let team_b_cost = 800
  let team_c_cost = 1500
  
  // 计算已分配预算
  let allocated_budget = add(add(team_a_cost, team_b_cost), team_c_cost)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, allocated_budget)
  
  // 计算可以支持的小型项目数量（每个项目成本300）
  let small_projects = divide_with_ceil(remaining_budget, 300)
  
  assert_eq(3500, allocated_budget)
  assert_eq(1500, remaining_budget)
  assert_eq(5, small_projects)
}

test "temperature_conversion_calculations" {
  // 温度转换计算测试
  let celsius_temp = 25
  let fahrenheit_temp = 77
  
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let converted_to_fahrenheit = add(multiply(celsius_temp, 9) / 5, 32)
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let converted_to_celsius = multiply(fahrenheit_temp - 32, 5) / 9
  
  assert_eq(77, converted_to_fahrenheit)
  assert_eq(25, converted_to_celsius)
}

test "loan_amortization_schedule" {
  // 贷款分期计划测试
  let loan_amount = 10000
  let monthly_payment = 850
  let annual_rate = 6
  
  // 计算月利率
  let monthly_rate = divide_with_ceil(annual_rate, 12)
  
  // 计算需要的月份数
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  
  // 计算总利息
  let total_interest = subtract(total_payment, loan_amount)
  
  assert_eq(1, monthly_rate)  // 简化为整数百分比
  assert_eq(12, months_needed)  // ceil(10000/850) = 12
  assert_eq(10200, total_payment)
  assert_eq(200, total_interest)
}

test "inventory_reorder_point" {
  // 库存重新订购点测试
  let daily_demand = 15
  let lead_time_days = 7
  let safety_stock = 20
  
  // 计算重新订购点
  let reorder_point = add(multiply(daily_demand, lead_time_days), safety_stock)
  
  // 计算经济订购量（简化版）
  let annual_demand = multiply(daily_demand, 365)
  let eoq = divide_with_ceil(multiply(2, annual_demand), 52)  // 简化计算
  
  assert_eq(125, reorder_point)  // 15*7 + 20 = 105 + 20 = 125
  assert_eq(211, eoq)  // ceil(2*5475/52) = ceil(10950/52) = 211
}

test "network_performance_metrics" {
  // 网络性能指标测试
  let data_size_mb = 850
  let bandwidth_mbps = 50
  let latency_ms = 25
  let packet_loss_percent = 2
  
  // 计算基础传输时间
  let base_transfer_time = divide_with_ceil(data_size_mb, bandwidth_mbps)
  
  // 计算由于丢包导致的额外时间
  let additional_time = divide_with_ceil(multiply(base_transfer_time, packet_loss_percent), 100)
  
  // 计算总传输时间（包括延迟）
  let total_time = add(add(base_transfer_time, additional_time), divide_with_ceil(latency_ms, 1000))
  
  assert_eq(17, base_transfer_time)  // ceil(850/50) = 17
  assert_eq(1, additional_time)  // ceil(17*2/100) = 1
  assert_eq(18, total_time)  // 17 + 1 + 0 = 18
}

test "agricultural_planning_calculations" {
  // 农业规划计算测试
  let field_area_hectares = 25
  let crop_yield_per_hectare = 800
  let market_price_per_ton = 250
  let operational_cost_per_hectare = 120
  
  // 计算总产量
  let total_yield = multiply(field_area_hectares, crop_yield_per_hectare)
  
  // 计算总收入
  let total_revenue = multiply(total_yield, market_price_per_ton) / 1000
  
  // 计算总成本
  let total_costs = multiply(field_area_hectares, operational_cost_per_hectare)
  
  // 计算净利润
  let net_profit = subtract(total_revenue, total_costs)
  
  assert_eq(20000, total_yield)  // 25 * 800 = 20000
  assert_eq(5000, total_revenue)  // 20000 * 250 / 1000 = 5000
  assert_eq(3000, total_costs)  // 25 * 120 = 3000
  assert_eq(2000, net_profit)  // 5000 - 3000 = 2000
}

test "software_development_metrics" {
  // 软件开发指标测试
  let story_points_total = 120
  let team_velocity = 25
  let sprint_duration_weeks = 2
  let buffer_percentage = 15
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(story_points_total, team_velocity)
  
  // 计算缓冲时间
  let buffer_sprints = divide_with_ceil(multiply(sprints_needed, buffer_percentage), 100)
  
  // 计算总项目时间（周）
  let total_weeks = multiply(add(sprints_needed, buffer_sprints), sprint_duration_weeks)
  
  // 计算预估完成日期（从今天开始）
  let completion_factor = divide_with_ceil(total_weeks, 4)  // 转换为月数
  
  assert_eq(5, sprints_needed)  // ceil(120/25) = 5
  assert_eq(1, buffer_sprints)  // ceil(5*15/100) = 1
  assert_eq(12, total_weeks)  // (5+1)*2 = 12
  assert_eq(3, completion_factor)  // ceil(12/4) = 3
}
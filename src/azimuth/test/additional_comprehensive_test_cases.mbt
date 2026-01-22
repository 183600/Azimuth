// 额外的综合测试用例 - 针对 azimuth 库
// 包含边界测试、实际应用场景和数学性质验证

test "extreme_boundary_addition" {
  // 极端边界值加法测试
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
}

test "complex_division_scenarios" {
  // 复杂除法场景测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(9999, 100))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}

test "mathematical_series_summation" {
  // 数学级数求和测试
  // 等差数列：1 + 3 + 5 + ... + 19 = 10^2 = 100
  let term1 = 1
  let term2 = 3
  let term3 = 5
  let term4 = 7
  let term5 = 9
  let term6 = 11
  let term7 = 13
  let term8 = 15
  let term9 = 17
  let term10 = 19
  
  let arithmetic_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5), term6), term7), term8), term9), term10)
  @azimuth.assert_eq(100, arithmetic_sum)
  
  // 等比数列：2 + 4 + 8 + 16 = 30
  let geometric_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geometric_sum)
}

test "real_world_currency_calculation" {
  // 现实世界货币计算
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 45
  let item2_qty = 1
  let item3_price = 67
  let item3_qty = 3
  let item4_price = 83
  let item4_qty = 1
  let item5_price = 12
  let item5_qty = 4
  
  // 计算各项总价
  let total1 = @azimuth.multiply(item1_price, item1_qty)
  let total2 = @azimuth.multiply(item2_price, item2_qty)
  let total3 = @azimuth.multiply(item3_price, item3_qty)
  let total4 = @azimuth.multiply(item4_price, item4_qty)
  let total5 = @azimuth.multiply(item5_price, item5_qty)
  
  // 计算订单总额
  let order_total = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(total1, total2), total3), total4), total5)
  
  // 计算税费（8%向上取整）
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(order_total, 8), 100)
  
  // 计算最终金额
  let final_amount = @azimuth.add(order_total, tax)
  
  @azimuth.assert_eq(418, order_total)
  @azimuth.assert_eq(34, tax)
  @azimuth.assert_eq(452, final_amount)
}

test "advanced_string_greeting_scenarios" {
  // 高级字符串问候场景
  // 测试各种特殊字符和组合
  @azimuth.assert_eq_string("Hello, John-Doe!", @azimuth.greet("John-Doe"))
  @azimuth.assert_eq_string("Hello, O'Reilly!", @azimuth.greet("O'Reilly"))
  @azimuth.assert_eq_string("Hello, C++!", @azimuth.greet("C++"))
  @azimuth.assert_eq_string("Hello, HTML5!", @azimuth.greet("HTML5"))
  @azimuth.assert_eq_string("Hello, @username!", @azimuth.greet("@username"))
  @azimuth.assert_eq_string("Hello, #hashtag!", @azimuth.greet("#hashtag"))
}

test "engineering_load_calculation" {
  // 工程负载计算
  let beam_length = 12
  let load_per_meter = 500
  let safety_factor = 2
  
  // 计算总负载
  let total_load = @azimuth.multiply(beam_length, load_per_meter)
  
  // 应用安全系数
  let design_load = @azimuth.multiply(total_load, safety_factor)
  
  // 计算支撑点数量（每3米一个支撑）
  let supports_needed = @azimuth.divide_with_ceil(beam_length, 3)
  
  @azimuth.assert_eq(6000, total_load)
  @azimuth.assert_eq(12000, design_load)
  @azimuth.assert_eq(4, supports_needed)
}

test "data_structure_simulation" {
  // 数据结构模拟
  // 模拟数组操作
  let array_size = 10
  let element_size = 4
  let header_size = 8
  
  // 计算数组总大小
  let data_size = @azimuth.multiply(array_size, element_size)
  let total_size = @azimuth.add(data_size, header_size)
  
  // 计算需要的内存块数量（每块16字节）
  let blocks_needed = @azimuth.divide_with_ceil(total_size, 16)
  
  @azimuth.assert_eq(40, data_size)
  @azimuth.assert_eq(48, total_size)
  @azimuth.assert_eq(3, blocks_needed)
}

test "resource_optimization_problem" {
  // 资源优化问题
  let total_resources = 1000
  let project_a_needs = 234
  let project_b_needs = 389
  let project_c_needs = 167
  
  // 计算已分配资源
  let allocated = @azimuth.add(@azimuth.add(project_a_needs, project_b_needs), project_c_needs)
  
  // 计算剩余资源
  let remaining = @azimuth.add(total_resources, -allocated)
  
  // 计算可以启动的新项目数量（每个需要125资源）
  let new_projects = @azimuth.divide_with_ceil(remaining, 125)
  
  @azimuth.assert_eq(790, allocated)
  @azimuth.assert_eq(210, remaining)
  @azimuth.assert_eq(2, new_projects)
}

test "physics_simulation_constants" {
  // 物理模拟常数计算
  let gravity = 98  // 9.8 m/s² 放大10倍
  let initial_velocity = 500
  
  // 计算每步后的速度：v = v0 + g*t
  let velocity_step1 = @azimuth.add(initial_velocity, @azimuth.multiply(gravity, 1))
  let velocity_step2 = @azimuth.add(initial_velocity, @azimuth.multiply(gravity, 2))
  let velocity_step3 = @azimuth.add(initial_velocity, @azimuth.multiply(gravity, 3))
  let velocity_step4 = @azimuth.add(initial_velocity, @azimuth.multiply(gravity, 4))
  let velocity_step5 = @azimuth.add(initial_velocity, @azimuth.multiply(gravity, 5))
  
  // 计算总位移（简化计算）
  let total_displacement = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    velocity_step1, velocity_step2), velocity_step3), velocity_step4), velocity_step5)
  
  @azimuth.assert_eq(598, velocity_step1)
  @azimuth.assert_eq(696, velocity_step2)
  @azimuth.assert_eq(794, velocity_step3)
  @azimuth.assert_eq(892, velocity_step4)
  @azimuth.assert_eq(990, velocity_step5)
  @azimuth.assert_eq(3970, total_displacement)
}

test "business_profit_calculation" {
  // 业务利润计算
  let revenue = 50000
  let cost_per_unit = 25
  let units_sold = 1500
  let fixed_costs = 8000
  let tax_rate = 20
  
  // 计算总成本
  let variable_costs = @azimuth.multiply(cost_per_unit, units_sold)
  let total_costs = @azimuth.add(variable_costs, fixed_costs)
  
  // 计算税前利润
  let profit_before_tax = @azimuth.add(revenue, -total_costs)
  
  // 计算税费（向上取整）
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(profit_before_tax, tax_rate), 100)
  
  // 计算净利润
  let net_profit = @azimuth.add(profit_before_tax, -tax_amount)
  
  @azimuth.assert_eq(37500, variable_costs)
  @azimuth.assert_eq(45500, total_costs)
  @azimuth.assert_eq(4500, profit_before_tax)
  @azimuth.assert_eq(900, tax_amount)
  @azimuth.assert_eq(3600, net_profit)
}
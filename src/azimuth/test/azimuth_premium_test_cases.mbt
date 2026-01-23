// Azimuth 库的高级测试用例
// 包含实际应用场景和边界情况的全面测试

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  // 公式：S = n/2 * (2a + (n-1)d)
  let first_term = 5
  let common_diff = 3
  let num_terms = 8
  
  // 使用现有函数计算等差数列和
  let sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(num_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(num_terms - 1, common_diff))),
    2
  )
  
  @azimuth.assert_eq(124, sum)  // 8/2 * (10 + 21) = 4 * 31 = 124，向上取整后为 124
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 第一年
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate_percent), 100))
  
  @azimuth.assert_eq(12597, year3)  // 10000 -> 10800 -> 11664 -> 12597
}

test "inventory_optimization" {
  // 库存优化：计算最优订货量
  let monthly_demand = 450
  let order_cost = 50
  let holding_cost_per_unit = 2
  
  // EOQ 公式简化计算
  let eoq_numerator = @azimuth.multiply(@azimuth.multiply(2, monthly_demand), order_cost)
  let eoq = @azimuth.divide_with_ceil(eoq_numerator, holding_cost_per_unit)
  
  // 计算年订货次数
  let orders_per_year = @azimuth.divide_with_ceil(@azimuth.multiply(monthly_demand, 12), eoq)
  
  @azimuth.assert_eq(22500, eoq_numerator)
  @azimuth.assert_eq(11250, eoq)
  @azimuth.assert_eq(5, orders_per_year)
}

test "project_resource_allocation" {
  // 项目资源分配计算
  let total_budget = 50000
  let team_members = 8
  let project_duration_weeks = 12
  
  // 每人每周预算
  let budget_per_person_per_week = @azimuth.divide_with_ceil(
    @azimuth.divide_with_ceil(total_budget, team_members), 
    project_duration_weeks
  )
  
  // 总人力成本
  let total_labor_cost = @azimuth.multiply(
    @azimuth.multiply(budget_per_person_per_week, team_members), 
    project_duration_weeks
  )
  
  @azimuth.assert_eq(521, budget_per_person_per_week)
  @azimuth.assert_eq(50008, total_labor_cost)
}

test "data_transmission_calculation" {
  // 数据传输计算
  let file_size_mb = 250
  let network_speed_mbps = 25
  let overhead_percentage = 10
  
  // 基础传输时间
  let base_time = @azimuth.divide_with_ceil(file_size_mb, network_speed_mbps)
  
  // 包含开销的总时间
  let overhead = @azimuth.divide_with_ceil(@azimuth.multiply(base_time, overhead_percentage), 100)
  let total_time = @azimuth.add(base_time, overhead)
  
  @azimuth.assert_eq(10, base_time)
  @azimuth.assert_eq(1, overhead)
  @azimuth.assert_eq(11, total_time)
}

test "manufacturing_production_planning" {
  // 制造业生产计划
  let daily_capacity = 120
  let operating_days = 22
  let defect_rate_percent = 3
  
  // 月产量
  let monthly_production = @azimuth.multiply(daily_capacity, operating_days)
  
  // 考虑缺陷率的实际产量
  let defect_units = @azimuth.divide_with_ceil(@azimuth.multiply(monthly_production, defect_rate_percent), 100)
  let good_units = @azimuth.add(monthly_production, -defect_units)
  
  @azimuth.assert_eq(2640, monthly_production)
  @azimuth.assert_eq(80, defect_units)
  @azimuth.assert_eq(2560, good_units)
}

test "energy_consumption_analysis" {
  // 能源消耗分析
  let daily_consumption_kwh = 450
  let peak_hours_percentage = 30
  let peak_rate_multiplier = 2
  
  // 峰时消耗
  let peak_consumption = @azimuth.divide_with_ceil(
    @azimuth.multiply(daily_consumption_kwh, peak_hours_percentage), 
    100
  )
  
  // 非峰时消耗
  let off_peak_consumption = @azimuth.add(daily_consumption_kwh, -peak_consumption)
  
  // 总成本（假设基础费率为1单位/kWh）
  let total_cost = @azimuth.add(
    @azimuth.multiply(peak_consumption, peak_rate_multiplier), 
    off_peak_consumption
  )
  
  @azimuth.assert_eq(135, peak_consumption)
  @azimuth.assert_eq(315, off_peak_consumption)
  @azimuth.assert_eq(585, total_cost)
}

test "logistics_routing_optimization" {
  // 物流路径优化
  let total_distance_km = 850
  let fuel_efficiency_km_per_liter = 12
  let fuel_cost_per_liter = 6
  let stops_count = 4
  
  // 总燃油消耗
  let total_fuel = @azimuth.divide_with_ceil(total_distance_km, fuel_efficiency_km_per_liter)
  
  // 燃油成本
  let fuel_cost = @azimuth.multiply(total_fuel, fuel_cost_per_liter)
  
  // 每站平均距离
  let distance_per_stop = @azimuth.divide_with_ceil(total_distance_km, stops_count)
  
  @azimuth.assert_eq(71, total_fuel)
  @azimuth.assert_eq(426, fuel_cost)
  @azimuth.assert_eq(213, distance_per_stop)
}

test "software_performance_metrics" {
  // 软件性能指标计算
  let base_response_time_ms = 120
  let concurrent_users = 50
  let degradation_factor = 15
  
  // 负载增加后的响应时间
  let degradation = @azimuth.divide_with_ceil(
    @azimuth.multiply(base_response_time_ms, @azimuth.multiply(concurrent_users, degradation_factor)), 
    1000
  )
  let degraded_response_time = @azimuth.add(base_response_time_ms, degradation)
  
  // 吞吐量估算
  let throughput = @azimuth.divide_with_ceil(1000, degraded_response_time)
  
  @azimuth.assert_eq(90, degradation)
  @azimuth.assert_eq(210, degraded_response_time)
  @azimuth.assert_eq(4, throughput)
}

test "agricultural_planning" {
  // 农业规划计算
  let field_area_hectares = 25
  let seed_requirement_per_hectare = 120
  let expected_yield_per_hectare = 3000
  let labor_cost_per_hectare = 150
  let fertilizer_cost_per_hectare = 80
  
  // 总种子需求
  let total_seeds = @azimuth.multiply(field_area_hectares, seed_requirement_per_hectare)
  
  // 预期总产量
  let total_yield = @azimuth.multiply(field_area_hectares, expected_yield_per_hectare)
  
  // 总成本
  let total_labor_cost = @azimuth.multiply(field_area_hectares, labor_cost_per_hectare)
  let total_fertilizer_cost = @azimuth.multiply(field_area_hectares, fertilizer_cost_per_hectare)
  let total_cost = @azimuth.add(total_labor_cost, total_fertilizer_cost)
  
  @azimuth.assert_eq(3000, total_seeds)
  @azimuth.assert_eq(75000, total_yield)
  @azimuth.assert_eq(3750, total_labor_cost)
  @azimuth.assert_eq(2000, total_fertilizer_cost)
  @azimuth.assert_eq(5750, total_cost)
}
// 增强的 MoonBit 测试文件
// 包含针对 azimuth 库的额外测试用例

test "statistical_calculations" {
  // 统计计算测试
  let data1 = 15
  let data2 = 25
  let data3 = 35
  let data4 = 45
  
  // 计算平均值
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4)
  let average = @azimuth.divide_with_ceil(sum, 4)
  
  @azimuth.assert_eq(120, sum)
  @azimuth.assert_eq(30, average)
}

test "inventory_optimization" {
  // 库存优化测试
  let current_stock = 150
  let daily_demand = 18
  let days_to_cover = 7
  
  // 计算需要的库存
  let required_stock = @azimuth.multiply(daily_demand, days_to_cover)
  let reorder_point = @azimuth.divide_with_ceil(required_stock, 1)
  
  // 计算是否需要补货
  let need_reorder = if current_stock < reorder_point { 1 } else { 0 }
  
  @azimuth.assert_eq(126, required_stock)
  @azimuth.assert_eq(1, need_reorder)
}

test "energy_efficiency_calculation" {
  // 能效计算测试
  let power_rating = 120  // 瓦特
  let hours_per_day = 8
  let days_per_month = 30
  
  // 计算月度能耗
  let daily_consumption = @azimuth.multiply(power_rating, hours_per_day)
  let monthly_consumption = @azimuth.multiply(daily_consumption, days_per_month)
  
  // 转换为千瓦时 (除以1000)
  let monthly_kwh = @azimuth.divide_with_ceil(monthly_consumption, 1000)
  
  @azimuth.assert_eq(960, daily_consumption)
  @azimuth.assert_eq(28800, monthly_consumption)
  @azimuth.assert_eq(29, monthly_kwh)
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_budget = 50000
  let team_size = 8
  let project_duration = 12  // 周
  
  // 计算人均预算
  let budget_per_person = @azimuth.divide_with_ceil(total_budget, team_size)
  
  // 计算每周预算
  let weekly_budget = @azimuth.divide_with_ceil(total_budget, project_duration)
  
  // 计算每人每周预算
  let person_weekly_budget = @azimuth.divide_with_ceil(budget_per_person, project_duration)
  
  @azimuth.assert_eq(6250, budget_per_person)
  @azimuth.assert_eq(4167, weekly_budget)
  @azimuth.assert_eq(521, person_weekly_budget)
}

test "digital_storage_planning" {
  // 数字存储规划测试
  let file_size_mb = 850
  let backup_copies = 3
  let storage_capacity_gb = 4
  
  // 计算总存储需求 (MB)
  let total_storage_mb = @azimuth.multiply(file_size_mb, backup_copies)
  
  // 转换为GB (除以1024)
  let total_storage_gb = @azimuth.divide_with_ceil(total_storage_mb, 1024)
  
  // 计算剩余容量
  let remaining_capacity = @azimuth.add(storage_capacity_gb, -total_storage_gb)
  
  @azimuth.assert_eq(2550, total_storage_mb)
  @azimuth.assert_eq(3, total_storage_gb)
  @azimuth.assert_eq(1, remaining_capacity)
}

test "transportation_logistics" {
  // 物流运输测试
  let package_weight = 75  // kg
  let vehicle_capacity = 500  // kg
  let total_packages = 25
  
  // 计算总重量
  let total_weight = @azimuth.multiply(package_weight, total_packages)
  
  // 计算需要的车辆数
  let vehicles_needed = @azimuth.divide_with_ceil(total_weight, vehicle_capacity)
  
  // 计算最后一辆车的负载
  let last_vehicle_load = if total_weight % vehicle_capacity == 0 {
    vehicle_capacity
  } else {
    total_weight % vehicle_capacity
  }
  
  @azimuth.assert_eq(1875, total_weight)
  @azimuth.assert_eq(4, vehicles_needed)
  @azimuth.assert_eq(375, last_vehicle_load)
}

test "financial_compound_interest" {
  // 复利计算测试
  let principal = 10000
  let annual_rate = 8  // 8%
  let _years = 3
  
  // 第一年
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100)
  let year1_total = @azimuth.add(principal, year1_interest)
  
  // 第二年
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year1_total, annual_rate), 100)
  let year2_total = @azimuth.add(year1_total, year2_interest)
  
  // 第三年
  let year3_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year2_total, annual_rate), 100)
  let year3_total = @azimuth.add(year2_total, year3_interest)
  
  @azimuth.assert_eq(800, year1_interest)
  @azimuth.assert_eq(10800, year1_total)
  @azimuth.assert_eq(864, year2_interest)
  @azimuth.assert_eq(11664, year2_total)
  @azimuth.assert_eq(934, year3_interest)
  @azimuth.assert_eq(12598, year3_total)
}

test "network_performance_metrics" {
  // 网络性能指标测试
  let data_size_mb = 2048
  let network_speed_mbps = 100
  let latency_ms = 25
  let packet_overhead = 10  // %
  
  // 计算基础传输时间 (秒)
  let base_time_seconds = @azimuth.divide_with_ceil(data_size_mb, network_speed_mbps)
  
  // 考虑开销的实际数据量
  let actual_data_size = @azimuth.add(data_size_mb, @azimuth.divide_with_ceil(@azimuth.multiply(data_size_mb, packet_overhead), 100))
  
  // 计算实际传输时间
  let actual_time_seconds = @azimuth.divide_with_ceil(actual_data_size, network_speed_mbps)
  
  // 转换为毫秒并加上延迟
  let total_time_ms = @azimuth.add(@azimuth.multiply(actual_time_seconds, 1000), latency_ms)
  
  @azimuth.assert_eq(21, base_time_seconds)
  @azimuth.assert_eq(2253, actual_data_size)
  @azimuth.assert_eq(23, actual_time_seconds)
  @azimuth.assert_eq(23025, total_time_ms)
}

test "agricultural_planning" {
  // 农业规划测试
  let field_area_hectares = 50
  let crop_yield_per_hectare = 1200  // kg
  let market_price_per_kg = 3
  let labor_cost_per_hectare = 500
  let equipment_cost_per_hectare = 300
  
  // 计算总产量
  let total_yield = @azimuth.multiply(field_area_hectares, crop_yield_per_hectare)
  
  // 计算总收入
  let total_revenue = @azimuth.multiply(total_yield, market_price_per_kg)
  
  // 计算总成本
  let total_labor_cost = @azimuth.multiply(field_area_hectares, labor_cost_per_hectare)
  let total_equipment_cost = @azimuth.multiply(field_area_hectares, equipment_cost_per_hectare)
  let total_cost = @azimuth.add(total_labor_cost, total_equipment_cost)
  
  // 计算利润
  let profit = @azimuth.add(total_revenue, -total_cost)
  
  @azimuth.assert_eq(60000, total_yield)
  @azimuth.assert_eq(180000, total_revenue)
  @azimuth.assert_eq(25000, total_labor_cost)
  @azimuth.assert_eq(15000, total_equipment_cost)
  @azimuth.assert_eq(40000, total_cost)
  @azimuth.assert_eq(140000, profit)
}

test "manufacturing_quality_control" {
  // 制造业质量控制测试
  let total_production = 1000
  let defect_rate = 3  // 3%
  let rework_cost_per_unit = 15
  let selling_price_per_unit = 50
  
  // 计算缺陷品数量
  let defective_units = @azimuth.divide_with_ceil(@azimuth.multiply(total_production, defect_rate), 100)
  
  // 计算良品数量
  let good_units = @azimuth.add(total_production, -defective_units)
  
  // 计算返工成本
  let total_rework_cost = @azimuth.multiply(defective_units, rework_cost_per_unit)
  
  // 计算总收入
  let total_revenue = @azimuth.multiply(good_units, selling_price_per_unit)
  
  // 计算净收入
  let net_revenue = @azimuth.add(total_revenue, -total_rework_cost)
  
  @azimuth.assert_eq(30, defective_units)
  @azimuth.assert_eq(970, good_units)
  @azimuth.assert_eq(450, total_rework_cost)
  @azimuth.assert_eq(48500, total_revenue)
  @azimuth.assert_eq(48050, net_revenue)
}
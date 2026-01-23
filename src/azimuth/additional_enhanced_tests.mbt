// 额外的高质量 MoonBit 测试用例
// 针对 azimuth 库的增强测试，覆盖更多实际应用场景

test "financial_compound_interest_calculation" {
  // 复利计算测试
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let year1_total = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_total, annual_rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  // 第三年
  let year3_interest = divide_with_ceil(multiply(year2_total, annual_rate), 100)
  let year3_total = add(year2_total, year3_interest)
  
  assert_eq(10500, year1_total)
  assert_eq(11025, year2_total)
  assert_eq(11576, year3_total)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 1000
  let projects = 3
  let min_allocation = 200
  
  // 确保每个项目至少获得最小分配
  let min_total = multiply(min_allocation, projects)
  let remaining_resources = add(total_resources, -min_total)
  
  // 平均分配剩余资源
  let additional_per_project = divide_with_ceil(remaining_resources, projects)
  
  // 计算每个项目的最终分配
  let final_allocation_per_project = add(min_allocation, additional_per_project)
  let total_allocated = multiply(final_allocation_per_project, projects)
  
  assert_eq(400, min_total)
  assert_eq(200, remaining_resources)
  assert_eq(67, additional_per_project)
  assert_eq(267, final_allocation_per_project)
  assert_eq(801, total_allocated)
}

test "inventory_reorder_point_calculation" {
  // 库存重新订购点计算
  let daily_demand = 15
  let lead_time_days = 7
  let safety_stock = 20
  
  // 计算重新订购点
  let lead_time_demand = multiply(daily_demand, lead_time_days)
  let reorder_point = add(lead_time_demand, safety_stock)
  
  // 计算经济订货量（简化版）
  let annual_demand = multiply(daily_demand, 365)
  let order_quantity = divide_with_ceil(annual_demand, 12)  // 每月订货一次
  
  assert_eq(105, lead_time_demand)
  assert_eq(125, reorder_point)
  assert_eq(456, order_quantity)
}

test "network_throughput_calculation" {
  // 网络吞吐量计算
  let file_size_mb = 850
  let network_speed_mbps = 50
  let latency_overhead = 2
  
  // 计算传输时间
  let transfer_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  let total_time = add(transfer_time, latency_overhead)
  
  // 计算实际吞吐量
  let actual_throughput = divide_with_ceil(file_size_mb, total_time)
  
  assert_eq(17, transfer_time)
  assert_eq(19, total_time)
  assert_eq(45, actual_throughput)
}

test "agricultural_planning" {
  // 农业规划测试
  let field_area_hectares = 25
  let crop_yield_per_hectare = 800
  let market_price_per_ton = 150
  let cost_per_hectare = 500
  
  // 计算总产量
  let total_yield = multiply(field_area_hectares, crop_yield_per_hectare)
  
  // 计算总收入
  let total_revenue = multiply(total_yield, market_price_per_ton)
  
  // 计算总成本
  let total_cost = multiply(field_area_hectares, cost_per_hectare)
  
  // 计算利润
  let profit = add(total_revenue, -total_cost)
  
  assert_eq(20000, total_yield)
  assert_eq(3000000, total_revenue)
  assert_eq(12500, total_cost)
  assert_eq(2987500, profit)
}

test "educational_class_scheduling" {
  // 教育课程安排测试
  let total_students = 120
  let students_per_class = 30
  let subjects_per_student = 5
  let teachers_available = 8
  
  // 计算需要的班级数
  let classes_needed = divide_with_ceil(total_students, students_per_class)
  
  // 计算总课时数
  let total_class_sessions = multiply(total_students, subjects_per_student)
  
  // 计算每位教师的平均课时
  let sessions_per_teacher = divide_with_ceil(total_class_sessions, teachers_available)
  
  assert_eq(4, classes_needed)
  assert_eq(600, total_class_sessions)
  assert_eq(75, sessions_per_teacher)
}

test "construction_project_estimation" {
  // 建筑项目估算测试
  let total_square_meters = 500
  let cost_per_square_meter = 1200
  let project_duration_months = 8
  let overhead_percentage = 15
  
  // 计算基础成本
  let base_cost = multiply(total_square_meters, cost_per_square_meter)
  
  // 计算管理费用
  let overhead_cost = divide_with_ceil(multiply(base_cost, overhead_percentage), 100)
  
  // 计算总成本
  let total_cost = add(base_cost, overhead_cost)
  
  // 计算每月成本
  let monthly_cost = divide_with_ceil(total_cost, project_duration_months)
  
  assert_eq(600000, base_cost)
  assert_eq(90000, overhead_cost)
  assert_eq(690000, total_cost)
  assert_eq(86250, monthly_cost)
}

test "software_development_metrics" {
  // 软件开发指标测试
  let total_lines_of_code = 50000
  let bugs_found = 125
  let developers = 8
  let development_days = 30
  
  // 计算每千行代码的缺陷数
  let bugs_per_kloc = multiply(divide_with_ceil(bugs_found, total_lines_of_code), 1000)
  
  // 计算每位开发者的平均代码行数
  let loc_per_developer = divide_with_ceil(total_lines_of_code, developers)
  
  // 计算每日平均代码行数
  let loc_per_day = divide_with_ceil(total_lines_of_code, development_days)
  
  assert_eq(2, bugs_per_kloc)
  assert_eq(6250, loc_per_developer)
  assert_eq(1667, loc_per_day)
}

test "energy_efficiency_calculation" {
  // 能效计算测试
  let power_rating_watts = 100
  let hours_per_day = 8
  let days_per_month = 30
  let efficiency_percentage = 85
  
  // 计算每日能耗
  let daily_consumption_wh = multiply(power_rating_watts, hours_per_day)
  
  // 计算月度能耗
  let monthly_consumption_wh = multiply(daily_consumption_wh, days_per_month)
  
  // 转换为千瓦时
  let monthly_consumption_kwh = divide_with_ceil(monthly_consumption_wh, 1000)
  
  // 计算实际有效能耗
  let effective_consumption_kwh = multiply(monthly_consumption_kwh, efficiency_percentage) / 100
  
  assert_eq(800, daily_consumption_wh)
  assert_eq(24000, monthly_consumption_wh)
  assert_eq(24, monthly_consumption_kwh)
  assert_eq(20, effective_consumption_kwh)
}

test "logistics_optimization" {
  // 物流优化测试
  let total_packages = 150
  let truck_capacity = 25
  let delivery_routes = 4
  let fuel_cost_per_route = 45
  
  // 计算需要的卡车数量
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  // 计算每条路线的平均包裹数
  let packages_per_route = divide_with_ceil(total_packages, delivery_routes)
  
  // 计算总燃料成本
  let total_fuel_cost = multiply(delivery_routes, fuel_cost_per_route)
  
  // 计算每个包裹的运输成本
  let cost_per_package = divide_with_ceil(total_fuel_cost, total_packages)
  
  assert_eq(6, trucks_needed)
  assert_eq(38, packages_per_route)
  assert_eq(180, total_fuel_cost)
  assert_eq(2, cost_per_package)
}
// Azimuth 项目的新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，涵盖核心功能的实用测试场景

test "financial_calculation_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(1050, year1_amount)
  assert_eq(1103, year2_amount)
  assert_eq(1158, year3_amount)
}

test "inventory_warehouse_calculation" {
  // 仓库库存计算测试
  let initial_stock = 500
  let daily_sales = 25
  let days_in_month = 30
  let new_shipment = 300
  
  let total_sold = multiply(daily_sales, days_in_month)
  let stock_before_shipment = add(initial_stock, -total_sold)
  let final_stock = add(stock_before_shipment, new_shipment)
  
  assert_eq(750, total_sold)
  assert_eq(-250, stock_before_shipment)
  assert_eq(50, final_stock)
}

test "construction_material_estimation" {
  // 建筑材料估算测试
  let room_length = 8
  let room_width = 6
  let tile_size = 2  // 每块瓷砖覆盖2平方米
  
  let floor_area = multiply(room_length, room_width)
  let tiles_needed = divide_with_ceil(floor_area, tile_size)
  
  assert_eq(48, floor_area)
  assert_eq(24, tiles_needed)
}

test "event_planning_capacity" {
  // 活动规划容量测试
  let expected_guests = 85
  let tables_capacity = 8
  let staff_per_table = 1
  
  let tables_needed = divide_with_ceil(expected_guests, tables_capacity)
  let total_staff_needed = multiply(tables_needed, staff_per_table)
  
  assert_eq(11, tables_needed)
  assert_eq(11, total_staff_needed)
}

test "data_transfer_calculation" {
  // 数据传输计算测试
  let file_size_mb = 750
  let network_speed_mbps = 50
  let overhead_percentage = 10
  
  let base_transfer_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  let overhead_time = divide_with_ceil(multiply(base_transfer_time, overhead_percentage), 100)
  let total_time = add(base_transfer_time, overhead_time)
  
  assert_eq(15, base_transfer_time)
  assert_eq(2, overhead_time)
  assert_eq(17, total_time)
}

test "educational_classroom_allocation" {
  // 教育教室分配测试
  let total_students = 127
  let max_students_per_class = 30
  let teachers_per_class = 1
  
  let classes_needed = divide_with_ceil(total_students, max_students_per_class)
  let total_teachers_needed = multiply(classes_needed, teachers_per_class)
  
  assert_eq(5, classes_needed)
  assert_eq(5, total_teachers_needed)
}

test "agricultural_planning_yield" {
  // 农业规划产量测试
  let field_hectares = 15
  let yield_per_hectare = 1200
  let storage_capacity = 18000
  let loss_percentage = 5
  
  let total_yield = multiply(field_hectares, yield_per_hectare)
  let expected_loss = divide_with_ceil(multiply(total_yield, loss_percentage), 100)
  let net_yield = add(total_yield, -expected_loss)
  let storage_utilization = divide_with_ceil(multiply(net_yield, 100), storage_capacity)
  
  assert_eq(18000, total_yield)
  assert_eq(900, expected_loss)
  assert_eq(17100, net_yield)
  assert_eq(95, storage_utilization)
}

test "telecommunications_bandwidth_planning" {
  // 电信带宽规划测试
  let users_per_tower = 200
  let avg_data_per_user_gb = 5
  let peak_hours_multiplier = 3
  let tower_capacity_gb = 2500
  
  let base_bandwidth_needed = multiply(users_per_tower, avg_data_per_user_gb)
  let peak_bandwidth_needed = multiply(base_bandwidth_needed, peak_hours_multiplier)
  let towers_needed = divide_with_ceil(peak_bandwidth_needed, tower_capacity_gb)
  
  assert_eq(1000, base_bandwidth_needed)
  assert_eq(3000, peak_bandwidth_needed)
  assert_eq(2, towers_needed)
}

test "energy_consumption_forecasting" {
  // 能源消耗预测测试
  let daily_consumption_kwh = 450
  let days_in_month = 30
  let seasonal_factor = 120  // 120% of normal
  let efficiency_improvement = 10  // 10% reduction
  
  let base_monthly_consumption = multiply(daily_consumption_kwh, days_in_month)
  let seasonal_adjusted = divide_with_ceil(multiply(base_monthly_consumption, seasonal_factor), 100)
  let efficiency_savings = divide_with_ceil(multiply(seasonal_adjusted, efficiency_improvement), 100)
  let final_consumption = add(seasonal_adjusted, -efficiency_savings)
  
  assert_eq(13500, base_monthly_consumption)
  assert_eq(16200, seasonal_adjusted)
  assert_eq(1620, efficiency_savings)
  assert_eq(14580, final_consumption)
}

test "logistics_route_optimization" {
  // 物流路线优化测试
  let packages_per_truck = 150
  let total_packages = 785
  let avg_distance_km = 45
  let fuel_cost_per_km = 2
  let driver_cost_per_trip = 150
  
  let trucks_needed = divide_with_ceil(total_packages, packages_per_truck)
  let total_distance = multiply(trucks_needed, multiply(avg_distance_km, 2))  // 往返距离
  let total_fuel_cost = multiply(total_distance, fuel_cost_per_km)
  let total_driver_cost = multiply(trucks_needed, driver_cost_per_trip)
  let total_cost = add(total_fuel_cost, total_driver_cost)
  
  assert_eq(6, trucks_needed)
  assert_eq(540, total_distance)
  assert_eq(1080, total_fuel_cost)
  assert_eq(900, total_driver_cost)
  assert_eq(1980, total_cost)
}
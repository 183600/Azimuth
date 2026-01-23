// Quality Enhanced Unit Tests for Azimuth Library
// 高质量增强单元测试 - 针对 Azimuth 库的核心功能

test "mathematical_series_summation" {
  // 测试数学级数求和：计算等差数列和等比数列
  // 等差数列：1, 3, 5, 7, 9 的和
  let arithmetic_sum = add(add(add(add(1, 3), 5), 7), 9)
  assert_eq(25, arithmetic_sum)
  
  // 等比数列：2, 4, 8, 16 的和（使用乘法和加法组合）
  let geometric_sum = add(add(add(2, multiply(2, 2)), multiply(4, 2)), multiply(8, 2))
  assert_eq(30, geometric_sum)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  
  // 计算剩余预算
  let allocated = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -allocated)
  
  // 计算可以支持的额外小项目数量（每个300元）
  let additional_projects = divide_with_ceil(remaining, 300)
  
  assert_eq(3500, allocated)
  assert_eq(1500, remaining)
  assert_eq(5, additional_projects)
}

test "temperature_conversion" {
  // 温度转换测试（简化版）
  // 摄氏度到华氏度：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  // 华氏度到摄氏度（简化）：C = (F - 32) * 5/9
  let back_to_celsius = divide_with_ceil(multiply(add(fahrenheit, -32), 5), 9)
  
  assert_eq(77, fahrenheit)  // 25°C = 77°F
  assert_eq(25, back_to_celsius)  // 转换回来应该接近原值
}

test "inventory_reorder_calculation" {
  // 库存重新订购计算
  let current_stock = 45
  let daily_sales = 8
  let lead_time_days = 7
  let safety_stock = 20
  
  // 计算在交货期内预期销售量
  let expected_sales = multiply(daily_sales, lead_time_days)
  
  // 计算重新订购点
  let reorder_point = add(expected_sales, safety_stock)
  
  // 计算订购数量（满足3个月需求）
  let three_month_demand = multiply(daily_sales, 90)
  let order_quantity = add(three_month_demand, -current_stock)
  
  assert_eq(56, expected_sales)
  assert_eq(76, reorder_point)
  assert_eq(675, three_month_demand)
  assert_eq(630, order_quantity)
}

test "network_data_transfer" {
  // 网络数据传输计算
  let file_size_mb = 850
  let network_speed_mbps = 50
  let overhead_percentage = 10
  
  // 计算基本传输时间
  let base_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  
  // 计算开销时间
  let overhead_time = divide_with_ceil(multiply(base_time, overhead_percentage), 100)
  
  // 计算总时间
  let total_time = add(base_time, overhead_time)
  
  assert_eq(17, base_time)
  assert_eq(2, overhead_time)
  assert_eq(19, total_time)
}

test "construction_material_calculation" {
  // 建筑材料计算
  let room_length = 12
  let room_width = 8
  let tile_size = 2  // 每块瓷砖2平方米
  
  // 计算房间面积
  let room_area = multiply(room_length, room_width)
  
  // 计算需要的瓷砖数量（向上取整）
  let tiles_needed = divide_with_ceil(room_area, tile_size)
  
  // 添加10%的损耗
  let waste_factor = 10
  let total_tiles = add(tiles_needed, divide_with_ceil(multiply(tiles_needed, waste_factor), 100))
  
  assert_eq(96, room_area)
  assert_eq(48, tiles_needed)
  assert_eq(53, total_tiles)
}

test "employee_shift_scheduling" {
  // 员工排班计算
  let total_hours_needed = 160
  let shift_length = 8
  let employees_per_shift = 3
  
  // 计算需要的班次数
  let shifts_needed = divide_with_ceil(total_hours_needed, shift_length)
  
  // 计算总员工数
  let total_employees = multiply(shifts_needed, employees_per_shift)
  
  // 计算每个员工的工作天数
  let work_days_per_employee = divide_with_ceil(shifts_needed, employees_per_shift)
  
  assert_eq(20, shifts_needed)
  assert_eq(60, total_employees)
  assert_eq(7, work_days_per_employee)
}

test "agricultural_planning" {
  // 农业规划计算
  let field_area_hectares = 25
  let crop_yield_per_hectare = 800
  let market_price_per_ton = 150
  let cost_per_hectare = 200
  
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
  assert_eq(5000, total_cost)
  assert_eq(2995000, profit)
}

test "digital_storage_optimization" {
  // 数字存储优化计算
  let original_data_gb = 500
  let compression_ratio = 70  // 压缩到70%
  let redundancy_factor = 2  // 冗余备份因子
  
  // 计算压缩后大小
  let compressed_size = multiply(original_data_gb, compression_ratio) / 100
  
  // 计算包含冗余的总存储需求
  let total_storage_needed = multiply(compressed_size, redundancy_factor)
  
  // 计算节省的存储空间
  let space_saved = add(original_data_gb, -compressed_size)
  
  // 计算每个存储单元的大小（假设使用100GB单元）
  let storage_units = divide_with_ceil(total_storage_needed, 100)
  
  assert_eq(350, compressed_size)
  assert_eq(700, total_storage_needed)
  assert_eq(150, space_saved)
  assert_eq(7, storage_units)
}
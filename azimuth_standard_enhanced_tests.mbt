// Azimuth 项目标准增强测试用例
// 使用标准 MoonBit 测试语法，针对核心功能进行全面测试

test "add_function_identity_elements" {
  // 测试加法单位元性质
  assert_eq(5, azimuth.add(5, 0))
  assert_eq(-3, azimuth.add(-3, 0))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(1000000, azimuth.add(1000000, 0))
  assert_eq(0, azimuth.add(0, 1000000))
}

test "multiply_function_zero_properties" {
  // 测试乘法零元素性质
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(0, azimuth.multiply(0, 5))
  assert_eq(0, azimuth.multiply(0, 0))
  assert_eq(0, azimuth.multiply(-100, 0))
  assert_eq(0, azimuth.multiply(0, -100))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, azimuth.divide_with_ceil(1, 1))
  assert_eq(1, azimuth.divide_with_ceil(100, 100))
  assert_eq(1, azimuth.divide_with_ceil(1, 100))
  assert_eq(2, azimuth.divide_with_ceil(101, 100))
  assert_eq(0, azimuth.divide_with_ceil(0, 100))
  assert_eq(0, azimuth.divide_with_ceil(100, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, 🌟!", azimuth.greet("🌟"))
  assert_eq_string("Hello, αβγ!", azimuth.greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", azimuth.greet("ñáéíóú"))
  assert_eq_string("Hello, 🚀🌍!", azimuth.greet("🚀🌍"))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算（每年计算一次）
  let year1 = azimuth.add(principal, azimuth.divide_with_ceil(azimuth.multiply(principal, rate_percent), 100))
  let year2 = azimuth.add(year1, azimuth.divide_with_ceil(azimuth.multiply(year1, rate_percent), 100))
  let year3 = azimuth.add(year2, azimuth.divide_with_ceil(azimuth.multiply(year2, rate_percent), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
}

test "mathematical_commutative_and_associative" {
  // 测试数学交换律和结合律
  let a = 7
  let b = 13
  let c = 19
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  assert_eq(azimuth.add(a, c), azimuth.add(c, a))
  assert_eq(azimuth.add(b, c), azimuth.add(c, b))
  
  // 加法结合律
  assert_eq(azimuth.add(azimuth.add(a, b), c), azimuth.add(a, azimuth.add(b, c)))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  assert_eq(azimuth.multiply(a, c), azimuth.multiply(c, a))
  assert_eq(azimuth.multiply(b, c), azimuth.multiply(c, b))
}

test "inventory_management_system" {
  // 测试库存管理系统计算
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  let new_shipment = 200
  
  // 计算一周后的库存
  let weekly_sales = azimuth.multiply(daily_sales, days)
  let stock_before_shipment = azimuth.add(initial_stock, -weekly_sales)
  let final_stock = azimuth.add(stock_before_shipment, new_shipment)
  
  assert_eq(175, weekly_sales)
  assert_eq(325, stock_before_shipment)
  assert_eq(525, final_stock)
}

test "project_resource_allocation" {
  // 测试项目资源分配计算
  let total_budget = 50000
  let team_members = 8
  let project_duration_weeks = 12
  
  // 计算每周预算
  let weekly_budget = azimuth.divide_with_ceil(total_budget, project_duration_weeks)
  
  // 计算每人每周预算
  let per_person_weekly_budget = azimuth.divide_with_ceil(weekly_budget, team_members)
  
  // 计算项目总人周
  let total_person_weeks = azimuth.multiply(team_members, project_duration_weeks)
  
  assert_eq(4167, weekly_budget)
  assert_eq(521, per_person_weekly_budget)
  assert_eq(96, total_person_weeks)
}

test "data_processing_pipeline" {
  // 测试数据处理管道计算
  let data_points = 1000
  let processing_rate_per_hour = 150
  let hours_per_day = 8
  
  // 计算处理天数
  let total_hours_needed = azimuth.divide_with_ceil(data_points, processing_rate_per_hour)
  let days_needed = azimuth.divide_with_ceil(total_hours_needed, hours_per_day)
  
  // 计算每天处理的数据点
  let daily_capacity = azimuth.multiply(processing_rate_per_hour, hours_per_day)
  
  assert_eq(7, total_hours_needed)
  assert_eq(1, days_needed)
  assert_eq(1200, daily_capacity)
}

test "energy_consumption_analysis" {
  // 测试能源消耗分析计算
  let daily_consumption_kwh = 120
  let days_in_month = 30
  let cost_per_kwh = 15
  
  // 计算月度消耗
  let monthly_consumption = azimuth.multiply(daily_consumption_kwh, days_in_month)
  
  // 计算月度成本（分为单位）
  let monthly_cost = azimuth.multiply(monthly_consumption, cost_per_kwh)
  
  // 计算年度预估成本
  let annual_cost = azimuth.multiply(monthly_cost, 12)
  
  assert_eq(3600, monthly_consumption)
  assert_eq(54000, monthly_cost)
  assert_eq(648000, annual_cost)
}
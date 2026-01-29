// 高质量 MoonBit 测试套件
// 针对 azimuth 库的核心功能进行全面测试

test "quality_financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1158, year3)  // 1000 -> 1050 -> 1103 -> 1158
}

test "quality_inventory_reorder_point" {
  // 库存重新订购点计算
  let daily_demand = 15
  let lead_time_days = 7
  let safety_stock = 20
  
  let reorder_point = add(multiply(daily_demand, lead_time_days), safety_stock)
  
  assert_eq(125, reorder_point)  // 15*7 + 20 = 105 + 20 = 125
}

test "quality_project_resource_allocation" {
  // 项目资源分配测试
  let total_budget = 50000
  let team_members = 8
  let project_duration_months = 6
  
  let budget_per_person = divide_with_ceil(total_budget, team_members)
  let monthly_budget_per_person = divide_with_ceil(budget_per_person, project_duration_months)
  
  assert_eq(6250, budget_per_person)  // ceil(50000/8) = 6250
  assert_eq(1042, monthly_budget_per_person)  // ceil(6250/6) = 1042
}

test "quality_data_transfer_calculation" {
  // 数据传输计算
  let file_size_mb = 850
  let network_speed_mbps = 50
  let overhead_percentage = 10
  
  let base_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  let overhead_time = divide_with_ceil(multiply(base_time, overhead_percentage), 100)
  let total_time = add(base_time, overhead_time)
  
  assert_eq(17, base_time)  // ceil(850/50) = 17
  assert_eq(2, overhead_time)  // ceil(17*10/100) = 2
  assert_eq(19, total_time)  // 17 + 2 = 19
}

test "quality_energy_efficiency_calculation" {
  // 能效计算测试
  let power_consumption_watts = 150
  let operating_hours_per_day = 8
  let days_per_month = 30
  let efficiency_rating = 85
  
  let daily_consumption = multiply(power_consumption_watts, operating_hours_per_day)
  let monthly_consumption = multiply(daily_consumption, days_per_month)
  let actual_efficiency = divide_with_ceil(multiply(monthly_consumption, efficiency_rating), 100)
  
  assert_eq(1200, daily_consumption)  // 150 * 8 = 1200
  assert_eq(36000, monthly_consumption)  // 1200 * 30 = 36000
  assert_eq(30600, actual_efficiency)  // ceil(36000 * 85 / 100) = 30600
}

test "quality_multilingual_greeting_system" {
  // 多语言问候系统测试
  let english_name = "Alice"
  let chinese_name = "小明"
  let spanish_name = "María"
  let japanese_name = "田中"
  
  let english_greeting = greet(english_name)
  let chinese_greeting = greet(chinese_name)
  let spanish_greeting = greet(spanish_name)
  let japanese_greeting = greet(japanese_name)
  
  assert_eq_string("Hello, Alice!", english_greeting)
  assert_eq_string("Hello, 小明!", chinese_greeting)
  assert_eq_string("Hello, María!", spanish_greeting)
  assert_eq_string("Hello, 田中!", japanese_greeting)
}

test "quality_manufacturing_production_calculation" {
  // 制造业生产计算
  let units_per_hour = 45
  let working_hours_per_day = 8
  let production_days = 22
  let defect_rate_percent = 3
  
  let daily_production = multiply(units_per_hour, working_hours_per_day)
  let total_production = multiply(daily_production, production_days)
  let expected_defects = divide_with_ceil(multiply(total_production, defect_rate_percent), 100)
  let good_units = add(total_production, -expected_defects)
  
  assert_eq(360, daily_production)  // 45 * 8 = 360
  assert_eq(7920, total_production)  // 360 * 22 = 7920
  assert_eq(238, expected_defects)  // ceil(7920 * 3 / 100) = 238
  assert_eq(7682, good_units)  // 7920 - 238 = 7682
}

test "quality_complex_shipping_calculation" {
  // 复杂运输计算
  let package_weight_kg = 12
  let distance_km = 250
  let base_rate_per_kg = 5
  let distance_rate_per_km = 2
  let fuel_surcharge_percent = 8
  
  let weight_cost = multiply(package_weight_kg, base_rate_per_kg)
  let distance_cost = multiply(distance_km, distance_rate_per_km)
  let subtotal = add(weight_cost, distance_cost)
  let fuel_surcharge = divide_with_ceil(multiply(subtotal, fuel_surcharge_percent), 100)
  let total_cost = add(subtotal, fuel_surcharge)
  
  assert_eq(60, weight_cost)  // 12 * 5 = 60
  assert_eq(500, distance_cost)  // 250 * 2 = 500
  assert_eq(560, subtotal)  // 60 + 500 = 560
  assert_eq(45, fuel_surcharge)  // ceil(560 * 8 / 100) = 45
  assert_eq(605, total_cost)  // 560 + 45 = 605
}

test "quality_educational_grading_system" {
  // 教育评分系统测试
  let exam_score = 78
  let assignment_score = 85
  let project_score = 92
  let participation_score = 88
  
  let exam_weight = 40
  let assignment_weight = 20
  let project_weight = 30
  let participation_weight = 10
  
  let weighted_exam = divide_with_ceil(multiply(exam_score, exam_weight), 100)
  let weighted_assignment = divide_with_ceil(multiply(assignment_score, assignment_weight), 100)
  let weighted_project = divide_with_ceil(multiply(project_score, project_weight), 100)
  let weighted_participation = divide_with_ceil(multiply(participation_score, participation_weight), 100)
  
  let final_grade = add(add(add(weighted_exam, weighted_assignment), weighted_project), weighted_participation)
  
  assert_eq(31, weighted_exam)  // ceil(78 * 40 / 100) = 31
  assert_eq(17, weighted_assignment)  // ceil(85 * 20 / 100) = 17
  assert_eq(28, weighted_project)  // ceil(92 * 30 / 100) = 28
  assert_eq(9, weighted_participation)  // ceil(88 * 10 / 100) = 9
  assert_eq(85, final_grade)  // 31 + 17 + 28 + 9 = 85
}

test "quality_agricultural_planning" {
  // 农业规划计算
  let field_area_hectares = 25
  let crop_yield_per_hectare = 800
  let market_price_per_ton = 150
  let labor_cost_per_hectare = 50
  let fertilizer_cost_per_hectare = 30
  
  let total_yield = multiply(field_area_hectares, crop_yield_per_hectare)
  let total_revenue = multiply(total_yield, market_price_per_ton)
  let total_labor_cost = multiply(field_area_hectares, labor_cost_per_hectare)
  let total_fertilizer_cost = multiply(field_area_hectares, fertilizer_cost_per_hectare)
  let total_cost = add(total_labor_cost, total_fertilizer_cost)
  let net_profit = add(total_revenue, -total_cost)
  
  assert_eq(20000, total_yield)  // 25 * 800 = 20000
  assert_eq(3000000, total_revenue)  // 20000 * 150 = 3000000
  assert_eq(1250, total_labor_cost)  // 25 * 50 = 1250
  assert_eq(750, total_fertilizer_cost)  // 25 * 30 = 750
  assert_eq(2000, total_cost)  // 1250 + 750 = 2000
  assert_eq(2998000, net_profit)  // 3000000 - 2000 = 2998000
}
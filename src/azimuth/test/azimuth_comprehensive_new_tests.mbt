// 新增的 MoonBit 测试用例 - 针对 azimuth 库的全面测试
// 包含实际应用场景和边界情况测试

test "monthly_budget_calculator" {
  // 月度预算计算测试
  let income = 5000
  let rent = 1500
  let utilities = 200
  let food = 600
  let transportation = 300
  let entertainment = 200
  
  // 计算总支出
  let total_expenses = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(rent, utilities), food), transportation), entertainment)
  
  // 计算剩余预算
  let remaining_budget = @azimuth.add(income, -total_expenses)
  
  @azimuth.assert_eq(2800, total_expenses)
  @azimuth.assert_eq(2200, remaining_budget)
}

test "inventory_reorder_calculation" {
  // 库存重新订购计算测试
  let current_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  
  let total_sold = @azimuth.multiply(daily_sales, days_to_simulate)
  let final_stock = @azimuth.add(current_stock, -total_sold)
  
  @azimuth.assert_eq(84, total_sold)
  @azimuth.assert_eq(66, final_stock)
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_budget = 10000
  let team_size = 8
  let equipment_cost_per_person = 500
  
  let personnel_cost = @azimuth.multiply(team_size, equipment_cost_per_person)
  let weekly_budget = @azimuth.divide_with_ceil(total_budget, 12)
  let remaining_budget = @azimuth.add(total_budget, -personnel_cost)
  
  @azimuth.assert_eq(4000, personnel_cost)
  @azimuth.assert_eq(833, weekly_budget)
  @azimuth.assert_eq(6000, remaining_budget)
}

test "data_processing_batch_calculation" {
  // 数据处理批计算测试
  let records_per_batch = 1000
  let total_records = 7532
  let processing_time_per_record = 2
  let parallel_processes = 4
  
  let total_batches = @azimuth.divide_with_ceil(total_records, records_per_batch)
  let total_processing_time = @azimuth.multiply(total_records, processing_time_per_record)
  let parallel_processing_time = @azimuth.divide_with_ceil(total_processing_time, parallel_processes)
  
  @azimuth.assert_eq(8, total_batches)
  @azimuth.assert_eq(15064, total_processing_time)
  @azimuth.assert_eq(3766, parallel_processing_time)
}

test "ecommerce_shipping_calculation" {
  // 电子商务运输计算测试
  let base_shipping_cost = 5
  let weight_rate_per_kg = 2
  let distance_rate_per_km = 1
  let package_weight = 15
  let shipping_distance = 120
  
  let weight_cost = @azimuth.multiply(package_weight, weight_rate_per_kg)
  let distance_cost = @azimuth.multiply(shipping_distance, distance_rate_per_km)
  let total_shipping_cost = @azimuth.add(@azimuth.add(base_shipping_cost, weight_cost), distance_cost)
  
  @azimuth.assert_eq(30, weight_cost)
  @azimuth.assert_eq(120, distance_cost)
  @azimuth.assert_eq(155, total_shipping_cost)
}

test "educational_grading_system" {
  // 教育评分系统测试
  let exam_score = 85
  let assignment_score = 75
  let project_score = 90
  let participation_score = 80
  
  let weighted_exam = @azimuth.multiply(exam_score, 40)
  let weighted_assignment = @azimuth.multiply(assignment_score, 25)
  let weighted_project = @azimuth.multiply(project_score, 25)
  let weighted_participation = @azimuth.multiply(participation_score, 10)
  
  let total_weighted = @azimuth.add(@azimuth.add(@azimuth.add(weighted_exam, weighted_assignment), weighted_project), weighted_participation)
  let final_grade = @azimuth.divide_with_ceil(total_weighted, 100)
  
  @azimuth.assert_eq(3400, weighted_exam)
  @azimuth.assert_eq(1875, weighted_assignment)
  @azimuth.assert_eq(2250, weighted_project)
  @azimuth.assert_eq(800, weighted_participation)
  @azimuth.assert_eq(8325, total_weighted)
  @azimuth.assert_eq(83, final_grade)
}

test "construction_material_estimation" {
  // 建筑材料估算测试
  let room_length = 12
  let room_width = 8
  let room_height = 3
  let tile_area = 2
  let paint_coverage_per_liter = 10
  
  let floor_area = @azimuth.multiply(room_length, room_width)
  let wall_area = @azimuth.multiply(2, @azimuth.multiply(@azimuth.add(room_length, room_width), room_height))
  let total_surface_area = @azimuth.add(floor_area, wall_area)
  
  let tiles_needed = @azimuth.divide_with_ceil(total_surface_area, tile_area)
  let paint_needed = @azimuth.divide_with_ceil(wall_area, paint_coverage_per_liter)
  
  @azimuth.assert_eq(96, floor_area)
  @azimuth.assert_eq(120, wall_area)
  @azimuth.assert_eq(216, total_surface_area)
  @azimuth.assert_eq(108, tiles_needed)
  @azimuth.assert_eq(12, paint_needed)
}

test "greeting_multilingual_support" {
  // 多语言问候支持测试
  let english_name = "John"
  let chinese_name = "张三"
  let spanish_name = "José"
  let emoji_name = "🌟"
  
  let english_greeting = @azimuth.greet(english_name)
  let chinese_greeting = @azimuth.greet(chinese_name)
  let spanish_greeting = @azimuth.greet(spanish_name)
  let emoji_greeting = @azimuth.greet(emoji_name)
  
  @azimuth.assert_eq_string("Hello, John!", english_greeting)
  @azimuth.assert_eq_string("Hello, 张三!", chinese_greeting)
  @azimuth.assert_eq_string("Hello, José!", spanish_greeting)
  @azimuth.assert_eq_string("Hello, 🌟!", emoji_greeting)
}
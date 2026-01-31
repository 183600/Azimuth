// 用户专注的测试用例 - 针对 azimuth 库的实用测试
// 包含日常应用场景和边界情况测试

test "shopping_cart_calculation" {
  // 购物车计算场景
  let apple_price = 5
  let apple_qty = 3
  let banana_price = 3
  let banana_qty = 4
  let orange_price = 7
  let orange_qty = 2
  
  let apple_total = multiply(apple_price, apple_qty)
  let banana_total = multiply(banana_price, banana_qty)
  let orange_total = multiply(orange_price, orange_qty)
  
  let cart_total = add(add(apple_total, banana_total), orange_total)
  
  assert_eq(15, apple_total)    // 5 * 3 = 15
  assert_eq(12, banana_total)   // 3 * 4 = 12
  assert_eq(14, orange_total)   // 7 * 2 = 14
  assert_eq(41, cart_total)     // 15 + 12 + 14 = 41
}

test "classroom_seating_arrangement" {
  // 教室座位安排计算
  let total_students = 47
  let seats_per_row = 6
  let rows_needed = divide_with_ceil(total_students, seats_per_row)
  
  assert_eq(8, rows_needed)  // ceil(47/6) = 8
}

test "weekly_work_hours_calculation" {
  // 每周工作时间计算
  let daily_hours = 8
  let work_days = 5
  let overtime_hours = 3
  
  let regular_weekly_hours = multiply(daily_hours, work_days)
  let total_weekly_hours = add(regular_weekly_hours, overtime_hours)
  
  assert_eq(40, regular_weekly_hours)  // 8 * 5 = 40
  assert_eq(43, total_weekly_hours)    // 40 + 3 = 43
}

test "batch_cooking_ingredients" {
  // 批量烹饪食材计算
  let flour_per_batch = 250
  let sugar_per_batch = 150
  let batches_to_make = 4
  
  let total_flour = multiply(flour_per_batch, batches_to_make)
  let total_sugar = multiply(sugar_per_batch, batches_to_make)
  let total_ingredients = add(total_flour, total_sugar)
  
  assert_eq(1000, total_flour)      // 250 * 4 = 1000
  assert_eq(600, total_sugar)       // 150 * 4 = 600
  assert_eq(1600, total_ingredients) // 1000 + 600 = 1600
}

test "international_greeting_scenarios" {
  // 国际化问候场景
  assert_eq_string("Hello, 张伟!", greet("张伟"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, ヨシダ!", greet("ヨシダ"))
  assert_eq_string("Hello, محمد!", greet("محمد"))
  assert_eq_string("Hello, João!", greet("João"))
}

test "construction_material_estimation" {
  // 建筑材料估算
  let wall_length = 12
  let wall_height = 3
  let bricks_per_square_meter = 50
  
  let wall_area = multiply(wall_length, wall_height)
  let total_bricks_needed = multiply(wall_area, bricks_per_square_meter)
  
  assert_eq(36, wall_area)           // 12 * 3 = 36
  assert_eq(1800, total_bricks_needed) // 36 * 50 = 1800
}

test "event_planning_seating" {
  // 活动规划座位安排
  let expected_guests = 85
  let tables_available = 12
  let guests_per_table_needed = divide_with_ceil(expected_guests, tables_available)
  
  assert_eq(8, guests_per_table_needed)  // ceil(85/12) = 8
}

test "fitness_progress_tracking" {
  // 健身进度跟踪
  let pushups_day1 = 15
  let pushups_day2 = 18
  let pushups_day3 = 22
  let pushups_day4 = 20
  let pushups_day5 = 25
  
  let weekly_total = add(add(add(add(pushups_day1, pushups_day2), pushups_day3), pushups_day4), pushups_day5)
  let daily_average = divide_with_ceil(weekly_total, 5)
  
  assert_eq(100, weekly_total)   // 15 + 18 + 22 + 20 + 25 = 100
  assert_eq(20, daily_average)   // ceil(100/5) = 20
}

test "digital_storage_planning" {
  // 数字存储规划
  let photo_size_mb = 4
  let video_size_mb = 120
  let document_size_mb = 2
  
  let photos_count = 50
  let videos_count = 5
  let documents_count = 100
  
  let photos_total = multiply(photo_size_mb, photos_count)
  let videos_total = multiply(video_size_mb, videos_count)
  let documents_total = multiply(document_size_mb, documents_count)
  
  let total_storage_needed = add(add(photos_total, videos_total), documents_total)
  
  assert_eq(200, photos_total)     // 4 * 50 = 200
  assert_eq(600, videos_total)     // 120 * 5 = 600
  assert_eq(200, documents_total)  // 2 * 100 = 200
  assert_eq(1000, total_storage_needed) // 200 + 600 + 200 = 1000
}

test "budget_allocation_distribution" {
  // 预算分配分布
  let total_budget = 5000
  let marketing_percentage = 30
  let development_percentage = 40
  let operations_percentage = 20
  let contingency_percentage = 10
  
  let marketing_budget = divide_with_ceil(multiply(total_budget, marketing_percentage), 100)
  let development_budget = divide_with_ceil(multiply(total_budget, development_percentage), 100)
  let operations_budget = divide_with_ceil(multiply(total_budget, operations_percentage), 100)
  let contingency_budget = divide_with_ceil(multiply(total_budget, contingency_percentage), 100)
  
  let allocated_total = add(add(add(marketing_budget, development_budget), operations_budget), contingency_budget)
  
  assert_eq(1500, marketing_budget)    // ceil(5000 * 30 / 100) = 1500
  assert_eq(2000, development_budget)  // ceil(5000 * 40 / 100) = 2000
  assert_eq(1000, operations_budget)   // ceil(5000 * 20 / 100) = 1000
  assert_eq(500, contingency_budget)   // ceil(5000 * 10 / 100) = 500
  assert_eq(5000, allocated_total)     // 1500 + 2000 + 1000 + 500 = 5000
}
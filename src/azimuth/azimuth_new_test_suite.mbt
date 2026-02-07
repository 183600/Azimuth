// Azimuth 项目新增 MoonBit 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法
// 测试各种实际应用场景和函数组合使用

test "shopping_cart_calculation" {
  // 购物车计算测试 - 实际商业应用场景
  let item1_price = 29
  let item1_quantity = 3
  let item2_price = 45
  let item2_quantity = 2
  let discount = 10
  
  // 计算商品小计
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算总价
  let total_before_discount = @azimuth.add(subtotal1, subtotal2)
  
  // 应用折扣
  let final_total = @azimuth.subtract(total_before_discount, discount)
  
  // 计算平均每件商品价格
  let total_items = @azimuth.add(item1_quantity, item2_quantity)
  let average_price = @azimuth.divide_with_ceil(final_total, total_items)
  
  @azimuth.assert_eq(87, subtotal1)
  @azimuth.assert_eq(90, subtotal2)
  @azimuth.assert_eq(177, total_before_discount)
  @azimuth.assert_eq(167, final_total)
  @azimuth.assert_eq(5, total_items)
  @azimuth.assert_eq(34, average_price)
}

test "time_conversion_calculations" {
  // 时间转换计算测试 - 实用时间处理场景
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let hours_to_seconds = @azimuth.multiply(hours, 3600)
  let minutes_to_seconds = @azimuth.multiply(minutes, 60)
  let total_seconds = @azimuth.add(@azimuth.add(hours_to_seconds, minutes_to_seconds), seconds)
  
  // 计算总分钟数（向上取整）
  let total_minutes = @azimuth.divide_with_ceil(total_seconds, 60)
  
  // 计算总小时数（向上取整）
  let total_hours = @azimuth.divide_with_ceil(total_seconds, 3600)
  
  @azimuth.assert_eq(7200, hours_to_seconds)
  @azimuth.assert_eq(2700, minutes_to_seconds)
  @azimuth.assert_eq(9930, total_seconds)
  @azimuth.assert_eq(166, total_minutes)
  @azimuth.assert_eq(3, total_hours)
}

test "batch_processing_simulation" {
  // 批处理模拟测试 - 数据处理场景
  let batch_size = 25
  let total_items = 187
  let processing_time_per_batch = 5
  
  // 计算需要的批次数
  let batches_needed = @azimuth.divide_with_ceil(total_items, batch_size)
  
  // 计算总处理时间
  let total_processing_time = @azimuth.multiply(batches_needed, processing_time_per_batch)
  
  // 计算最后一个批次的项目数
  let full_batches = @azimuth.divide_with_ceil(total_items, batch_size) - 1
  let items_processed = @azimuth.multiply(full_batches, batch_size)
  let last_batch_size = @azimuth.subtract(total_items, items_processed)
  
  @azimuth.assert_eq(8, batches_needed)
  @azimuth.assert_eq(40, total_processing_time)
  @azimuth.assert_eq(7, full_batches)
  @azimuth.assert_eq(175, items_processed)
  @azimuth.assert_eq(12, last_batch_size)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试 - 项目管理场景
  let total_budget = 1000
  let team_members = 8
  let project_duration = 5
  let reserve_fund = 150
  
  // 计算可用预算
  let available_budget = @azimuth.subtract(total_budget, reserve_fund)
  
  // 计算每人每天的预算
  let total_person_days = @azimuth.multiply(team_members, project_duration)
  let budget_per_person_day = @azimuth.divide_with_ceil(available_budget, total_person_days)
  
  // 计算每个团队成员的总预算
  let budget_per_person = @azimuth.multiply(budget_per_person_day, project_duration)
  
  // 验证总分配不超过可用预算
  let total_allocated = @azimuth.multiply(budget_per_person, team_members)
  
  @azimuth.assert_eq(850, available_budget)
  @azimuth.assert_eq(40, total_person_days)
  @azimuth.assert_eq(22, budget_per_person_day)
  @azimuth.assert_eq(110, budget_per_person)
  @azimuth.assert_eq(880, total_allocated)
}

test "grade_calculation_system" {
  // 成绩计算系统测试 - 教育应用场景
  let assignment_score = 85
  let midterm_score = 78
  let final_score = 92
  let participation_score = 15
  
  // 计算加权成绩（作业30%，期中30%，期末35%，参与5%）
  let assignment_weighted = @azimuth.divide_with_ceil(@azimuth.multiply(assignment_score, 30), 100)
  let midterm_weighted = @azimuth.divide_with_ceil(@azimuth.multiply(midterm_score, 30), 100)
  let final_weighted = @azimuth.divide_with_ceil(@azimuth.multiply(final_score, 35), 100)
  let participation_weighted = @azimuth.divide_with_ceil(@azimuth.multiply(participation_score, 5), 100)
  
  // 计算总成绩
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(assignment_weighted, midterm_weighted), final_weighted), participation_weighted)
  
  // 确定字母等级（简化版）
  let grade_level = @azimuth.divide_with_ceil(total_score, 10)
  
  @azimuth.assert_eq(26, assignment_weighted)
  @azimuth.assert_eq(24, midterm_weighted)
  @azimuth.assert_eq(33, final_weighted)
  @azimuth.assert_eq(1, participation_weighted)
  @azimuth.assert_eq(84, total_score)
  @azimuth.assert_eq(9, grade_level)
}

test "fitness_tracking_calculations" {
  // 健身追踪计算测试 - 健康应用场景
  let daily_steps_goal = 10000
  let actual_steps_monday = 8500
  let actual_steps_tuesday = 12000
  let actual_steps_wednesday = 7800
  let actual_steps_thursday = 11000
  let actual_steps_friday = 9200
  
  // 计算总步数
  let total_steps = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(actual_steps_monday, actual_steps_tuesday), actual_steps_wednesday), actual_steps_thursday), actual_steps_friday)
  
  // 计算平均每日步数
  let days_tracked = 5
  let average_steps = @azimuth.divide_with_ceil(total_steps, days_tracked)
  
  // 计算总目标步数
  let total_goal = @azimuth.multiply(daily_steps_goal, days_tracked)
  
  // 计算完成率
  let completion_percentage = @azimuth.divide_with_ceil(@azimuth.multiply(total_steps, 100), total_goal)
  
  // 计算需要额外步数来达到目标
  let steps_needed = @azimuth.subtract(total_goal, total_steps)
  
  @azimuth.assert_eq(48500, total_steps)
  @azimuth.assert_eq(9700, average_steps)
  @azimuth.assert_eq(50000, total_goal)
  @azimuth.assert_eq(97, completion_percentage)
  @azimuth.assert_eq(1500, steps_needed)
}

test "cooking_recipe_scaler" {
  // 烹饪食谱缩放测试 - 生活应用场景
  let original_servings = 4
  let desired_servings = 7
  let flour_per_serving = 150
  let sugar_per_serving = 50
  let eggs_per_serving = 1
  
  // 计算缩放因子
  let scale_factor = @azimuth.divide_with_ceil(@azimuth.multiply(desired_servings, 100), @azimuth.multiply(original_servings, 100))
  
  // 计算新配料的量
  let new_flour = @azimuth.divide_with_ceil(@azimuth.multiply(flour_per_serving, desired_servings), 1)
  let new_sugar = @azimuth.divide_with_ceil(@azimuth.multiply(sugar_per_serving, desired_servings), 1)
  let new_eggs = @azimuth.divide_with_ceil(@azimuth.multiply(eggs_per_serving, desired_servings), 1)
  
  // 计算总原料量
  let total_dry_ingredients = @azimuth.add(new_flour, new_sugar)
  
  @azimuth.assert_eq(7, scale_factor)
  @azimuth.assert_eq(1050, new_flour)
  @azimuth.assert_eq(350, new_sugar)
  @azimuth.assert_eq(7, new_eggs)
  @azimuth.assert_eq(1400, total_dry_ingredients)
}

test "travel_itinerary_planner" {
  // 旅行行程规划测试 - 旅游应用场景
  let total_budget = 2000
  let accommodation_per_night = 120
  let nights = 5
  let daily_food_budget = 60
  let activities_total = 350
  let emergency_fund = 200
  
  // 计算住宿总费用
  let total_accommodation = @azimuth.multiply(accommodation_per_night, nights)
  
  // 计算食物总费用
  let total_food = @azimuth.multiply(daily_food_budget, nights)
  
  // 计算基本费用
  let basic_expenses = @azimuth.add(@azimuth.add(total_accommodation, total_food), activities_total)
  
  // 计算可用额外预算
  let available_extra = @azimuth.subtract(@azimuth.subtract(total_budget, basic_expenses), emergency_fund)
  
  // 计算每日可用额外花费
  let daily_extra = @azimuth.divide_with_ceil(available_extra, nights)
  
  // 计算总实际花费
  let actual_total = @azimuth.add(basic_expenses, emergency_fund)
  
  @azimuth.assert_eq(600, total_accommodation)
  @azimuth.assert_eq(300, total_food)
  @azimuth.assert_eq(1250, basic_expenses)
  @azimuth.assert_eq(550, available_extra)
  @azimuth.assert_eq(110, daily_extra)
  @azimuth.assert_eq(1450, actual_total)
}
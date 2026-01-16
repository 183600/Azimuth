// Azimuth 项目实用测试用例
// 使用标准 MoonBit 测试语法，专注于实际应用场景

test "currency_calculations" {
  // 货币计算测试
  
  // 计算购物车总价
  let item1_price = 299
  let item2_price = 199
  let item3_price = 99
  let subtotal = @azimuth.add(@azimuth.add(item1_price, item2_price), item3_price)
  
  // 计算税费 (8%)
  let tax_rate = 8
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  let total = @azimuth.add(subtotal, tax)
  
  @azimuth.assert_eq(597, subtotal)
  @azimuth.assert_eq(47, tax)
  @azimuth.assert_eq(644, total)
}

test "physics_motion_calculations" {
  // 物理运动计算测试
  
  // 计算距离: 距离 = 速度 × 时间
  let velocity = 60  // km/h
  let time = 2.5     // hours
  let distance = @azimuth.multiply(velocity, 25) / 10
  @azimuth.assert_eq(150, distance)
  
  // 计算动能: 动能 = 0.5 × 质量 × 速度²
  let mass = 1000    // kg
  let speed = 20     // m/s
  let kinetic_energy = @azimuth.multiply(@azimuth.multiply(500, mass), @azimuth.multiply(speed, speed)) / 10000
  @azimuth.assert_eq(200000, kinetic_energy)
}

test "statistics_basic" {
  // 基础统计计算测试
  
  // 计算平均值
  let numbers = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(85, 90), 78), 92), 88)
  let count = 5
  let average = numbers / count
  @azimuth.assert_eq(433, numbers)
  @azimuth.assert_eq(86, average)
  
  // 计算范围 (最大值 - 最小值)
  let max_val = 92
  let min_val = 78
  let range = @azimuth.add(max_val, -min_val)
  @azimuth.assert_eq(14, range)
}

test "cooking_measurements" {
  // 烹饪计量测试
  
  // 配方调整：将4人份调整为6人份
  let flour_4_servings = 200  // 克
  let sugar_4_servings = 100  // 克
  let eggs_4_servings = 2     // 个
  
  let scaling_factor = 6  // 目标份数
  let original_servings = 4  // 原始份数
  let multiplier = @azimuth.multiply(scaling_factor, 10) / original_servings / 10
  
  let flour_6_servings = @azimuth.multiply(flour_4_servings, multiplier)
  let sugar_6_servings = @azimuth.multiply(sugar_4_servings, multiplier)
  let eggs_6_servings = @azimuth.multiply(eggs_4_servings, multiplier)
  
  @azimuth.assert_eq(300, flour_6_servings)
  @azimuth.assert_eq(150, sugar_6_servings)
  @azimuth.assert_eq(3, eggs_6_servings)
}

test "fitness_tracking" {
  // 健身追踪测试
  
  // 计算每周消耗卡路里
  let monday_calories = 450
  let wednesday_calories = 380
  let friday_calories = 520
  let saturday_calories = 600
  
  let weekly_total = @azimuth.add(@azimuth.add(@azimuth.add(monday_calories, wednesday_calories), friday_calories), saturday_calories)
  let daily_average = weekly_total / 4
  
  @azimuth.assert_eq(1950, weekly_total)
  @azimuth.assert_eq(487, daily_average)
  
  // 计算月度目标 (每周目标 × 4)
  let weekly_goal = 2000
  let monthly_goal = @azimuth.multiply(weekly_goal, 4)
  @azimuth.assert_eq(8000, monthly_goal)
}

test "gaming_experience_system" {
  // 游戏经验系统测试
  
  // 计算任务经验值
  let quest_base_xp = 100
  let difficulty_multiplier = 2
  let completion_bonus = 50
  let quest_xp = @azimuth.add(@azimuth.multiply(quest_base_xp, difficulty_multiplier), completion_bonus)
  
  // 计算等级提升所需总经验
  let current_level = 5
  let xp_per_level = 1000
  let total_xp_needed = @azimuth.multiply(current_level, xp_per_level)
  
  @azimuth.assert_eq(250, quest_xp)
  @azimuth.assert_eq(5000, total_xp_needed)
}

test "digital_storage_calculations" {
  // 数字存储计算测试
  
  // 计算文件总大小
  let file1_size = 1024    // KB
  let file2_size = 2048    // KB
  let file3_size = 512     // KB
  let total_size_kb = @azimuth.add(@azimuth.add(file1_size, file2_size), file3_size)
  
  // 转换为 MB
  let total_size_mb = total_size_kb / 1024
  
  @azimuth.assert_eq(3584, total_size_kb)
  @azimuth.assert_eq(3, total_size_mb)
  
  // 计算传输时间 (假设 1 MB/s)
  let transfer_time_seconds = total_size_mb
  let transfer_time_minutes = transfer_time_seconds / 60
  @azimuth.assert_eq(0, transfer_time_minutes)
}

test "project_management" {
  // 项目管理测试
  
  // 计算项目工期
  let planning_days = 5
  let development_days = 15
  let testing_days = 8
  let deployment_days = 2
  
  let total_project_days = @azimuth.add(@azimuth.add(@azimuth.add(planning_days, development_days), testing_days), deployment_days)
  let working_weeks = total_project_days / 5
  
  @azimuth.assert_eq(30, total_project_days)
  @azimuth.assert_eq(6, working_weeks)
  
  // 计算项目成本
  let daily_cost = 500
  let total_cost = @azimuth.multiply(total_project_days, daily_cost)
  @azimuth.assert_eq(15000, total_cost)
}

test "energy_consumption" {
  // 能源消耗测试
  
  // 计算家电月度耗电量
  let refrigerator_watts = 150
  let tv_watts = 200
  let computer_watts = 300
  
  let daily_hours_refrigerator = 24
  let daily_hours_tv = 4
  let daily_hours_computer = 8
  
  let daily_wh_refrigerator = @azimuth.multiply(refrigerator_watts, daily_hours_refrigerator)
  let daily_wh_tv = @azimuth.multiply(tv_watts, daily_hours_tv)
  let daily_wh_computer = @azimuth.multiply(computer_watts, daily_hours_computer)
  
  let daily_total_wh = @azimuth.add(@azimuth.add(daily_wh_refrigerator, daily_wh_tv), daily_wh_computer)
  let monthly_total_kwh = @azimuth.multiply(daily_total_wh, 30) / 1000
  
  @azimuth.assert_eq(5300, daily_total_wh)
  @azimuth.assert_eq(159, monthly_total_kwh)
}

test "travel_planning" {
  // 旅行规划测试
  
  // 计算旅行预算
  let flight_cost = 2500
  let accommodation_per_night = 300
  let daily_food_budget = 150
  let activities_budget = 800
  
  let trip_days = 7
  let accommodation_total = @azimuth.multiply(accommodation_per_night, trip_days)
  let food_total = @azimuth.multiply(daily_food_budget, trip_days)
  
  let total_budget = @azimuth.add(@azimuth.add(@azimuth.add(flight_cost, accommodation_total), food_total), activities_budget)
  
  @azimuth.assert_eq(2100, accommodation_total)
  @azimuth.assert_eq(1050, food_total)
  @azimuth.assert_eq(6450, total_budget)
  
  // 计算每人平均费用 (假设2人)
  let travelers = 2
  let cost_per_person = total_budget / travelers
  @azimuth.assert_eq(3225, cost_per_person)
}
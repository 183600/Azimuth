// Azimuth 增强测试用例
// 使用标准 MoonBit 测试语法

// 导入 azimuth 库
use azimuth

test "enhanced_mathematical_series" {
  // 测试数学级数计算
  // 计算等差数列: 2, 5, 8, 11, 14 的和
  let first_term = 2
  let common_diff = 3
  let num_terms = 5
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let series_sum = azimuth.divide_with_ceil(
    azimuth.multiply(num_terms, azimuth.add(azimuth.multiply(2, first_term), azimuth.multiply(num_terms - 1, common_diff))),
    2
  )
  
  azimuth.assert_eq(40, series_sum)
}

test "enhanced_time_zone_conversion" {
  // 测试时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = 8  // 与UTC的时差
  
  // 计算UTC时间
  let utc_time = azimuth.add(local_time, -time_difference)
  
  // 处理负数时间（前一天）
  let adjusted_utc_time = if utc_time < 0 {
    azimuth.add(utc_time, 24)
  } else {
    utc_time
  }
  
  azimuth.assert_eq(6, adjusted_utc_time)
}

test "enhanced_recipe_scaling" {
  // 测试食谱缩放计算
  let original_servings = 4
  let desired_servings = 7
  let flour_per_serving = 150  // 克
  let sugar_per_serving = 50   // 克
  
  // 计算缩放因子
  let scaling_factor = azimuth.divide_with_ceil(azimuth.multiply(desired_servings, 100), original_servings)
  
  // 计算新配料的量
  let new_flour = azimuth.divide_with_ceil(azimuth.multiply(flour_per_serving, scaling_factor), 100)
  let new_sugar = azimuth.divide_with_ceil(azimuth.multiply(sugar_per_serving, scaling_factor), 100)
  
  azimuth.assert_eq(263, new_flour)
  azimuth.assert_eq(88, new_sugar)
}

test "enhanced_fitness_tracking" {
  // 测试健身追踪计算
  let daily_step_goal = 10000
  let current_steps = 7500
  let calories_per_step = 0.04  // 假设每步消耗0.04卡路里
  
  // 计算剩余步数
  let remaining_steps = azimuth.add(daily_step_goal, -current_steps)
  
  // 计算已消耗卡路里（使用整数运算）
  let calories_burned = azimuth.divide_with_ceil(azimuth.multiply(current_steps, 4), 100)
  
  azimuth.assert_eq(2500, remaining_steps)
  azimuth.assert_eq(300, calories_burned)
}

test "enhanced_digital_storage" {
  // 测试数字存储计算
  let file_size_mb = 850
  let drive_capacity_gb = 4
  let drive_capacity_mb = azimuth.multiply(drive_capacity_gb, 1024)
  
  // 计算可以存储多少个这样的文件
  let files_count = azimuth.divide_with_ceil(drive_capacity_mb, file_size_mb)
  
  // 计算剩余空间
  let used_space = azimuth.multiply(files_count, file_size_mb)
  let remaining_space = azimuth.add(drive_capacity_mb, -used_space)
  
  azimuth.assert_eq(5, files_count)
  azimuth.assert_eq(174, remaining_space)
}

test "enhanced_project_management" {
  // 测试项目管理计算
  let total_tasks = 47
  let completed_tasks = 23
  let team_members = 5
  let days_remaining = 10
  
  // 计算剩余任务
  let remaining_tasks = azimuth.add(total_tasks, -completed_tasks)
  
  // 计算每人每天需要完成的任务数
  let tasks_per_person_per_day = azimuth.divide_with_ceil(remaining_tasks, azimuth.multiply(team_members, days_remaining))
  
  azimuth.assert_eq(24, remaining_tasks)
  azimuth.assert_eq(1, tasks_per_person_per_day)
}

test "enhanced_travel_planning" {
  // 测试旅行规划计算
  let distance_km = 450
  let average_speed_kmh = 80
  let fuel_efficiency_km_per_l = 15
  let fuel_price_per_liter = 150  // 分
  
  // 计算旅行时间（小时）
  let travel_time = azimuth.divide_with_ceil(distance_km, average_speed_kmh)
  
  // 计算所需燃料（升）
  let fuel_needed = azimuth.divide_with_ceil(distance_km, fuel_efficiency_km_per_l)
  
  // 计算燃料成本（分）
  let fuel_cost = azimuth.multiply(fuel_needed, fuel_price_per_liter)
  
  azimuth.assert_eq(6, travel_time)
  azimuth.assert_eq(30, fuel_needed)
  azimuth.assert_eq(4500, fuel_cost)
}

test "enhanced_temperature_conversion" {
  // 测试温度转换计算
  let celsius = 25
  
  // 摄氏度转华氏度: F = C * 9/5 + 32
  let fahrenheit = azimuth.add(azimuth.divide_with_ceil(azimuth.multiply(celsius, 9), 5), 32)
  
  // 华氏度转摄氏度: C = (F - 32) * 5/9
  let celsius_back = azimuth.divide_with_ceil(azimuth.multiply(fahrenheit - 32, 5), 9)
  
  azimuth.assert_eq(77, fahrenheit)
  azimuth.assert_eq(25, celsius_back)
}

test "enhanced_communication_cost" {
  // 测试通信成本计算
  let local_calls_minutes = 120
  let long_distance_minutes = 45
  let local_rate_per_minute = 10  // 分
  let long_distance_rate_per_minute = 25  // 分
  let monthly_base_fee = 2000  // 分
  
  // 计算本地通话费用
  let local_cost = azimuth.multiply(local_calls_minutes, local_rate_per_minute)
  
  // 计算长途通话费用
  let long_distance_cost = azimuth.multiply(long_distance_minutes, long_distance_rate_per_minute)
  
  // 计算总费用
  let total_cost = azimuth.add(azimuth.add(local_cost, long_distance_cost), monthly_base_fee)
  
  azimuth.assert_eq(1200, local_cost)
  azimuth.assert_eq(1125, long_distance_cost)
  azimuth.assert_eq(4325, total_cost)
}

test "enhanced_greeting_multilingual" {
  // 测试多语言问候功能
  let english_name = "Alice"
  let chinese_name = "小明"
  let japanese_name = "田中"
  
  // 测试不同语言的问候
  let english_greeting = azimuth.greet(english_name)
  let chinese_greeting = azimuth.greet(chinese_name)
  let japanese_greeting = azimuth.greet(japanese_name)
  
  azimuth.assert_eq_string("Hello, Alice!", english_greeting)
  azimuth.assert_eq_string("Hello, 小明!", chinese_greeting)
  azimuth.assert_eq_string("Hello, 田中!", japanese_greeting)
}
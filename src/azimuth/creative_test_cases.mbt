// 创意测试用例 - 针对 azimuth 库的高级测试场景
// 使用标准的 MoonBit 测试语法

test "geometric_calculations" {
  // 几何计算：计算矩形面积和周长
  let length = 8
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(40, area)      // 8 * 5 = 40
  assert_eq(26, perimeter) // 2 * (8 + 5) = 26
}

test "temperature_conversion" {
  // 温度转换模拟：摄氏度到华氏度
  let celsius = 25
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 1), 32)
  
  assert_eq(77, fahrenheit) // (25 * 9/5) + 32 = 45 + 32 = 77
}

test "loan_interest_calculation" {
  // 贷款利息计算
  let principal = 10000
  let annual_rate = 5
  let years = 3
  
  let yearly_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let total_interest = multiply(yearly_interest, years)
  let total_amount = add(principal, total_interest)
  
  assert_eq(500, yearly_interest)  // 10000 * 5 / 100 = 500
  assert_eq(1500, total_interest)  // 500 * 3 = 1500
  assert_eq(11500, total_amount)   // 10000 + 1500 = 11500
}

test "batch_processing_simulation" {
  // 批处理模拟：计算处理批次
  let total_items = 157
  let batch_size = 20
  
  let full_batches = divide_with_ceil(total_items, batch_size)
  let last_batch_size = total_items - multiply(full_batches - 1, batch_size)
  
  assert_eq(8, full_batches)      // ceil(157/20) = 8
  assert_eq(17, last_batch_size)  // 157 - 7*20 = 17
}

test "team_collaboration_metrics" {
  // 团队协作指标计算
  let team_members = 6
  let tasks_per_member = 8
  let collaboration_overhead = 15  // 15% 额外工作量
  
  let individual_tasks = multiply(team_members, tasks_per_member)
  let overhead_tasks = divide_with_ceil(multiply(individual_tasks, collaboration_overhead), 100)
  let total_tasks = add(individual_tasks, overhead_tasks)
  
  assert_eq(48, individual_tasks)  // 6 * 8 = 48
  assert_eq(8, overhead_tasks)    // ceil(48 * 15 / 100) = 8
  assert_eq(56, total_tasks)       // 48 + 8 = 56
}

test "digital_storage_calculation" {
  // 数字存储计算
  let file_size_mb = 850
  let drive_capacity_gb = 4
  let drive_capacity_mb = multiply(drive_capacity_gb, 1024)
  
  let files_that_fit = divide_with_ceil(drive_capacity_mb, file_size_mb)
  let remaining_space = add(drive_capacity_mb, -multiply(files_that_fit - 1, file_size_mb))
  
  assert_eq(4096, drive_capacity_mb)  // 4 * 1024 = 4096
  assert_eq(5, files_that_fit)        // ceil(4096/850) = 5
  assert_eq(746, remaining_space)     // 4096 - 4*850 = 746
}

test "time_zone_conversion" {
  // 时区转换模拟
  let local_time = 14  // 下午2点
  let time_difference = -8  // 时差
  
  let remote_time_raw = add(local_time, time_difference)
  let remote_time = if remote_time_raw < 0 {
    add(remote_time_raw, 24)
  } else if remote_time_raw > 23 {
    add(remote_time_raw, -24)
  } else {
    remote_time_raw
  }
  
  assert_eq(6, remote_time)  // 14 + (-8) = 6 (早上6点)
}

test "recipe_scaling" {
  // 食谱缩放：调整配料比例
  let original_servings = 4
  let desired_servings = 7
  let flour_amount = 200
  
  let scaling_factor = divide_with_ceil(multiply(desired_servings, 100), original_servings)
  let new_flour_amount = divide_with_ceil(multiply(flour_amount, scaling_factor), 100)
  
  assert_eq(175, scaling_factor)     // ceil(7*100/4) = 175
  assert_eq(350, new_flour_amount)   // ceil(200*175/100) = 350
}

test "fitness_tracking" {
  // 健身追踪：计算卡路里消耗
  let calories_per_minute = 8
  let workout_duration_minutes = 45
  let weekly_workouts = 3
  
  let calories_per_workout = multiply(calories_per_minute, workout_duration_minutes)
  let weekly_calories = multiply(calories_per_workout, weekly_workouts)
  let monthly_calories = multiply(weekly_calories, 4)
  
  assert_eq(360, calories_per_workout)  // 8 * 45 = 360
  assert_eq(1080, weekly_calories)     // 360 * 3 = 1080
  assert_eq(4320, monthly_calories)    // 1080 * 4 = 4320
}

test "greeting_personalization" {
  // 个性化问候：基于时间的问候
  let user_name = "张三"
  let base_greeting = greet(user_name)
  
  // 模拟添加时间相关的前缀
  let time_aware_greeting = "早上好, " + user_name + "!"
  
  assert_eq_string("Hello, 张三!", base_greeting)
  assert_eq_string("早上好, 张三!", time_aware_greeting)
}
// 实际应用场景测试用例
// 专注于真实世界中的应用场景和边缘情况

test "bank_account_balance_calculation" {
  // 银行账户余额计算测试
  let initial_balance = 1000
  let deposit1 = 500
  let deposit2 = 300
  let withdrawal1 = 200
  let withdrawal2 = 150
  
  // 计算最终余额
  let total_deposits = azimuth::@azimuth.add(deposit1, deposit2)
  let total_withdrawals = azimuth::@azimuth.add(withdrawal1, withdrawal2)
  let final_balance = azimuth::@azimuth.add(initial_balance, azimuth::@azimuth.add(total_deposits, -total_withdrawals))
  
  // 验证结果：1000 + (500+300) - (200+150) = 1000 + 800 - 350 = 1450
  azimuth::@azimuth.assert_eq(1450, final_balance)
}

test "pizza_order_calculation" {
  // 披萨订单计算测试
  let pizza_price = 89
  let topping_price = 15
  let delivery_fee = 25
  let discount = 20
  
  // 计算总价
  let base_total = azimuth::@azimuth.multiply(pizza_price, 2)  // 2个披萨
  let toppings_total = azimuth::@azimuth.multiply(topping_price, 3)  // 3种配料
  let subtotal = azimuth::@azimuth.add(base_total, toppings_total)
  let final_total = azimuth::@azimuth.add(azimuth::@azimuth.add(subtotal, delivery_fee), -discount)
  
  // 验证结果：(89*2) + (15*3) + 25 - 20 = 178 + 45 + 25 - 20 = 228
  azimuth::@azimuth.assert_eq(228, final_total)
}

test "fitness_tracker_calculations" {
  // 健身追踪器计算测试
  let steps_per_day = 10000
  let days = 7
  let calories_per_step = 0.04  // 简化为整数计算：4/100
  let weight_loss_per_calorie = 0.0001  // 简化为整数计算：1/10000
  
  // 计算总步数
  let total_steps = azimuth::@azimuth.multiply(steps_per_day, days)
  
  // 计算总消耗卡路里（简化为整数）
  let total_calories = azimuth::@azimuth.multiply(total_steps, 4) / 100
  
  // 计算减重（简化为整数）
  let weight_loss = total_calories / 10000
  
  // 验证结果
  azimuth::@azimuth.assert_eq(70000, total_steps)
  azimuth::@azimuth.assert_eq(2800, total_calories)
  azimuth::@azimuth.assert_eq(0, weight_loss)  // 整数除法，结果为0
}

test "classroom_grade_averaging" {
  // 教室成绩平均计算测试
  let student1_grade = 85
  let student2_grade = 92
  let student3_grade = 78
  let student4_grade = 88
  let student5_grade = 95
  
  // 计算总分和平均分
  let total_grades = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(student1_grade, student2_grade), student3_grade), student4_grade), student5_grade)
  let average_grade = total_grades / 5
  
  // 验证结果：(85+92+78+88+95)/5 = 438/5 = 87（整数除法）
  azimuth::@azimuth.assert_eq(438, total_grades)
  azimuth::@azimuth.assert_eq(87, average_grade)
}

test "construction_material_estimation" {
  // 建筑材料估算测试
  let room_length = 5
  let room_width = 4
  let room_height = 3
  let door_area = 2
  let window_area = 3
  
  // 计算墙面面积
  let wall_perimeter = azimuth::@azimuth.multiply(azimuth::@azimuth.add(room_length, room_width), 2)
  let wall_area = azimuth::@azimuth.multiply(wall_perimeter, room_height)
  
  // 计算需要涂漆的面积
  let paintable_area = azimuth::@azimuth.add(wall_area, azimuth::@azimuth.add(-door_area, -window_area))
  
  // 验证结果：((5+4)*2)*3 - 2 - 3 = 54 - 5 = 49
  azimuth::@azimuth.assert_eq(54, wall_area)
  azimuth::@azimuth.assert_eq(49, paintable_area)
}

test "digital_storage_conversion" {
  // 数字存储转换测试
  let gigabytes = 5
  let megabytes_per_gb = 1024
  let kilobytes_per_mb = 1024
  
  // 转换为千字节
  let total_megabytes = azimuth::@azimuth.multiply(gigabytes, megabytes_per_gb)
  let total_kilobytes = azimuth::@azimuth.multiply(total_megabytes, kilobytes_per_mb)
  
  // 验证结果：5 * 1024 * 1024 = 5242880
  azimuth::@azimuth.assert_eq(5120, total_megabytes)
  azimuth::@azimuth.assert_eq(5242880, total_kilobytes)
}

test "recipe_ingredient_scaling" {
  // 食谱配料缩放测试
  let original_servings = 4
  let target_servings = 6
  let flour_per_serving = 150
  let sugar_per_serving = 50
  let eggs_per_serving = 1
  
  // 计算缩放比例
  let scale_factor = azimuth::@azimuth.multiply(target_servings, 100) / azimuth::@azimuth.multiply(original_servings, 100)
  
  // 计算新配料量
  let new_flour = azimuth::@azimuth.multiply(flour_per_serving, target_servings) / original_servings
  let new_sugar = azimuth::@azimuth.multiply(sugar_per_serving, target_servings) / original_servings
  let new_eggs = azimuth::@azimuth.multiply(eggs_per_serving, target_servings) / original_servings
  
  // 验证结果
  azimuth::@azimuth.assert_eq(225, new_flour)   // 150*6/4 = 225
  azimuth::@azimuth.assert_eq(75, new_sugar)   // 50*6/4 = 75
  azimuth::@azimuth.assert_eq(1, new_eggs)     // 1*6/4 = 1（整数除法）
}

test "travel_time_estimation" {
  // 旅行时间估算测试
  let distance = 450  // 公里
  let average_speed = 90  // 公里/小时
  let break_time = 30  // 分钟
  
  // 计算驾驶时间
  let driving_hours = distance / average_speed
  let driving_minutes = azimuth::@azimuth.multiply(driving_hours, 60)
  
  // 计算总时间
  let total_minutes = azimuth::@azimuth.add(driving_minutes, break_time)
  let total_hours = total_minutes / 60
  let remaining_minutes = total_minutes % 60
  
  // 验证结果
  azimuth::@azimuth.assert_eq(5, driving_hours)
  azimuth::@azimuth.assert_eq(300, driving_minutes)
  azimuth::@azimuth.assert_eq(330, total_minutes)
  azimuth::@azimuth.assert_eq(5, total_hours)
  azimuth::@azimuth.assert_eq(30, remaining_minutes)
}

test "social_media_engagement_metrics" {
  // 社交媒体参与度指标测试
  let followers = 1000
  let likes = 150
  let comments = 25
  let shares = 10
  
  // 计算参与度指标
  let total_engagement = azimuth::@azimuth.add(azimuth::@azimuth.add(likes, comments), shares)
  let engagement_rate = azimuth::@azimuth.multiply(total_engagement, 100) / followers
  
  // 计算不同类型参与度的比例
  let like_ratio = azimuth::@azimuth.multiply(likes, 100) / total_engagement
  let comment_ratio = azimuth::@azimuth.multiply(comments, 100) / total_engagement
  let share_ratio = azimuth::@azimuth.multiply(shares, 100) / total_engagement
  
  // 验证结果
  azimuth::@azimuth.assert_eq(185, total_engagement)
  azimuth::@azimuth.assert_eq(18, engagement_rate)  // 185*100/1000 = 18.5 -> 18（整数除法）
  azimuth::@azimuth.assert_eq(81, like_ratio)      // 150*100/185 = 81.08 -> 81
  azimuth::@azimuth.assert_eq(13, comment_ratio)   // 25*100/185 = 13.51 -> 13
  azimuth::@azimuth.assert_eq(5, share_ratio)      // 10*100/185 = 5.4 -> 5
}

test "utility_bill_calculation" {
  // 公用事业账单计算测试
  let base_rate = 25  // 基础费率
  let usage_kwh = 320  // 用电量（千瓦时）
  let rate_per_kwh = 8  // 每千瓦时费用（分）
  let tax_rate = 5  // 税率（百分比）
  
  // 计算能源费用
  let energy_cost = azimuth::@azimuth.multiply(usage_kwh, rate_per_kwh)
  
  // 转换为元（从分）
  let energy_cost_yuan = energy_cost / 100
  
  // 计算小计
  let subtotal = azimuth::@azimuth.add(base_rate, energy_cost_yuan)
  
  // 计算税费
  let tax_amount = azimuth::@azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算总账单
  let total_bill = azimuth::@azimuth.add(subtotal, tax_amount)
  
  // 验证结果
  azimuth::@azimuth.assert_eq(2560, energy_cost)    // 320*8 = 2560分
  azimuth::@azimuth.assert_eq(25, energy_cost_yuan) // 2560/100 = 25元
  azimuth::@azimuth.assert_eq(50, subtotal)         // 25+25 = 50元
  azimuth::@azimuth.assert_eq(2, tax_amount)        // 50*5/100 = 2.5 -> 2元
  azimuth::@azimuth.assert_eq(52, total_bill)       // 50+2 = 52元
}
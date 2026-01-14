// 创意测试场景 - 展示实际应用中的测试用例
test "calendar_days_calculation" {
  // 测试日历天数计算
  // 计算两个日期之间的天数（简化版）
  let start_day = 15
  let start_month = 3  // 3月
  let end_day = 10
  let end_month = 4    // 4月
  
  // 假设每月30天（简化计算）
  let days_in_start_month = 30
  let days_in_between_months = @azimuth.@azimuth.multiply(end_month - start_month - 1, 30)
  
  let total_days = @azimuth.@azimuth.add(
    @azimuth.@azimuth.add(
      days_in_start_month - start_day,
      days_in_between_months
    ),
    end_day
  )
  
  @azimuth.@azimuth.assert_eq(total_days, 26)  // (30-15) + 0 + 10 = 26天
}
test "shopping_discount_calculation" {
  // 测试购物折扣计算
  let item_price = 120
  let quantity = 3
  let discount_threshold = 300  // 满300元享受折扣
  let discount_rate = 10        // 10%折扣
  
  // 计算总价
  let subtotal = @azimuth.@azimuth.multiply(item_price, quantity)
  
  // 判断是否享受折扣
  let has_discount = subtotal >= discount_threshold
  let discount_amount = if has_discount {
    @azimuth.@azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  let final_total = @azimuth.@azimuth.add(subtotal, -discount_amount)
  
  @azimuth.@azimuth.assert_eq(subtotal, 360)        // 120 * 3 = 360
  @azimuth.@azimuth.assert_eq(discount_amount, 36)  // 360 * 10 / 100 = 36
  @azimuth.@azimuth.assert_eq(final_total, 324)     // 360 - 36 = 324
}
test "battery_life_estimation" {
  // 测试电池续航时间估算
  let battery_capacity_mah = 4000  // 电池容量：4000mAh
  let device_consumption_ma = 200  // 设备功耗：200mA
  
  // 计算续航时间（小时）
  let battery_life_hours = battery_capacity_mah / device_consumption_ma
  
  @azimuth.@azimuth.assert_eq(battery_life_hours, 20)  // 4000 / 200 = 20小时
  
  // 计算在节能模式下的续航时间（功耗降低50%）
  let eco_consumption_ma = device_consumption_ma / 2
  let eco_battery_life_hours = battery_capacity_mah / eco_consumption_ma
  
  @azimuth.@azimuth.assert_eq(eco_battery_life_hours, 40)  // 4000 / 100 = 40小时
}
test "data_storage_conversion" {
  // 测试数据存储单位转换
  let file_size_mb = 15  // 15MB
  
  // 转换为KB（1MB = 1024KB，使用近似值1000）
  let file_size_kb = @azimuth.@azimuth.multiply(file_size_mb, 1000)
  
  // 转换为GB（1GB = 1024MB，使用近似值1000）
  let file_size_gb = file_size_mb / 1000
  
  @azimuth.@azimuth.assert_eq(file_size_kb, 15000)  // 15 * 1000 = 15000KB
  @azimuth.@azimuth.assert_eq(file_size_gb, 0)      // 15 / 1000 = 0（整数除法）
  
  // 计算多个文件的总大小
  let file_count = 5
  let total_size_mb = @azimuth.@azimuth.multiply(file_size_mb, file_count)
  
  @azimuth.@azimuth.assert_eq(total_size_mb, 75)  // 15 * 5 = 75MB
}
test "fitness_calorie_calculation" {
  // 测试健身卡路里计算
  let calories_per_minute_running = 10   // 跑步每分钟消耗10卡路里
  let calories_per_minute_walking = 4    // 步行每分钟消耗4卡路里
  let running_minutes = 20
  let walking_minutes = 30
  
  // 计算总消耗卡路里
  let running_calories = @azimuth.@azimuth.multiply(calories_per_minute_running, running_minutes)
  let walking_calories = @azimuth.@azimuth.multiply(calories_per_minute_walking, walking_minutes)
  let total_calories = @azimuth.@azimuth.add(running_calories, walking_calories)
  
  @azimuth.@azimuth.assert_eq(running_calories, 200)  // 10 * 20 = 200
  @azimuth.@azimuth.assert_eq(walking_calories, 120)  // 4 * 30 = 120
  @azimuth.@azimuth.assert_eq(total_calories, 320)    // 200 + 120 = 320
}
test "loan_interest_calculation" {
  // 测试贷款利息计算（简化版）
  let principal = 10000    // 本金：10000元
  let annual_rate = 6      // 年利率：6%
  let loan_years = 3       // 贷款年限：3年
  
  // 计算年利息
  let yearly_interest = @azimuth.@azimuth.multiply(principal, annual_rate) / 100
  
  // 计算总利息
  let total_interest = @azimuth.@azimuth.multiply(yearly_interest, loan_years)
  
  // 计算还款总额
  let total_repayment = @azimuth.@azimuth.add(principal, total_interest)
  
  @azimuth.@azimuth.assert_eq(yearly_interest, 600)    // 10000 * 6 / 100 = 600
  @azimuth.@azimuth.assert_eq(total_interest, 1800)    // 600 * 3 = 1800
  @azimuth.@azimuth.assert_eq(total_repayment, 11800)  // 10000 + 1800 = 11800
}
test "cooking_recipe_scaling" {
  // 测试烹饪食谱缩放
  let original_servings = 4    // 原食谱份数
  let desired_servings = 6     // 期望份数
  let flour_per_serving = 100  // 每份面粉用量（克）
  let sugar_per_serving = 50   // 每份糖用量（克）
  
  // 计算缩放比例
  let scale_ratio = desired_servings * 1000 / original_servings / 1000  // 避免浮点数
  
  // 计算新配方用量
  let flour_needed = @azimuth.@azimuth.multiply(flour_per_serving, desired_servings) / original_servings * original_servings / original_servings
  let sugar_needed = @azimuth.@azimuth.multiply(sugar_per_serving, desired_servings) / original_servings * original_servings / original_servings
  
  // 简化计算：直接按份数比例计算
  let flour_total = @azimuth.@azimuth.multiply(flour_per_serving, desired_servings)
  let sugar_total = @azimuth.@azimuth.multiply(sugar_per_serving, desired_servings)
  
  @azimuth.@azimuth.assert_eq(flour_total, 600)  // 100 * 6 = 600g
  @azimuth.@azimuth.assert_eq(sugar_total, 300)  // 50 * 6 = 300g
}
test "travel_cost_estimation" {
  // 测试旅行成本估算
  let distance_km = 500       // 距离：500公里
  let fuel_consumption = 8    // 油耗：8L/100km
  let fuel_price_per_liter = 7  // 油价：7元/L
  let accommodation_days = 3
  let accommodation_cost_per_day = 150
  
  // 计算燃油费用
  let fuel_needed = @azimuth.@azimuth.multiply(distance_km, fuel_consumption) / 100
  let fuel_cost = @azimuth.@azimuth.multiply(fuel_needed, fuel_price_per_liter)
  
  // 计算住宿费用
  let accommodation_cost = @azimuth.@azimuth.multiply(accommodation_days, accommodation_cost_per_day)
  
  // 计算总费用
  let total_cost = @azimuth.@azimuth.add(fuel_cost, accommodation_cost)
  
  @azimuth.@azimuth.assert_eq(fuel_needed, 40)      // 500 * 8 / 100 = 40L
  @azimuth.@azimuth.assert_eq(fuel_cost, 280)       // 40 * 7 = 280元
  @azimuth.@azimuth.assert_eq(accommodation_cost, 450)  // 3 * 150 = 450元
  @azimuth.@azimuth.assert_eq(total_cost, 730)      // 280 + 450 = 730元
}
test "grade_point_average_calculation" {
  // 测试平均绩点计算
  let credit1 = 3
  let grade1 = 4  // A
  let credit2 = 4
  let grade2 = 3  // B
  let credit3 = 2
  let grade3 = 4  // A
  
  // 计算总学分
  let total_credits = @azimuth.@azimuth.add(@azimuth.@azimuth.add(credit1, credit2), credit3)
  
  // 计算加权总分
  let weighted_score1 = @azimuth.@azimuth.multiply(credit1, grade1)
  let weighted_score2 = @azimuth.@azimuth.multiply(credit2, grade2)
  let weighted_score3 = @azimuth.@azimuth.multiply(credit3, grade3)
  let total_weighted_score = @azimuth.@azimuth.add(@azimuth.@azimuth.add(weighted_score1, weighted_score2), weighted_score3)
  
  // 计算GPA（使用整数除法）
  let gpa = total_weighted_score / total_credits
  
  @azimuth.@azimuth.assert_eq(total_credits, 9)        // 3 + 4 + 2 = 9
  @azimuth.@azimuth.assert_eq(total_weighted_score, 28) // 3*4 + 4*3 + 2*4 = 12 + 12 + 8 = 32
  @azimuth.@azimuth.assert_eq(gpa, 3)                  // 32 / 9 = 3（整数除法）
}
test "social_media_engagement_rate" {
  // 测试社交媒体参与度计算
  let followers = 1000     // 粉丝数
  let likes = 150          // 点赞数
  let comments = 30        // 评论数
  let shares = 20          // 分享数
  
  // 计算总互动数
  let total_engagement = @azimuth.@azimuth.add(@azimuth.@azimuth.add(likes, comments), shares)
  
  // 计算参与度（百分比）
  let engagement_rate = @azimuth.@azimuth.multiply(total_engagement, 100) / followers
  
  @azimuth.@azimuth.assert_eq(total_engagement, 200)  // 150 + 30 + 20 = 200
  @azimuth.@azimuth.assert_eq(engagement_rate, 20)    // 200 * 100 / 1000 = 20%
}
// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖更多实际应用场景

// 导入 azimuth 库中的函数
use azimuth.*

test "calculate_compound_interest" {
  // 计算复利的实际应用场景
  // 公式：A = P(1 + r/n)^(nt)
  // 简化版本：A = P(1 + r*t)
  
  let principal = 10000  // 本金 10000 元
  let rate = 5           // 年利率 5%
  let years = 3          // 投资 3 年
  
  // 简化的复利计算
  let growth_factor = add(100, multiply(rate, years))  // 100 + 5*3 = 115
  let final_amount = multiply(principal, growth_factor)  // 10000 * 115 = 1150000
  let actual_amount = final_amount / 100  // 1150000 / 100 = 11500
  
  assert_eq(11500, actual_amount)
}

test "calculate_bmi" {
  // 计算身体质量指数 (BMI) 的实际应用场景
  // 公式：BMI = 体重(kg) / (身高(m))^2
  // 简化版本：使用整数计算避免浮点数
  
  let weight = 70      // 体重 70kg
  let height_cm = 175  // 身高 175cm
  let height_m = height_cm / 100  // 转换为米：1.75m
  
  // 简化的BMI计算（避免浮点数）：BMI * 10000 = 体重 * 10000 / (身高^2)
  let bmi_times_100 = multiply(weight, 10000) / multiply(height_m, height_m)
  
  // 预期BMI约为22.86，所以BMI*100约为2286
  assert_eq(2288, bmi_times_100)
}

test "calculate_loan_payment" {
  // 计算贷款月供的实际应用场景
  // 简化的等额本息计算
  
  let loan_amount = 200000  // 贷款金额 20万元
  let annual_rate = 6       // 年利率 6%
  let loan_years = 20       // 贷款年限 20年
  let months = multiply(loan_years, 12)  // 总月数：240个月
  
  // 简化的月供计算（不考虑复利效应）
  let total_interest = multiply(loan_amount, annual_rate) * loan_years / 100
  let total_payment = add(loan_amount, total_interest)
  let monthly_payment = total_payment / months
  
  // 验证计算结果
  assert_eq(1666, monthly_payment)
}

test "calculate_speed_distance_time" {
  // 计算速度、距离、时间的物理应用场景
  
  // 场景1：计算距离
  let speed1 = 60   // 速度 60 km/h
  let time1 = 2     // 时间 2 小时
  let distance1 = multiply(speed1, time1)  // 距离 = 速度 × 时间
  assert_eq(120, distance1)
  
  // 场景2：计算平均速度
  let distance2 = 300  // 距离 300 km
  let time2 = 5        // 时间 5 小时
  let avg_speed = distance2 / time2  // 平均速度 = 距离 / 时间
  assert_eq(60, avg_speed)
  
  // 场景3：计算时间
  let distance3 = 180  // 距离 180 km
  let speed3 = 90      // 速度 90 km/h
  let time3 = distance3 / speed3  // 时间 = 距离 / 速度
  assert_eq(2, time3)
}

test "calculate_temperature_conversions" {
  // 温度转换的实际应用场景
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化版本：F = C × 2 + 30（近似值）
  let celsius1 = 25
  let fahrenheit1 = add(multiply(celsius1, 2), 30)  // 25 × 2 + 30 = 80
  assert_eq(80, fahrenheit1)
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  // 简化版本：C = (F - 30) / 2（近似值）
  let fahrenheit2 = 80
  let celsius2 = add(fahrenheit2, -30) / 2  // (80 - 30) / 2 = 25
  assert_eq(25, celsius2)
  
  // 开尔文转摄氏度：K = C + 273
  let celsius3 = 20
  let kelvin1 = add(celsius3, 273)  // 20 + 273 = 293
  assert_eq(293, kelvin1)
  
  // 摄氏度转开尔文
  let kelvin2 = 300
  let celsius4 = add(kelvin2, -273)  // 300 - 273 = 27
  assert_eq(27, celsius4)
}

test "calculate_discounted_price" {
  // 计算折扣价格的商业应用场景
  
  let original_price = 500  // 原价 500 元
  let discount_rate = 20    // 折扣率 20%
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_rate) / 100  // 500 × 20 / 100 = 100
  assert_eq(100, discount_amount)
  
  // 计算折后价格
  let discounted_price = add(original_price, -discount_amount)  // 500 - 100 = 400
  assert_eq(400, discounted_price)
  
  // 计算会员价（额外95折）
  let member_discount = 5
  let member_discount_amount = multiply(discounted_price, member_discount) / 100  // 400 × 5 / 100 = 20
  let member_price = add(discounted_price, -member_discount_amount)  // 400 - 20 = 380
  assert_eq(380, member_price)
}

test "calculate_calorie_intake" {
  // 计算卡路里摄入量的健康应用场景
  
  // 基础代谢率计算（简化版）
  let weight = 70    // 体重 70kg
  let height = 175   // 身高 175cm
  let age = 30       // 年龄 30岁
  
  // 男性基础代谢率简化公式：BMR = 10 × 体重 + 6.25 × 身高 - 5 × 年龄 + 5
  // 简化版本：BMR = 10 × 体重 + 6 × 身高 / 10 - 5 × 年龄 + 5
  let bmr_part1 = multiply(weight, 10)  // 10 × 70 = 700
  let bmr_part2 = multiply(height, 6) / 10  // 6 × 175 / 10 = 105
  let bmr_part3 = multiply(age, 5)  // 5 × 30 = 150
  let bmr = add(add(add(bmr_part1, bmr_part2), -bmr_part3), 5)  // 700 + 105 - 150 + 5 = 660
  assert_eq(660, bmr)
  
  // 每日总消耗（考虑活动水平）
  let activity_factor = 150  // 活动因子 1.5，表示为150
  let daily_calories = multiply(bmr, activity_factor) / 100  // 660 × 150 / 100 = 990
  assert_eq(990, daily_calories)
}

test "calculate_data_storage" {
  // 计算数据存储容量的技术应用场景
  
  // 计算文件总大小
  let file_count = 1000      // 文件数量
  let avg_file_size = 2      // 平均文件大小 2MB
  let total_size_mb = multiply(file_count, avg_file_size)  // 1000 × 2 = 2000MB
  assert_eq(2000, total_size_mb)
  
  // 转换为GB（1GB = 1024MB，简化为1000MB）
  let total_size_gb = total_size_mb / 1000  // 2000 / 1000 = 2GB
  assert_eq(2, total_size_gb)
  
  // 考虑冗余备份（3倍备份）
  let redundancy_factor = 3
  let storage_with_backup = multiply(total_size_mb, redundancy_factor)  // 2000 × 3 = 6000MB
  assert_eq(6000, storage_with_backup)
  
  // 转换为TB（1TB = 1000GB，简化计算）
  let storage_tb = storage_with_backup / 1000000  // 6000MB / 1000000 = 0.006TB
  assert_eq(0, storage_tb)  // 整数除法，结果为0
}

test "calculate_project_timeline" {
  // 计算项目时间线的管理应用场景
  
  // 项目各阶段持续时间（天）
  let planning_days = 10
  let design_days = 15
  let development_days = 45
  let testing_days = 20
  let deployment_days = 5
  
  // 计算总项目时间
  let total_days = add(add(add(add(planning_days, design_days), development_days), testing_days), deployment_days)
  assert_eq(95, total_days)
  
  // 转换为工作周（每周5个工作日）
  let total_weeks = total_days / 5
  assert_eq(19, total_weeks)
  
  // 考虑缓冲时间（20%缓冲）
  let buffer_percentage = 20
  let buffer_days = multiply(total_days, buffer_percentage) / 100  // 95 × 20 / 100 = 19
  let timeline_with_buffer = add(total_days, buffer_days)  // 95 + 19 = 114
  assert_eq(114, timeline_with_buffer)
}

test "calculate_investment_returns" {
  // 计算投资回报的金融应用场景
  
  let initial_investment = 50000  // 初始投资 50000 元
  let annual_return = 8           // 年化收益率 8%
  let investment_years = 5        // 投资年限 5年
  
  // 简化的复利计算
  let yearly_multiplier = add(100, annual_return)  // 100 + 8 = 108
  let total_multiplier = yearly_multiplier  // 简化：不考虑真正的复利
  
  // 计算最终价值
  let final_value_unscaled = multiply(initial_investment, total_multiplier)  // 50000 × 108 = 5400000
  let final_value = final_value_unscaled / 100  // 5400000 / 100 = 54000
  assert_eq(54000, final_value)
  
  // 计算总收益
  let total_return = add(final_value, -initial_investment)  // 54000 - 50000 = 4000
  assert_eq(4000, total_return)
  
  // 计算收益率
  let return_percentage = multiply(total_return, 100) / initial_investment  // 4000 × 100 / 50000 = 8
  assert_eq(8, return_percentage)
}
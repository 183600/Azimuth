// Azimuth 项目额外的 MoonBit 测试用例
// 包含实用的数学运算和字符串处理测试

test "arithmetic_progression_sum" {
  // 计算等差数列的和：2, 5, 8, 11, 14
  // 公式：n/2 * (2a + (n-1)d)
  let n = 5
  let a = 2
  let d = 3
  
  let sum = @azimuth.multiply(n, @azimuth.add(@azimuth.multiply(2, a), @azimuth.multiply(n - 1, d))) / 2
  @azimuth.assert_eq(40, sum)
}

test "discount_calculation" {
  // 测试折扣计算场景
  let original_price = 1200
  let discount_percentage = 15
  let additional_discount = 50
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let first_discounted_price = @azimuth.add(original_price, -discount_amount)
  
  // 应用额外折扣
  let final_price = @azimuth.add(first_discounted_price, -additional_discount)
  
  @azimuth.assert_eq(970, final_price)  // 1200 - 180 - 50 = 970
}

test "speed_distance_time" {
  // 测试速度、距离、时间的关系计算
  let distance = 300  // 公里
  let speed = 60      // 公里/小时
  
  // 计算时间：时间 = 距离 / 速度
  let time_hours = distance / speed
  let time_minutes = @azimuth.multiply(time_hours, 60)
  
  @azimuth.assert_eq(5, time_hours)
  @azimuth.assert_eq(300, time_minutes)
  
  // 计算返回行程（速度不同）
  let return_speed = 75
  let return_time = distance / return_speed
  @azimuth.assert_eq(4, return_time)
  
  // 总时间
  let total_time = @azimuth.add(time_hours, return_time)
  @azimuth.assert_eq(9, total_time)
}

test "body_mass_index" {
  // 测试BMI计算（简化整数版本）
  let weight_kg = 70    // 体重
  let height_m = 175    // 身高（厘米）
  let height_m_squared = @azimuth.multiply(height_m, height_m) / 10000  // 转换为米并平方
  
  // BMI = 体重(kg) / 身高²(m²)
  let bmi = @azimuth.multiply(weight_kg, 10000) / @azimuth.multiply(height_m, height_m)
  
  @azimuth.assert_eq(22, bmi)  // 70 * 10000 / (175 * 175) = 22.85 ≈ 22
}

test "loan_repayment" {
  // 测试贷款还款计算（简化版）
  let principal = 10000  // 本金
  let annual_rate = 6    // 年利率百分比
  let loan_years = 3     // 贷款年限
  
  // 简单利息计算：总利息 = 本金 × 利率 × 年数
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, loan_years)) / 100
  let total_repayment = @azimuth.add(principal, total_interest)
  
  // 月还款额（简化）
  let total_months = @azimuth.multiply(loan_years, 12)
  let monthly_payment = total_repayment / total_months
  
  @azimuth.assert_eq(1800, total_interest)   // 10000 × 6 × 3 / 100 = 1800
  @azimuth.assert_eq(11800, total_repayment) // 10000 + 1800 = 11800
  @azimuth.assert_eq(327, monthly_payment)   // 11800 / 36 ≈ 327
}

test "data_storage_units" {
  // 测试数据存储单位转换
  let megabytes = 1024
  
  // 转换为千字节
  let kilobytes = @azimuth.multiply(megabytes, 1024)
  @azimuth.assert_eq(1048576, kilobytes)
  
  // 转换为千兆字节
  let gigabytes = megabytes / 1024
  @azimuth.assert_eq(1, gigabytes)
  
  // 计算总位数（1字节 = 8位）
  let total_bits = @azimuth.multiply(kilobytes, 8)
  @azimuth.assert_eq(8388608, total_bits)
}

test "temperature_differential" {
  // 测试温差计算
  let day_high_celsius = 32    // 白天最高温度
  let day_low_celsius = 18     // 夜间最低温度
  
  // 计算温差
  let temp_diff = @azimuth.add(day_high_celsius, -day_low_celsius)
  @azimuth.assert_eq(14, temp_diff)
  
  // 转换为华氏度温差（F = C × 9/5）
  let temp_diff_fahrenheit = @azimuth.multiply(temp_diff, 9) / 5
  @azimuth.assert_eq(25, temp_diff_fahrenheit)  // 14 × 9 / 5 = 25.2 ≈ 25
  
  // 平均温度
  let avg_temp_celsius = @azimuth.add(day_high_celsius, day_low_celsius) / 2
  @azimuth.assert_eq(25, avg_temp_celsius)
}

test "fuel_consumption" {
  // 测试燃油消耗计算
  let distance_km = 500    // 行驶距离
  let fuel_liters = 40     // 消耗燃油
  
  // 计算每百公里油耗
  let consumption_per_100km = @azimuth.multiply(fuel_liters, 100) / distance_km
  @azimuth.assert_eq(8, consumption_per_100km)  // 40 × 100 / 500 = 8
  
  // 计算总续航里程（假设油箱容量为60升）
  let tank_capacity = 60
  let total_range = @azimuth.multiply(tank_capacity, 100) / consumption_per_100km
  @azimuth.assert_eq(750, total_range)  // 60 × 100 / 8 = 750
  
  // 计算燃油费用（假设每升7元）
  let price_per_liter = 7
  let total_cost = @azimuth.multiply(fuel_liters, price_per_liter)
  @azimuth.assert_eq(280, total_cost)  // 40 × 7 = 280
}

test "project_management_metrics" {
  // 测试项目管理指标计算
  let total_tasks = 48
  let completed_tasks = 32
  let team_members = 6
  let project_days = 15
  
  // 计算完成率
  let completion_rate = @azimuth.multiply(completed_tasks, 100) / total_tasks
  @azimuth.assert_eq(66, completion_rate)  // 32 × 100 / 48 = 66.66 ≈ 66
  
  // 计算剩余任务
  let remaining_tasks = @azimuth.add(total_tasks, -completed_tasks)
  @azimuth.assert_eq(16, remaining_tasks)
  
  // 计算每人平均完成任务数
  let tasks_per_person = completed_tasks / team_members
  @azimuth.assert_eq(5, tasks_per_person)
  
  // 计算平均每天完成任务数
  let tasks_per_day = completed_tasks / project_days
  @azimuth.assert_eq(2, tasks_per_day)
  
  // 预计剩余天数
  let remaining_days = @azimuth.multiply(remaining_tasks, project_days) / completed_tasks
  @azimuth.assert_eq(7, remaining_days)  // 16 × 15 / 32 = 7.5 ≈ 7
}

test "phone_number_formatting" {
  // 测试电话号码格式化（通过字符串处理）
  let area_code = "010"
  let prefix = "8888"
  let line_number = "6666"
  
  // 格式化为：(010) 8888-6666
  let formatted_number = "(" + area_code + ") " + prefix + "-" + line_number
  @azimuth.assert_eq_string("(010) 8888-6666", formatted_number)
  
  // 测试国际格式：+86 10 8888 6666
  let country_code = "+86"
  let international_format = country_code + " " + area_code + " " + prefix + " " + line_number
  @azimuth.assert_eq_string("+86 10 8888 6666", international_format)
  
  // 测试使用greet函数处理电话号码
  let phone_greeting = @azimuth.greet(formatted_number)
  @azimuth.assert_eq_string("Hello, (010) 8888-6666!", phone_greeting)
}
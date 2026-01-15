// 新增的 MoonBit 测试用例
// 覆盖各种实际应用场景和边界情况

test "array_sum_calculation" {
  // 模拟数组求和：计算 1+2+3+4+5+6+7+8+9+10
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5), 6), 7), 8), 9), 10)
  @azimuth.assert_eq(55, sum)
}

test "velocity_calculation" {
  // 物理计算：速度 = 距离 / 时间
  let distance = 300  // 米
  let time = 60       // 秒
  let velocity = distance / time
  @azimuth.assert_eq(5, velocity)
  
  // 计算加速度：a = (v - u) / t
  let final_velocity = 20
  let initial_velocity = 5
  let acceleration_time = 3
  let acceleration = (final_velocity - initial_velocity) / acceleration_time
  @azimuth.assert_eq(5, acceleration)
}

test "bank_interest_calculation" {
  // 银行利息计算：单利
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  let yearly_interest = @azimuth.multiply(principal, annual_rate) / 100
  let total_interest = @azimuth.multiply(yearly_interest, years)
  let final_amount = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(11500, final_amount)
}

test "inventory_management" {
  // 库存管理计算
  let initial_stock = 500
  let units_sold = 120
  let units_received = 80
  let damaged_units = 15
  
  let remaining_stock = @azimuth.add(@azimuth.add(initial_stock, -units_sold), units_received)
  let final_stock = @azimuth.add(remaining_stock, -damaged_units)
  
  @azimuth.assert_eq(445, final_stock)
}

test "phone_number_formatting" {
  // 电话号码格式化测试
  let area_code = "010"
  let prefix = "1234"
  let line_number = "5678"
  
  let formatted_number = area_code + "-" + prefix + "-" + line_number
  @azimuth.assert_eq_string("Hello, 010-1234-5678!", @azimuth.greet(formatted_number))
}

test "grade_point_average" {
  // GPA计算
  let grade1 = 85
  let grade2 = 90
  let grade3 = 78
  let grade4 = 92
  let grade5 = 88
  
  let total_points = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(grade1, grade2), grade3), grade4), grade5)
  let gpa = total_points / 5
  
  @azimuth.assert_eq(86, gpa)
}

test "data_storage_calculation" {
  // 数据存储计算
  let file_size_mb = 25
  let num_files = 100
  let overhead_percent = 10
  
  let total_size = @azimuth.multiply(file_size_mb, num_files)
  let overhead = @azimuth.multiply(total_size, overhead_percent) / 100
  let actual_storage_needed = @azimuth.add(total_size, overhead)
  
  @azimuth.assert_eq(2750, actual_storage_needed)
}

test "fuel_consumption" {
  // 燃料消耗计算
  let distance_km = 500
  let fuel_efficiency = 15  // 每升汽油行驶的公里数
  let price_per_liter = 8
  
  let fuel_needed = distance_km / fuel_efficiency
  let total_cost = @azimuth.multiply(fuel_needed, price_per_liter)
  
  @azimuth.assert_eq(266, total_cost)
}

test "bmi_calculation" {
  // BMI计算（简化版，避免浮点数）
  let weight_kg = 70
  let height_cm = 175
  
  // BMI = 体重(kg) / (身高(m))^2
  // 为避免浮点数，我们使用：BMI * 10000 = 体重 * 10000 / (身高^2)
  let height_squared = @azimuth.multiply(height_cm, height_cm)
  let bmi_times_10000 = @azimuth.multiply(weight_kg, 10000) / height_squared
  
  @azimuth.assert_eq(22, bmi_times_10000)  // 实际BMI约为22.85，乘以10000后约等于22
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_hour = 15  // 下午3点
  let local_minute = 30
  let time_zone_offset = -8  // 东八区到UTC的偏移
  
  let local_total_minutes = @azimuth.add(@azimuth.multiply(local_hour, 60), local_minute)
  let utc_total_minutes = @azimuth.add(local_total_minutes, time_zone_offset * 60)
  
  // 处理跨日情况
  let utc_hour = (utc_total_minutes / 60 + 24) % 24
  let utc_minute = utc_total_minutes % 60
  
  @azimuth.assert_eq(7, utc_hour)
  @azimuth.assert_eq(30, utc_minute)
}
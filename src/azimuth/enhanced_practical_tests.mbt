// Enhanced practical test cases for azimuth library
// These tests focus on real-world scenarios and edge cases

test "practical_discount_calculation" {
  // 实际折扣计算：商品价格、数量、折扣率
  let unit_price = 199
  let quantity = 3
  let discount_rate = 15  // 15% 折扣
  
  // 计算原价
  let original_total = multiply(unit_price, quantity)
  // 计算折扣金额
  let discount_amount = multiply(original_total, discount_rate) / 100
  // 计算最终价格
  let final_price = add(original_total, -discount_amount)
  
  assert_eq(507, final_price)  // 199*3 - 199*3*15/100 = 597 - 89 = 508 (整数除法)
}

test "practical_speed_conversion" {
  // 实际速度转换：km/h 转 m/s
  let km_per_hour = 72
  
  // 转换公式：m/s = km/h × 1000 / 3600 ≈ km/h × 5 / 18
  let meters_per_second = multiply(km_per_hour, 5) / 18
  
  assert_eq(20, meters_per_second)  // 72 × 5 / 18 = 20
  
  // 反向转换：m/s 转 km/h
  let back_to_kmh = multiply(meters_per_second, 18) / 5
  assert_eq(72, back_to_kmh)
}

test "practical_bmi_calculation" {
  // 实际BMI计算：体重(kg) / 身高(m)²
  // 使用整数计算避免浮点数
  let weight_kg = 70
  let height_cm = 175
  
  // 转换为厘米：BMI = weight(kg) × 10000 / height(cm)²
  let height_squared = multiply(height_cm, height_cm)
  let bmi_times_100 = multiply(weight_kg, 10000) / height_squared
  
  assert_eq(22, bmi_times_100)  // 70 × 10000 / 30625 ≈ 22.85，整数部分为22
}

test "practical_loan_calculation" {
  // 实际贷款计算：月供 = [本金 × 月利率 × (1+月利率)^月数] / [(1+月利率)^月数 - 1]
  // 简化为整数计算
  let principal = 100000  // 10万本金
  let annual_rate = 6     // 6%年利率
  let months = 12         // 1年12期
  
  let monthly_rate = annual_rate / 12  // 月利率 0.5%
  let total_interest = multiply(principal, monthly_rate) * months / 100
  let total_payment = add(principal, total_interest)
  let monthly_payment = total_payment / months
  
  assert_eq(8833, monthly_payment)  // 简化计算的月供
}

test "practical_temperature_conversion" {
  // 实际温度转换：摄氏度转华氏度 F = C × 9/5 + 32
  // 使用整数近似：F ≈ C × 2 + 30
  let celsius = 25
  
  let fahrenheit_approx = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit_approx)  // 25 × 2 + 30 = 80
  
  // 更精确的整数计算：F = C × 9 / 5 + 32
  let fahrenheit_precise = add(multiply(celsius, 9) / 5, 32)
  assert_eq(77, fahrenheit_precise)  // 25 × 9 / 5 + 32 = 77
}

test "practical_time_zone_conversion" {
  // 实际时区转换
  let local_hour = 15  // 下午3点
  let time_zone_diff = -8  // 与目标时区相差8小时
  
  // 计算目标时间
  let target_hour = add(local_hour, time_zone_diff)
  // 处理负数时间（前一天）
  let adjusted_hour = if (target_hour < 0) { add(target_hour, 24) } else { target_hour }
  
  assert_eq(7, adjusted_hour)  // 15 - 8 = 7 (早上7点)
}

test "practical_calorie_calculation" {
  // 实际卡路里计算：基础代谢率 BMR
  // 男性简化公式：BMR = 88.362 + (13.397 × 体重kg) + (4.799 × 身高cm) - (5.677 × 年龄)
  // 使用整数近似
  let weight_kg = 70
  let height_cm = 175
  let age = 30
  
  let bmr_approx = add(add(add(88, multiply(13, weight_kg) / 10), multiply(4, height_cm) / 10), multiply(-5, age))
  assert_eq(1658, bmr_approx)  // 简化计算的基础代谢率
}

test "practical_fuel_consumption" {
  // 实际油耗计算：百公里油耗
  let distance_km = 500
  let fuel_liters = 40
  
  // 计算百公里油耗
  let consumption_per_100km = multiply(fuel_liters, 100) / distance_km
  assert_eq(8, consumption_per_100km)  // 40 × 100 / 500 = 8
  
  // 计算总费用
  let price_per_liter = 8
  let total_cost = multiply(fuel_liters, price_per_liter)
  assert_eq(320, total_cost)
}

test "practical_data_storage" {
  // 实际数据存储计算
  let file_size_mb = 25
  let number_of_files = 100
  
  // 计算总存储需求（MB）
  let total_storage_mb = multiply(file_size_mb, number_of_files)
  assert_eq(2500, total_storage_mb)
  
  // 转换为GB
  let total_storage_gb = total_storage_mb / 1024
  assert_eq(2, total_storage_gb)  // 2500 / 1024 ≈ 2.44，整数部分为2
  
  // 计算备份所需空间（双倍）
  let backup_space = multiply(total_storage_mb, 2)
  assert_eq(5000, backup_space)
}

test "practical_phone_number_validation" {
  // 实际手机号验证（简化）
  let phone_number = "13812345678"
  
  // 检查长度
  let length_check = phone_number.length() == 11
  assert_true(length_check)
  
  // 检查是否以特定前缀开头
  let starts_with_138 = phone_number.starts_with("138")
  assert_true(starts_with_138)
  
  // 测试greet函数与电话号码
  let phone_greeting = greet(phone_number)
  assert_eq_string("Hello, 13812345678!", phone_greeting)
}
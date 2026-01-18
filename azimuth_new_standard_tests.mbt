// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，包含8个多样化测试用例

// 导入azimuth模块以使用相关函数
use azimuth

test "arithmetic_sequence" {
  // 等差数列计算测试
  let first_term = 3
  let difference = 5
  let n = 4
  
  // 计算第n项：a_n = a_1 + (n-1)*d
  let nth_term = add(first_term, multiply(difference, n - 1))
  assert_eq(18, nth_term)  // 3 + 3*5 = 18
  
  // 计算前n项和：S_n = n/2 * (2*a_1 + (n-1)*d)
  let sum = multiply(n, add(multiply(2, first_term), multiply(difference, n - 1))) / 2
  assert_eq(42, sum)  // 4/2 * (6 + 15) = 42
}

test "geometric_progression" {
  // 等比数列计算测试
  let first_term = 2
  let ratio = 3
  let n = 4
  
  // 计算第n项：a_n = a_1 * r^(n-1)
  let nth_term = multiply(first_term, multiply(ratio, multiply(ratio, ratio)))
  assert_eq(54, nth_term)  // 2 * 3^3 = 54
  
  // 计算前n项和：S_n = a_1 * (r^n - 1) / (r - 1)
  let r_power_n = multiply(ratio, multiply(ratio, multiply(ratio, ratio)))
  let sum = multiply(first_term, add(r_power_n, -1)) / add(ratio, -1)
  assert_eq(80, sum)  // 2 * (81-1) / 2 = 80
}

test "temperature_conversion_precise" {
  // 精确温度转换测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  
  // 测试水的冰点
  let freezing_c = 0
  let freezing_f = add(multiply(multiply(freezing_c, 9), 5), 32)
  assert_eq(32, freezing_f)
  
  // 测试正常体温
  let normal_c = 37
  let normal_f = add(multiply(multiply(normal_c, 9), 5), 32)
  assert_eq(98, normal_f)  // 37*9/5 + 32 = 98.6 ≈ 98 (整数计算)
  
  // 测试沸点
  let boiling_c = 100
  let boiling_f = add(multiply(multiply(boiling_c, 9), 5), 32)
  assert_eq(212, boiling_f)
}

test "currency_conversion" {
  // 货币转换测试
  let usd_amount = 100
  let eur_rate = 0.85  // 1 USD = 0.85 EUR (简化为整数计算)
  let gbp_rate = 0.73  // 1 USD = 0.73 GBP (简化为整数计算)
  let jpy_rate = 110   // 1 USD = 110 JPY
  
  // 转换为欧元 (简化计算)
  let eur_amount = multiply(usd_amount, 85) / 100
  assert_eq(85, eur_amount)
  
  // 转换为英镑 (简化计算)
  let gbp_amount = multiply(usd_amount, 73) / 100
  assert_eq(73, gbp_amount)
  
  // 转换为日元
  let jpy_amount = multiply(usd_amount, jpy_rate)
  assert_eq(11000, jpy_amount)
}

test "loan_calculation" {
  // 贷款计算测试
  let principal = 10000  // 本金
  let annual_rate = 6    // 年利率6%
  let years = 5          // 贷款年限
  
  // 简单利息计算
  let total_interest = multiply(principal, multiply(annual_rate, years)) / 100
  let total_payment = add(principal, total_interest)
  assert_eq(13000, total_payment)  // 10000 + 10000*6*5/100 = 13000
  
  // 月供计算 (简化版)
  let months = multiply(years, 12)
  let monthly_payment = total_payment / months
  assert_eq(216, monthly_payment)  // 13000/60 ≈ 216
  
  // 验证总还款额
  let verified_total = multiply(monthly_payment, months)
  assert_eq(12960, verified_total)  // 216*60 = 12960 (取整误差)
}

test "speed_distance_time" {
  // 速度、距离、时间计算测试
  let speed = 60  // km/h
  let time = 2.5  // hours (简化为整数计算)
  
  // 计算距离：距离 = 速度 × 时间
  let distance = multiply(speed, multiply(25, 10)) / 10
  assert_eq(150, distance)  // 60 * 2.5 = 150 km
  
  // 计算时间：时间 = 距离 / 速度
  let new_distance = 180
  let calculated_time = multiply(new_distance, 10) / speed
  assert_eq(30, calculated_time)  // 180/60 = 3 小时 (30*10/60 = 3)
  
  // 计算速度：速度 = 距离 / 时间
  let new_time = 2  // hours
  let calculated_speed = new_distance / new_time
  assert_eq(90, calculated_speed)  // 180/2 = 90 km/h
}

test "percentage_calculations" {
  // 百分比计算测试
  let original_value = 200
  let percentage = 25
  
  // 计算百分比值
  let percentage_value = multiply(original_value, percentage) / 100
  assert_eq(50, percentage_value)  // 200 * 25% = 50
  
  // 计算增加后的值
  let increased_value = add(original_value, percentage_value)
  assert_eq(250, increased_value)  // 200 + 50 = 250
  
  // 计算减少后的值
  let decreased_value = add(original_value, -percentage_value)
  assert_eq(150, decreased_value)  // 200 - 50 = 150
  
  // 计算变化百分比
  let new_value = 150
  let change = add(new_value, -original_value)
  let change_percentage = multiply(change, 100) / original_value
  assert_eq(-25, change_percentage)  // (150-200)/100 = -25%
}

test "array_index_calculation" {
  // 数组索引计算测试
  let rows = 4
  let cols = 5
  
  // 二维数组转一维索引：index = row * cols + col
  let row = 2
  let col = 3
  let index_1d = add(multiply(row, cols), col)
  assert_eq(13, index_1d)  // 2*5 + 3 = 13
  
  // 一维数组转二维索引
  let index = 17
  let calculated_row = index / cols
  let calculated_col = index - multiply(calculated_row, cols)
  assert_eq(3, calculated_row)  // 17/5 = 3
  assert_eq(2, calculated_col)  // 17 - 3*5 = 2
  
  // 验证转换的一致性
  let verify_index = add(multiply(calculated_row, cols), calculated_col)
  assert_eq(index, verify_index)
}
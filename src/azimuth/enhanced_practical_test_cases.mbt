// 增强实用测试用例
// 10个新的 MoonBit 测试用例，覆盖不同的实用场景

test "arithmetic_sequence_calculation" {
  // 等差数列计算测试
  let first_term = 3
  let common_difference = 4
  let n = 7
  
  // 计算第n项：a_n = a_1 + (n-1)d
  let nth_term = add(first_term, multiply(common_difference, subtract(n, 1)))
  
  // 计算前n项和：S_n = n/2 * (a_1 + a_n)
  let sum_first_n = multiply(divide_with_ceil(n, 2), add(first_term, nth_term))
  
  assert_eq(nth_term, 27)  // 3 + 6*4 = 27
  assert_eq(sum_first_n, 105)  // 7/2 * (3+27) = 105
}

test "percentage_calculations" {
  // 百分比计算测试
  let original_value = 250
  let increase_rate = 15  // 15%
  let discount_rate = 20  // 20%
  
  // 计算增长后的值
  let increase_amount = divide_with_ceil(multiply(original_value, increase_rate), 100)
  let increased_value = add(original_value, increase_amount)
  
  // 计算折扣后的值
  let discount_amount = divide_with_ceil(multiply(increased_value, discount_rate), 100)
  let final_value = subtract(increased_value, discount_amount)
  
  // 计算总变化率
  let total_change = subtract(final_value, original_value)
  let total_change_rate = divide_with_ceil(multiply(total_change, 100), original_value)
  
  assert_eq(increase_amount, 38)
  assert_eq(increased_value, 288)
  assert_eq(discount_amount, 58)
  assert_eq(final_value, 230)
  assert_eq(total_change, -20)
  assert_eq(total_change_rate, -8)
}

test "time_unit_conversion" {
  // 时间单位转换测试
  let days = 5
  let hours_per_day = 24
  let minutes_per_hour = 60
  let seconds_per_minute = 60
  
  // 转换为小时
  let total_hours = multiply(days, hours_per_day)
  
  // 转换为分钟
  let total_minutes = multiply(total_hours, minutes_per_hour)
  
  // 转换为秒
  let total_seconds = multiply(total_minutes, seconds_per_minute)
  
  // 计算工作小时数（每天8小时）
  let work_hours_per_day = 8
  let total_work_hours = multiply(days, work_hours_per_day)
  
  // 计算工作天数
  let work_days_needed = divide_with_ceil(total_hours, work_hours_per_day)
  
  assert_eq(total_hours, 120)
  assert_eq(total_minutes, 7200)
  assert_eq(total_seconds, 432000)
  assert_eq(total_work_hours, 40)
  assert_eq(work_days_needed, 15)
}

test "data_statistics_analysis" {
  // 数据统计分析测试
  let data_sum = add(add(add(add(add(add(45, 67), 23), 89), 12), 56), 78)
  let data_count = 7
  
  // 计算平均值
  let mean_value = divide_with_ceil(data_sum, data_count)
  
  // 计算最大值（简化）
  let max_value = 89
  
  // 计算最小值（简化）
  let min_value = 12
  
  // 计算极差
  let range = subtract(max_value, min_value)
  
  // 计算中位数近似值（排序后：12, 23, 45, 56, 67, 78, 89）
  let median_value = 56
  
  // 计算标准差近似值（简化计算）
  let variance_approx = divide_with_ceil(multiply(range, range), 4)
  let std_dev_approx = divide_with_ceil(variance_approx, 2)
  
  assert_eq(data_sum, 370)
  assert_eq(mean_value, 53)
  assert_eq(range, 77)
  assert_eq(median_value, 56)
  assert_eq(variance_approx, 1483)
  assert_eq(std_dev_approx, 742)
}

test "geometric_shape_calculations" {
  // 几何图形计算测试
  let radius = 8
  let pi_approx = 22  // 使用22/7近似π
  
  // 计算圆的周长：C = 2πr
  let circumference = divide_with_ceil(multiply(multiply(2, pi_approx), radius), 7)
  
  // 计算圆的面积：A = πr²
  let area = divide_with_ceil(multiply(pi_approx, multiply(radius, radius)), 7)
  
  // 计算球的体积：V = 4/3πr³
  let volume = divide_with_ceil(multiply(multiply(4, pi_approx), multiply(radius, multiply(radius, radius))), multiply(3, 7))
  
  // 计算圆柱的表面积（假设高度为12）
  let height = 12
  let cylinder_surface_area = add(circumference, multiply(multiply(2, pi_approx), multiply(radius, height)))
  
  assert_eq(circumference, 50)
  assert_eq(area, 201)
  assert_eq(volume, 2145)
  assert_eq(cylinder_surface_area, 603)
}

test "currency_exchange_calculation" {
  // 货币兑换计算测试
  let usd_amount = 1000
  let eur_to_usd_rate = 108  // 1 EUR = 1.08 USD，使用整数108表示
  let gbp_to_usd_rate = 127  // 1 GBP = 1.27 USD，使用整数127表示
  let jpy_to_usd_rate = 1    // 1 JPY = 0.0067 USD，使用整数1表示
  
  // USD转EUR
  let eur_amount = divide_with_ceil(multiply(usd_amount, 100), eur_to_usd_rate)
  
  // USD转GBP
  let gbp_amount = divide_with_ceil(multiply(usd_amount, 100), gbp_to_usd_rate)
  
  // USD转JPY
  let jpy_amount = multiply(usd_amount, divide_with_ceil(100, jpy_to_usd_rate))
  
  // 计算兑换费用（1%）
  let fee_rate = 1
  let total_fee = divide_with_ceil(multiply(usd_amount, fee_rate), 100)
  let net_amount = subtract(usd_amount, total_fee)
  
  // 验证兑换一致性（简化）
  let eur_back_to_usd = divide_with_ceil(multiply(eur_amount, eur_to_usd_rate), 100)
  
  assert_eq(eur_amount, 926)
  assert_eq(gbp_amount, 787)
  assert_eq(jpy_amount, 100000)
  assert_eq(total_fee, 10)
  assert_eq(net_amount, 990)
  assert_eq(eur_back_to_usd, 1000)
}

test "speed_distance_time_calculations" {
  // 速度距离时间计算测试
  let distance = 450  // 公里
  let speed = 90      // 公里/小时
  let time_hours = divide_with_ceil(distance, speed)
  
  // 计算分钟数
  let time_minutes = multiply(time_hours, 60)
  
  // 计算剩余分钟数（如果有）
  let exact_time_hours = divide_with_ceil(distance, speed)
  let remaining_minutes = subtract(time_minutes, multiply(exact_time_hours, 60))
  
  // 计算不同速度下的时间
  let speed_slow = 60
  let time_slow = divide_with_ceil(distance, speed_slow)
  
  let speed_fast = 120
  let time_fast = divide_with_ceil(distance, speed_fast)
  
  // 计算平均速度
  let total_time = add(time_slow, time_fast)
  let total_distance = multiply(distance, 2)
  let avg_speed = divide_with_ceil(total_distance, total_time)
  
  assert_eq(time_hours, 5)
  assert_eq(time_minutes, 300)
  assert_eq(remaining_minutes, 0)
  assert_eq(time_slow, 8)
  assert_eq(time_fast, 4)
  assert_eq(avg_speed, 75)
}

test "temperature_conversion_extended" {
  // 扩展温度转换测试
  let celsius = 37
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  // 摄氏度转开尔文：K = C + 273.15（简化为273）
  let kelvin = add(celsius, 273)
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  let celsius_back = divide_with_ceil(multiply(subtract(fahrenheit, 32), 5), 9)
  
  // 开尔文转摄氏度：C = K - 273
  let celsius_from_kelvin = subtract(kelvin, 273)
  
  // 开尔文转华氏度：F = (K - 273) × 9/5 + 32
  let fahrenheit_from_kelvin = add(divide_with_ceil(multiply(subtract(kelvin, 273), 9), 5), 32)
  
  // 计算温度差
  let temp_diff_celsius = subtract(fahrenheit_from_kelvin, celsius)
  let temp_diff_fahrenheit = subtract(fahrenheit, celsius_back)
  
  assert_eq(fahrenheit, 99)
  assert_eq(kelvin, 310)
  assert_eq(celsius_back, 37)
  assert_eq(celsius_from_kelvin, 37)
  assert_eq(fahrenheit_from_kelvin, 99)
  assert_eq(temp_diff_celsius, 62)
  assert_eq(temp_diff_fahrenheit, 62)
}

test "number_system_conversion_extended" {
  // 扩展数制转换测试
  let decimal_number = 156
  
  // 转换为二进制（手动计算）
  let bit7 = divide_with_ceil(decimal_number, 128)  // 2^7 = 128
  let remainder1 = subtract(decimal_number, multiply(bit7, 128))
  
  let bit6 = divide_with_ceil(remainder1, 64)   // 2^6 = 64
  let remainder2 = subtract(remainder1, multiply(bit6, 64))
  
  let bit5 = divide_with_ceil(remainder2, 32)   // 2^5 = 32
  let remainder3 = subtract(remainder2, multiply(bit5, 32))
  
  let bit4 = divide_with_ceil(remainder3, 16)   // 2^4 = 16
  let remainder4 = subtract(remainder3, multiply(bit4, 16))
  
  let bit3 = divide_with_ceil(remainder4, 8)    // 2^3 = 8
  let remainder5 = subtract(remainder4, multiply(bit3, 8))
  
  let bit2 = divide_with_ceil(remainder5, 4)    // 2^2 = 4
  let remainder6 = subtract(remainder5, multiply(bit2, 4))
  
  let bit1 = divide_with_ceil(remainder6, 2)    // 2^1 = 2
  let bit0 = subtract(remainder6, multiply(bit1, 2))  // 2^0 = 1
  
  // 转换为八进制
  let octal_digit2 = divide_with_ceil(decimal_number, 64)
  let octal_remainder1 = subtract(decimal_number, multiply(octal_digit2, 64))
  let octal_digit1 = divide_with_ceil(octal_remainder1, 8)
  let octal_digit0 = subtract(octal_remainder1, multiply(octal_digit1, 8))
  
  // 转换为十六进制
  let hex_digit2 = divide_with_ceil(decimal_number, 256)
  let hex_remainder1 = subtract(decimal_number, multiply(hex_digit2, 256))
  let hex_digit1 = divide_with_ceil(hex_remainder1, 16)
  let hex_digit0 = subtract(hex_remainder1, multiply(hex_digit1, 16))
  
  // 验证二进制位
  let binary_weighted_sum = add(add(add(add(add(add(add(add(
    multiply(bit7, 128), multiply(bit6, 64)), multiply(bit5, 32)), 
    multiply(bit4, 16)), multiply(bit3, 8)), multiply(bit2, 4)), 
    multiply(bit1, 2)), bit0)
  
  assert_eq(bit7, 1)
  assert_eq(bit6, 0)
  assert_eq(bit5, 0)
  assert_eq(bit4, 1)
  assert_eq(bit3, 1)
  assert_eq(bit2, 1)
  assert_eq(bit1, 0)
  assert_eq(bit0, 0)
  assert_eq(binary_weighted_sum, 156)
  
  assert_eq(octal_digit2, 2)
  assert_eq(octal_digit1, 3)
  assert_eq(octal_digit0, 4)
  
  assert_eq(hex_digit2, 0)
  assert_eq(hex_digit1, 9)
  assert_eq(hex_digit0, 12)
}

test "comprehensive_business_scenario" {
  // 综合业务场景测试
  let product_cost = 50
  let selling_price = 85
  let monthly_sales = 120
  let months = 6
  
  // 计算单个产品的利润
  let profit_per_unit = subtract(selling_price, product_cost)
  
  // 计算利润率
  let profit_margin = divide_with_ceil(multiply(profit_per_unit, 100), product_cost)
  
  // 计算月利润
  let monthly_profit = multiply(profit_per_unit, monthly_sales)
  
  // 计算半年总利润
  let total_profit = multiply(monthly_profit, months)
  
  // 计算年利润预测
  let annual_profit = multiply(monthly_profit, 12)
  
  // 计算盈亏平衡点（固定成本假设为10000）
  let fixed_costs = 10000
  let break_even_units = divide_with_ceil(fixed_costs, profit_per_unit)
  let break_even_months = divide_with_ceil(break_even_units, monthly_sales)
  
  // 计算投资回报率（初始投资50000）
  let initial_investment = 50000
  let roi_months = divide_with_ceil(initial_investment, monthly_profit)
  let annual_roi = divide_with_ceil(multiply(annual_profit, 100), initial_investment)
  
  assert_eq(profit_per_unit, 35)
  assert_eq(profit_margin, 70)
  assert_eq(monthly_profit, 4200)
  assert_eq(total_profit, 25200)
  assert_eq(annual_profit, 50400)
  assert_eq(break_even_units, 286)
  assert_eq(break_even_months, 3)
  assert_eq(roi_months, 12)
  assert_eq(annual_roi, 101)
}
// 新增的标准 MoonBit 测试用例
// 覆盖核心功能的边界情况和实际应用场景

test "algebraic_expressions_evaluation" {
  // 代数表达式求值测试
  let x = 5
  let y = 3
  
  // 计算: 2x² + 3y - 7
  let term1 = multiply(2, multiply(x, x))  // 2 * 5² = 50
  let term2 = multiply(3, y)               // 3 * 3 = 9
  let result = subtract(add(term1, term2), 7)  // 50 + 9 - 7 = 52
  
  assert_eq(50, term1)
  assert_eq(9, term2)
  assert_eq(52, result)
}

test "time_conversion_calculations" {
  // 时间转换计算测试
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = add(add(multiply(hours, 3600), multiply(minutes, 60)), seconds)
  
  // 转换回小时、分钟、秒
  let back_to_hours = divide_with_ceil(total_seconds, 3600)
  let remaining_seconds = subtract(total_seconds, multiply(back_to_hours, 3600))
  let back_to_minutes = divide_with_ceil(remaining_seconds, 60)
  
  assert_eq(9930, total_seconds)
  assert_eq(3, back_to_hours)  // 向上取整
  assert_eq(165, remaining_seconds)
  assert_eq(3, back_to_minutes)
}

test "geometric_area_calculations" {
  // 几何面积计算测试
  let length = 8
  let width = 6
  
  // 矩形面积
  let rectangle_area = multiply(length, width)
  
  // 三角形面积（底乘高除以2，向上取整）
  let triangle_base = length
  let triangle_height = width
  let triangle_area = divide_with_ceil(multiply(triangle_base, triangle_height), 2)
  
  // 梯形面积（上底+下底）×高÷2
  let top_base = 4
  let bottom_base = width
  let trapezoid_height = length
  let trapezoid_area = divide_with_ceil(multiply(add(top_base, bottom_base), trapezoid_height), 2)
  
  assert_eq(48, rectangle_area)
  assert_eq(24, triangle_area)
  assert_eq(40, trapezoid_area)
}

test "statistical_variance_calculation" {
  // 统计方差计算测试
  // 数据集: [2, 4, 6, 8, 10]
  let data1 = 2
  let data2 = 4
  let data3 = 6
  let data4 = 8
  let data5 = 10
  
  // 计算平均值
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  let mean = divide_with_ceil(sum, 5)
  
  // 计算离差平方和
  let diff1 = subtract(data1, mean)
  let diff2 = subtract(data2, mean)
  let diff3 = subtract(data3, mean)
  let diff4 = subtract(data4, mean)
  let diff5 = subtract(data5, mean)
  
  let variance_sum = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), multiply(diff3, diff3)), multiply(diff4, diff4)), multiply(diff5, diff5))
  let variance = divide_with_ceil(variance_sum, 5)
  
  assert_eq(30, sum)
  assert_eq(6, mean)
  assert_eq(40, variance_sum)
  assert_eq(8, variance)
}

test "currency_conversion_rounding" {
  // 货币转换舍入测试
  let usd_amount = 100
  let exchange_rate = 83  // 1 USD = 83 CNY
  
  // 转换为人民币
  let cny_amount = multiply(usd_amount, exchange_rate)
  
  // 计算不同面额的张数
  let note_100_count = divide_with_ceil(cny_amount, 100)
  let remaining_after_100 = subtract(cny_amount, multiply(note_100_count - 1, 100))
  
  let note_50_count = divide_with_ceil(remaining_after_100, 50)
  let remaining_after_50 = subtract(remaining_after_100, multiply(note_50_count - 1, 50))
  
  let note_20_count = divide_with_ceil(remaining_after_50, 20)
  
  assert_eq(8300, cny_amount)
  assert_eq(83, note_100_count)
  assert_eq(1, remaining_after_100)
  assert_eq(1, note_50_count)
  assert_eq(1, remaining_after_50)
  assert_eq(1, note_20_count)
}

test "temperature_scale_conversions" {
  // 温标转换测试
  let celsius = 25
  
  // 摄氏度转华氏度: F = C × 9/5 + 32
  let fahrenheit = add(multiply(celsius, 9), 160)  // 乘以9/5等于乘以9再除以5
  let fahrenheit_final = divide_with_ceil(fahrenheit, 5)
  
  // 摄氏度转开尔文: K = C + 273.15（简化为整数）
  let kelvin = add(celsius, 273)
  
  // 华氏度转摄氏度（反向验证）
  let celsius_back = multiply(subtract(multiply(fahrenheit_final, 5), 160), 1)
  let celsius_back_final = divide_with_ceil(celsius_back, 9)
  
  assert_eq(385, fahrenheit)
  assert_eq(77, fahrenheit_final)
  assert_eq(298, kelvin)
  assert_eq(25, celsius_back_final)
}

test "compound_interest_simplified" {
  // 简化复利计算测试
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let year1_total = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_total, rate_percent), 100)
  let year2_total = add(year1_total, year2_interest)
  
  // 第三年
  let year3_interest = divide_with_ceil(multiply(year2_total, rate_percent), 100)
  let year3_total = add(year2_total, year3_interest)
  
  // 总收益
  let total_profit = subtract(year3_total, principal)
  
  assert_eq(50, year1_interest)
  assert_eq(1050, year1_total)
  assert_eq(53, year2_interest)
  assert_eq(1103, year2_total)
  assert_eq(56, year3_interest)
  assert_eq(1159, year3_total)
  assert_eq(159, total_profit)
}

test "digital_storage_conversions" {
  // 数字存储转换测试
  let megabytes = 1024
  
  // 转换为千字节
  let kilobytes = multiply(megabytes, 1024)
  
  // 转换为千兆字节
  let gigabytes = divide_with_ceil(megabytes, 1024)
  
  // 计算文件存储需求
  let file_size_mb = 15
  let files_count = 100
  let total_storage_mb = multiply(file_size_mb, files_count)
  let storage_gb_needed = divide_with_ceil(total_storage_mb, 1024)
  
  // 计算剩余空间
  let available_gb = 2
  let available_mb = multiply(available_gb, 1024)
  let remaining_mb = subtract(available_mb, total_storage_mb)
  
  assert_eq(1048576, kilobytes)
  assert_eq(1, gigabytes)
  assert_eq(1500, total_storage_mb)
  assert_eq(2, storage_gb_needed)
  assert_eq(2048, available_mb)
  assert_eq(548, remaining_mb)
}

test "game_score_calculations" {
  // 游戏得分计算测试
  let base_score = 100
  let level_multiplier = 3
  let combo_bonus = 50
  let time_bonus = 25
  let penalty = 20
  
  // 计算最终得分
  let level_score = multiply(base_score, level_multiplier)
  let combo_score = add(level_score, combo_bonus)
  let time_score = add(combo_score, time_bonus)
  let final_score = subtract(time_score, penalty)
  
  // 计算星级（每100分一星）
  let star_rating = divide_with_ceil(final_score, 100)
  
  // 计算排名分数（基于星级和剩余分数）
  let remainder_score = subtract(final_score, multiply(star_rating - 1, 100))
  let rank_points = add(multiply(star_rating, 1000), remainder_score)
  
  assert_eq(300, level_score)
  assert_eq(350, combo_score)
  assert_eq(375, time_score)
  assert_eq(355, final_score)
  assert_eq(4, star_rating)
  assert_eq(55, remainder_score)
  assert_eq(4055, rank_points)
}

test "network_data_transmission" {
  // 网络数据传输测试
  let file_size_mb = 50
  let network_speed_mbps = 10
  let overhead_percent = 10
  
  // 计算理论传输时间（秒）
  let theoretical_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  
  // 考虑开销的实际时间
  let overhead_factor = add(100, overhead_percent)
  let actual_time = divide_with_ceil(multiply(theoretical_time, overhead_factor), 100)
  
  // 转换为分钟
  let time_minutes = divide_with_ceil(actual_time, 60)
  
  // 计算带宽利用率
  let utilized_bandwidth = multiply(network_speed_mbps, 100)
  let efficiency = divide_with_ceil(multiply(100, network_speed_mbps), network_speed_mbps)
  
  assert_eq(5, theoretical_time)
  assert_eq(6, actual_time)
  assert_eq(1, time_minutes)
  assert_eq(1000, utilized_bandwidth)
  assert_eq(100, efficiency)
}
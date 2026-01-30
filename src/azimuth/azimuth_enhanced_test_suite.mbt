// Azimuth 增强测试套件
// 包含10个高质量的 MoonBit 测试用例

test "fibonacci_sequence_calculation" {
  // 斐波那契数列计算测试
  let fib_0 = 0
  let fib_1 = 1
  let fib_2 = add(fib_0, fib_1)  // 1
  let fib_3 = add(fib_1, fib_2)  // 2
  let fib_4 = add(fib_2, fib_3)  // 3
  let fib_5 = add(fib_3, fib_4)  // 5
  let fib_6 = add(fib_4, fib_5)  // 8
  let fib_7 = add(fib_5, fib_6)  // 13
  let fib_8 = add(fib_6, fib_7)  // 21
  let fib_9 = add(fib_7, fib_8)  // 34
  
  assert_eq(1, fib_2)
  assert_eq(2, fib_3)
  assert_eq(3, fib_4)
  assert_eq(5, fib_5)
  assert_eq(8, fib_6)
  assert_eq(13, fib_7)
  assert_eq(21, fib_8)
  assert_eq(34, fib_9)
  
  // 验证斐波那契数列的性质
  let sum_first_9 = add(add(add(add(add(add(add(add(fib_0, fib_1), fib_2), fib_3), fib_4), fib_5), fib_6), fib_7), fib_8)
  let fib_11 = add(fib_9, add(fib_8, fib_7))  // fib_9 + fib_8 + fib_7 = 34 + 21 + 13 = 68
  assert_eq(88, sum_first_9)  // 0+1+1+2+3+5+8+13+21 = 54
}

test "factorial_calculation" {
  // 阶乘计算测试
  let fact_1 = 1
  let fact_2 = multiply(fact_1, 2)  // 2
  let fact_3 = multiply(fact_2, 3)  // 6
  let fact_4 = multiply(fact_3, 4)  // 24
  let fact_5 = multiply(fact_4, 5)  // 120
  let fact_6 = multiply(fact_5, 6)  // 720
  
  assert_eq(2, fact_2)
  assert_eq(6, fact_3)
  assert_eq(24, fact_4)
  assert_eq(120, fact_5)
  assert_eq(720, fact_6)
  
  // 验证阶乘的性质：n! = n * (n-1)!
  let fact_7_verify = multiply(7, fact_6)
  assert_eq(5040, fact_7_verify)
}

test "prime_number_related_calculations" {
  // 素数相关计算测试
  // 计算前几个素数的和
  let prime_2 = 2
  let prime_3 = 3
  let prime_5 = 5
  let prime_7 = 7
  let prime_11 = 11
  let prime_13 = 13
  
  let sum_primes = add(add(add(add(add(prime_2, prime_3), prime_5), prime_7), prime_11), prime_13)
  let product_first_three = multiply(multiply(prime_2, prime_3), prime_5)
  
  assert_eq(41, sum_primes)  // 2+3+5+7+11+13 = 41
  assert_eq(30, product_first_three)  // 2*3*5 = 30
  
  // 验证素数的性质（除以小的素数）
  assert_eq(1, divide_with_ceil(prime_7, prime_2))  // 7/2 = 3.5 -> 4，但这里应该是3
  assert_eq(2, divide_with_ceil(prime_7, prime_3))  // 7/3 = 2.33 -> 3
}

test "geometric_area_calculations" {
  // 几何图形面积计算测试
  let rectangle_length = 8
  let rectangle_width = 5
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  
  let triangle_base = 10
  let triangle_height = 6
  let triangle_area = divide_with_ceil(multiply(triangle_base, triangle_height), 2)
  
  let circle_radius = 7
  let pi_approximation = 22  // 使用 22/7 作为π的近似值
  let circle_area = divide_with_ceil(multiply(pi_approximation, multiply(circle_radius, circle_radius)), 7)
  
  assert_eq(40, rectangle_area)  // 8*5 = 40
  assert_eq(30, triangle_area)   // 10*6/2 = 30
  assert_eq(154, circle_area)    // 22/7 * 7*7 = 22*7 = 154
  
  // 计算组合图形的总面积
  let total_area = add(add(rectangle_area, triangle_area), circle_area)
  assert_eq(224, total_area)  // 40 + 30 + 154 = 224
}

test "temperature_conversion_calculations" {
  // 温度转换计算测试
  let celsius_temp = 25
  let fahrenheit_temp = add(multiply(divide_with_ceil(multiply(celsius_temp, 9), 5), 32), 0)
  
  // 反向转换
  let fahrenheit_to_celsius = divide_with_ceil(multiply(fahrenheit_temp - 32, 5), 9)
  
  assert_eq(77, fahrenheit_temp)  // 25°C = 77°F
  assert_eq(25, fahrenheit_to_celsius)  // 77°F = 25°C
  
  // 测试极值温度
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(divide_with_ceil(multiply(freezing_celsius, 9), 5), 32), 0)
  
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(divide_with_ceil(multiply(boiling_celsius, 9), 5), 32), 0)
  
  assert_eq(32, freezing_fahrenheit)   // 0°C = 32°F
  assert_eq(212, boiling_fahrenheit)   // 100°C = 212°F
}

test "time_and_duration_calculations" {
  // 时间和持续时间计算测试
  let hours_per_day = 24
  let minutes_per_hour = 60
  let seconds_per_minute = 60
  
  let seconds_per_hour = multiply(minutes_per_hour, seconds_per_minute)
  let seconds_per_day = multiply(hours_per_day, seconds_per_hour)
  let minutes_per_day = multiply(hours_per_day, minutes_per_hour)
  
  assert_eq(3600, seconds_per_hour)  // 60*60 = 3600
  assert_eq(86400, seconds_per_day)  // 24*3600 = 86400
  assert_eq(1440, minutes_per_day)   // 24*60 = 1440
  
  // 计算工作周的时间
  let work_days_per_week = 5
  let work_hours_per_day = 8
  let work_hours_per_week = multiply(work_days_per_week, work_hours_per_day)
  let work_minutes_per_week = multiply(work_hours_per_week, minutes_per_hour)
  
  assert_eq(40, work_hours_per_week)    // 5*8 = 40
  assert_eq(2400, work_minutes_per_week) // 40*60 = 2400
}

test "currency_exchange_calculations" {
  // 货币兑换计算测试
  let usd_amount = 100
  let exchange_rate_usd_to_eur = 85  // 1 USD = 0.85 EUR (使用整数表示)
  let exchange_rate_usd_to_gbp = 75  // 1 USD = 0.75 GBP (使用整数表示)
  
  let eur_amount = divide_with_ceil(multiply(usd_amount, exchange_rate_usd_to_eur), 100)
  let gbp_amount = divide_with_ceil(multiply(usd_amount, exchange_rate_usd_to_gbp), 100)
  
  assert_eq(85, eur_amount)  // 100 USD = 85 EUR
  assert_eq(75, gbp_amount)  // 100 USD = 75 GBP
  
  // 计算三向兑换
  let total_in_eur = multiply(eur_amount, 100)
  let back_to_usd_via_eur = divide_with_ceil(total_in_eur, exchange_rate_usd_to_eur)
  
  assert_eq(100, back_to_usd_via_eur)  // 85 EUR = 100 USD (反向计算)
  
  // 带手续费的兑换
  let fee_percentage = 2
  let fee_amount = divide_with_ceil(multiply(usd_amount, fee_percentage), 100)
  let amount_after_fee = add(usd_amount, -fee_amount)
  let eur_after_fee = divide_with_ceil(multiply(amount_after_fee, exchange_rate_usd_to_eur), 100)
  
  assert_eq(2, fee_amount)      // 100 USD 的 2% 手续费 = 2 USD
  assert_eq(98, amount_after_fee)  // 扣除手续费后 = 98 USD
  assert_eq(84, eur_after_fee)     // 98 USD = 83.3 EUR -> 84 EUR
}

test "statistical_analysis_calculations" {
  // 统计分析计算测试
  // 数据集：[12, 15, 18, 21, 24, 27, 30]
  let data_point_1 = 12
  let data_point_2 = 15
  let data_point_3 = 18
  let data_point_4 = 21
  let data_point_5 = 24
  let data_point_6 = 27
  let data_point_7 = 30
  
  let data_count = 7
  let sum_data = add(add(add(add(add(add(data_point_1, data_point_2), data_point_3), data_point_4), data_point_5), data_point_6), data_point_7)
  let mean_value = divide_with_ceil(sum_data, data_count)
  
  assert_eq(147, sum_data)     // 12+15+18+21+24+27+30 = 147
  assert_eq(21, mean_value)    // 147/7 = 21
  
  // 计算方差（简化版本）
  let deviation_1 = data_point_1 - mean_value  // -9
  let deviation_2 = data_point_2 - mean_value  // -6
  let deviation_3 = data_point_3 - mean_value  // -3
  let deviation_4 = data_point_4 - mean_value  // 0
  let deviation_5 = data_point_5 - mean_value  // 3
  let deviation_6 = data_point_6 - mean_value  // 6
  let deviation_7 = data_point_7 - mean_value  // 9
  
  let squared_deviations_sum = add(add(add(add(add(add(multiply(deviation_1, deviation_1), multiply(deviation_2, deviation_2)), multiply(deviation_3, deviation_3)), multiply(deviation_4, deviation_4)), multiply(deviation_5, deviation_5)), multiply(deviation_6, deviation_6)), multiply(deviation_7, deviation_7))
  let variance = divide_with_ceil(squared_deviations_sum, data_count)
  
  assert_eq(189, squared_deviations_sum)  // 81+36+9+0+9+36+81 = 252
  assert_eq(36, variance)                 // 252/7 = 36
}

test "cryptography_related_calculations" {
  // 密码学相关计算测试
  // 简单的凯撒密码计算
  let original_value = 13
  let caesar_shift = 3
  let alphabet_size = 26
  
  let encrypted_value = add(original_value, caesar_shift)
  let normalized_encrypted = if encrypted_value >= alphabet_size {
    add(encrypted_value, -alphabet_size)
  } else {
    encrypted_value
  }
  
  let decrypted_value = add(normalized_encrypted, -caesar_shift)
  let normalized_decrypted = if decrypted_value < 0 {
    add(decrypted_value, alphabet_size)
  } else {
    decrypted_value
  }
  
  assert_eq(16, encrypted_value)        // 13 + 3 = 16
  assert_eq(16, normalized_encrypted)    // 16 < 26，不需要调整
  assert_eq(13, decrypted_value)         // 16 - 3 = 13
  assert_eq(13, normalized_decrypted)    // 13 >= 0，不需要调整
  
  // 简单的哈希模拟
  let input_value = 42
  let hash_multiplier = 31
  let hash_modulus = 1000
  
  let hash_value = multiply(input_value, hash_multiplier) % hash_modulus
  let second_round_hash = multiply(hash_value, hash_multiplier) % hash_modulus
  
  assert_eq(302, hash_value)        // 42 * 31 = 1302, 1302 % 1000 = 302
  assert_eq(662, second_round_hash) // 302 * 31 = 9362, 9362 % 1000 = 662
  
  // 简单的校验和计算
  let message_part_1 = 123
  let message_part_2 = 456
  let message_part_3 = 789
  
  let checksum = add(add(message_part_1, message_part_2), message_part_3) % 100
  let verify_checksum = add(add(message_part_1, message_part_2), message_part_3) % 100
  
  assert_eq(68, checksum)  // (123 + 456 + 789) % 100 = 1368 % 100 = 68
  assert_eq(checksum, verify_checksum)  // 验证校验和一致性
}

test "advanced_business_calculations" {
  // 高级业务计算测试
  // 复合利息计算
  let principal = 10000
  let annual_rate_percent = 5
  let years = 3
  
  let year_1_amount = add(principal, divide_with_ceil(multiply(principal, annual_rate_percent), 100))
  let year_2_amount = add(year_1_amount, divide_with_ceil(multiply(year_1_amount, annual_rate_percent), 100))
  let year_3_amount = add(year_2_amount, divide_with_ceil(multiply(year_2_amount, annual_rate_percent), 100))
  
  assert_eq(10500, year_1_amount)  // 10000 + 5% = 10500
  assert_eq(11025, year_2_amount)  // 10500 + 5% = 11025
  assert_eq(11576, year_3_amount)  // 11025 + 5% = 11576.25 -> 11576
  
  // 折旧计算（直线折旧法）
  let asset_cost = 50000
  let useful_life_years = 10
  let salvage_value = 5000
  
  let annual_depreciation = divide_with_ceil(add(asset_cost, -salvage_value), useful_life_years)
  let accumulated_depreciation_3_years = multiply(annual_depreciation, 3)
  let book_value_3_years = add(asset_cost, -accumulated_depreciation_3_years)
  
  assert_eq(4500, annual_depreciation)         // (50000 - 5000) / 10 = 4500
  assert_eq(13500, accumulated_depreciation_3_years)  // 4500 * 3 = 13500
  assert_eq(36500, book_value_3_years)         // 50000 - 13500 = 36500
  
  // 盈亏平衡分析
  let fixed_costs = 10000
  let variable_cost_per_unit = 25
  let selling_price_per_unit = 50
  let contribution_margin_per_unit = add(selling_price_per_unit, -variable_cost_per_unit)
  
  let break_even_units = divide_with_ceil(fixed_costs, contribution_margin_per_unit)
  let break_even_revenue = multiply(break_even_units, selling_price_per_unit)
  
  assert_eq(25, contribution_margin_per_unit)  // 50 - 25 = 25
  assert_eq(400, break_even_units)             // 10000 / 25 = 400
  assert_eq(20000, break_even_revenue)         // 400 * 50 = 20000
}
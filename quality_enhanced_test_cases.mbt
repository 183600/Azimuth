// 高质量增强测试用例 - 使用标准 MoonBit 测试语法
// 测试 azimuth 库的核心功能

test "arithmetic_sequence_validation" {
  // 测试等差数列计算
  let first_term = 3
  let common_difference = 5
  let terms_count = 6
  
  // 计算等差数列和: S = n/2 * (2a + (n-1)d)
  let double_sum = multiply(terms_count, add(multiply(2, first_term), multiply(terms_count - 1, common_difference)))
  let sequence_sum = divide_with_ceil(double_sum, 2)
  
  // 验证计算结果
  assert_eq(sequence_sum, 78)
  
  // 验证末项计算: a_n = a + (n-1)d
  let last_term = add(first_term, multiply(terms_count - 1, common_difference))
  assert_eq(last_term, 28)
}

test "financial_compound_interest" {
  // 测试复利计算
  let principal = 10000
  let annual_rate = 5  // 5%
  let compounds_per_year = 12
  
  // 简化的年复合计算
  let year1_end = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_end = add(year1_end, divide_with_ceil(multiply(year1_end, annual_rate), 100))
  let year3_end = add(year2_end, divide_with_ceil(multiply(year2_end, annual_rate), 100))
  
  // 验证复合增长
  assert_eq(year1_end, 10500)
  assert_eq(year2_end, 11025)
  assert_eq(year3_end, 11576)
  
  // 计算总收益
  let total_return = subtract(year3_end, principal)
  assert_eq(total_return, 1576)
}

test "inventory_management" {
  // 测试库存管理系统
  let initial_stock = 500
  let daily_sales = 45
  let delivery_batch = 200
  let safety_stock = 100
  let days_period = 7
  
  // 计算一周销售
  let weekly_sales = multiply(daily_sales, days_period)
  
  // 计算期末库存（假设在第4天收到补货）
  let stock_before_delivery = subtract(initial_stock, multiply(daily_sales, 3))
  let stock_after_delivery = add(stock_before_delivery, delivery_batch)
  let final_stock = subtract(stock_after_delivery, multiply(daily_sales, 4))
  
  // 计算是否需要补货
  let need_reorder = final_stock < safety_stock
  let reorder_quantity = need_reorder ? delivery_batch : 0
  
  assert_eq(weekly_sales, 315)
  assert_eq(final_stock, 265)
  assert_eq(reorder_quantity, 200)
}

test "temperature_conversion" {
  // 测试温度转换计算
  let celsius_freezing = 0
  let celsius_boiling = 100
  let celsius_room = 22
  
  // 摄氏度转华氏度: F = C * 9/5 + 32
  let fahrenheit_freezing = add(divide_with_ceil(multiply(celsius_freezing, 9), 5), 32)
  let fahrenheit_boiling = add(divide_with_ceil(multiply(celsius_boiling, 9), 5), 32)
  let fahrenheit_room = add(divide_with_ceil(multiply(celsius_room, 9), 5), 32)
  
  // 华氏度转摄氏度: C = (F - 32) * 5/9
  let back_to_celsius_room = divide_with_ceil(multiply(subtract(fahrenheit_room, 32), 5), 9)
  
  assert_eq(fahrenheit_freezing, 32)
  assert_eq(fahrenheit_boiling, 212)
  assert_eq(fahrenheit_room, 72)
  assert_eq(back_to_celsius_room, 22)
}

test "algorithmic_efficiency" {
  // 测试算法效率计算
  let dataset_size = 1000
  let linear_complexity = dataset_size
  let quadratic_complexity = multiply(dataset_size, dataset_size)
  let logarithmic_complexity = divide_with_ceil(dataset_size, 2)
  
  // 计算不同算法的执行时间（假设单位操作时间为1ms）
  let linear_time = linear_complexity
  let quadratic_time = divide_with_ceil(quadratic_complexity, 1000)  // 转换为秒
  let logarithmic_time = logarithmic_complexity
  
  // 计算性能比较
  let quadratic_vs_linear = divide_with_ceil(quadratic_time, linear_time)
  let linear_vs_logarithmic = divide_with_ceil(linear_time, logarithmic_time)
  
  assert_eq(linear_time, 1000)
  assert_eq(quadratic_time, 1000)
  assert_eq(logarithmic_time, 500)
  assert_eq(quadratic_vs_linear, 1)
  assert_eq(linear_vs_logarithmic, 2)
}

test "geometric_progression" {
  // 测试等比数列
  let first_term = 2
  let common_ratio = 3
  let terms_count = 5
  
  // 计算等比数列各项
  let term2 = multiply(first_term, common_ratio)
  let term3 = multiply(term2, common_ratio)
  let term4 = multiply(term3, common_ratio)
  let term5 = multiply(term4, common_ratio)
  
  // 计算等比数列和: S = a * (r^n - 1) / (r - 1)
  let ratio_power = multiply(multiply(multiply(common_ratio, common_ratio), common_ratio), common_ratio)
  let sum_numerator = multiply(first_term, subtract(ratio_power, 1))
  let sequence_sum = divide_with_ceil(sum_numerator, subtract(common_ratio, 1))
  
  assert_eq(term2, 6)
  assert_eq(term3, 18)
  assert_eq(term4, 54)
  assert_eq(term5, 162)
  assert_eq(sequence_sum, 242)
}

test "signal_processing" {
  // 测试信号处理计算
  let sampling_rate = 44100  // Hz
  let duration_seconds = 3
  let frequency = 440  // A4音符
  
  // 计算总采样数
  let total_samples = multiply(sampling_rate, duration_seconds)
  
  // 计算周期数
  let period_length = divide_with_ceil(sampling_rate, frequency)
  let total_periods = divide_with_ceil(total_samples, period_length)
  
  // 计算数字量化级别（16位）
  let quantization_levels = multiply(2, 16)
  let dynamic_range_db = multiply(6, 16)  // 近似值
  
  assert_eq(total_samples, 132300)
  assert_eq(period_length, 101)
  assert_eq(total_periods, 1310)
  assert_eq(quantization_levels, 32)
  assert_eq(dynamic_range_db, 96)
}

test "network_throughput" {
  // 测试网络吞吐量计算
  let file_size_mb = 250
  let network_speed_mbps = 100
  let latency_ms = 10
  let packet_overhead = 20  // %
  
  // 计算理论传输时间
  let theoretical_time_sec = divide_with_ceil(file_size_mb, network_speed_mbps)
  
  // 考虑开销的实际传输时间
  let effective_speed = multiply(network_speed_mbps, subtract(100, packet_overhead) / 100)
  let actual_time_sec = divide_with_ceil(file_size_mb, effective_speed)
  
  // 考虑延迟的总时间
  let latency_impact = divide_with_ceil(multiply(latency_ms, 1000), theoretical_time_sec * 1000)
  let total_time_sec = add(actual_time_sec, divide_with_ceil(latency_impact, 1000))
  
  assert_eq(theoretical_time_sec, 3)
  assert_eq(actual_time_sec, 4)
  assert_eq(total_time_sec, 4)
}

test "cryptography_basic" {
  // 测试基础密码学运算
  let plaintext = 42
  let key_a = 17
  let key_b = 23
  let modulus = 256
  
  // 简单的仿射密码: E(x) = (ax + b) mod m
  let encrypted_step1 = multiply(plaintext, key_a)
  let encrypted_step2 = add(encrypted_step1, key_b)
  let encrypted = encrypted_step2 % modulus
  
  // 解密过程（简化版本）
  let decrypted_step1 = subtract(encrypted, key_b)
  let decrypted_approx = divide_with_ceil(multiply(decrypted_step1, 1), key_a)
  
  // 密钥强度评估
  let key_space = multiply(modulus, modulus)
  let key_strength_bits = divide_with_ceil(key_space, 2)
  
  assert_eq(encrypted_step1, 714)
  assert_eq(encrypted_step2, 737)
  assert_eq(encrypted, 225)
  assert_eq(decrypted_step1, 202)
  assert_eq(decrypted_approx, 12)
  assert_eq(key_space, 65536)
  assert_eq(key_strength_bits, 32768)
}

test "physics_projectile_motion" {
  // 测试抛物运动计算
  let initial_velocity = 20  // m/s
  let launch_angle = 45      // degrees
  let gravity = 10           // m/s²
  let time_steps = 4
  
  // 计算水平和垂直分量
  let velocity_x = divide_with_ceil(multiply(initial_velocity, 707), 1000)  // cos(45°) ≈ 0.707
  let velocity_y = divide_with_ceil(multiply(initial_velocity, 707), 1000)  // sin(45°) ≈ 0.707
  
  // 计算飞行时间: t = 2v_y/g
  let flight_time = divide_with_ceil(multiply(2, velocity_y), gravity)
  
  // 计算最大高度: h = v_y²/(2g)
  let max_height = divide_with_ceil(multiply(velocity_y, velocity_y), multiply(2, gravity))
  
  // 计算射程: R = v_x * t
  let range = multiply(velocity_x, flight_time)
  
  // 计算不同时间点的位置
  let height_at_1s = subtract(multiply(velocity_y, 1), divide_with_ceil(multiply(gravity, multiply(1, 1)), 2))
  let height_at_2s = subtract(multiply(velocity_y, 2), divide_with_ceil(multiply(gravity, multiply(2, 2)), 2))
  
  assert_eq(velocity_x, 14)
  assert_eq(velocity_y, 14)
  assert_eq(flight_time, 3)
  assert_eq(max_height, 10)
  assert_eq(range, 42)
  assert_eq(height_at_1s, 9)
  assert_eq(height_at_2s, 8)
}
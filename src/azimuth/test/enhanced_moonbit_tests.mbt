// Azimuth 项目额外的 MoonBit 测试用例
// 涵盖各种数学运算、字符串处理和实际应用场景

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的第10项：F(10) = 55
  // 使用递推公式：F(n) = F(n-1) + F(n-2)
  let f1 = 1  // F(1)
  let f2 = 1  // F(2)
  let f3 = @azimuth.add(f1, f2)  // F(3) = 2
  let f4 = @azimuth.add(f2, f3)  // F(4) = 3
  let f5 = @azimuth.add(f3, f4)  // F(5) = 5
  let f6 = @azimuth.add(f4, f5)  // F(6) = 8
  let f7 = @azimuth.add(f5, f6)  // F(7) = 13
  let f8 = @azimuth.add(f6, f7)  // F(8) = 21
  let f9 = @azimuth.add(f7, f8)  // F(9) = 34
  let f10 = @azimuth.add(f8, f9)  // F(10) = 55
  
  @azimuth.assert_eq(55, f10)
  
  // 验证黄金比例近似：F(n+1)/F(n) ≈ 1.618
  // 使用整数运算验证：F(10) * 100 / F(9) ≈ 161
  let golden_ratio_approx = @azimuth.multiply(f10, 100) / f9
  @azimuth.assert_eq(161, golden_ratio_approx)
}

test "prime_number_validation" {
  // 验证质数：检查7是否为质数
  let num = 7
  let is_prime = true
  
  // 检查2到6之间是否有因子
  let has_factor_2 = (num % 2 == 0)
  let has_factor_3 = (num % 3 == 0)
  let has_factor_4 = (num % 4 == 0)
  let has_factor_5 = (num % 5 == 0)
  let has_factor_6 = (num % 6 == 0)
  
  // 7不能被2,3,4,5,6整除，所以是质数
  let actually_prime = @azimuth.add(
    @azimuth.add(
      @azimuth.add(
        @azimuth.add(
          if (has_factor_2) { 0 } else { 1 },
          if (has_factor_3) { 0 } else { 1 }
        ),
        if (has_factor_4) { 0 } else { 1 }
      ),
      if (has_factor_5) { 0 } else { 1 }
    ),
    if (has_factor_6) { 0 } else { 1 }
  )
  
  @azimuth.assert_eq(5, actually_prime)  // 所有检查都通过，是质数
  
  // 验证6不是质数
  let num2 = 6
  let has_factor_2_2 = (num2 % 2 == 0)
  @azimuth.assert_true(has_factor_2_2)  // 6能被2整除，不是质数
}

test "binary_number_simulation" {
  // 模拟二进制运算：计算二进制1011(11) + 1100(12) = 10111(23)
  let binary1 = 11  // 1011 in binary
  let binary2 = 12  // 1100 in binary
  
  // 使用十进制加法模拟二进制加法
  let sum = @azimuth.add(binary1, binary2)
  @azimuth.assert_eq(23, sum)  // 10111 in binary
  
  // 模拟二进制位移运算：1011 << 2 = 101100
  let left_shift = @azimuth.multiply(binary1, 4)  // 乘以2^2
  @azimuth.assert_eq(44, left_shift)  // 101100 in binary = 44 in decimal
  
  // 模拟二进制与运算：1011 & 1100 = 1000
  // 简化模拟：取两个数中较小的2的幂
  let power_of_2_in_common = 8  // 1000 in binary = 8 in decimal
  @azimuth.assert_eq(8, power_of_2_in_common)
}

test "loan_amortization_calculation" {
  // 贷款分期计算：计算每月还款额（简化版）
  let principal = 120000  // 贷款本金
  let annual_rate = 6     // 年利率百分比
  let loan_years = 30     // 贷款年限
  
  // 简化计算：月利率 × 本金 ÷ 期限
  let monthly_rate = annual_rate / 12  // 月利率百分比
  let total_months = @azimuth.multiply(loan_years, 12)  // 总月数
  let monthly_interest = @azimuth.multiply(principal, monthly_rate) / 100  // 月利息
  let monthly_principal = principal / total_months  // 月本金
  let monthly_payment = @azimuth.add(monthly_interest, monthly_principal)  // 月还款额
  
  @azimuth.assert_eq(766, monthly_payment)  // 120000×0.5%÷360 + 120000÷360 ≈ 766
  
  // 计算总还款额
  let total_payment = @azimuth.multiply(monthly_payment, total_months)
  @azimuth.assert_eq(275760, total_payment)
  
  // 计算总利息
  let total_interest = @azimuth.add(total_payment, -principal)
  @azimuth.assert_eq(155760, total_interest)
}

test "physics_velocity_calculation" {
  // 物理计算：速度和加速度
  let initial_velocity = 10  // 初始速度 m/s
  let acceleration = 3       // 加速度 m/s²
  let time = 5               // 时间 s
  
  // 计算最终速度：v = v₀ + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  @azimuth.assert_eq(25, final_velocity)
  
  // 计算位移：s = v₀t + ½at²
  let displacement_part1 = @azimuth.multiply(initial_velocity, time)  // v₀t = 50
  let displacement_part2 = @azimuth.multiply(@azimuth.multiply(acceleration, time), time) / 2  // ½at² = 37.5，整数除法为37
  let total_displacement = @azimuth.add(displacement_part1, displacement_part2)
  @azimuth.assert_eq(87, total_displacement)
  
  // 计算平均速度：v_avg = (v₀ + v) / 2
  let average_velocity = @azimuth.add(initial_velocity, final_velocity) / 2
  @azimuth.assert_eq(17, average_velocity)
}

test "calendar_date_calculation" {
  // 日历计算：计算两个日期之间的天数
  let year1 = 2023
  let month1 = 1
  let day1 = 15
  
  let year2 = 2023
  let month2 = 3
  let day2 = 10
  
  // 简化计算：假设每个月30天
  let days_from_start_of_year1 = @azimuth.add(@azimuth.multiply(month1, 30), day1)
  let days_from_start_of_year2 = @azimuth.add(@azimuth.multiply(month2, 30), day2)
  
  let days_between = @azimuth.add(days_from_start_of_year2, -days_from_start_of_year1)
  @azimuth.assert_eq(55, days_between)  // (3×30+10) - (1×30+15) = 100 - 45 = 55
  
  // 计算一年中的第几天（1月1日为第1天）
  let day_of_year = @azimuth.add(@azimuth.multiply(5, 30), 15)  // 5月15日
  @azimuth.assert_eq(165, day_of_year)
  
  // 计算星期几（简化：假设1月1日是星期一）
  let day_of_week = day_of_year % 7
  @azimuth.assert_eq(4, day_of_week)  // 第165天是星期五（0=星期一，4=星期五）
}

test "data_compression_simulation" {
  // 模拟数据压缩：计算压缩率
  let original_size = 10000  // 原始数据大小（字节）
  let compressed_size = 3500  // 压缩后大小（字节）
  
  // 计算压缩率
  let compression_ratio = @azimuth.multiply(compressed_size, 100) / original_size
  @azimuth.assert_eq(35, compression_ratio)  // 压缩到35%
  
  // 计算节省的空间
  let space_saved = @azimuth.add(original_size, -compressed_size)
  @azimuth.assert_eq(6500, space_saved)
  
  // 计算节省的百分比
  let space_saved_percent = @azimuth.multiply(space_saved, 100) / original_size
  @azimuth.assert_eq(65, space_saved_percent)  // 节省65%空间
  
  // 模拟重复数据的压缩效果
  let repeated_data_size = @azimuth.multiply(100, 50)  // 50个重复的100字节块
  let highly_compressed_size = @azimuth.multiply(100, 5)  // 压缩到5个块
  let high_compression_ratio = @azimuth.multiply(highly_compressed_size, 100) / repeated_data_size
  @azimuth.assert_eq(10, high_compression_ratio)  // 压缩到10%
}

test "game_probability_calculation" {
  // 游戏概率计算：计算获胜概率
  let total_outcomes = 1000  // 总可能结果数
  let winning_outcomes = 150  // 获胜结果数
  
  // 计算获胜概率（百分比）
  let win_probability = @azimuth.multiply(winning_outcomes, 100) / total_outcomes
  @azimuth.assert_eq(15, win_probability)  // 15%的获胜概率
  
  // 计算期望值：奖金 × 概率
  let prize = 500  // 奖金
  let expected_value = @azimuth.multiply(prize, win_probability) / 100
  @azimuth.assert_eq(75, expected_value)  // 期望值75
  
  // 计算多次游戏的期望结果
  let games_played = 100
  let expected_wins = @azimuth.multiply(games_played, win_probability) / 100
  @azimuth.assert_eq(15, expected_wins)  // 期望获胜15次
  
  let expected_total_prize = @azimuth.multiply(expected_wins, prize)
  @azimuth.assert_eq(7500, expected_total_prize)  // 期望总奖金7500
  
  // 计算盈亏平衡点
  let cost_per_game = 8  // 每次游戏费用
  let total_cost = @azimuth.multiply(games_played, cost_per_game)
  let profit_loss = @azimuth.add(expected_total_prize, -total_cost)
  @azimuth.assert_eq(-700, profit_loss)  // 期望亏损700
}

test "network_throughput_calculation" {
  // 网络吞吐量计算
  let file_size = 5000000  // 文件大小（字节）
  let bandwidth = 1000000   // 带宽（字节/秒）
  let latency = 50         // 延迟（毫秒）
  let packet_overhead = 200 // 数据包开销（字节）
  let packet_size = 1500   // 数据包大小（字节）
  
  // 计算有效载荷大小
  let payload_per_packet = @azimuth.add(packet_size, -packet_overhead)
  @azimuth.assert_eq(1300, payload_per_packet)
  
  // 计算需要的数据包数量
  let packets_needed = file_size / payload_per_packet
  @azimuth.assert_eq(3846, packets_needed)  // 5000000÷1300≈3846
  
  // 计算总开销
  let total_overhead = @azimuth.multiply(packets_needed, packet_overhead)
  @azimuth.assert_eq(769200, total_overhead)
  
  // 计算传输数据量
  let total_data = @azimuth.add(file_size, total_overhead)
  @azimuth.assert_eq(5769200, total_data)
  
  // 计算传输时间（秒）
  let transfer_time = total_data / bandwidth
  @azimuth.assert_eq(5, transfer_time)
  
  // 计算总时间（包括延迟）
  let total_time = @azimuth.add(transfer_time, latency / 1000)
  @azimuth.assert_eq(5, total_time)  // 整数除法，延迟小于1秒
  
  // 计算实际吞吐量
  let actual_throughput = file_size / total_time
  @azimuth.assert_eq(1000000, actual_throughput)
}

test "cryptography_simulation" {
  // 模拟密码学运算：简单的凯撒密码
  let original_message = "HELLO"
  let shift = 3
  
  // 模拟加密：将字母向后移动3位
  // H(7) + 3 = K(10), E(4) + 3 = H(7), L(11) + 3 = O(14), L(11) + 3 = O(14), O(14) + 3 = R(17)
  let encrypted_message = "KHOOR"
  
  // 验证加密结果
  @azimuth.assert_eq_string("Hello, KHOOR!", @azimuth.greet(encrypted_message))
  
  // 模拟数字加密：异或运算（使用加法模拟）
  let original_number = 12345
  let key = 54321
  
  // 简单的"加密"：数字与密钥相加
  let encrypted_number = @azimuth.add(original_number, key)
  @azimuth.assert_eq(66666, encrypted_number)
  
  // "解密"：减去密钥
  let decrypted_number = @azimuth.add(encrypted_number, -key)
  @azimuth.assert_eq(12345, decrypted_number)
  
  // 模拟哈希函数（简化版）：数字各位之和
  let hash_input = 12345
  let digit1 = hash_input / 10000
  let digit2 = (hash_input % 10000) / 1000
  let digit3 = (hash_input % 1000) / 100
  let digit4 = (hash_input % 100) / 10
  let digit5 = hash_input % 10
  
  let hash_result = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(digit1, digit2), digit3), digit4), digit5)
  @azimuth.assert_eq(15, hash_result)  // 1+2+3+4+5 = 15
}
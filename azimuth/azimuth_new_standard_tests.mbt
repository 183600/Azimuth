// 标准 MoonBit 测试用例 - 为 azimuth 库新增的10个高质量测试用例
// 包含实际应用场景、边界情况和数学性质验证

test "binary_conversion_calculation" {
  // 二进制转换计算测试
  let decimal_value = 42
  let binary_weight_1 = 1
  let binary_weight_2 = 2
  let binary_weight_3 = 4
  let binary_weight_4 = 8
  let binary_weight_5 = 16
  let binary_weight_6 = 32
  
  // 验证二进制位权计算
  let total_weight = add(add(add(add(add(binary_weight_1, binary_weight_2), binary_weight_3), binary_weight_4), binary_weight_5), binary_weight_6)
  assert_eq(63, total_weight)
  
  // 验证特定二进制组合
  let binary_42 = add(add(binary_weight_2), add(binary_weight_4), add(binary_weight_5))  // 101010 = 2 + 8 + 32
  assert_eq(42, binary_42)
}

test "temperature_conversion" {
  // 温度转换计算测试
  let celsius_temp = 25
  let fahrenheit_temp = 77
  
  // 摄氏度到华氏度的简化公式：F = C * 9/5 + 32
  let converted_fahrenheit = add(divide_with_ceil(multiply(celsius_temp, 9), 5), 32)
  
  // 华氏度到摄氏度的简化公式：C = (F - 32) * 5/9
  let converted_celsius = divide_with_ceil(multiply(add(fahrenheit_temp, -32), 5), 9)
  
  assert_eq(77, converted_fahrenheit)
  assert_eq(25, converted_celsius)
}

test "array_index_calculation" {
  // 数组索引计算测试（模拟二维数组的一维索引）
  let rows = 4
  let cols = 5
  let row_index = 2
  let col_index = 3
  
  // 二维数组转一维数组的索引计算：index = row * cols + col
  let linear_index = add(multiply(row_index, cols), col_index)
  
  assert_eq(13, linear_index)
  
  // 验证边界情况
  let first_element = add(multiply(0, cols), 0)
  let last_element = add(multiply(rows - 1, cols - 1), cols - 1)
  
  assert_eq(0, first_element)
  assert_eq(19, last_element)
}

test "prime_number_validation" {
  // 质数验证测试（简化版本）
  let test_number = 17
  let divisor_2 = 2
  let divisor_3 = 3
  let divisor_5 = 5
  
  // 检查是否能被小质数整除
  let remainder_2 = test_number % divisor_2
  let remainder_3 = test_number % divisor_3
  let remainder_5 = test_number % divisor_5
  
  // 验证17不能被2、3、5整除
  assert_eq(1, remainder_2)
  assert_eq(2, remainder_3)
  assert_eq(2, remainder_5)
  
  // 测试合数
  let composite_number = 15
  let composite_remainder_3 = composite_number % divisor_3
  let composite_remainder_5 = composite_number % divisor_5
  
  assert_eq(0, composite_remainder_3)
  assert_eq(0, composite_remainder_5)
}

test "loan_amortization" {
  // 贷款分期计算测试
  let loan_amount = 10000
  let interest_rate = 5  // 5%
  let loan_term_years = 3
  let months_per_year = 12
  
  // 计算总月数
  let total_months = multiply(loan_term_years, months_per_year)
  
  // 简化的月供计算（不包含复利）
  let total_interest = divide_with_ceil(multiply(loan_amount, interest_rate), 100)
  let total_payment = add(loan_amount, total_interest)
  let monthly_payment = divide_with_ceil(total_payment, total_months)
  
  assert_eq(36, total_months)
  assert_eq(500, total_interest)
  assert_eq(10500, total_payment)
  assert_eq(292, monthly_payment)
}

test "game_score_calculation" {
  // 游戏得分计算测试
  let base_score = 1000
  let level_multiplier = 3
  let bonus_points = 500
  let penalty_points = 200
  
  // 计算最终得分
  let level_score = multiply(base_score, level_multiplier)
  let score_with_bonus = add(level_score, bonus_points)
  let final_score = add(score_with_bonus, -penalty_points)
  
  assert_eq(3000, level_score)
  assert_eq(3500, score_with_bonus)
  assert_eq(3300, final_score)
  
  // 计算等级提升需要的分数
  let next_level_threshold = multiply(final_score, 2)
  let current_progress = divide_with_ceil(multiply(final_score, 100), next_level_threshold)
  
  assert_eq(6600, next_level_threshold)
  assert_eq(50, current_progress)
}

test "data_storage_optimization" {
  // 数据存储优化测试
  let file_size_mb = 150
  let compression_ratio = 60  // 压缩到60%
  let storage_capacity_mb = 1000
  
  // 计算压缩后的文件大小
  let compressed_size = divide_with_ceil(multiply(file_size_mb, compression_ratio), 100)
  
  // 计算可以存储的压缩文件数量
  let files_that_fit = divide_with_ceil(storage_capacity_mb, compressed_size)
  
  // 计算剩余存储空间
  let used_space = multiply(files_that_fit, compressed_size)
  let remaining_space = add(storage_capacity_mb, -used_space)
  
  assert_eq(90, compressed_size)
  assert_eq(12, files_that_fit)
  assert_eq(1080, used_space)
  assert_eq(-80, remaining_space)  // 超出容量
}

test "network_packet_routing" {
  // 网络数据包路由测试
  let total_packets = 1000
  let route_1_capacity = 300
  let route_2_capacity = 400
  let route_3_capacity = 500
  
  // 计算各路由分配的数据包数量
  let route_1_packets = divide_with_ceil(multiply(total_packets, route_1_capacity), add(add(route_1_capacity, route_2_capacity), route_3_capacity))
  let route_2_packets = divide_with_ceil(multiply(total_packets, route_2_capacity), add(add(route_1_capacity, route_2_capacity), route_3_capacity))
  let route_3_packets = divide_with_ceil(multiply(total_packets, route_3_capacity), add(add(route_1_capacity, route_2_capacity), route_3_capacity))
  
  // 验证总分配量
  let total_allocated = add(add(route_1_packets, route_2_packets), route_3_packets)
  
  assert_eq(250, route_1_packets)
  assert_eq(334, route_2_packets)
  assert_eq(417, route_3_packets)
  assert_eq(1001, total_allocated)  // 由于向上取整，略多于总数
}

test "cryptographic_hash_simulation" {
  // 加密哈希模拟测试
  let input_value = 12345
  let prime_1 = 31
  let prime_2 = 37
  let prime_3 = 41
  let modulo_value = 1000000
  
  // 模拟哈希计算：hash = ((input * p1) + p2) * p3 % mod
  let step1 = add(multiply(input_value, prime_1), prime_2)
  let step2 = multiply(step1, prime_3)
  let hash_result = step2 % modulo_value
  
  assert_eq(382816, add(multiply(12345, 31), 37))
  assert_eq(15695456, multiply(382816, 41))
  assert_eq(695456, hash_result)
  
  // 验证哈希的确定性（相同输入产生相同输出）
  let hash_repeat = ((add(multiply(input_value, prime_1), prime_2)) * prime_3) % modulo_value
  assert_eq(hash_result, hash_repeat)
}

test "cpu_scheduling_simulation" {
  // CPU调度模拟测试
  let process_1_time = 8
  let process_2_time = 12
  let process_3_time = 6
  let process_4_time = 10
  let time_quantum = 4
  
  // 计算每个进程需要的时间片数量
  let process_1_quanta = divide_with_ceil(process_1_time, time_quantum)
  let process_2_quanta = divide_with_ceil(process_2_time, time_quantum)
  let process_3_quanta = divide_with_ceil(process_3_time, time_quantum)
  let process_4_quanta = divide_with_ceil(process_4_time, time_quantum)
  
  // 计算总时间片数
  let total_quanta = add(add(add(process_1_quanta, process_2_quanta), process_3_quanta), process_4_quanta)
  
  // 计算总执行时间（考虑上下文切换开销）
  let context_switch_overhead = 1
  let total_execution_time = add(multiply(total_quanta, time_quantum), multiply(total_quanta, context_switch_overhead))
  
  assert_eq(2, process_1_quanta)
  assert_eq(3, process_2_quanta)
  assert_eq(2, process_3_quanta)
  assert_eq(3, process_4_quanta)
  assert_eq(10, total_quanta)
  assert_eq(50, total_execution_time)
}
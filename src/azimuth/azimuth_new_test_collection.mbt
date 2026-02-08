// Azimuth 新测试集合
// 包含8个高质量的 MoonBit 测试用例，使用标准测试语法

test "prime_number_analysis" {
  // 质数分析测试
  // 计算前5个质数的和与积
  let prime1 = 2
  let prime2 = 3
  let prime3 = 5
  let prime4 = 7
  let prime5 = 11
  
  // 计算质数和
  let prime_sum = add(add(add(add(prime1, prime2), prime3), prime4), prime5)
  
  // 计算质数积
  let prime_product = multiply(multiply(multiply(multiply(prime1, prime2), prime3), prime4), prime5)
  
  // 计算平均值（向上取整）
  let prime_count = 5
  let prime_average = divide_with_ceil(prime_sum, prime_count)
  
  assert_eq(prime_sum, 28)
  assert_eq(prime_product, 2310)
  assert_eq(prime_average, 6)
}

test "time_zone_conversion" {
  // 时区转换测试
  let local_time = 14  // 下午2点
  
  // 转换到不同时区
  let utc_time = subtract(local_time, 8)  // UTC+8转UTC
  let est_time = subtract(utc_time, 5)    // UTC转EST (UTC-5)
  let pst_time = subtract(utc_time, 8)    // UTC转PST (UTC-8)
  
  // 处理负时间（前一天）
  let adjusted_utc = divide_with_ceil(utc_time + 24, 24) % 24
  let adjusted_est = divide_with_ceil(est_time + 24, 24) % 24
  let adjusted_pst = divide_with_ceil(pst_time + 24, 24) % 24
  
  assert_eq(utc_time, 6)
  assert_eq(est_time, 1)
  assert_eq(pst_time, -2)
  assert_eq(adjusted_utc, 6)
  assert_eq(adjusted_est, 1)
  assert_eq(adjusted_pst, 22)
}

test "inventory_optimization" {
  // 库存优化测试
  let warehouse_capacity = 1000
  let item_a_weight = 15
  let item_b_weight = 25
  let item_c_weight = 30
  
  // 计算不同组合的总重量
  let combo1 = add(add(multiply(20, item_a_weight), multiply(15, item_b_weight)), multiply(10, item_c_weight))
  let combo2 = add(add(multiply(25, item_a_weight), multiply(10, item_b_weight)), multiply(15, item_c_weight))
  let combo3 = add(add(multiply(15, item_a_weight), multiply(20, item_b_weight)), multiply(12, item_c_weight))
  
  // 计算最优组合的利用率
  let optimal_weight = combo1
  let utilization = divide_with_ceil(multiply(optimal_weight, 100), warehouse_capacity)
  
  // 计算剩余容量
  let remaining_capacity = subtract(warehouse_capacity, optimal_weight)
  
  assert_eq(combo1, 825)
  assert_eq(combo2, 900)
  assert_eq(combo3, 910)
  assert_eq(optimal_weight, 825)
  assert_eq(utilization, 83)
  assert_eq(remaining_capacity, 175)
}

test "signal_processing" {
  // 信号处理测试
  // 模拟数字信号处理中的采样和量化
  
  let original_signal = 3.7  // 原始信号值
  let sampling_rate = 44100
  let bit_depth = 16
  
  // 量化过程（简化）
  let quantized_value = divide_with_ceil(multiply(original_signal, 100), 1)
  let quantized_levels = multiply(2, bit_depth)
  
  // 计算采样数据量（每秒）
  let bytes_per_sample = divide_with_ceil(bit_depth, 8)
  let bytes_per_second = multiply(sampling_rate, bytes_per_sample)
  
  // 计算1分钟的存储需求
  let minute_storage = multiply(bytes_per_second, 60)
  
  assert_eq(quantized_value, 370)
  assert_eq(quantized_levels, 32)
  assert_eq(bytes_per_sample, 2)
  assert_eq(bytes_per_second, 88200)
  assert_eq(minute_storage, 5292000)
}

test "cryptography_basics" {
  // 密码学基础测试
  let message = 42
  let key = 7
  let modulus = 101
  
  // 简单的模运算加密
  let encrypted = multiply(message, key) % modulus
  
  // 解密（使用模逆元，这里简化处理）
  let inverse_key = 43  // 7的模逆元（模101）
  let decrypted = multiply(encrypted, inverse_key) % modulus
  
  // 验证加密强度
  let key_space = modulus
  let brute_force_attempts = divide_with_ceil(key_space, 2)
  
  assert_eq(encrypted, 91)
  assert_eq(decrypted, 42)
  assert_eq(key_space, 101)
  assert_eq(brute_force_attempts, 51)
}

test "graph_theory_paths" {
  // 图论路径测试
  // 简单无向图：A-B-C-D，边权重分别为3,5,2
  
  let edge_ab = 3
  let edge_bc = 5
  let edge_cd = 2
  
  // 计算不同路径的长度
  let path_a_b = edge_ab
  let path_a_c = add(edge_ab, edge_bc)
  let path_a_d = add(add(edge_ab, edge_bc), edge_cd)
  let path_b_d = add(edge_bc, edge_cd)
  
  // 计算最短路径的平均值
  let total_paths = add(add(add(path_a_b, path_a_c), path_a_d), path_b_d)
  let path_count = 4
  let average_path = divide_with_ceil(total_paths, path_count)
  
  assert_eq(path_a_b, 3)
  assert_eq(path_a_c, 8)
  assert_eq(path_a_d, 10)
  assert_eq(path_b_d, 7)
  assert_eq(total_paths, 28)
  assert_eq(average_path, 7)
}

test "statistical_variance" {
  // 统计方差计算测试
  // 数据集：[4, 8, 15, 16, 23, 42]
  
  let data1 = 4
  let data2 = 8
  let data3 = 15
  let data4 = 16
  let data5 = 23
  let data6 = 42
  
  // 计算平均值
  let data_sum = add(add(add(add(add(data1, data2), data3), data4), data5), data6)
  let data_count = 6
  let mean = divide_with_ceil(data_sum, data_count)
  
  // 计算离差平方和（简化）
  let diff1 = subtract(data1, mean)
  let diff2 = subtract(data2, mean)
  let diff3 = subtract(data3, mean)
  let variance_sum = add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), multiply(diff3, diff3))
  
  // 计算方差（简化）
  let variance = divide_with_ceil(variance_sum, data_count)
  
  assert_eq(data_sum, 108)
  assert_eq(mean, 18)
  assert_eq(diff1, -14)
  assert_eq(diff2, -10)
  assert_eq(diff3, -3)
  assert_eq(variance_sum, 305)
  assert_eq(variance, 51)
}

test "physics_projectile" {
  // 物理抛体运动测试
  let initial_velocity = 20
  let gravity = 10
  let angle_degrees = 45
  
  // 计算水平和垂直分量（简化）
  let vx = divide_with_ceil(multiply(initial_velocity, 707), 1000)  // cos(45°) ≈ 0.707
  let vy = divide_with_ceil(multiply(initial_velocity, 707), 1000)  // sin(45°) ≈ 0.707
  
  // 计算飞行时间
  let flight_time = divide_with_ceil(multiply(2, vy), gravity)
  
  // 计算最大高度
  let max_height = divide_with_ceil(multiply(vy, vy), multiply(2, gravity))
  
  // 计算射程
  let range = multiply(vx, flight_time)
  
  assert_eq(vx, 14)
  assert_eq(vy, 14)
  assert_eq(flight_time, 3)
  assert_eq(max_height, 10)
  assert_eq(range, 42)
}
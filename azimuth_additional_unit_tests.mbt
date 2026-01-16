// Azimuth 项目额外单元测试
// 使用标准 MoonBit 测试语法，提供新的测试场景

test "array_index_calculations" {
  // 数组索引计算测试
  
  // 二维数组线性化索引: row * width + column
  let row = 3
  let column = 4
  let width = 10
  let linear_index = azimuth::add(azimuth::multiply(row, width), column)
  assert_eq(34, linear_index)
  
  // 三维数组线性化索引: ((depth * height + row) * width + column)
  let depth = 2
  let height = 5
  let three_d_index = azimuth::multiply(azimuth::add(azimuth::multiply(depth, height), row), width)
  three_d_index = azimuth::add(three_d_index, column)
  assert_eq(134, three_d_index)
}

test "recursive_sequence_tests" {
  // 递归序列测试
  
  // 平方数序列: 1, 4, 9, 16, 25...
  let n1 = 1
  let n2 = azimuth::multiply(2, 2)  // 4
  let n3 = azimuth::multiply(3, 3)  // 9
  let n4 = azimuth::multiply(4, 4)  // 16
  let n5 = azimuth::multiply(5, 5)  // 25
  
  // 验证平方数序列的和: 1 + 4 + 9 + 16 + 25 = 55
  let square_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(n1, n2), n3), n4), n5)
  assert_eq(55, square_sum)
  
  // 立方数序列: 1, 8, 27, 64...
  let c1 = azimuth::multiply(azimuth::multiply(1, 1), 1)  // 1
  let c2 = azimuth::multiply(azimuth::multiply(2, 2), 2)  // 8
  let c3 = azimuth::multiply(azimuth::multiply(3, 3), 3)  // 27
  
  // 验证立方数序列的和: 1 + 8 + 27 = 36
  let cube_sum = azimuth::add(azimuth::add(c1, c2), c3)
  assert_eq(36, cube_sum)
}

test "prime_number_calculations" {
  // 质数相关计算测试
  
  // 计算前5个质数的和: 2 + 3 + 5 + 7 + 11 = 28
  let prime1 = 2
  let prime2 = 3
  let prime3 = 5
  let prime4 = 7
  let prime5 = 11
  
  let prime_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(prime1, prime2), prime3), prime4), prime5)
  assert_eq(28, prime_sum)
  
  // 计算质数的乘积: 2 * 3 * 5 = 30
  let prime_product = azimuth::multiply(azimuth::multiply(prime1, prime2), prime3)
  assert_eq(30, prime_product)
  
  // 验证哥德巴赫猜想的一个例子: 10 = 3 + 7
  let even_number = 10
  let goldbach_sum = azimuth::add(prime2, prime4)  // 3 + 7
  assert_eq(even_number, goldbach_sum)
}

test "percentage_calculations" {
  // 百分比计算测试
  
  // 计算百分比: 部分 / 总数 * 100
  let part = 25
  let total = 200
  let percentage = azimuth::multiply(azimuth::multiply(part, 100), 1) / total
  assert_eq(12, percentage)  // 25/200 = 12.5%，取整数部分
  
  // 计算增长百分比: (新值 - 旧值) / 旧值 * 100
  let old_value = 80
  let new_value = 120
  let increase = azimuth::add(new_value, -old_value)
  let growth_percentage = azimuth::multiply(azimuth::multiply(increase, 100), 1) / old_value
  assert_eq(50, growth_percentage)  // (120-80)/80 = 50%
  
  // 计算折扣金额: 原价 * 折扣率 / 100
  let original_price = 500
  let discount_rate = 15
  let discount_amount = azimuth::multiply(azimuth::multiply(original_price, discount_rate), 1) / 100
  let final_price = azimuth::add(original_price, -discount_amount)
  assert_eq(425, final_price)
}

test "speed_distance_time_calculations" {
  // 速度、距离、时间计算测试
  
  // 计算距离: 速度 * 时间
  let speed = 60  // km/h
  let time = 2.5  // 小时，使用整数近似: 2.5 ≈ 5/2
  let distance = azimuth::multiply(azimuth::multiply(speed, 5), 1) / 2
  assert_eq(150, distance)  // 60 * 2.5 = 150 km
  
  // 计算平均速度: 总距离 / 总时间
  let total_distance = 300
  let total_time = 4
  let average_speed = azimuth::multiply(total_distance, 1) / total_time
  assert_eq(75, average_speed)
  
  // 计算相对速度: 同向运动，v_rel = v1 - v2
  let speed1 = 80
  let speed2 = 50
  let relative_speed = azimuth::add(speed1, -speed2)
  assert_eq(30, relative_speed)
}

test "data_storage_calculations" {
  // 数据存储计算测试
  
  // 计算文件总大小: 文件数量 * 平均文件大小
  let file_count = 1000
  let avg_file_size = 2048  // bytes
  let total_size = azimuth::multiply(file_count, avg_file_size)
  assert_eq(2048000, total_size)
  
  // 转换为KB: 总字节数 / 1024
  let total_kb = azimuth::multiply(total_size, 1) / 1024
  assert_eq(2000, total_kb)
  
  // 转换为MB: 总KB数 / 1024
  let total_mb = azimuth::multiply(total_kb, 1) / 1024
  assert_eq(1, total_mb)  // 取整数部分
  
  // 计算存储利用率: 已用空间 / 总空间 * 100
  let used_space = 750
  let total_space = 1000
  let utilization = azimuth::multiply(azimuth::multiply(used_space, 100), 1) / total_space
  assert_eq(75, utilization)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  
  // O(1) 常数时间: 总是返回1
  let constant_time_ops = 1
  assert_eq(1, constant_time_ops)
  
  // O(n) 线性时间: 与输入大小成正比
  let n = 100
  let linear_time_ops = azimuth::multiply(n, 1)
  assert_eq(100, linear_time_ops)
  
  // O(n²) 二次时间: 与输入大小的平方成正比
  let quadratic_time_ops = azimuth::multiply(n, n)
  assert_eq(10000, quadratic_time_ops)
  
  // O(log n) 对数时间: 使用整数近似
  // log2(1024) = 10
  let log_input = 1024
  let log_time_ops = 10  // 近似值
  assert_eq(10, log_time_ops)
  
  // O(n log n) 线性对数时间
  let n_log_n_ops = azimuth::multiply(n, log_time_ops)
  assert_eq(1000, n_log_n_ops)
}

test "physics_formula_calculations" {
  // 物理公式计算测试
  
  // 计算动能: KE = 1/2 * m * v²
  let mass = 10  // kg
  let velocity = 5  // m/s
  let kinetic_energy = azimuth::multiply(azimuth::multiply(mass, azimuth::multiply(velocity, velocity)), 1) / 2
  assert_eq(125, kinetic_energy)  // 0.5 * 10 * 25 = 125 J
  
  // 计算势能: PE = m * g * h
  let height = 20  // m
  let gravity = 10  // m/s² (简化值)
  let potential_energy = azimuth::multiply(azimuth::multiply(mass, gravity), height)
  assert_eq(2000, potential_energy)  // 10 * 10 * 20 = 2000 J
  
  // 计算总机械能: KE + PE
  let total_mechanical_energy = azimuth::add(kinetic_energy, potential_energy)
  assert_eq(2125, total_mechanical_energy)
  
  // 计算功率: P = W / t
  let work = 5000  // J
  let time = 10  // s
  let power = azimuth::multiply(work, 1) / time
  assert_eq(500, power)  // 5000 / 10 = 500 W
}

test "probability_calculations" {
  // 概率计算测试
  
  // 计算组合数 C(n,k) = n! / (k! * (n-k)!)
  // 简化计算: C(5,2) = (5 * 4) / (2 * 1) = 10
  let n = 5
  let k = 2
  let combinations = azimuth::multiply(azimuth::multiply(n, azimuth::add(n, -1)), 1) / azimuth::multiply(k, azimuth::add(k, -1))
  assert_eq(10, combinations)
  
  // 计算排列数 P(n,k) = n! / (n-k)!
  // 简化计算: P(5,3) = 5 * 4 * 3 = 60
  let permutations = azimuth::multiply(azimuth::multiply(n, azimuth::add(n, -1)), azimuth::add(n, -2))
  assert_eq(60, permutations)
  
  // 计算概率: 有利结果数 / 总结果数
  let favorable_outcomes = 3
  let total_outcomes = 8
  let probability = azimuth::multiply(azimuth::multiply(favorable_outcomes, 100), 1) / total_outcomes
  assert_eq(37, probability)  // 3/8 = 37.5%，取整数部分
}

test "number_system_conversions" {
  // 数制转换测试
  
  // 十进制转二进制位值计算
  // 13 = 1*2³ + 1*2² + 0*2¹ + 1*2⁰ = 8 + 4 + 0 + 1
  let decimal_num = 13
  let bit3 = azimuth::multiply(1, azimuth::multiply(2, azimuth::multiply(2, 2)))  // 1*2³ = 8
  let bit2 = azimuth::multiply(1, azimuth::multiply(2, 2))  // 1*2² = 4
  let bit1 = azimuth::multiply(0, 2)  // 0*2¹ = 0
  let bit0 = azimuth::multiply(1, 1)  // 1*2⁰ = 1
  
  let binary_calculation = azimuth::add(azimuth::add(azimuth::add(bit3, bit2), bit1), bit0)
  assert_eq(decimal_num, binary_calculation)
  
  // 十进制转十六进制计算
  // 255 = 15*16¹ + 15*16⁰ = 240 + 15 = FF
  let decimal_255 = 255
  let hex_high = azimuth::multiply(15, 16)  // 15*16¹ = 240
  let hex_low = azimuth::multiply(15, 1)    // 15*16⁰ = 15
  let hex_calculation = azimuth::add(hex_high, hex_low)
  assert_eq(decimal_255, hex_calculation)
  
  // 十进制转八进制计算
  // 64 = 1*8² + 0*8¹ + 0*8⁰ = 64 + 0 + 0 = 100
  let decimal_64 = 64
  let oct_64 = azimuth::multiply(1, azimuth::multiply(8, 8))  // 1*8² = 64
  assert_eq(decimal_64, oct_64)
}